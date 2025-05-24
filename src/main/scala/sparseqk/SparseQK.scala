package sparseqk

import chisel3._
import chisel3.util._
import chisel3.ChiselEnum   // non-deprecated

/** Attention–head sparsity patterns. */
object PatternType extends ChiselEnum {
  val Grid, NoFlag,
  NoBoundary, KBoundary, QBoundary,
  TwoDBoundary, AShape, VerticalSlash = Value
}

/** Grid-aware Q × Kᵀ tile multiplier. */
class SparseQK(
                val BLOCK_M:  Int,
                val BLOCK_N:  Int,
                val D:        Int,
                val dataWidth: Int = 16,
                val stride:    Int = 4,
                val phase:     Int = 0) extends Module {

  private val prodW = dataWidth * 2
  private val sumW  = prodW + log2Ceil(D)   // + guard bits
  private val normW = sumW + 4 // extra bits for normalization accuracy

  val io = IO(new Bundle {
    val patternFlag = Input(PatternType())
    val enable      = Input(Bool())
    val done        = Input(Bool()) // to flush accumulated results
    val qIn  = Input(Vec(BLOCK_M, Vec(D,        SInt(dataWidth.W))))
    val kIn  = Input(Vec(D,        Vec(BLOCK_N, SInt(dataWidth.W))))
    val qkOut = Output(Vec(BLOCK_M, Vec(BLOCK_N, SInt(sumW.W))))
    val valid = Output(Bool())
  })

  /* dot product (combinational) */
  private def dot(q: Vec[SInt], k: Vec[SInt]): SInt = {
    val partials = Wire(Vec(D, SInt(prodW.W)))
    for (i <- 0 until D) partials(i) := q(i) * k(i)
    partials.reduceTree(_ +& _).asSInt
  }

  // state registers to accumulate over cycles
  val o = RegInit(VecInit(Seq.fill(BLOCK_M)(VecInit(Seq.fill(BLOCK_N)(0.S(sumW.W))))))
  val m = RegInit(VecInit(Seq.fill(BLOCK_M)(0.S(sumW.W))))
  val l = RegInit(VecInit(Seq.fill(BLOCK_M)(1.S(normW.W))))

  io.qkOut.foreach(_.foreach(_ := 0.S(sumW.W)))
  io.valid := false.B

  when(io.enable) {
    switch(io.patternFlag) {

      is(PatternType.Grid) {
        for (i <- 0 until BLOCK_M; j <- 0 until BLOCK_N) {
          val hit = (i % stride == phase) && (j % stride == phase)
          when(hit.B) {
            io.qkOut(i)(j) :=
              dot(io.qIn(i), VecInit.tabulate(D)(d => io.kIn(d)(j)))
          }
        }
        io.valid := true.B
      }

      is(PatternType.NoFlag) {
        for (i <- 0 until BLOCK_M; j <- 0 until BLOCK_N)
          io.qkOut(i)(j) :=
            dot(io.qIn(i), VecInit.tabulate(D)(d => io.kIn(d)(j)))
        io.valid := true.B
      }

      is(PatternType.AShape) {
        val nInit = 4
        val nLocal = stride
        val scores = Wire(Vec(BLOCK_M, Vec(BLOCK_N, SInt(sumW.W))))
        val maxTmp = Wire(Vec(BLOCK_M, SInt(sumW.W)))        
        val sumExpTmp = Wire(Vec(BLOCK_M, SInt(normW.W)))
        val outMat = Wire(Vec(BLOCK_M, Vec(BLOCK_N, SInt(sumW.W))))

        for (i <- 0 until BLOCK_M) {
          val localMaxInit = m(i)
          val localMaxVal = (0 until BLOCK_N).map { j =>
            val localHit = (j >= (i - nLocal)) && (j <= i)
            val initHit  = (j < nInit)
            val valid = localHit || initHit
            val kVec = VecInit.tabulate(D)(d => io.kIn(d)(j))
            val dotVal = Mux(valid.B, dot(io.qIn(i), kVec), 0.S(sumW.W))
            io.qkOut(i)(j) := dotVal
            scores(i)(j) := dotVal
            dotVal
          }.fold(localMaxInit)((acc, value) => Mux(value > acc, value, acc))

          maxTmp(i) := localMaxVal
        }

        

         for (i <- 0 until BLOCK_M) {
          val sumVec = (0 until BLOCK_N).map { j =>
            val shifted = scores(i)(j) - maxTmp(i)
            val shiftAmt = (shifted +& 4.S).asUInt.apply(4, 0)
            val expApprox: SInt = Mux(shifted > (-4).S, (1.U << shiftAmt).asSInt, 1.S)
            outMat(i)(j) := expApprox
            expApprox
          }
          sumExpTmp(i) := sumVec.reduce(_ +& _)
        }

        for (i <- 0 until BLOCK_M) {
          m(i) := maxTmp(i)
          l(i) := l(i) + sumExpTmp(i)
          for (j <- 0 until BLOCK_N) {
            val localHit = (j >= (i - nLocal)) && (j <= i)
            val initHit  = (j < nInit)
            val valid = localHit || initHit
            when(valid.B){
              o(i)(j) := o(i)(j) + ((outMat(i)(j) << 8) / sumExpTmp(i))
            }
          }
        }
      }


      is(PatternType.VerticalSlash) {
      val step = stride
      val scores = Wire(Vec(BLOCK_M, Vec(BLOCK_N, SInt(sumW.W))))
      val maxTmp = Wire(Vec(BLOCK_M, SInt(sumW.W)))
      val outMat = Wire(Vec(BLOCK_M, Vec(BLOCK_N, SInt(sumW.W))))

      val sumExpTmp = Wire(Vec(BLOCK_M, SInt(normW.W)))
      for (i <- 0 until BLOCK_M) sumExpTmp(i) := 0.S

      for (i <- 0 until BLOCK_M) {
        val localMax = (0 until BLOCK_N).map { j =>
          val verticalHit = (j % step == phase)
          val kVec = VecInit.tabulate(D)(d => io.kIn(d)(j))
          val dotVal = Mux(verticalHit.B, dot(io.qIn(i), kVec), 0.S(sumW.W))
          io.qkOut(i)(j) := dotVal
          scores(i)(j) := dotVal
          dotVal
        }.fold(m(i))((acc, value) => Mux(value > acc, value, acc))

        maxTmp(i) := localMax
      }

      for (i <- 0 until BLOCK_M) {
        val sumVec = (0 until BLOCK_N).map { j =>
          val shifted = scores(i)(j) - maxTmp(i)
          val shiftAmt = (shifted +& 4.S).asUInt.apply(4, 0)
          val expApprox: SInt = Mux(shifted > (-4).S, (1.U << shiftAmt).asSInt, 1.S)
          outMat(i)(j) := expApprox
          expApprox
        }
        sumExpTmp(i) := sumVec.reduce(_ +& _)
      }

      for (i <- 0 until BLOCK_M) {
        m(i) := maxTmp(i)
        l(i) := l(i) + sumExpTmp(i)
        for (j <- 0 until BLOCK_N) {
          val verticalHit = (j % step == phase)
          when(verticalHit.B){
            o(i)(j) := o(i)(j) + ((outMat(i)(j) << 8) / sumExpTmp(i))
          }
        }
      }
    }

      is(PatternType.NoBoundary)    { io.valid := true.B }
      is(PatternType.KBoundary)     { io.valid := true.B }
      is(PatternType.QBoundary)     { io.valid := true.B }
      is(PatternType.TwoDBoundary)  { io.valid := true.B }
    }
  }

  when(io.done) {
    io.valid := true.B
  }
}
