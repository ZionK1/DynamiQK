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

  val io = IO(new Bundle {
    val patternFlag = Input(PatternType())
    val enable      = Input(Bool())
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

  // defaults
  io.qkOut.foreach(_.foreach(_ := 0.S(sumW.W)))
  io.valid := false.B

  when(io.enable) {
    switch(io.patternFlag) {

      /* ─── Grid: skip non-aligned rows/cols ─── */
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

      /* ─── NoFlag: dense multiply ─── */
      is(PatternType.NoFlag) {
        for (i <- 0 until BLOCK_M; j <- 0 until BLOCK_N)
          io.qkOut(i)(j) :=
            dot(io.qIn(i), VecInit.tabulate(D)(d => io.kIn(d)(j)))
        io.valid := true.B
      }

      /* placeholders */
      is(PatternType.NoBoundary)    { io.valid := true.B }
      is(PatternType.KBoundary)     { io.valid := true.B }
      is(PatternType.QBoundary)     { io.valid := true.B }
      is(PatternType.TwoDBoundary)  { io.valid := true.B }
      is(PatternType.AShape)        { io.valid := true.B }
      is(PatternType.VerticalSlash) { io.valid := true.B }
    }
  }
}
