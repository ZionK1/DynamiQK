package sparseqk

import chisel3._
import chisel3.util._

/** Detects a causal “A-shape” sparsity pattern *without* constructing the full
  * attention matrix.  The pattern is derived solely from:
  *   – the per-row activity mask,  rActive(i) : OR₍d₎  (Q(i,d) ≠ 0)
  *   – the per-column activity mask, cActive(j) : OR₍d₎ (K(d,j) ≠ 0)
  *   – the activity vector of the **last row** (i = BLOCK_M-1) restricted to
  *     the lower triangle (j ≤ i).
  *
  * `strideGuess` = contiguous 1’s from j = 0 until first 0 on last row  
  * `initGuess`   = contiguous 1’s from j = BLOCK_N-1 downwards until first 0
  *
  * A cell (i,j) is **allowed** iff
  * {{{
  *      j <  strideGuess                       //   vertical strip  OR
  *   || j >= max(0, i - initGuess + 1)         //   causal band
  * }}}
  */
class AShapePatternDetector(
  val BLOCK_M:   Int,
  val BLOCK_N:   Int,
  val D:         Int,
  val maxStride: Int,
  val maxInit:   Int,
  val dataWidth: Int = 8
) extends Module {

  private val SCWidth = log2Ceil(maxStride + 1)           // for strideGuess
  private val ICWidth = math.max(1, log2Ceil(maxInit + 1))// for initGuess

  val io = IO(new Bundle {
    val qIn = Input(Vec(BLOCK_M, Vec(D, SInt(dataWidth.W))))
    val kIn = Input(Vec(D,         Vec(BLOCK_N, SInt(dataWidth.W))))

    val isAShape    = Output(Bool())
    val strideGuess = Output(UInt(SCWidth.W))
    val initGuess   = Output(UInt(ICWidth.W))
    val dbg_barWidth    = Output(UInt(8.W))
    val dbg_barWidthLim = Output(UInt(8.W))
    val dbg_lastRowActive = Output(Vec(BLOCK_N, Bool()))
  })

val lastRow          = BLOCK_M - 1
val lastRowQMask     = Wire(Vec(D, Bool()))              // 1 ⇒ Q(lastRow,d) ≠ 0
val lastRowProdMask  = Wire(Vec(BLOCK_N, Bool()))        // 1 ⇒ Σ Q·K ≠ 0

for (d <- 0 until D)
  lastRowQMask(d) := (io.qIn(lastRow)(d) =/= 0.S)

for (j <- 0 until BLOCK_N) {
  var acc: Bool = false.B
  for (d <- 0 until D) {
    val termNZ = lastRowQMask(d) && (io.kIn(d)(j) =/= 0.S)
    acc        = acc || termNZ
  }
  lastRowProdMask(j) := acc          
}
io.dbg_lastRowActive := lastRowProdMask

val lastRowBits   = lastRowProdMask.asUInt                

val inverted      = ~lastRowBits & ((BigInt(1) << BLOCK_N) - 1).U
val firstZeroFromLeft = PriorityEncoder(Reverse(inverted))

val barWidth   = Mux(inverted === 0.U, BLOCK_N.U, firstZeroFromLeft)

val barWidthLim   = Mux(barWidth > maxStride.U, maxStride.U, barWidth) 

io.dbg_barWidth    := barWidth
io.dbg_barWidthLim := barWidthLim
val bandWidthLim = barWidthLim

val valid = WireInit(true.B)
@inline def aNonZero(i: Int, j: Int): Boolean = {
  val q = io.qIn(i)(j)   
  val k = io.kIn(j)(j)   
  (q * k) != 0
}

for (i <- 0 until BLOCK_M; j <- 0 until BLOCK_N if j <= i) {

  val jUInt = j.U                                       
  val iUInt = i.U
  val inStrip = jUInt < barWidthLim
  val bandLo  = Mux(bandWidthLim >= (iUInt + 1.U), 0.U, iUInt - bandWidthLim + 1.U )
  val inBand  = jUInt >= bandLo
  when (inStrip || inBand) {
    if ( !aNonZero(i, j)){
    valid := false.B
    }
  }
}
  
  io.isAShape    := valid
  io.strideGuess := Mux(valid, barWidthLim, 0.U)
  io.initGuess   := Mux(valid, bandWidthLim, 0.U)
}

class VerticalSlashStrideDetector(
  val BLOCK_M:  Int = 8,
  val BLOCK_N:  Int = 8,
  val D:        Int = 32,
  val lastQ:    Int = 4,
  val dataWidth:Int = 8,
  val accWidth: Int = 32) extends Module {

  require(lastQ <= BLOCK_M, "lastQ cannot exceed BLOCK_M")

  val colIdxW = log2Ceil(BLOCK_N max 1)
  val io = IO(new Bundle {
    val start  = Input(Bool())
    val qIn    = Input(Vec(BLOCK_M, Vec(D, SInt(dataWidth.W))))
    val kIn    = Input(Vec(D,        Vec(BLOCK_N, SInt(dataWidth.W))))

    val done   = Output(Bool())
    val stride = Output(UInt(colIdxW.W))
    val last2  = Output(Vec(2, Vec(BLOCK_N, SInt(accWidth.W)))) 
  })

  /* ---------------- FSM ---------------- */
  val sIdle :: sCompute :: sStride :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  /* ---------------- Row/depth counters ---------------- */
  val rowRel = RegInit(0.U(log2Ceil(lastQ).W)) // 0 .. lastQ‑1
  val depth  = RegInit(0.U(log2Ceil(D).W))

  /* ---------------- Buffers ---------------- */
  val accVec = RegInit(VecInit(Seq.fill(BLOCK_N)(0.S(accWidth.W))))
  val rowBuf1 = Reg(Vec(BLOCK_N, SInt(accWidth.W))) // second‑to‑last
  val rowBuf0 = Reg(Vec(BLOCK_N, SInt(accWidth.W))) // last row

  /* ---------------- Helpers ---------------- */
  private def abs(x: SInt): SInt = Mux(x < 0.S, (-x).asSInt, x)
  private def qSel(i: UInt, d: UInt) = io.qIn(BLOCK_M.U - lastQ.U + i)(d)
  private def kSel(d: UInt, j: UInt) = io.kIn(d)(j)
  private def isVert(x: SInt): Bool = x > 1.S

  /* ---------------- Kick ---------------- */
  when(state === sIdle && io.start) {
    rowRel := 0.U; depth := 0.U
    accVec.foreach(_ := 0.S)
    state := sCompute
  }

  /* ---------------- Phase 1 – dot‑product sketch ---------------- */
  when(state === sCompute) {
    // MAC along depth
    for (j <- 0 until BLOCK_N) {
      accVec(j) := accVec(j) + qSel(rowRel, depth) * kSel(depth, j.U)
    }

    when(depth === (D-1).U) { 
      for (j <- 0 until BLOCK_N) {
        val a = abs(accVec(j))
        when(rowRel === (lastQ-2).U) { rowBuf1(j) := a }
        when(rowRel === (lastQ-1).U) { rowBuf0(j) := a }
        accVec(j) := 0.S // clear for next row
      }

      when(rowRel === (lastQ-1).U) { state := sStride } 
      .otherwise             { rowRel := rowRel + 1.U }
      depth := 0.U
    }.otherwise {
      depth := depth + 1.U
    }
  }

  /* ---------------- Phase 2 – stride detection ---------------- */
  val strideReg   = RegInit(0.U(colIdxW.W))
  val foundFirst  = RegInit(false.B)
  val firstColReg = Reg(UInt(colIdxW.W))
  val colPtr      = RegInit(0.U(colIdxW.W))

  when(state === sStride) {
    when(colPtr === BLOCK_N.U) { // reached end without 2nd vertical
      state := sDone
    } .otherwise {
      val hit = isVert(rowBuf0(colPtr))
      when(hit && !foundFirst) {
        foundFirst := true.B
        firstColReg := colPtr
      } .elsewhen(hit && foundFirst) {
        strideReg := colPtr - firstColReg
        state     := sDone
      }
      colPtr := colPtr + 1.U
    }
  }

  /* ---------------- Outputs ---------------- */
  io.stride := strideReg
  io.last2(0) := rowBuf1
  io.last2(1) := rowBuf0
  io.done   := (state === sDone)

  when(state === sDone) { state := sIdle }
}