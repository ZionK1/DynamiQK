package sparseqk

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AShapePatternDetectorSpec extends AnyFlatSpec
  with ChiselScalatestTester
  with Matchers {

  behavior of "AShapePatternDetector (A-shape recognition)"

  it should "flag a valid A-shape and report the proper stride & init" in {
    /* --------------- Tunable pattern parameters ------------------ */
    val N          = 8   // BLOCK_M = BLOCK_N
    val G          = 2   // left global-strip width
    val W          = 2   // diagonal band half-width
    val maxStride  = 4   // detector's search range (≥ W)
    val maxInit    = 3   // detector's search range (≥ G)
    val D          = N   // one-hot encoding convenience
    val dataWidth  = 8

    test(new AShapePatternDetector(
      BLOCK_M   = N, BLOCK_N = N, D = D,
      maxStride = maxStride, maxInit = maxInit,
      dataWidth = dataWidth
    )) { dut =>
      def inAShape(i: Int, j: Int): Boolean =
        j <= i && (j < G || (i - j) < W)

      for (i <- 0 until N; d <- 0 until D) dut.io.qIn(i)(d).poke(0.S)
      for (d <- 0 until D; j <- 0 until N) dut.io.kIn(d)(j).poke(0.S)

      for (d <- 0 until D) dut.io.kIn(d)(d).poke(1.S)

      for (i <- 0 until N; j <- 0 until N if inAShape(i, j))
        dut.io.qIn(i)(j).poke(1.S)

      dut.clock.step(1)

      val qkProduct = Array.ofDim[Int](N, N)

      for (i <- 0 until N; j <- 0 until N) {
        var acc = 0
        for (d <- 0 until D) {
          val q = dut.io.qIn(i)(d).peek().litValue.toInt
          val k = dut.io.kIn(d)(j).peek().litValue.toInt
          acc += q * k
        }
        qkProduct(i)(j) = acc
      }

      println("\nDot-product A-shape attention matrix A = Q·K  (entries are Int):")
      qkProduct.foreach { row =>
        println(row.map("%2d".format(_)).mkString(" "))
      }



      /* ---- 6. check detector outputs ---------------------------- */
      val expectedStride = G         // largest stride the DUT is allowed to use
      val expectedInit   = W                 // smallest init covering j < G

      val bw  = dut.io.dbg_barWidth.peek().litValue
      val bwL = dut.io.dbg_barWidthLim.peek().litValue
      println(s"runtime vertical bar Width  = $bw")
      println(s"runtime diagonal bar Width = $bwL")

      val lastRow = (0 until N).map { j =>
        dut.io.dbg_lastRowActive(j).peek().litToBoolean
      }

      println("\nLast row activity of the A-shape attention matrix:")
      println(lastRow.map(b => if (b) '1' else '0').mkString(" "))

      dut.io.isAShape   .expect(true.B,  "Detector failed to recognise the A-shape")
      dut.io.strideGuess.expect(expectedStride.U,
                                s"Stride guess should be $expectedStride")
      dut.io.initGuess  .expect(expectedInit.U,
                                s"Init guess should be $expectedInit")
    }
  }
}

class VerticalSlashStrideDetectorSpec extends AnyFlatSpec
  with ChiselScalatestTester
  with Matchers {

  behavior of "VerticalSlashStrideDetector (stride recognition)"

  private def loadPattern(dut: VerticalSlashStrideDetector, N: Int, stride: Int, D: Int): Unit = {
    val lastQ = N / 2

    for (i <- 0 until N; d <- 0 until 2) dut.io.qIn(i)(d).poke(0.S)
    for (d <- 0 until 2; j <- 0 until N) dut.io.kIn(d)(j).poke(0.S)

    for (col <- 0 until N by stride) dut.io.kIn(0)(col).poke(1.S)

    for (i <- 0 until N){
       dut.io.qIn(i)(0).poke(2.S)
    }

    for (row <- (N - lastQ) until N) {
      dut.io.qIn(row)(1).poke(1.S)
      dut.io.kIn(1)(row).poke(1.S)
    }
  }

/** Pretty-print the lower-triangular part of A = Q·Kᵀ  */
private def computeAttention(
    dut: VerticalSlashStrideDetector, N: Int, stride: Int
): Array[Array[Int]] = {
  val D = 2
  val A = Array.ofDim[Int](N, N)
  for (i <- 0 until N; j <- 0 until N) {
    var acc = 0
    for (d <- 0 until D) {
      val q = dut.io.qIn(i)(d).peek().litValue.toInt
      val k = dut.io.kIn(d)(j).peek().litValue.toInt
      acc += q * k
    }
    A(i)(j) = if (j <= i  ) acc else 0      
  }
  A
}

  private def run(N: Int, stride: Int): Unit = {
    val lastQ = N / 2
    val D = 8
    val dataWidth = 8

    test(new VerticalSlashStrideDetector(BLOCK_M = N, BLOCK_N = N, D = D, lastQ = lastQ, dataWidth = dataWidth)) { dut =>

      loadPattern(dut, N, stride, D)

      dut.clock.step(1)

      val attn = computeAttention(dut, N, stride)
      println(s"Vertical-slash tester: Dot‑product attention matrix A (${N}×${N}), stride = $stride:")
      attn.foreach(row => println(row.map("%2d".format(_)).mkString(" "))) // pretty‑print

      /* ---- Kick the detector and wait for done ---- */
      dut.io.start.poke(true.B); dut.clock.step(); dut.io.start.poke(false.B)
      var cycles = 0
      while (!dut.io.done.peek().litToBoolean && cycles < 500) { dut.clock.step(); cycles += 1 }
      assert(dut.io.done.peek().litToBoolean, s"Detector did not finish (stalled $cycles cycles)")

      /* ---- Check outputs ---- */
      dut.io.stride.expect(stride.U, s"Stride should be $stride")

      val lastRow = (0 until N).map(j => dut.io.last2(1)(j).peek().litValue.toInt)
      println("Vertical-slash detection module tester last row captured in hardware:")
      println(lastRow.map("%2d".format(_)).mkString(" "))
    }
  }

  it should "detect stride = 2 in 8×8 pattern" in { run(8, 2) }
  it should "detect stride = 3 in 16×16 pattern" in { run(16, 3) }
}
