package sparseqk

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

class SparseQKSpec extends AnyFlatSpec
  with ChiselScalatestTester
  with Matchers {

  /*  Design / test parameters  */
  val BM     = 8
  val BN     = 8
  val D      = 16
  val DW     = 8
  val STRIDE = 4    // keep a stride that divides BM,BN
  val PHASE  = 0
  val ITERS  = 5    // or 10/100 as you wish

  /* ---------- helpers ---------- */
  private val rnd = new Random(0x1234_beef)

  private def randVal() = rnd.between(-3, 4)

  private def dot(a: Seq[Int], b: Seq[Int]) =
    a.zip(b).map { case (x,y) => x*y }.sum

  private def denseRef(q: Seq[Seq[Int]], k: Seq[Seq[Int]]) = {
    val out = Array.ofDim[Int](q.length, k.head.length)
    for (i <- q.indices; j <- k.head.indices)
      out(i)(j) = dot(q(i), k.map(_(j)))
    out
  }

  private def gridMask(i: Int, j: Int): Boolean =
    (i % STRIDE == PHASE) && (j % STRIDE == PHASE)

  /* ---------- test ---------- */
  behavior of "SparseQK Grid flag"

  it should "finish quickly and show a speed-up" in {
    test(new SparseQK(BM, BN, D, DW, STRIDE, PHASE)) { dut =>

      def time(nsBlock: => Unit): Long = {
        val t0 = System.nanoTime(); nsBlock; System.nanoTime() - t0
      }

      var denseNs = 0L
      var gridNs  = 0L

      for (_ <- 0 until ITERS) {

        /* build Q & K with grid sparsity */
        val qMat = Seq.tabulate(BM) { r =>
          Seq.tabulate(D) { _ =>
            if (r % STRIDE == PHASE) randVal() else 0
          }
        }
        val kMat = Seq.tabulate(D) { _ =>
          Seq.tabulate(BN) { c =>
            if (c % STRIDE == PHASE) randVal() else 0
          }
        }

        /* --- dense path --- */
        denseNs += time {
          dut.io.patternFlag.poke(PatternType.NoFlag)
          dut.io.enable.poke(true.B)
          for (i <- 0 until BM; d <- 0 until D) dut.io.qIn(i)(d).poke(qMat(i)(d).S)
          for (d <- 0 until D; j <- 0 until BN) dut.io.kIn(d)(j).poke(kMat(d)(j).S)
          dut.clock.step(1)
          val gold = denseRef(qMat, kMat)
          for (i <- 0 until BM; j <- 0 until BN)
            dut.io.qkOut(i)(j).expect(gold(i)(j).S)
        }

        /* --- grid path --- */
        gridNs += time {
          dut.io.patternFlag.poke(PatternType.Grid)
          // same inputs still applied
          dut.clock.step(1)
          val gold = denseRef(qMat, kMat)
          for (i <- 0 until BM; j <- 0 until BN) {
            val expected = if (gridMask(i,j)) gold(i)(j) else 0
            dut.io.qkOut(i)(j).expect(expected.S)
          }
        }
      }

      /* simple console log */
      println(f"\n--- SparseQK ($ITERS trial) ---")
      println(f"Dense (NoFlag) : ${denseNs/1e6}%.2f ms")
      println(f"Grid  (flag)   : ${gridNs /1e6}%.2f ms")
      println(f"Speed-up       : ${denseNs.toDouble/gridNs}%.2fx\n")

      gridNs should be < denseNs   // sanity
    }
  }
}
