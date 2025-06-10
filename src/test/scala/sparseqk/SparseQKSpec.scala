package sparseqk

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random
import sparseqk.SparseQKModel.{PatternType => ModelPattern}

class SparseQKSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  /*  Design/test parameters  */
  val BM = 8
  val BN = 8
  val D = 16
  val DW = 8
  val STRIDE = 4 // keep a stride that divides BM,BN
  val PHASE = 0
  val ITERS = 1

  /* ---------- helpers ---------- */
  private val rnd = new Random(0x1234_beef)

  private def randVal(): Int = rnd.between(-3, 4)

  private def dot(a: Seq[Int], b: Seq[Int]): Int =
    a.zip(b).map { case (x, y) => x * y }.sum

  // Reference for verifying the Scala model
  private def denseRef(q: Seq[Seq[Int]], k: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val out = Array.ofDim[Int](q.length, k.head.length)
    for (i <- q.indices; j <- k.head.indices)
      out(i)(j) = dot(q(i), k.map(_(j)))
    out.map(_.toSeq).toSeq
  }

  private def gridMask(i: Int, j: Int): Boolean =
    (i % STRIDE == PHASE) && (j % STRIDE == PHASE)

  private def aShapeMask(i: Int, j: Int, nInit: Int = 4, nLocal: Int = STRIDE): Boolean =
    (j < nInit) || (j >= (i - nLocal) && j <= i)

  private def verticalMask(j: Int): Boolean =
    (j % STRIDE == PHASE)

  private val halfM = BM / 2
  private val halfN = BN / 2

  // NoBoundary: same as dense (always true)
  private def noBoundaryMask(i: Int, j: Int): Boolean = true

  private def kBoundaryMask(i: Int, j: Int): Boolean = {
    val iHalf = i < halfM
    val jHalf = j < halfN
    iHalf == jHalf
  }

  private def qBoundaryMask(i: Int, j: Int): Boolean = {
    val iHalf = i < halfM
    val jHalf = j < halfN
    iHalf == jHalf
  }

  private def twoDBoundaryMask(i: Int, j: Int): Boolean = {
    val topHalf    = (i < halfM) && (j < halfN)
    val bottomHalf = (i >= halfM) && (j >= halfN)
    topHalf || bottomHalf
  }

  /* ---------- test ---------- */
  behavior of "SparseQK Full Pattern Suite"

  it should "generate inputs for each pattern and compare NoFlag vs. the pattern" in {
    test(new SparseQK(BM, BN, D, DW, STRIDE, PHASE)) { dut =>

      // 1) list of all sparsity patterns (excluding NoFlag)
      val patternMasks: Seq[(PatternType.Type, (Int, Int) => Boolean)] = Seq(
        PatternType.Grid -> gridMask,
        PatternType.AShape -> ((i, j) => aShapeMask(i, j)),
        PatternType.VerticalSlash -> ((i, j) => verticalMask(j)),
        PatternType.NoBoundary -> ((i, j) => noBoundaryMask(i, j)),
        PatternType.KBoundary -> ((i, j) => kBoundaryMask(i, j)),
        PatternType.QBoundary -> ((i, j) => qBoundaryMask(i, j)),
        PatternType.TwoDBoundary -> ((i, j) => twoDBoundaryMask(i, j))
      )

      // 2) timing accumulators
      var denseNs = 0L
      var gridNs = 0L
      var aNs = 0L
      var vNs = 0L
      var noBoundNs = 0L
      var kBoundNs = 0L
      var qBoundNs = 0L
      var twoDBoundNs = 0L

      def timeNs(block: => Unit): Long = {
        val t0 = System.nanoTime()
        block
        System.nanoTime() - t0
      }

      // 3) for each iteration *and* each pattern
      for (_ <- 0 until ITERS) {
        // 1) build “one-hot” Q and masked K once per iteration
        val qMat = Seq.tabulate(BM, D) { (i, d) =>
          if (d == i) randVal() else 0
        }
        val kMat = Seq.tabulate(D, BN) { (d, j) =>
          if (d < BM && patternMasks.exists(_._2(d, j))) randVal() else 0
        }

        // poke inputs
        for (i <- 0 until BM; d <- 0 until D)
          dut.io.qIn(i)(d).poke(qMat(i)(d).S)
        for (d <- 0 until D; j <- 0 until BN)
          dut.io.kIn(d)(j).poke(kMat(d)(j).S)

        // compute golden full-dense once
        val goldDense = denseRef(qMat, kMat)

        // 2) Dense path timing (once per iteration)
        denseNs += timeNs {
          dut.io.enable.poke(true.B)
          dut.io.patternFlag.poke(PatternType.NoFlag)
          dut.clock.step(1)
          for (i <- 0 until BM; j <- 0 until BN)
            dut.io.qkOut(i)(j).expect(goldDense(i)(j).S)
        }

        // --- 3b) Sparse path timing ---
        for ((pat, maskFn) <- patternMasks) {
          val patNs = timeNs {
            dut.io.patternFlag.poke(pat)
            dut.clock.step(1)
            for (i <- 0 until BM; j <- 0 until BN) {
              val expected = if (maskFn(i, j)) goldDense(i)(j) else 0
              dut.io.qkOut(i)(j).expect(expected.S)
            }
          }

          // accumulate into correct bucket
          pat match {
            case PatternType.Grid => gridNs += patNs
            case PatternType.AShape => aNs += patNs
            case PatternType.VerticalSlash => vNs += patNs
            case PatternType.NoBoundary => noBoundNs += patNs
            case PatternType.KBoundary => kBoundNs += patNs
            case PatternType.QBoundary => qBoundNs += patNs
            case PatternType.TwoDBoundary => twoDBoundNs += patNs
          }
        }

        // 4) Print out timings and speedups
        println(f"\n--- SparseQK ($ITERS iterations per pattern) ---")
        println(f"Dense        (NoFlag)           : ${denseNs / 1e6}%.2f ms")
        println(f"Grid                            : ${gridNs / 1e6}%.2f ms (speedup ${denseNs.toDouble / gridNs}%.2fx)")
        println(f"A-Shape                         : ${aNs / 1e6}%.2f ms (speedup ${denseNs.toDouble / aNs}%.2fx)")
        println(f"Vertical-Slash                  : ${vNs / 1e6}%.2f ms (speedup ${denseNs.toDouble / vNs}%.2fx)")
        println(f"NoBoundary   (dense fallback)   : ${noBoundNs / 1e6}%.2f ms (speedup ${denseNs.toDouble / noBoundNs}%.2fx)")
        println(f"KBoundary                       : ${kBoundNs / 1e6}%.2f ms (speedup ${denseNs.toDouble / kBoundNs}%.2fx)")
        println(f"QBoundary                       : ${qBoundNs / 1e6}%.2f ms (speedup ${denseNs.toDouble / qBoundNs}%.2fx)")
        println(f"TwoDBoundary                    : ${twoDBoundNs / 1e6}%.2f ms (speedup ${denseNs.toDouble / twoDBoundNs}%.2fx)\n")

        // 5) Sanity checks: each sparse pattern should be at least as fast as dense
        gridNs should be < denseNs
        aNs should be < denseNs
        vNs should be < denseNs
        noBoundNs should be <= denseNs // identical mask, timing noise allowed
        kBoundNs should be < denseNs
        qBoundNs should be < denseNs
        twoDBoundNs should be < denseNs
      }
    }
  }
}

class SparseQKModelSpec extends AnyFlatSpec with Matchers {

  /*  Test parameters  */
  val BM = 8
  val BN = 8
  val D = 16
  val STRIDE = 4 // must divide BM,BN
  val PHASE = 0
  val ITERS = 10 // random trials

  private val rnd = new Random(0x1234_beef)

  // random integer between -3 and 3
  private def randVal(): Int = rnd.between(-3, 4)

  // simple dot product
  private def dot(a: Seq[Int], b: Seq[Int]): Int =
    a.zip(b).map { case (x, y) => x * y }.sum

  // dense Q×Kᵀ reference
  private def denseRef(q: Seq[Seq[Int]], k: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val out = Array.ofDim[Int](q.length, k.head.length)
    for {
      i <- q.indices
      j <- k.head.indices
    } {
      out(i)(j) = dot(q(i), k.map(_(j)))
    }
    out.map(_.toSeq).toSeq
  }

  // mask definitions matching RTL behavior
  private def gridMask(i: Int, j: Int): Boolean =
    (i % STRIDE == PHASE) && (j % STRIDE == PHASE)

  private def aShapeMask(i: Int, j: Int, nInit: Int = 4, nLocal: Int = STRIDE): Boolean = {
    val initHit = j < nInit
    val localHit = (j >= (i - nLocal)) && (j <= i)
    initHit || localHit
  }

  private def verticalMask(j: Int): Boolean =
    (j % STRIDE == PHASE)

  "SparseQKModel" should "match NoFlag (dense) exactly" in {
    for (_ <- 0 until ITERS) {
      val qMat = Seq.fill(BM, D)(randVal())
      val kMat = Seq.fill(D, BN)(randVal())

      val modelOut = SparseQKModel(
        patternFlag = SparseQKModel.PatternType.NoFlag,
        enable = true,
        qIn = qMat,
        kIn = kMat,
        stride = STRIDE,
        phase = PHASE
      )

      val goldDense = denseRef(qMat, kMat)
      modelOut shouldEqual goldDense
    }
  }

  it should "match Grid pattern" in {
    for (_ <- 0 until ITERS) {
      val qMat = Seq.fill(BM, D)(randVal())
      val kMat = Seq.fill(D, BN)(randVal())
      val goldDense = denseRef(qMat, kMat)

      val modelOut = SparseQKModel(
        patternFlag = SparseQKModel.PatternType.Grid,
        enable = true,
        qIn = qMat,
        kIn = kMat,
        stride = STRIDE,
        phase = PHASE
      )

      for {
        i <- 0 until BM
        j <- 0 until BN
      } {
        val expected = if (gridMask(i, j)) goldDense(i)(j) else 0
        modelOut(i)(j) shouldBe expected
      }
    }
  }

  it should "match AShape pattern" in {
    for (_ <- 0 until ITERS) {
      val qMat = Seq.fill(BM, D)(randVal())
      val kMat = Seq.fill(D, BN)(randVal())
      val goldDense = denseRef(qMat, kMat)

      val modelOut = SparseQKModel(
        patternFlag = SparseQKModel.PatternType.AShape,
        enable = true,
        qIn = qMat,
        kIn = kMat,
        stride = STRIDE,
        phase = PHASE
      )

      for {
        i <- 0 until BM
        j <- 0 until BN
      } {
        val expected = if (aShapeMask(i, j)) goldDense(i)(j) else 0
        modelOut(i)(j) shouldBe expected
      }
    }
  }

  it should "match VerticalSlash pattern" in {
    for (_ <- 0 until ITERS) {
      val qMat = Seq.fill(BM, D)(randVal())
      val kMat = Seq.fill(D, BN)(randVal())
      val goldDense = denseRef(qMat, kMat)

      val modelOut = SparseQKModel(
        patternFlag = SparseQKModel.PatternType.VerticalSlash,
        enable = true,
        qIn = qMat,
        kIn = kMat,
        stride = STRIDE,
        phase = PHASE
      )

      for {
        i <- 0 until BM
        j <- 0 until BN
      } {
        val expected = if (verticalMask(j)) goldDense(i)(j) else 0
        modelOut(i)(j) shouldBe expected
      }
    }
  }

  it should "return all zeros when enable=false regardless of pattern" in {
    val qMat = Seq.fill(BM, D)(randVal())
    val kMat = Seq.fill(D, BN)(randVal())

    for (pat <- SparseQKModel.PatternType.values) {
      val modelOut = SparseQKModel(
        patternFlag = pat,
        enable = false,
        qIn = qMat,
        kIn = kMat,
        stride = STRIDE,
        phase = PHASE
      )
      modelOut.flatten.forall(_ == 0) shouldBe true
    }
  }
}