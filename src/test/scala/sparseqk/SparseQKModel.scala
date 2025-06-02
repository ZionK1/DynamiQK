package sparseqk

object SparseQKModel {
  type Matrix = Seq[Seq[Int]]

  object PatternType extends Enumeration {
    val Grid, NoFlag,
    NoBoundary, KBoundary, QBoundary,
    TwoDBoundary, AShape, VerticalSlash = Value
  }

  /**
   * Compute the SparseQK output matrix (q × kᵀ) under various sparsity patterns.
   *
   * @param patternFlag which sparsity pattern to apply
   * @param enable      when false, returns all-zeros
   * @param qIn         Q matrix: BM × D
   * @param kIn         K matrix: D × BN
   * @param stride      grid stride (only for Grid, AShape, VerticalSlash)
   * @param phase       grid phase  (only for Grid, AShape, VerticalSlash)
   * @return            BM × BN result of Q × Kᵀ under the given pattern
   */
  def apply(
             patternFlag: PatternType.Value,
             enable:      Boolean,
             qIn:         Matrix,
             kIn:         Matrix,
             stride:      Int = 4,
             phase:       Int = 0
           ): Matrix = {
    val BM = qIn.length
    val D  = if (qIn.isEmpty) 0 else qIn.head.length
    val BN = if (kIn.isEmpty) 0 else kIn.head.length

    // compute half‐sizes for inter‐modality splits
    val halfM = BM / 2
    val halfN = BN / 2

    // simple dot product
    def dot(a: Seq[Int], b: Seq[Int]): Int =
      a.zip(b).map { case (x, y) => x * y }.sum

    // build dense Q×Kᵀ (BM × BN)
    val dense: Matrix = Seq.tabulate(BM, BN) { (i, j) =>
      dot(qIn(i), kIn.map(_(j)))
    }

    // if disabled, return all zeros
    if (!enable) {
      Seq.fill(BM, BN)(0)
    } else {
      patternFlag match {
        case PatternType.NoFlag =>
          // dense path
          dense

        case PatternType.Grid =>
          // grid: only (i % stride == phase) && (j % stride == phase)
          Seq.tabulate(BM, BN) { (i, j) =>
            if ((i % stride == phase) && (j % stride == phase)) dense(i)(j)
            else 0
          }

        case PatternType.AShape =>
          // A-shape: for each row i:
          //   j < nInit OR (j >= (i - nLocal) && j <= i)
          val nInit  = 4
          val nLocal = stride
          Seq.tabulate(BM, BN) { (i, j) =>
            val initHit  = j < nInit
            val localHit = (j >= (i - nLocal)) && (j <= i)
            if (initHit || localHit) dense(i)(j)
            else 0
          }

        case PatternType.VerticalSlash =>
          // VerticalSlash: only columns j where (j % stride == phase)
          Seq.tabulate(BM, BN) { (i, j) =>
            if ((j % stride == phase)) dense(i)(j)
            else 0
          }

        case PatternType.NoBoundary =>
          // NoBoundary: fallback to fully dense (same as NoFlag)
          dense

        case PatternType.KBoundary =>
          // KBoundary: restrict keys to same half as each query
          Seq.tabulate(BM, BN) { (i, j) =>
            val iHalf = i < halfM
            val jHalf = j < halfN
            if (iHalf == jHalf) dense(i)(j) else 0
          }

        case PatternType.QBoundary =>
          // QBoundary: restrict queries to same half as each key
          Seq.tabulate(BM, BN) { (i, j) =>
            val iHalf = i < halfM
            val jHalf = j < halfN
            if (iHalf == jHalf) dense(i)(j) else 0
          }

        case PatternType.TwoDBoundary =>
          // TwoDBoundary: 2×2 block‐diagonal
          Seq.tabulate(BM, BN) { (i, j) =>
            val topHalf    = (i < halfM) && (j < halfN)
            val bottomHalf = (i >= halfM) && (j >= halfN)
            if (topHalf || bottomHalf) dense(i)(j)
            else 0
          }
      }
    }
  }
}
