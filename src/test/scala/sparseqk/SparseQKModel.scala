package sparseqk

object SparseQKModel {
  type Matrix = Seq[Seq[Int]]

  object PatternType extends Enumeration {
    val Grid, NoFlag,
        NoBoundary, KBoundary, QBoundary,
        TwoDBoundary, AShape, VerticalSlash = Value
  }

  /** 
   * Compute the SparseQK output matrix.
   *
   * @param patternFlag which sparsity pattern to apply
   * @param enable      when false, returns all-zeros
   * @param qIn         Q matrix: BM × D
   * @param kIn         K matrix: D × BN
   * @param stride      grid stride (only for Grid)
   * @param phase       grid phase  (only for Grid)
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

    // simple dot product
    def dot(a: Seq[Int], b: Seq[Int]): Int =
      a.zip(b).map { case (x,y) => x * y }.sum

    // build dense Q×Kᵀ
    val dense: Matrix = Seq.tabulate(BM, BN) { (i, j) =>
      dot(qIn(i), kIn.map(_(j)))
    }

    // apply pattern
    if (!enable) {
      // disabled => all zeros
      Seq.fill(BM, BN)(0)
    } else patternFlag match {
      case PatternType.NoFlag =>
        // dense path
        dense

      case PatternType.Grid =>
        // grid: only (i%stride==phase) && (j%stride==phase)
        Seq.tabulate(BM, BN) { (i, j) =>
          if ((i % stride == phase) && (j % stride == phase)) dense(i)(j)
          else 0
        }

      // other patterns are placeholders in RTL => all zeros
      case PatternType.NoBoundary   => Seq.fill(BM, BN)(0)
      case PatternType.KBoundary    => Seq.fill(BM, BN)(0)
      case PatternType.QBoundary    => Seq.fill(BM, BN)(0)
      case PatternType.TwoDBoundary => Seq.fill(BM, BN)(0)
      case PatternType.AShape       => Seq.fill(BM, BN)(0)
      case PatternType.VerticalSlash=> Seq.fill(BM, BN)(0)
    }
  }
}
