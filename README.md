# DynamiQK

This is a [Chisel](https://chisel-lang.org/) generator that produces an optimized Query-Key (Q×K) attention hardware module capable of detecting and exploiting sparse attention patterns (Grid, A‑shape, Tri‑shape, Vertical‑slash) for both inter‑modality and intra‑modality attention. By tailoring the datapath to each pattern, it reduces computation and memory bandwidth in vision‑language and multimodal transformer architectures.

> The generator reads pattern metadata, builds Compressed Sparse Row (CSR) masks, and emits a fully parameterized Chisel module for the QK compute engine as well as a Pattern Detection Module.

## Authors

* Zion Kang <zkang5@ucsc.edu>
* Changyang Zeng <czeng14@ucsc.edu>

## Current Implementation
* Pattern Detection Module
  * Chisel Component
  * Tester
* QK Module
  * Chisel component
    * Performs optimized computations for inter/intramodal sparse pattern inputs
  * Scala Model
  * Tester
    * Reports performance benefit from pattern-aware QxK multiplication

## Testing
To run the included tests, enter the following in a sbt shell:
```console
sbt test
```
Note: The default parameters for the test take a few minutes to run. You can lower the dimensions of the inputs by making BM and BN smaller, which will significantly drop the runtime. However, there will be less of speedup, given the nature of less workload. Conversely, raising BM, BN, and ITERS will lead to a greater speedup reported as the dense case gets worse with more required computations.

## Future Goals
* Implement pattern detection for all sparsity patterns (Currently only capable of A-shape and Vertical Slash)
* Generating appropriate multi-modal matrix input
  * Option 1: Run MMInference flow to reproduce matrix inputs/fetch pre-existing input data from MMInference repo
  * Option 2: Generate sparse matrix input data through GenerativeAI
* Benchmark performance and functionality of DynamiQK vs. SpAtten (related work) for QK computation step, right now the tester doesn't support softmax-normalized attention score validation.

## Documentation
* SparseQK
  * Parameters:
    * patternFlag
      * At runtime, this input selects which sparsity pattern to use (e.g., PatternType.Grid, PatternType.AShape, etc.)
    * enable
      * Acts as a “compute enable” signal. When enable is high, the module executes the selected pattern’s compute logic (i.e., performs dot‐products or accumulations)
    * done
      * Used only by the “multi‐cycle” patterns (AShape and VerticalSlash) to tell the module its finished feeding it all sub‐windows—now it can assert valid and hold out whatever final, accumulated results it has in its internal registersdifferent inputs; once you raise done, the module drives valid = true to signal that the final accumulated
    * qIn
      * Represents all BLOCK_M query vectors (each of length D). Conceptually, it’s an M × D matrix of Q values
    * kIn
      * Represents all D key vectors for the BLOCK_N columns—i.e., conceptually it’s the transpose of N × D matrix
* SparseQKSpec
  * Parameters:
    * BM
      * Number of query rows (i.e., the height of the Q matrix block) that the tester uses. In other words, there are 8 query vectors in each test tile
    * BN
      * Number of key columns (i.e., the width of the K matrix block). Equivalently, there are 8 key vectors in each test tile
    * D
      * Length of each query and key vector. Every dot product is over 16 elements
    * DW
      * Bitwidth of each element in Q and K (in the hardware module). Here, each signed element is an 8-bit integer
    * STRIDE
      * Spacing parameter for grid-based sparsity patterns. For example, with STRIDE = 4, only indices where i % 4 == PHASE and j % 4 == PHASE are “hits.” It must divide both BM and BN so that the grid lines align perfectly
    * PHASE
      * Offset within the stride pattern. A phase of 0 means the grid “lines” occur at indices 0, 4, 8, …; changing PHASE would shift those lines to 1, 5, 9, …, etc.
    * ITERS
      * Number of random test iterations (for timing measurements) that the tester runs per pattern. Each iteration repopulates Q/K randomly and accumulates cycle-accurate timing so you can compute average speedups
* SparseQKModel
  * Parameters:
    * patternFlag: PatternType.Value – which sparsity pattern to apply (e.g. NoFlag,Grid, …)
    * enable: Boolean – when false, returns an all-zero tile; when true, computes according to patternFlag
    * qIn: Matrix – a BM × D Seq[Seq[Int]] holding the query rows
    * kIn: Matrix – a D × BN Seq[Seq[Int]] holding the key columns
    * stride: Int – grid‐pattern stride (default 4)
    * phase: Int – grid‐pattern phase offset (default 0)
* PatternDetector
  * Parameters:
    * qIn
      * Represents all BLOCK_M query vectors (each of length D). Conceptually, it’s an M × D matrix of Q values
    * kIn
      * Represents all D key vectors for the BLOCK_N columns—i.e., conceptually it’s the transpose of a N × D matrix
    * isAShape
      * output of a-shape detection
    * strideGuess
      * output of width of vertical regions in the a-shape region
    * initGuess
      * output of hypotenuse region in the a-shape region
    * Stride
      * gap of vertical columns in the attention matrix
* PatternDetectorSpec
  * BM
    * Number of query rows (i.e., the height of the Q matrix block) that the tester uses. In other words, there are 8 query vectors in each test tile
  * BN
    * Number of key columns (i.e., the width of the K matrix block). Equivalently, there are 8 key vectors in each test tile
  * D
    * Length of each query and key vector. Every dot product is over 16 elements
  * DW
    * Bitwidth of each element in Q and K (in the hardware module). Here, each signed element is an 8-bit integer
  * maxStride
    * The maximum number of the A-shape parameter (width of the vertical band) that the detector searches
  * maxInit
    * The maximum number of the A-shape parameter (width of the hypotenuse band) that the detector searches
  * lastQ
    * The small portion of the attention matrix for the vertical-slash detector to search for correct parameters

## Related Works
* [MMInference](https://github.com/microsoft/MInference)
* [SpAtten](https://github.com/mit-han-lab/spatten)