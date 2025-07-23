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

## Related Works
* [MMInference](https://github.com/microsoft/MInference)
* [SpAtten](https://github.com/mit-han-lab/spatten)
