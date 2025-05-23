# DynamiQK

This is a [Chisel](https://chisel-lang.org/) generator that produces an optimized Query-Key (Q×K) attention hardware module capable of detecting and exploiting sparse attention patterns (Grid, A‑shape, Tri‑shape, Vertical‑slash) for both inter‑modality and intra‑modality attention. By tailoring the datapath to each pattern, it reduces computation and memory bandwidth in vision‑language and multimodal transformer architectures.

> The generator reads pattern metadata, builds Compressed Sparse Row (CSR) masks, and emits a fully parameterized Chisel module for the QK compute engine.

## Authors

* Zion Kang <zkang5@ucsc.edu>
* Changyang Zeng <czeng14@ucsc.edu>

## Current Implementation
* Scala model for QK optimizations
* Chisel component for QK optimizations
  * Contains optimizations for all sparse patterns (Grid head, A-shape, vertical-slash, etc.)
* Testing harness for QK optimization Chisel component

## Future Goals
* Integrating pattern detection into pipeline
  * Option 1: Separate pattern detection Chisel module
  * Option 2: Embed pattern detection into current QK module
  * Note: Pattern detection in MMInference is done on software level, so we may not be able to perform both in same stage at hardware level
* Generating appropriate multi-modal matrix input
  * Option 1: Run MMInference flow to reproduce matrix inputs/fetch pre-existing input data from MMInference repo
  * Option 2: Generate sparse matrix input data through GenerativeAI
* Benchmark performance of DynamiQK vs. SpAtten (related work) for QK computation step

## Documentation
* SparseQK
  * Parameters:
    * BLOCK_M: Int – number of query rows per tile
    * BLOCK_N: Int – number of key columns per tile
    * D: Int – inner-dot-product dimension
    * dataWidth: Int – bit-width of input SInts (default 16)
    * stride: Int – pattern grid stride (default 4)
    * phase: Int – pattern grid phase offset (default 0)
* SparseQKSpec
  * Parameters:
    * BM: Int – number of query rows/block (default 8)
    * BN: Int – number of key columns/block (default 8)
    * D: Int – dot-product inner dimension (default 16)
    * DW: Int – data width in bits for input SInts (default 8)
    * STRIDE: Int – grid pattern stride, must divide BM and BN (default 4)
    * PHASE: Int – grid pattern phase offset (default 0)
    * ITERS: Int – number of test iterations for timing and correctness (default 5)
* SparseQKModel

## Testing
To run the included tests, enter the following in a sbt shell:
```console
sbt test
```

## References
* [MMInfernece](https://github.com/microsoft/MInference)
* [SpAtten](https://github.com/mit-han-lab/spatten)