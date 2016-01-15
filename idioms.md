# Scalan's Meta-programming Idioms   

### Intro

Scalan is a framework for development of domain-specific compilers in Scala. In particular it supports meta-programming bases on staged evaluation. Visit [Scalan Readme](https://github.com/scalan/scalan/blob/master/README.md) for general introduction about Scalan and how to get started. 

The following is the introduction to meta-programming idioms available in Scalan shown by examples.

### Idiom 1: Staged Evaluation

*Staged Evaluation* refers to a special mode of program evaluation (or interpretation). Whereas the standard evaluation aims to produce output data for a given input data, staged evaluation of program `P` produces graph-based intermediate representation of `P`. For example, given a program

```
  def mvm(matrix: Matrix[Double], vector: Vector[Double]): Vector[Double] =
    DenseVector(matrix.rows.mapBy(r => r dot vector))
```
staged evaluation of it will result in construction of the following graph

![](graphs/aamvm.dot.png)

### Idiom 2: Virtualized Code

 
