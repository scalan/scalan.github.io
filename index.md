# Scalan Framework Documentation   

### Getting Started 

Introduction to Scalan can be found [here](https://github.com/scalan/scalan/blob/master/README.md)

### Scalan IR Graphs

Scalan compilation pipeline uses a graph-based intermediate representation.
The resulting graphs can be produced after each compilation stage and visualized using Graphviz.
See [graphical notation](https://github.com/scalan/scalan.github.io/blob/master/graphnotation.md) for details.

### Scalan Metaprogramming Idioms
Metaprogramming style supported by Scalan can be found [here](https://github.com/scalan/scalan.github.io/blob/master/idioms.md).

### Accelerating hotspots in Scala 

Scalan can be used for automatic optimization of hotspots in Scala programs written in functional programming style. Intermediate representation, IR, of the hotspot is captured by [Scalanizer](https://github.com/scalan/scalanizer) plugin and optimized by domain-specific compiler developed with Scalan. See [demonstration project](https://github.com/scalan/scalanizer-demo) for details.
