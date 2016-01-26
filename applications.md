# Scalan Applications 

### Table of contents
- [Introduction](#introduction)
- [Collections](#collections)
- [Linear Algebra and Matrix operations](#matrices)
- [Graphs](#graphs)

### Introduction

The core of Scalan framework is domain-neutral and can be used for different domains. Here we describe example
applications from Scalan distribution.

<a name="collections"></a> 
### Collections

Abstract data type `Collection` is used in Scalan to abstract form concrete representation type such as `Array`, `List`
etc. Domain specific abstractions implemented using `Collection` interface can be automatically specialized with respect
to any such representation.

See [source code](https://github.com/scalan/scalan/blob/master/collections/src/main/scala/scalan/collections/Collections.scala) of
abstract data type definition and concrete implementation classes as well as [related tests](https://github.com/scalan/scalan/blob/master/collections/src/test/scala/scalan/collections/CollectionExamplesSuite.scala).


 <a name="matrices"></a> 
### Linear Algebra and Matrix operations 

To be written. 
See [examples](https://github.com/scalan/scalan/blob/master/linear-algebra/src/test/scala/scalan/linalgebra/LinearAlgebraExamples.scala) and [tests](https://github.com/scalan/scalan/blob/master/lms-backend/linear-algebra/src/it/scala/scalan/linalgebra/LmsLinAlgItTests.scala).

 <a name="graphs"></a> 
### Graphs

To be written.
See [examples](https://github.com/scalan/scalan/blob/master/graphs/src/test/scala/scalan/graphs/GraphExamples.scala) and [tests](https://github.com/scalan/scalan/blob/master/lms-backend/tests/src/it/scala/scalan/graphs/LmsMSTItTests.scala).
