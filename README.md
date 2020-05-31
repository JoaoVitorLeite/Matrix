# Matrix

### Description

Class implementation to represent matrices in Scala

### About

This class was implemented using a generic type, which was implemented [here](https://github.com/JoaoVitorLeite/Matrix/blob/master/src/main/scala/matrix/Num.scala), which supports some basic algebraic operations and is defined for types Int, Float, Double, Long. The tests made are of simple character, and for this the Scala Test was used. In total 27 tests were performed.

### Project Structure

```bash
src
|
+---.gitignore
|   build.sbt
|   LICENSE
|   README.md
|                               
+---src
|   +---main
|   |   \---scala
|   |       \---matrix
|   |               Matrix.scala
|   |               Num.scala
|   |               
|   \---test
|       \---scala
|           \---matrix
|                   MatrixSuite.scala
|                   

```

### Compile

To compile the files it is necessary to be in the root directory(where 
the `build.sbt` file is), the use the `sbt compile` command.

### Test

To perform the tests it is necessary to be in the root 
directory(where the `build.sbt` file is), then use the `sbt test` 
command. The tests used FunSuite and assert's.

### IDE

The IDE used was [Intellij Idea](https://www.jetbrains.com/idea/).

### References

* [Link](https://www.mathsisfun.com/algebra/matrix-introduction.html)
* [Link](https://www.mathwords.com/i/inverse_of_a_matrix.htm)

### License

The LICENSE used is [Apache-2.0](https://github.com/JoaoVitorLeite/Matrix/blob/master/LICENSE).
