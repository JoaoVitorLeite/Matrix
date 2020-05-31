package matrix

import org.scalatest.FunSuite

class MatrixSuite extends FunSuite{

  test("T1"){
    val a = new Matrix[Double](2,3, List(List(1D,2D,3D), List(4D,5D,6D)))
    assert(a(0) == List(1,2,3))
  }
  test("T2"){
    val a = new Matrix[Double](2,3, List(List(1D,2D,3D), List(4D,5D,6D)))
    assert(a(0,2) == 3)
  }
  test("T3"){
    val l1 = List.fill(3,3)(math.random)
    val l2 = List.fill(3,3)(math.random)
    val A = new Matrix[Double](3,3, l1)
    val B = new Matrix[Double](3,3, l2)
    assert((A + B) == (B + A))
  }
  test("T4"){
    val l1 = List(List(1,0,9), List(2,5,6), List(7,4,3))
    val l2 = List(List(0,-1,2), List(4,7,9), List(3,5,6))
    val l3 = List(List(9,-5,3), List(2,4,7), List(8,5,2))
    val A = new Matrix[Int](3,3, l1)
    val B = new Matrix[Int](3,3, l2)
    val C = new Matrix[Int](3,3, l3)
    assert((A + B) + C == A + (B + C))
  }
  test("T5"){
    val l1 = List.fill(3,3)(math.random)
    val l2 = List.fill(3,3)(math.random)
    val l3 = List.fill(3,3)(math.random)
    val A = new Matrix[Double](3,3, l1)
    val B = new Matrix[Double](3,3, l2)
    val C = new Matrix[Double](3,3, l3)
    assert(A + B + C == A + B + C)
  }
  test("T6"){
    val l1 = List.fill(3,3)(math.random)
    val l2 = List.fill(3,3)(math.random)
    val A = new Matrix[Double](3,3, l1)
    val B = new Matrix[Double](3,3, l2)
    assert((A - B) == -(B - A))
  }
  test("T7"){
    val l1 = List.fill(3,3)(math.random)
    val l2 = List.fill(3,3)(math.random)
    val l3 = List.fill(3,3)(math.random)
    val A = new Matrix[Double](3,3, l1)
    val B = new Matrix[Double](3,3, l2)
    val C = new Matrix[Double](3,3, l3)
    assert((A - B) - C != A - (B - C))
  }
  test("T8"){
    val l1 = List.fill(3,3)(math.random)
    val l2 = List.fill(3,3)(math.random)
    val l3 = List.fill(3,3)(math.random)
    val A = new Matrix[Double](3,3, l1)
    val B = new Matrix[Double](3,3, l2)
    val C = new Matrix[Double](3,3, l3)
    assert(A - B - C == A - B - C)
  }
  test("T9"){
    val l1 = List.fill(3,3)(math.random)
    val l2 = l1.map(x => x.map(y => y*3))
    val A = new Matrix[Double](3,3, l1)
    val B = new Matrix[Double](3,3, l2)
    assert(A * 3 == B)
  }
  test("T10"){
    val l1 = List(List(1,0,9), List(2,5,6), List(7,4,3))
    val l2 = List(List(0,-1,2), List(4,7,9), List(3,5,6))
    val l3 = List(List(9,-5,3), List(2,4,7), List(8,5,2))
    val A = new Matrix[Int](3,3, l1)
    val B = new Matrix[Int](3,3, l2)
    val C = new Matrix[Int](3,3, l3)
    assert(A * (B*C) == (A * B) * C)
  }
  test("T11"){
    val l1 = List(List(1,0,9), List(2,5,6), List(7,4,3))
    val l2 = List(List(0,-1,2), List(4,7,9), List(3,5,6))
    val l3 = List(List(9,-5,3), List(2,4,7), List(8,5,2))
    val A = new Matrix[Int](3,3, l1)
    val B = new Matrix[Int](3,3, l2)
    val C = new Matrix[Int](3,3, l3)
    assert(A * (B + C) == A*B + A*C)
  }
  test("T12"){
    val l1 = List(List(1,0,9), List(2,5,6), List(7,4,3))
    val l2 = List(List(0,-1,2), List(4,7,9), List(3,5,6))
    val l3 = List(List(9,-5,3), List(2,4,7), List(8,5,2))
    val A = new Matrix[Int](3,3, l1)
    val B = new Matrix[Int](3,3, l2)
    val C = new Matrix[Int](3,3, l3)
    assert((A + B) * C == A*C + B*C)
  }
  test("T13"){
    val l1 = List.fill(3,3)(math.random)
    val l2 = l1.map(x => x.map(y => -y))
    val A = new Matrix[Double](3,3, l1)
    val B = new Matrix[Double](3,3, l2)
    assert(-A == B)
  }
  test("T14"){
    val l1 = List.fill(3,3)(math.random)
    val A = new Matrix[Double](3,3, l1)
    assert(A.T.T == A)
  }
  test("T15"){
    val l1 = List.fill(3,3)(math.random)
    val l2 = List.fill(3,3)(math.random)
    val A = new Matrix[Double](3,3, l1)
    val B = new Matrix[Double](3,3, l2)
    assert((A + B).T == A.T + B.T)
  }
  test("T16"){
    val l1 = List.fill(3,3)(math.random)
    val l2 = List.fill(3,3)(math.random)
    val A = new Matrix[Double](3,3, l1)
    val B = new Matrix[Double](3,3, l2)
    assert((A*B).T == B.T * A.T)
  }
  test("T17"){
    val l1 = List.fill(3,3)(math.random)
    val A = new Matrix[Double](3,3, l1)
    assert((A*3).T == A.T * 3)
  }
  test("T18"){
    val l1 = List(List(1,2,3), List(4,5,6), List(7,8,9))
    val l2 = List(List(1,3), List(7,9))
    val A = new Matrix[Int](3,3,l1)
    val B = new Matrix[Int](2,2,l2)
    assert(A.cofactor(1,1) == B)
  }
  test("T19"){
    val l1 = List(List(19))
    val A = new Matrix[Int](1,1,l1)
    assert(A.determinant == 19)
  }
  test("T20"){
    val l1 = List(List(5,9), List(0,3))
    val A = new Matrix[Int](2,2,l1)
    assert(A.determinant == 15)
  }
  test("T21"){
    val l1 = List(List(1,5,6), List(-7,-9,6), List(3,2,1))
    val A = new Matrix[Int](3,3, l1)
    assert(A.determinant == 182L)
  }
  test("T22"){
    val l1 = List(List(1,5), List(0,2))
    val l2 = List(List(2L,0L), List(-5L,1L))
    val A = new Matrix[Int](2,2,l1)
    val B = new Matrix[Long](2,2,l2)
    assert(A.cofactorMatrix == B)
  }
  test("T23"){
    val l1 = List(List(1,5), List(0,2))
    val l2 = List(List(2L,-5L), List(0L,1L))
    val A = new Matrix[Int](2,2,l1)
    val B = new Matrix[Long](2,2,l2)
    assert(A.adjointMatrix == B)
  }
  test("T24"){
    val l1 = List(List(2,1), List(5,3))
    val l2 = List(List(3D,-1D), List(-5D,2D))
    val A = new Matrix[Int](2,2,l1)
    val B = new Matrix[Double](2,2,l2)
    assert(A.inverse == B)
  }
  test("T25"){
    val l1 = List(List(2,1), List(5,3))
    val l2 = List(List(2,1), List(5,-3))
    val A = new Matrix[Int](2,2,l1)
    val B = new Matrix[Int](2,2,l2)
    assert(!(A == B))
  }
  test("T26"){
    val A = new Matrix[Int](2,2, List(List(1,5), List(0,2)))
    val B = new Matrix[Int](2,2, List(List(2,5), List(9,7)))
    val C = new Matrix[Int](4,2, List(List(1,5), List(0,2), List(2,5), List(9,7)))
    assert((A joinByRow B) == C)
  }
  test("T27"){
    val A = new Matrix[Int](2,2, List(List(1,5), List(0,2)))
    val B = new Matrix[Int](2,2, List(List(2,5), List(9,7)))
    val C = new Matrix[Int](2,4, List(List(1,5,2,5), List(0,2,9,7)))
    assert((A joinByCol B) == C)
  }

}
