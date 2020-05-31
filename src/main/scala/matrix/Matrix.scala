package matrix

import Num.Implicits._

/***
 * @author João Vitor Leite
 */

/***
 * Class to represent Matrix in Scala
 * @param row Number of rows
 * @param col Number of columns
 * @param values List of Lits with the values
 * @param op implicit for type class
 * @tparam A Type class
 */
class Matrix[A](val row: A, val col: A, values: List[List[A]])(implicit op: Num[A]){

  /***
   * Method apply to get a row from the matrix
   * @param row Index of the row
   * @return List of values
   */
  def apply(row: Int): scala.List[A] = values(row)

  /***
   * Method to get a value from the matrix
   * @param row Index of the row
   * @param col Index of the column
   * @return A value from the matrix
   */
  def apply(row: A, col: A): A = values(row.toINT)(col.toINT)

  /***
   * Private method for adding or subtracting Lists
   * @param xs A list
   * @param ys A list
   * @param f Function to apply in the lists
   * @return List of values
   */
  private def ADSUBVector(xs: List[A], ys: List[A])(f: (A,A) => A): List[A] = (xs,ys) match{
    case (_, Nil) => xs
    case (Nil, _) => ys
    case (x::xs1, y::ys1) => f(x,y)::ADSUBVector(xs1,ys1)(f)
  }

  /***
   * Method to perform the addition of matrices
   * @param other A matrix
   * @return Matrix resulting from the addition of two matrices
   */
  def + (other: Matrix[A]): Matrix[A] = {
    require(other.row == row && other.col == col)
    new Matrix[A](row, col, (for(i <- 0 until row.toINT)yield ADSUBVector(this(i), other(i))((x,y) => x+y)).toList)
  }

  /***
   * Method to perform the subtraction of matrices
   * @param other A matrix
   * @return Matrix resulting from the subtraction of two matrices
   */
  def - (other: Matrix[A]): Matrix[A] = {
    require(other.row == row && other.col == col)
    new Matrix[A](row, col, (for(i <- 0 until row.toINT)yield  ADSUBVector(this(i), other(i))((x,y) => x-y)).toList)
  }

  /***
   * Method to perform the multiplication by a constant
   * @param const A constant
   * @return Matrix resulting from the multiplication by a constant
   */
  def * (const: A): Matrix[A] = {
    new Matrix[A](row, col, values.map(x => x.map(_*const)))
  }

  /***
   * Method for finding the "negative matrix
   * @return Matrix with all members multiplied by -1
   */
  def unary_- : Matrix[A] = new Matrix[A](row, col, values.map(x => x.map(-_)))

  /***
   * Method to get a row from the matrix
   * @param r Index of the row
   * @return List of values
   */
  def Row(r: Int): List[A] = values(r)

  /***
   * Method to get a column from the matrix
   * @param c Index of the column
   * @return List of values
   */
  def Col(c: Int): List[A] = values.map(x => x(c))

  /***
   * Method for calculating the inner product between two lists of values
   * @param v1 A list
   * @param v2 A list
   * @return A number
   */
  private def multipleSum(v1: List[A], v2: List[A]): A = (for(i <- v1.indices) yield v1(i)*v2(i)).toList.foldLeft(op.zero)((x,y) => x+y)

  /***
   * Mdthod to perform de multiplication of matrices
   * @param other A matrix
   * @return Matrix resulting from the multiplication of two matrices
   */
  def * (other: Matrix[A]): Matrix[A] = {
    require(col == other.row)
    val res = (for{
      i <- 0 until row.toINT
      j <- 0 until other.col.toINT
    }yield multipleSum(this.Row(i), other.Col(j))).toList.grouped(other.col.toINT).toList
    new Matrix[A](row, other.col, res)
  }

  /***
   * Method to find the transpose matrix
   * @return Transpose matrix
   */
  def T: Matrix[A] = new Matrix[A](col, row, (for(i <- 0 until col.toINT) yield Col(i)).toList)

  /***
   * Method to remove element on a given index from a list
   * @param xs A list
   * @param pos Index to remove the element from the list
   * @return List without the element in the passed position
   */
  private def filterElem(xs: List[A], pos: Int): List[A] = {
    @scala.annotation.tailrec
    def loop(n: Int, acc: List[A]): List[A] = {
      if(n == xs.length) acc
      else
        if(n == pos) loop(n+1, acc) else loop(n+1, acc:+xs(n))
    }
    loop(0, Nil)
  }

  /***
   * Cofactor of element from the matrix
   * @param r Row of the cofactor
   * @param c Column of the cofactor
   * @return Cofactor matrix of Crc
   */
  def cofactor(r: Int, c: Int): Matrix[A] = {
    val aux = (for{
      i <- 0 until row.toINT
      if i != r
    }yield values(i)).toList
    new Matrix[A](row-op.one, col - op.one, aux.map(x => filterElem(x, c)))
  }

  /***
   * Determinant
   * @return Determinant of the matrix
   */
  def determinant: Long = {
    if(row == 1 && col == 1) values.head.head.toLONG
    else if (row == 2 && col == 2) (values.head.head * values(1)(1) - values.head(1) * values(1).head).toLONG
    else {
      (for{
        i <- 0 until col.toINT
      }yield math.pow(-1, i).toLong * values.head(i).toLONG * cofactor(0,i).determinant).toList.sum
    }
  }

  /***
   * Identity matrix
   * @param row Number of rows
   * @param col Number of columns
   * @return Identity matrix
   */
  def identityMatrix(row: A, col: A): Matrix[A] = {
    require(row == col)
    val aux = List.fill(row.toINT, col.toINT)(op.zero)
    new Matrix[A](row, col, (for(i <- 0 until row.toINT)yield aux(i).updated(i, op.one)).toList)
  }

  /***
   * Cofactor matrix
   * @return Cofactor matrix
   */
  def cofactorMatrix: Matrix[Long] = {
    val res = (for{
      i <- 0 until row.toINT
      j <- 0 until col.toINT
    }yield math.pow(-1, i+j).toLong * cofactor(i,j).determinant).toList.grouped(row.toINT).toList
    new Matrix(row.toLONG, col.toLONG, res)
  }

  /***
   * Adjoint matrix
   * @return Adjoint matrix
   */
  def adjointMatrix: Matrix[Long] = cofactorMatrix.T

  /***
   * Inverse matrix
   * @return Inverse Matrix
   */
  def inverse: Matrix[Double] = {
    val det = determinant
    require(det != 0)
    val adj = adjointMatrix
    val res = for{
      i <- 0 until row.toINT
    }yield adj(i).map(x => x.toDouble/det)
    new Matrix[Double](row.toDOUBLE, col.toDOUBLE, res.toList)
  }

  /***
   * Values from the matrix
   * @return List of list with the values from the matrix
   */
  def Values: List[List[A]] = values

  /***
   * Method to join matrices by rows
   * @param other A matrix
   * @return A matrix
   */
  def joinByRow(other: Matrix[A]): Matrix[A] ={
    @scala.annotation.tailrec
    def foo(ys: List[List[A]], acc: List[List[A]]): List[List[A]] ={
      if(ys.isEmpty) acc
      else foo(ys.tail, acc :+ ys.head)
    }
    new Matrix[A](row+other.row, col, foo(other.Values, this.values))
  }

  /***
   * Method to join matrices by columns
   * @param other A matrix
   * @return A matrix
   */
  def joinByCol(other: Matrix[A]): Matrix[A] ={
    val res = for{
      i <- 0 until other.row.toINT
    }yield values(i) ++ other.Values(i)
    new Matrix[A](row, col + other.col, res.toList)
  }

  /***
   * Method to compare matrices
   * @param obj Any object
   * @return Return true if the matrices are equal, otherwise false
   */
  override def equals(obj: Any): Boolean = obj match {
    case v: Matrix[A] if row != v.row || col != v.col  => false
    case s: Matrix[A] => this.Values == s.Values
    case _ => false
  }

  /***
   * Method to return the matrix in string format
   * @return String format of the matrix
   */
  override def toString: String = values.map(x =>  "[" + x.mkString(", ") + "]").mkString("\n")
}

