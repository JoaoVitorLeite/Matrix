package matrix


import Num.Implicits._

class Matrix[A](val row: A, val col: A, values: List[List[A]])(implicit op: Num[A]){

  def apply(row: Int): scala.List[A] = values(row)

  def apply(row: A, col: A): A = values(row.toINT)(col.toINT)

  private def ADSUBVector(xs: List[A], ys: List[A])(f: (A,A) => A): List[A] = (xs,ys) match{
    case (_, Nil) => xs
    case (Nil, _) => ys
    case (x::xs1, y::ys1) => f(x,y)::ADSUBVector(xs1,ys1)(f)
  }

  def + (other: Matrix[A]): Matrix[A] = {
    require(other.row == row && other.col == col)
    new Matrix[A](row, col, (for(i <- 0 until row.toINT)yield ADSUBVector(this(i), other(i))((x,y) => x+y)).toList)
  }

  def - (other: Matrix[A]): Matrix[A] = {
    require(other.row == row && other.col == col)
    new Matrix[A](row, col, (for(i <- 0 until row.toINT)yield  ADSUBVector(this(i), other(i))((x,y) => x-y)).toList)
  }

  def * (const: A): Matrix[A] = {
    new Matrix[A](row, col, values.map(x => x.map(_*const)))
  }

  def unary_- : Matrix[A] = new Matrix[A](row, col, values.map(x => x.map(-_)))

  def Row(r: Int): List[A] = values(r)

  def Col(c: Int): List[A] = values.map(x => x(c))

  private def multipleSum(v1: List[A], v2: List[A]): A = (for(i <- v1.indices) yield v1(i)*v2(i)).toList.foldLeft(op.zero)((x,y) => x+y)

  def * (other: Matrix[A]): Matrix[A] = {
    require(col == other.row)
    val res = (for{
      i <- 0 until row.toINT
      j <- 0 until other.col.toINT
    }yield multipleSum(this.Row(i), other.Col(j))).toList.grouped(other.col.toINT).toList
    new Matrix[A](row, other.col, res)
  }

  def T: Matrix[A] = new Matrix[A](col, row, (for(i <- 0 until col.toINT) yield Col(i)).toList)

  private def filterElem(xs: List[A], pos: Int): List[A] = {
    @scala.annotation.tailrec
    def loop(n: Int, acc: List[A]): List[A] = {
      if(n == xs.length) acc
      else
        if(n == pos) loop(n+1, acc) else loop(n+1, acc:+xs(n))
    }
    loop(0, Nil)
  }

  def cofactor(r: Int, c: Int): Matrix[A] = {
    val aux = (for{
      i <- 0 until row.toINT
      if i != r
    }yield values(i)).toList
    new Matrix[A](row-op.one, col - op.one, aux.map(x => filterElem(x, c)))
  }

  def determinant: Long = {
    if(row == 1 && col == 1) values.head.head.toLONG
    else if (row == 2 && col == 2) (values.head.head * values(1)(1) - values.head(1) * values(1).head).toLONG
    else {
      (for{
        i <- 0 until col.toINT
      }yield math.pow(-1, i).toLong * values.head(i).toLONG * cofactor(0,i).determinant).toList.sum
    }
  }

  def identityMatrix(row: A, col: A): Matrix[A] = {
    require(row == col)
    val aux = List.fill(row.toINT, col.toINT)(op.zero)
    new Matrix[A](row, col, (for(i <- 0 until row.toINT)yield aux(i).updated(i, op.one)).toList)
  }

  def cofactorMatrix: Matrix[Long] = {
    val res = (for{
      i <- 0 until row.toINT
      j <- 0 until col.toINT
    }yield math.pow(-1, i+j).toLong * cofactor(i,j).determinant).toList.grouped(row.toINT).toList
    new Matrix(row.toLONG, col.toLONG, res)
  }

  def adjointMatrix: Matrix[Long] = cofactorMatrix.T

  def inverse: Matrix[Double] = {
    val det = determinant
    require(det != 0)
    val adj = adjointMatrix
    val res = for{
      i <- 0 until row.toINT
    }yield adj(i).map(x => x.toDouble/det)
    new Matrix[Double](row.toDOUBLE, col.toDOUBLE, res.toList)
  }

  def Values: List[List[A]] = values

  def joinByRow(other: Matrix[A]): Matrix[A] ={
    @scala.annotation.tailrec
    def foo(ys: List[List[A]], acc: List[List[A]]): List[List[A]] ={
      if(ys.isEmpty) acc
      else foo(ys.tail, acc :+ ys.head)
    }
    new Matrix[A](row+other.row, col, foo(other.Values, this.values))
  }

  def joinByCol(other: Matrix[A]): Matrix[A] ={
    val res = for{
      i <- 0 until other.row.toINT
    }yield values(i) ++ other.Values(i)
    new Matrix[A](row, col + other.col, res.toList)
  }

  override def equals(obj: Any): Boolean = obj match {
    case v: Matrix[A] if row != v.row || col != v.col  => false
    case s: Matrix[A] => this.Values == s.Values
    case _ => false
  }

  override def toString: String = values.map(x =>  "[" + x.mkString(", ") + "]").mkString("\n")
}

