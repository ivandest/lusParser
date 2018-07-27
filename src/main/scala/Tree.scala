trait Tree[+T] {
def elem: T
  def left: Tree[T]
  def right: Tree[T]
}

object EmptyTree extends Tree[Nothing] {
  def elem =
    throw new NoSuchElementException("EmptyTree.elem")
  def left =
    throw new NoSuchElementException("EmptyTree.left")
  def right =
    throw new NoSuchElementException("EmptyTree.right")
}

/*class Branch[+T](
                  val elem: T,
                  val left: Tree[T],
                  val right: Tree[T]
                ) extends Tree[T]{
  override def equals(other: Any) = other match {
    case that: Branch[T] => this.elem == that.elem &&
      this.left == that.left &&
      this.right == that.right
    case _ => false
}*/

/*
  def canEqual(other: Any) = other.isInstanceOf[Branch[_]]
  override def hashCode: Int =
    41 * (
      41 * (
        41 + elem.hashCode
        ) + left.hashCode
      ) + right.hashCode*/

class IntTree(root: Int) extends Tree[Int] {

  var _rootElem: Int = root

  //  var _left = 1
  //  var _right = 2

  private var _leftTree: IntTree = _
  private var _rightTree: IntTree = _

  def elem = root

  def left = _leftTree

  def left_=(leftTree: IntTree): Unit = {
    _leftTree = leftTree
  }

  def right = _rightTree

  def right_=(rightTree: IntTree): Unit = {
    _rightTree = rightTree
  }


}

class TupleTree(root: Tuple2[Int, Array[String]]) extends Tree[Tuple2[Int, Array[String]]] {

  var _rootElem: Tuple2[Int, Array[String]] = root

  private var _leftTree: TupleTree = _
  private var _rightTree: TupleTree = _

  def elem = root

  def left = _leftTree
  def left_=(leftTree: TupleTree): Unit = {
    _leftTree = leftTree
  }

  def right = _rightTree
  def right_=(rightTree: TupleTree): Unit = {
    _rightTree = rightTree
  }
}
