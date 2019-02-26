object BT extends App{


  abstract class Tree[T] {


    def insert(key: T) : Tree[T]
    def remove(key: T) : Tree[T]
    def reverse() : Tree[T]

    def search(value: T) : Unit
    def traversal(str: String) : Unit

    def max: T
    def min: T

    def isEmpty: Boolean
  }


  case class EmptyTree[T<: AnyVal]()(implicit f: T => Double) extends Tree[T] {
    def insert(key: T) : Tree[T] = NonEmptyTree[T](key, EmptyTree[T](), EmptyTree[T]())
    def remove(key: T): Tree[T] = this
    def reverse(): Tree[T] = this

    def max = throw new NoSuchElementException("EmptyTree.max")
    def min = throw new NoSuchElementException("EmptyTree.min")

    def traversal(str: String): Unit = {}
    def search(key: T): Unit = println("This tree haven't " + key)

    def isEmpty = true
  }

  case class NonEmptyTree[T<: AnyVal](key: T, left: Tree[T], right: Tree[T])(implicit f: T => Double) extends Tree[T]{
    def insert(value: T) : Tree[T] = value match {
      case this.key=> this
      case _ =>
        if (value>(key)) NonEmptyTree[T](key, left, right.insert(value))
        else NonEmptyTree[T](key, left.insert(value), right)

    }

    def remove(value: T) : Tree[T] = value match {
      case this.key => this match {
        case NonEmptyTree(_, EmptyTree(), EmptyTree()) => EmptyTree()
        case NonEmptyTree(_, left, EmptyTree()) => left
        case NonEmptyTree(_, EmptyTree(), right) => right
        case NonEmptyTree(_, left, right)=> {
          NonEmptyTree(right.min, left, right.remove(right.min))
        }
      }
      case _ =>
        if (value>key) NonEmptyTree(key, left, right.remove(value))
        else NonEmptyTree(key, left.remove(value), right)

    }

    def max: T = {
      def loop(t: Tree[T], value: T): T = t match{
        case EmptyTree() => value
        case NonEmptyTree(value, _, right) => loop(right, value)
      }
      if (isEmpty) throw new NoSuchElementException("EmptyTree.max")
      loop(right, key)
    }

    def min: T = {
      def loop(t: Tree[T], value: T): T = t match {
        case EmptyTree() => value
        case NonEmptyTree(value, left, _) => loop(left, value)
      }
      if (isEmpty) throw new NoSuchElementException("EmptyTree.min")
      loop(left, key)
    }

    def isEmpty = false

    def search(value: T): Unit = this match {
      case NonEmptyTree(_,_,_) =>
        if (value==this.key) println("This tree have " + value)
        else if (value>this.key) this.right.search(value)
        else this.left.search(value)

      case _ => println("This tree haven't " + value)
    }

    override def toString: String =
      if (isEmpty) "."
      else "{" + left + " " + key + " " + right + "}"

    def traversal(order: String): Unit = this match {
      case NonEmptyTree(_,_,_) =>
        if (order == "preorder") println(this.key)
        this.left.traversal(order)
        if (order == "inorder") println(this.key)
        this.right.traversal(order)
        if (order == "postorder") println(this.key)

      case _ =>
    }

    def reverse(): Tree[T] =  this match {
      case NonEmptyTree(key, left, right) => NonEmptyTree(key, right.reverse, left.reverse)
      case _ => EmptyTree()
    }

  }

  val tree = EmptyTree[Int]()
}
