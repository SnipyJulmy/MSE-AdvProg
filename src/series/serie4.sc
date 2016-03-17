import scala.annotation.tailrec

// Question 1
sealed abstract class Expr

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Product(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match
{
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case Product(e1, e2) => eval(e1) * eval(e2)
}

def show(e: Expr): String = e match
{
    case Number(n) => n.toString
    case Product(Sum(e1, e2), e3) => "(" + show(Sum(e1, e2)) + ")*" + show(e3)
    case Product(e1, Sum(e2, e3)) => show(e3) + "(" + show(Sum(e1, e2)) + ")"
    case Sum(e1, e2) => show(e1) + "+" + show(e2)
    case Product(e1, e2) => show(e1) + "*" + show(e2)
}

val expr0 = Sum(Product(Number(2), Number(3)), Number(4))
println("Expr0: " + show(expr0))
assert(eval(expr0) == 10)
val expr1 = Product(Number(4), Number(12))
println("Expr1: " + show(expr1))
assert(eval(expr1) == 48)
val expr2 = Product(Sum(Number(2), Number(3)), Number(4))
println("Expr2: " + show(expr2))
assert(eval(expr2) == 20)
val expr3 = Product(Number(2), Sum(Number(3), Number(4)))
println("Expr3: " + show(expr3))
assert(eval(expr3) == 14)

// Question 2

sealed abstract class BinaryTree

case class Leaf(value: Int) extends BinaryTree

case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

def sumLeaves(binaryTree: BinaryTree): Int = binaryTree match
{
    case Leaf(n) => n
    case Node(l, r) => sumLeaves(l) + sumLeaves(r)
}

def smallest(binaryTree: BinaryTree): Int = binaryTree match
{
    case Leaf(n) => n
    case Node(l, r) => Math.min(smallest(l), smallest(r))
}

// Question 3

val l1 = List(1, 5, 6, 7, 9, 3, 1, 5, 7, 9)

/*
 * Complexity of last : O(n)
 * because wee need to browse all over the list
 */
@tailrec
def last[T](l: List[T]): T = l match
{
    case Nil => sys.error("Nil has no last")
    case head :: Nil => head
    case head :: tail => last(tail)
}

def init[T](l: List[T]): List[T] = l match
{
    case Nil => Nil
    case head :: Nil => Nil
    case head :: tail => head :: init(tail)
}
/*
 * Complexity of reverse : O(n^2)
 * See explanation for concat
 */
def reverse[T](list: List[T]): List[T] =
{
    @tailrec
    def reverseTail(l: List[T], acc: List[T]): List[T] = l match
    {
        case Nil => acc
        case head :: tail => reverseTail(tail, head :: acc)
    }
    reverseTail(list, Nil)
}

/*
 * Complexity of concat : O(n * O(::))
 * because wee need to browse all over the list and apply the O(::)
 * to all element which is O(n) too so the complexity of concat is O(n^2)
 */
def concat[T](list1: List[T], list2: List[T]): List[T] =
{
    (list1, list2) match
    {
        case (Nil, Nil) => Nil
        case (Nil, head :: tail) => head :: concat(Nil, tail)
        case (head :: tail, b) => head :: concat(tail, b)
    }
}

def take[T](l: List[T], nbr: Int): List[T] = (l, nbr) match
{
    case (Nil, n) => Nil
    case (list, 0) => Nil
    case (head :: tail, n) => head :: take(tail, n - 1)
}

def drop[T](l: List[T], nbr: Int): List[T] = (l, nbr) match
{
    case (Nil, n) => Nil
    case (head :: tail, 1) => tail
    case (head :: tail, n) => drop(tail, n - 1)
}

def apply[T](l: List[T], position: Int): T =
{
    assert(position <= l.length)
    (l, position) match
    {
        case (Nil, n) => sys.error("Nil has no nth element")
        case (head :: tail, 0) => head
        case (head :: tail, n) => apply(tail, n - 1)
    }
}

last(l1)
init(l1)
reverse(l1)
val list1 = List(1, 2, 3)
val list2 = List(4, 5, 6)
concat(list1, list2)
l1
concat(take(l1, 3), drop(l1, 3))
apply(l1, 3)
apply(l1, 4)

def any[T](predicate: T => Boolean)(list: List[T]): Boolean =
{
    (list, predicate) match
    {
        case (Nil, p) => false
        case (head :: tail, p) => if (p(head)) true else any(p)(tail)
    }
}

def every[T](predicate: T => Boolean)(list: List[T]): Boolean =
{
    (list, predicate) match
    {
        case (Nil,p) => true
        case (head::tail,p) => if(!p(head)) false else every(p)(tail)
    }
}

def pred: Int => Boolean = (x) => x == 9
def pair: Int => Boolean = (x) => (x % 2) == 0

val l2 = List(2,4,6,8,10,56)
val l3 = List(3,4,6,8,20)
val l4 = List(2,4,6,8,9)

def t = every(pair)_

t(l2)
assert(t(l2))
t(l3)
assert(!t(l3))
t(l4)
assert(!t(l4))
