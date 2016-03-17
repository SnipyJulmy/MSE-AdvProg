import scala.annotation.tailrec

def foo(x: Int) =
{
    x match
    {
        case 1 => 2
        case 2 => 1
        case _ =>
    }
}

def patFoo(x: Any) =
{
    x match
    {
        case a: Int if (a % 4) == 0 => true
        case b: String => "QWERTZUIOPASDFGHJKLYXCVNM".contains(b)
        case b: Char => "QWERTZUIOPASDFGHJKLYXCVNM".contains(b)
        case c: Boolean => true
        case _ => false
    }
}

patFoo(100)
patFoo("a")
patFoo('A')
patFoo(true)
patFoo(false)
patFoo(null)

abstract class Expr

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match
{
    case Number(n) ⇒ n
    case Sum(e1, e2) ⇒ eval(e1) + eval(e2)
}

Sum(Number(5), Number(4))

val fruit = 1 :: 2 :: 3 :: 4 :: Nil
val h = fruit.head
val t = fruit.tail

val x = 6 :: 4 :: 98 :: 87 :: 1 :: 214 :: Nil

def insert(elem: Int, l: List[Int]): List[Int] =
{
    if (l.isEmpty)
        elem :: Nil
    else if (elem <= l.head)
        elem :: l
    else
        l.head :: insert(elem, l.tail)
}

def insert_patternMatching(elem : Int, l : List[Int]) : List[Int] =
{
    l match
        {
        case Nil => List(elem)
        case head :: rest =>
            if (elem <= head)
                elem :: l
            else
                head :: insert_patternMatching(elem,rest)
    }
}

def lsort(l: List[Int]): List[Int] =
{
    if (l.isEmpty)
        Nil
    else
        //insert(l.head, lsort(l.tail))
        insert_patternMatching(l.head,lsort(l.tail))
}

lsort(x)
