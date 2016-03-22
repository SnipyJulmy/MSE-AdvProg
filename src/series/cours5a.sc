val l1 = List(1, 2, 3, 4)
val l2 = List(4, 3, 2, 1)

(l1 union l2).distinct

l1 ::: l2

l1 intersect l2

l1.mkString("list : ", " , ", " !")

l1.map(Math.pow(_, 2).toInt)

List.range(1, 100, 3).filter(_ % 8 == 0)
List("apple", "orange", "banana", "peach").map(_.length).min

def fac(n: => Int) = (1 to n).product

List.range(1, 100, 3).reduceLeft((x, y) => if (x > y) x else y)
List.range(1, 100, 3).reduceRight((x, y) => if (x > y) x else y)

List("a", "b", "c").reduceLeft(_ + _)
List("a", "b", "c").reduceRight(_ + _)

List.range(0, 10).foldRight(0)((x, y) => x + y)
List.range(3, 100, 2).foldLeft(false)((x, y) => x || (y % 2 == 0))

List(1, 2, 3).foldLeft(List(4))((list, e) => e :: list)

List(1, 2, 3).foldRight(List(4))((list, e) => list :: e)

def length[A](x: List[A]): Int =
{
    x.foldRight(0)((x,y) => y + 1)
}

length(List(1,2,3,4))

def map[A, B](x: List[A], f: A => B): List[B] =
{
   x.foldRight(List[B]())((x,y) => f(x)::y)
}

map(List(1,2,3,4,5),(x : Int) => x * 2)

val lz = List(1,2,3)
val lz2 = List(1,2,3,4)


def dup[A](l : List[A]) : List[A] =
{
    l.flatMap((x) => List(x,x))
}

dup(lz)
