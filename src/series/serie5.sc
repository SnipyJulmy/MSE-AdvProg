import scala.annotation.tailrec

// Question 1

// a
def lengthStrings(list: List[String]): List[Int] =
{
    list.map(_.length)
}

// b
def dup[A](elem: A, nbr: Int): List[A] =
{
    nbr match
    {
        case 0 => Nil
        case n => elem :: dup(elem, n - 1)
    }
}

def dot(list1: List[Int], list2: List[Int]): List[Int] =
{
    (list1 zip list2).map(x => x._1 * x._2)
}

longuest(List("abc", "asdf", "pkafdajdakda"))
dup(List(1, 2), 3)
dot(List(1, 2, 3), List(4, 5, 6))
isPresent(List(1,2,3,4,5),6)

// Question 2

//noinspection FoldTrueAnd
def areTrue(list: List[Boolean]): Boolean =
{
    list.foldRight(true)((x, y) => x && y)
}

//noinspection SimplifiableFoldOrReduce
def lString(list: List[String]) : Int =
{
    list.map(_.length).foldLeft(0)(_ + _)
}

def longuest(list: List[String]) : (Int,String) =
{
    list.map(s => (s.length,s)).foldLeft((-1,"abdullah"))((x,y) => if(x._1 > y._1 ) x else y)
}

def isPresent[A](list: List[A], elem : A) =
{
    list.map(_ == elem).foldLeft(false)((x,y) => x || y)
}
