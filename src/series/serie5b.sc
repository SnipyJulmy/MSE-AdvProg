// Question 3
def primeSum(max: Int): List[(Int, Int)] =
    for
    {
        i <- (1 to max).toList
        j <- (i to max).toList
        if isPrime(i + j)
    } yield (i, j)

def primeSum3(max: Int): List[(Int, Int, Int)] =
    for
    {
        i <- (1 to max).toList
        j <- (1 to max).toList
        k <- (1 to max).toList
        if isPrime(i * 2 + j % k)
    } yield (i, j, k)

def isPrime(i: Int): Boolean =
    i match
    {
        case _i if i <= 1 => false
        case 2 => false
        case _ => !(2 until i).exists(x => i % x == 0)
    }

val ps = primeSum(1)
val pa = primeSum3(100)

removeDupFold(ps)
removeDupPatternMatching(ps).filter(x => isPrime(x._1) && isPrime(x._2))

def removeDupPatternMatching(list: List[(Int, Int)]): List[(Int, Int)] =
    list match
    {
        case Nil => Nil
        case x :: xs =>
            if (xs.contains((x._2, x._1)))
                removeDupPatternMatching(xs)
            else
                x :: removeDupPatternMatching(xs)

    }

def removeDupFold(list: List[(Int, Int)]): List[(Int, Int)] =
{
    list.foldLeft(List[(Int, Int)]())((x, y) =>
        if (x.contains((y._2, y._1)))
            x
        else y :: x)
}

val cities = List("Berlin", "Paris", "Moscow", "Bejing")
val relatives = List("Dr", "King", "One-eyed")
val travellers = List("Adolf","Jozef","Reihnart")

val l = for
    {
        c <- cities
        r <- relatives
        t <- travellers
        if r.startsWith("O")
    } yield (c,r,t)

l.foreach(x => println("Dear "+x._2+", Wish you were here in "+x._1+"! Love, " + x._3))
