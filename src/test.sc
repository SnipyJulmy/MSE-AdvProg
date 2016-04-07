for (i <- 0 until 9) yield i

def filter[A](list: List[A], a: A => Boolean): List[A] =
    list match
    {
        case Nil => Nil
        case x :: xs => if (a(x)) x :: filter(xs, a) else filter(xs, a)
    }

val l1 = List(1, 2, 3, 4, 5, 6, 7, 8)
filter(l1, (x: Int) => x % 2 == 0)

def partition[A](list: List[A], a: A => Boolean): (List[A], List[A]) =
{
    def partitionAcc(l: (List[A], List[A], List[A])): (List[A], List[A], List[A]) =
    {
        l match
        {
            case (Nil, acc1, acc2) => l
            case (x :: xs, acc1, acc2) =>
                if (a(x))
                    partitionAcc(xs, x :: acc1, acc2)
                else
                    partitionAcc(xs, acc1, x :: acc2)
        }
    }

    val res = partitionAcc((list, List[A](), List[A]()))
    (res._2, res._3)
}

partition(l1, (x: Int) => x % 2 == 0)
