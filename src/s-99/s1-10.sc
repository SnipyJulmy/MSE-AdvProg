val l1 = List(1, 2, 3, 4, 5, 6, 7)

def last[A](list: List[A]): A =
{
    list match
    {
        case Nil => sys.error("List is empty")
        case x :: Nil => x
        case x :: xs => last(xs)
    }
}

def penultimate[A](list: List[A]): A =
{
    list match
    {
        case Nil => sys.error("List is empty")
        case xp :: xl :: Nil => xp
        case x :: xs => penultimate(xs)
    }
}

def nth[A](list: List[A], nbr: Int): A =
{
    (list, nbr) match
    {
        case (x :: xs, 0) => x
        case (Nil, n) if n <= 1 =>
            sys.error(s"List length is ${list.length}, $nbr dont exist")
        case (x :: xs, n) => nth(xs, n - 1)
    }
}

def length[A](list: List[A]): Int =
{
    list match
    {
        case Nil => 0
        case x :: xs => 1 + length(xs)
    }
}

def reverse[A](list: List[A]): List[A] =
{
    def reverseAcc(l: List[A], acc: List[A]): List[A] =
    {
        (l, acc) match
        {
            case (Nil, a) => a
            case (x :: xs, a) => reverseAcc(xs, x :: a)
        }
    }
    reverseAcc(list, List[A]())
}


def palindrome[A](list: List[A]): Boolean =
{
    list == reverse(list)
}

def flatten(list: List[Any]): List[Any] =
{
    list.foldRight(List[Any]())((elem, acc) =>
        elem match
        {
            case x :: xs => x :: flatten(xs) ::: acc
            case x => x :: acc
        }
    )
}

val l2 = List('a', 'a', 'a', 'b', 'b', 'c', 'd', 'c', 'd', 'd')

def compress[A](list: List[A]): List[A] =
{
    list.foldRight(List[A]())((elem, acc) =>
        (elem, acc) match
        {
            case (e, x :: xs) if e == x => x :: xs
            case (e, a) => e :: a
        }
    )
}

def pack[A](list: List[A]): List[List[A]] =
{
    list match
    {
        case x1::x2::xs => Nil
    }
}
