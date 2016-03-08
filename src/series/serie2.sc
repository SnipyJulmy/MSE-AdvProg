import scala.annotation.tailrec

/* Exerice 1 */
def fac(x: Int): Int =
{
    @tailrec
    def facTail(x: Int, acc: Int): Int =
    {
        if (x == 0)
            acc
        else
            facTail(x - 1, acc * x)
    }
    facTail(x, 1)
}
/* Exercice 2 */
def fibNotTail(x: Int): Int =
{
    if (x == 0)
        0
    else if (x == 1)
        1
    else
        fibNotTail(x - 1) + fibNotTail(x - 2)
}
/* Exercice 3 */
def fib(x: Int): Int =
{
    @tailrec
    def fibTail(a: Int, b: Int, x: Int): Int =
    {
        if (x > 0) fibTail(b, a + b, x - 1) else a
    }
    fibTail(1, 1, x)
}
def sum(f: Int => Int, a: Int, b: Int): Int =
{
    @tailrec
    def iter(a: Int, acc: Int): Int =
    {
        if (a == b)
            acc + f(a)
        else
            iter(a + 1, acc + f(a))
    }
    iter(a, 0)
}
def mul(x: Int) = (y: Int) => x * y
mul(3)(4)
def product(f: Int => Int) =
{
    (a: Int) => (b: Int) =>
    {
        @tailrec
        def iter(a1: Int, acc: Int): Int =
        {
            if (a1 == b)
                acc
            else
                iter(a1 + 1, acc * f(a1))
        }
        iter(a, 1)
    }
}
def facProduct(x: Int) = product((x) => x)(1)(x + 1)
def general(op: (Int, Int) => Int)=
{
    (init : Int) => (f: Int => Int) => (a: Int) => (b: Int) =>
    {
        @tailrec
        def iter(a1: Int, acc: Int): Int =
        {
            if (a1 == b)
                acc
            else
                iter(a1 + 1, op(acc, f(a1)))
        }
        iter(a, init)
    }
}
general(_ + _)(0)(2 * _)(3)(9)
general(_ * _)(1)(2 * _)(3)(9)
def productNoInit = general(_ * _)
def productGen = general(_ * _)(1)
def sumGen = general(_ + _)(0)
val a = productGen(2 * _)(3)(9)

6 + 8 + 10 + 12 + 14 + 16
