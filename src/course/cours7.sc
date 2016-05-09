case class Foo(n : Int) extends Ordered[Foo]
{
    override def compare(that: Foo): Int =
    {
        n - that.n
    }
}

val a = new Foo(2)
val b = new Foo(3)
val c = new Foo(2)

a < b
a > b
a == b
a == c
a >= c


