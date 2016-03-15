import scala.collection.immutable.Stream

abstract class IntSet
{
    def add(x: Int): IntSet

    def contains(x: Int): Boolean

    def foreach(f: Int => Unit): Unit

    def union(other: IntSet): IntSet

    def intersection(other: IntSet): IntSet

    def excl(x: Int): IntSet

    def +(x:Int):IntSet = add(x)
    def -(x:Int):IntSet = excl(x)
}

object Empty extends IntSet()
{
    override def contains(x: Int): Boolean = false

    override def add(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

    override def foreach(f: (Int) => Unit): Unit = ()

    override def union(other: IntSet): IntSet = other

    override def intersection(other: IntSet): IntSet = Empty

    override def toString: String = s"-"

    override def excl(x: Int): IntSet = Empty
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet
{
    override def add(x: Int): IntSet =
    {
        if (x < elem) new NonEmpty(elem, left add x, right)
        else if (x > elem) new NonEmpty(elem, left, right add x)
        else this
    }

    override def contains(x: Int): Boolean =
    {
        if (x < elem) left contains x
        else if (x > elem) right contains x
        else true
    }

    override def toString: String = s"($left|$elem|$right)"

    def foreach(f: Int => Unit): Unit =
    {
        f(elem)
        left.foreach(f)
        right.foreach(f)
    }

    override def union(other: IntSet): IntSet =
    {
        other.union(left).union(right).add(elem)
    }

    override def intersection(other: IntSet): IntSet =
    {
        if (other.contains(elem))
            Empty.union(left.intersection(other)).union(right.intersection(other)).add(elem)
        else
            left.intersection(other) union right.intersection(other)
    }

    override def excl(x: Int): IntSet =
    {
        if(x == elem)
            left.union(right)
        else
            (left.excl(x) union right.excl(x)).add(elem)
    }
}

Empty.add(1).add(2).add(3).add(4).add(5).add(6).foreach(x => println(x + 1))

val a = Empty.add(2).add(3).add(5).add(6).add(7).add(8)
val c = Empty.add(2).add(3).add(5).add(6).add(7).add(8)
val b = Empty.add(1).add(5).add(6).add(8)

a union b
a intersection b

a intersection a

a.excl(2).excl(3).excl(8)

val o1 = Empty + 3 + 4 + 12 + 5
val o2 = o1 - 3 - 4
