trait Stack[A >: Null]
{
    def push(elem: A): Stack[A]

    def top: A

    def pop: Stack[A]
}

class EmptyStack[A >: Null]() extends Stack[A]
{
    override def push(elem: A): Stack[A] = new ElemStack[A](elem :: Nil)

    override def top: A = null

    override def pop: Stack[A] = this

    override def toString = s"EmptyStack()"
}

class ElemStack[A >: Null](data: List[A]) extends Stack[A]
{
    override def push(elem: A): Stack[A] = new ElemStack[A](elem :: data)

    override def top: A = data.head

    override def pop: Stack[A] = new ElemStack[A](data.tail)

    override def toString = "ElemStack(" + data.map(_.toString).mkString + ")"
}

val a = new EmptyStack[Int]

a.push(1).push(2).push(3)