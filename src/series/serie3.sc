class Rational(n : Int, d : Int)
{
    require(d != 0)

    val g = pgdc(n,d)

    val numerator = n / g
    val denominator = d / g

    def this(n:Int) = this(n,1)


    def add(b: Rational) =
    {
        new Rational(
            numerator*b.denominator+b.numerator*denominator,
            denominator*b.denominator
        )
    }

    def neg() =
    {
        new Rational(
            -numerator,
            denominator
        )
    }

    def less(b : Rational) =
    {
        numerator * b.denominator > denominator * b.numerator
    }

    def greater(b: Rational) =
    {
        numerator * b.denominator < denominator * b.numerator
    }

    def max(b: Rational) =
    {
        if(this < b)
            b
        else
            this
    }

    def sous(b : Rational) =
    {
        new Rational(
            numerator*b.denominator-b.numerator*denominator,
            denominator*b.denominator
        )
    }

    def times(b : Rational) =
    {
        new Rational(
            numerator * b.numerator,
            denominator * b.denominator
        )
    }

    def div(b : Rational) =
    {
        new Rational(
            numerator * b.denominator,
            denominator * b.numerator
        )
    }

    def equals(b: Rational) =
    {
        numerator * b.denominator == denominator * b.numerator
    }

    def +(b:Rational) = add(b)
    def -(b:Rational) = sous(b)
    def *(b:Rational) = times(b)
    def /(b:Rational) = div(b)
    def ==(b:Rational) = equals(b)
    def <(b:Rational) = less(b)
    def >(b:Rational) = greater(b)
    def unary_- = neg()
    def unary_+ = neg() neg()

    private def rearrange(): Rational =
    {
        val pgcdValue = pgdc(numerator,denominator)
        if(numerator < 0 && denominator < 0)
            new Rational(-numerator / pgcdValue,-denominator / pgcdValue)
        else
            new Rational(numerator / pgcdValue,denominator / pgcdValue)
    }

    private def pgdc(a:Int,b:Int):Int =
    {
        def pgcdAbs(a1:Int,b1:Int) =
        {
            if (b1 > a1)
                pgdc(b1,a1)
            else if (b1 == 0)
                a1
            else
                pgdc(b1, a1 - b1)
        }
        def abs(a : Int) = if(a < 0) -a else a
        pgcdAbs(abs(a),abs(b))
    }
    override def toString = s"$numerator/$denominator"
}

object Rational
{
    implicit def intToRational(x:Int) : Rational = new Rational(x)
}

val a = new Rational(1,2)
val b = new Rational(1,3)
val c = new Rational(2,1)
val d = new Rational(3,1)

val e = d.neg()
e * a

d * d

2 * d
d * 2


