class Rational(n : Int, d : Int)
{
    val numerator = n
    val denominator = d

    def add(b: Rational) =
    {
        new Rational(
            numerator*b.denominator+b.numerator*denominator,
            denominator*b.denominator
        ).rearrange()
    }

    def sous(b : Rational) =
    {
        new Rational(
            numerator*b.denominator-b.numerator*denominator,
            denominator*b.denominator
        ).rearrange()
    }

    def times(b : Rational) =
    {
        new Rational(
            numerator * b.numerator,
            denominator * b.denominator
        ).rearrange()
    }

    def div(b : Rational) =
    {
        new Rational(
            numerator * b.denominator,
            denominator * b.numerator
        ).rearrange()
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

    private def rearrange(): Rational =
    {
        val pgcdValue = pgdc(numerator,denominator)
        new Rational(numerator / pgcdValue, denominator / pgcdValue)
    }

    private def pgdc(a:Int,b:Int):Int =
    {
        if (b > a)
            pgdc(b,a)
        else if (b == 0)
            a
        else
            pgdc(b, a - b)
    }

    override def toString = s"$numerator/$denominator"
}

val a = new Rational(1,2)
val b = new Rational(1,3)
val c = new Rational(2,1)
val d = new Rational(3,1)

