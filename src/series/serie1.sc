/* Part 1 */
// Square
def square(x:Double) = x*x
// Pow 4
def pow4(x:Double) = square(square(x))
// g : (Int,Boolean) => String
def bar(x:Int,y:Boolean) = "Hello"

def abs(x: Double):Double =
{
    if(x < 0) -x else x
}

def isGoodEnough(epsilon:Double): Boolean =
{
    abs(epsilon) < 0.00001
}

def newton(x0: Double, f: (Double) => Double, ff: (Double) => Double): Double =
{
    val epsilon = f(x0)/ff(x0)
    if (isGoodEnough(epsilon))
        x0 - epsilon
    else
        newton(x0-epsilon,f,ff)
}

/* Part 2 */
def sqrt(double: Double):Double =
{
    def f(x:Double) = square(x)-double
    def ff(x:Double) = 2.0*x
    val x0 = 10
    newton(x0,f,ff)
}
def sqrtNested(double : Double):Double =
    newton(10.0,(x) => square(x) - double, (x) => 2.0*x)

def cubicRoot(double: Double):Double =
{
    def f(x:Double) = x*x*x-double
    def ff(x:Double) = 3.0*square(x)
    val x0 = 2
    newton(x0,f,ff)
}

def and(x:Boolean,y:Boolean):Boolean =
{
    if(x) y else false
}

def or(x:Boolean,y:Boolean):Boolean =
{
    if(x) x else y
}

cubicRoot(27)
sqrtNested(81)
sqrt(81)
