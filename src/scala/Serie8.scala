import utils.time

/**
  * Created by snipy on 26.04.16.
  */
object Serie8 extends App
{
    def integrate(a: Double, b: Double, n: Int, f: (Double => Double)): Double =
    {
        val step = (b - a) / n
        val deltas = (a to b by step).toList
        val yList = deltas.map(f(_))
        val y0 = yList.head
        val yi = yList.tail.dropRight(1)
        val yn = yList.last

        val A = step / 2 * (y0 + yn + 2 * yi.sum)
        A
    }

    def integrateStream(a: Double, b: Double, n: Int, f: (Double => Double)): Double =
    {
        val step = (b - a) / n
        val deltas = (a to b by step)
            .view
            .map(f(_))
        val y0 = deltas.head
        val yi = deltas.tail.dropRight(1).sum
        val yn = deltas.last

        val A = step / 2 * (y0 + yn + 2 * yi)
        A
    }

    def integratePar(a: Double, b: Double, n: Int, f: (Double => Double)): Double =
    {
        println("1")
        val step = (b - a) / n
        println("2")
        val deltas = a until b by step
        println("3")
        val y0 = f(deltas.head)
        println("4")
        val yi1 = deltas.tail
        println("5")
        val yi = yi1.par.map(f(_)).sum
        println("6")
        val yn = f(b)
        println("7")

        val A = step / 2 * (y0 + yn + 2 * yi)
        A
    }


    override def main(args: Array[String])
    {
        val sinX = integrate(1.0, 2.0, 100, Math.sin)
        println(s"Int_2^3 Sin(x) dx = $sinX")

        /*
        val t1 = time
        {
            val a = integrate(1.0,2.0,64e6.toInt,Math.sin)
            println(a)
        }
        */

        val t2 = time
        {
            val a = integrateStream(1.0, 2.0, 64e6.toInt, Math.sin)
            println(a)
        }

        val t3 = time
        {
            val a = integratePar(1.0, 2.0, 64e6.toInt, Math.sin)
            println(a)
        }

        // println(t1)
        println(t2)
        println(t3)
    }
}
