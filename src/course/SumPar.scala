/**
  * Created by snipy on 26.04.16.
  */

import utils._
import org.scalameter.api._

object SumPar extends App
{

}

object SumParBenchmark extends Bench.LocalTime
{
    val a = new Array[BigInt](6 * 1000 * 1000)

    for (i <- a.indices) a(i) = i

    {
        val sum = a.sum
    }

    {
        val sum = a.par.sum
    }
}