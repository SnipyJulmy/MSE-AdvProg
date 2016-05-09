/**
  * Created by snipy on 03.05.16.
  */
class TestConv
{

    abstract class Temperature(degrees: Int)
    {
        val deg = degrees
    }

    case class Celsius(degrees: Int) extends Temperature(degrees = degrees)

    case class Kelvin(degrees: Int) extends Temperature(degrees = degrees)

    implicit def CtoK(temp: Celsius): Kelvin = new Kelvin(temp.deg)

    implicit def KtoC(temp: Kelvin): Celsius = new Celsius(temp.deg)

    implicit def ItoC(temp: Int): Celsius = new Celsius(temp)

    implicit def ItoKelvin(temp: Int): Kelvin = new Kelvin(temp)

    def main(args: Array[String])
    {
        val a: Celsius = 30
        val b: Kelvin = 30
        val c: Kelvin = Celsius(10)
        val d: Celsius = c
        val e: Temperature = d
    }
}
