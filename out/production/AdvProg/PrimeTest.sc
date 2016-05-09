import scala.PrimeAKS._

val s = (1000 to 10000).toStream.filter(isPrimeAKS(_))
s.take(3).toList
