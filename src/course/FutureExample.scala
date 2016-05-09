import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.util.{Failure, Success}

/**
  * Created by snipy on 26.04.16
  */
object FutureExample extends App
{
    val f:Future[Int] = Future(3 + 4)

    f.onComplete
    {
        case Success(result) => println(result)
        case Failure(t) => println(t.getMessage)
    }
}
