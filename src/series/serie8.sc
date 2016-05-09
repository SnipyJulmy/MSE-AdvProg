val a = 1.0
val b = 2.0
val n = 400*400*400
def f(x : Double) : Double = Math.sin(x)

val step = (b-a) / n

val deltas = (a to b by step).toList
val yList = deltas.map(f(_))
val y0 = yList.head
val yi = yList.tail.dropRight(1)
val yn = yList.last

val A = step/2 * (y0 + yn + 2 * yi.sum)
