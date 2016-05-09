def queens(n: Int): List[List[(Int, Int)]] =
{
    def placeQueens(k: Int): List[List[(Int, Int)]] =
        if (k == 0)
            List(List())
        else
            for
            {
                queens <- placeQueens(k - 1)
                column <- 1 to n
                queen = (k, column)
                if isSafe(queen, queens)
            } yield queen :: queens
    placeQueens(n)
}


def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) =
    queens forall (q => !inCheck(queen, q))

def inCheck(q1: (Int, Int), q2: (Int, Int)) =
    q1._1 == q2._1 || // same row
        q1._2 == q2._2 || // same column
        (q1._1 - q2._1).abs == (q1._2 - q2._2).abs // on diagonal

queens(5)

def printChessBoard(list: List[List[(Int, Int)]]): String =
{
    def extractSolution(list: List[(Int, Int)]): String =
    {
        val board = (for (i <- 1 to list.length;
                          j <- 1 to list.length) yield (i, j)).toList

        def constructString(positions: List[(Int, Int)], board: List[(Int, Int)], length: Int): String =
        {
            board match
            {
                case Nil => "|"
                case (x1, x2) :: xs =>
                    if (list.contains((x1, x2)))
                        if (x2 >= length)
                            "|\u265b\n" + constructString(list, xs, length)
                        else
                            "|\u265b" + constructString(list, xs, length)
                    else if (x2 >= length)
                        "|_\n" + constructString(list, xs, length)
                    else
                        "|_" + constructString(list, xs, length)
            }
        }

        constructString(list, board, list.length)
    }
    list.map(extractSolution(_)).foldLeft("")((x, y) => x + "\n" + y)
}
