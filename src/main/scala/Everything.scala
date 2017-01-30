import BoardStatus.BoardStatus

import scala.io.StdIn
import scala.util.Random

object BoardStatus extends Enumeration {
  type BoardStatus = Value
  val HumanWon, ComputerWon, Tie, InProgress = Value
}

/**
  * Tic Tack Toe REPL. Loop until the game is over.
  */
object TicTackToe {
  type Player = Char
  val HumanPlayer = 'X'
  val ComputerPlayer = 'O'
  val NoPlayer = ' '

  def main(args: Array[String]): Unit = {

    val board = Board()
    var humanMoveStr = ""

    println("Starting...")

    while (board.getBoardStatus() == BoardStatus.InProgress) {
      var humanMoveError: Option[String] = None
      do {
        println("Enter your move: ")
        humanMoveStr = StdIn.readLine()
        humanMoveError = board.getMoveError(humanMoveStr)

        humanMoveError.foreach { error => println(error) }
      } while (humanMoveError.isDefined)

      val move = humanMoveStr.toInt

      println(s"You entered: $move")
      board.place(HumanPlayer, move)

      if (board.getBoardStatus() == BoardStatus.InProgress) {
        board.smarterComputerPlace(ComputerPlayer, HumanPlayer)
      }
      board.print()
    }

    println("Game over. " + board.getBoardStatus())
  }
}

case class Cell(value: Char){
  import TicTackToe._

  def isEmpty(): Boolean = {
    value.equals(NoPlayer)
  }

  override def toString(): String = {
    value.toString
  }
}

object Board {
  import TicTackToe._

  def apply(): Board ={
    new Board(Array.fill[Cell](9)(Cell(NoPlayer)))
  }
}

class Board(boardData: Array[Cell]) {
  import TicTackToe._
  val WinningCombinations = List((0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7), (2,5,8), (0,4,8), (2,4,6))

  def place(value: Char, move: Int): Unit = {
    boardData(move) = Cell(value)
  }

  def isFree(move: Int): Boolean = {
    boardData(move).value.equals(NoPlayer)
  }

  def isWinner(player: Player): Boolean = {
    WinningCombinations.exists{ case(cell1, cell2, cell3) =>
        boardData(cell1).value == player &&
        boardData(cell2).value == player &&
        boardData(cell3).value == player
    }
  }

  def getBoardStatus(): BoardStatus = {
    if (isWinner(HumanPlayer)){
      BoardStatus.HumanWon
    } else if (isWinner(ComputerPlayer)){
      BoardStatus.ComputerWon
    } else {

      // No winner? Make sure the board isn't full (in which case it's a tie).
      val nonEmptySpots = boardData.map { cell =>
        if (!cell.isEmpty()) 0 else 1
      }.sum

      if (nonEmptySpots == 0) {
        BoardStatus.Tie
      } else {
        BoardStatus.InProgress
      }
    }
  }

  def print() = {
    println(
      boardData.grouped(3).map { row =>
        row.mkString(" | ")
      }.mkString("\n----------\n")
    )
  }

  /**
    * Used to place computer moves...randomly
    */
  def randomComputerPlace(player: Player): Unit ={
    var done = false
    while (!done) {
      val move = Random.nextInt(6)
      if (isFree(move)){
        place(player, move)
        done = true
      }
    }
  }

  /**
    * Smarter computer logic.
    *
    * Rules:
    *   Block if needed
    *   Place in the middle if available (TODO)
    *   Place in a corner if available (and middle taken) (TODO)
   */
  def smarterComputerPlace(player: Player, opponent: Player): Unit = {
    val spotNeedingBlock = needsBlock(opponent)

    if (spotNeedingBlock.isDefined){
      place(player, spotNeedingBlock.get)
    } else{
      randomComputerPlace(player)
    }
  }

  //
  // Is there a potential spot that the opponent can win
  // via on the next route? Return one such spot.
  //
  private def needsBlock(opponent: Player): Option[Int] = {
    val possibilities =
      WinningCombinations.flatMap { case(cell1, cell2, cell3) =>
        if (boardData(cell1).value == opponent &&
            boardData(cell2).value == opponent &&
            boardData(cell3).value == NoPlayer){
          Some(cell3)

        } else if (boardData(cell1).value == opponent &&
                   boardData(cell2).value == NoPlayer &&
                   boardData(cell3).value == opponent) {
          Some(cell2)

        } else if (boardData(cell1).value == NoPlayer &&
                    boardData(cell2).value == opponent &&
                    boardData(cell3).value == opponent) {
          Some(cell1)
        } else {
          None
        }
      }

    possibilities.headOption
  }


  def getMoveError(moveStr: String): Option[String] = {
    try {
      val move = moveStr.toInt
      if (move < 0 || move > 8){
        Some("Move must be 0 through 8!")
      } else if (!isFree(move)){
        Some("Spot contains a move!")
      } else {
        None
      }
    }catch {
      case e: NumberFormatException => Some(s"$moveStr is not a number")
    }
  }
}