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
object TicTacToe {
  import BoardStatus._

  type Player = Char
  val HumanPlayer = 'X'
  val ComputerPlayer = 'O'
  val NoPlayer = ' '

  def main(args: Array[String]): Unit = {

    val board = Board()
    var humanMoveStr = ""

    println("Starting...")

    while (board.getBoardStatus() == InProgress) {
      var humanMoveError: Option[String] = None
      do {
        println("Enter your move: ")
        humanMoveStr = StdIn.readLine()
        humanMoveError = board.getMoveError(humanMoveStr)

        humanMoveError.foreach { error => println(error) }
      } while (humanMoveError.isDefined)

      val move = humanMoveStr.toInt - 1

      board.place(HumanPlayer, move)

      if (board.getBoardStatus() == InProgress) {
        board.smarterComputerPlace(ComputerPlayer, HumanPlayer)
      }
      board.print()
    }

    println("Game over. " + board.getBoardStatus())
  }
}

case class Cell(value: Char){
  import TicTacToe._

  def isEmpty(): Boolean = {
    value.equals(NoPlayer)
  }

  override def toString(): String = {
    value.toString
  }
}

object Board {
  import TicTacToe._

  def apply(): Board ={
    new Board(Array.fill[Cell](9)(Cell(NoPlayer)))
  }
}

class Board(cells: Array[Cell]) {
  import TicTacToe._
  val WinningCombinations = List((0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7), (2,5,8), (2,4,6), (0,4,8))

  def place(value: Char, move: Int): Unit = {
    cells(move) = Cell(value)
  }

  def isFree(move: Int): Boolean = {
    cells(move).value.equals(NoPlayer)
  }

  def isWinner(player: Player): Boolean = {
    WinningCombinations.exists{ case(cell1, cell2, cell3) =>
        cells(cell1).value == player &&
        cells(cell2).value == player &&
        cells(cell3).value == player
    }
  }

  def getBoardStatus(): BoardStatus = {
    if (isWinner(HumanPlayer)){
      BoardStatus.HumanWon
    } else if (isWinner(ComputerPlayer)){
      BoardStatus.ComputerWon
    } else {

      // No winner? Make sure the board isn't full (in which case it's a tie).
      val nonEmptySpots = cells.map { cell =>
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
      cells.grouped(3).map { row =>
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
      val move = Random.nextInt(cells.size)
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
    *   Place winning move if possible (TODO)
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
        if (cells(cell1).value == opponent &&
            cells(cell2).value == opponent &&
            cells(cell3).value == NoPlayer){
          Some(cell3)

        } else if (cells(cell1).value == opponent &&
                   cells(cell2).value == NoPlayer &&
                   cells(cell3).value == opponent) {
          Some(cell2)

        } else if (cells(cell1).value == NoPlayer &&
                    cells(cell2).value == opponent &&
                    cells(cell3).value == opponent) {
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
      if (move < 1 || move > 9){
        Some("Move must be 1 through 9!")
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