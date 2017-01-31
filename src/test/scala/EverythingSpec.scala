import org.scalatest.{Matchers, WordSpec}

class EverythingSpec extends WordSpec with Matchers{
  import TicTacToe._

  "A board with three computer player spots in a row" should {
    val boardCells = Array.fill[Cell](9)(Cell(NoPlayer))
    boardCells(0) = Cell(ComputerPlayer)
    boardCells(1) = Cell(ComputerPlayer)
    boardCells(2) = Cell(ComputerPlayer)
    boardCells(4) = Cell(HumanPlayer)
    boardCells(5) = Cell(HumanPlayer)
    boardCells(6) = Cell(HumanPlayer)
    val board = new Board(boardCells)

    "show the human didn't win but the computer did" in {
      board.isWinner(HumanPlayer) should be (false)
      board.isWinner(ComputerPlayer) should be (true)
    }

    "and that cells are correctly marked as free or taken" in {
      board.isFree(3) should be(true)
      board.isFree(2) should be(false)
    }
  }
}
