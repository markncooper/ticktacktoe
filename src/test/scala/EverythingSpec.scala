import org.scalatest.{Matchers, WordSpec}

class EverythingSpec extends WordSpec with Matchers{
  import TicTackToe._

  "A board with three in a row" should {
    "be marked as won" in {
      val boardData = Array.fill[Cell](9)(Cell(NoPlayer))
      boardData(0) = Cell(ComputerPlayer)
      boardData(1) = Cell(ComputerPlayer)
      boardData(2) = Cell(ComputerPlayer)
      val board = new Board(boardData)

      board.isWinner(HumanPlayer) should be (false)
      board.isWinner(ComputerPlayer) should be (true)

      board.isFree(3) should be (true)
      board.isFree(2) should be (false)
    }
  }
}
