import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Player(number: Int, cards: ListBuffer[Int]) {
  def topCard: Int = cards.head

  def getTopCard(): Int = cards.remove(0)

  def addCardToBottom(card: Int): Unit = cards += card

  def hasLost = cards.isEmpty

  def takeCards(myCard: Int, otherCard: Int): Unit = {
    addCardToBottom(myCard)
    addCardToBottom(otherCard)
  }

  def playWith(otherPlayer: Player): Unit = {
    val myCard = getTopCard()
    val otherCard = otherPlayer.getTopCard()

    if (myCard > otherCard) {
      takeCards(myCard, otherCard)
    } else {
      otherPlayer.takeCards(otherCard, myCard)
    }
  }

  def score: Int = {
    cards
      .reverse
      .zipWithIndex
      .map {
        case (c, i) => c * (i + 1)
      }.sum
  }

  def copy(n: Int): Player = Player(number, ListBuffer.from(cards.take(n)))

  def copy: Player = Player(number, ListBuffer.from(cards))

}

object Day22 extends App {

  implicit val playerParser = new Mapper[Player] {
    private val nameReg = """Player (\d+):""".r

    override def map(line: String): Player = {
      line.split("\n").map(_.trim).filterNot(_.isEmpty).toList match {
        case nameReg(num) :: rest =>
          Player(
            num.toInt,
            ListBuffer.from(rest.map(_.toInt))
          )
      }
    }
  }

  def readFile(filename: String): Seq[Player] =
    Source
      .fromResource(filename)
      .getLines
      .map(_.trim)
      .mkString("\n")
      .split("\n\n")
      .map(playerParser.map)


  def part1(players: Seq[Player]): Long = {
    val me = players.head
    val crab = players.last

    while (!(me.hasLost || crab.hasLost)) {
      me.playWith(crab)
    }

    if (me.hasLost)
      crab.score
    else
      me.score
  }


  def part2(players: Seq[Player]): Long = {

    def playRound(me: Player, crab: Player): Player = {
      val (myCard, otherCard) = (me.getTopCard(), crab.getTopCard())

      val winner = {
        if (me.cards.size >= myCard && crab.cards.size >= otherCard) {
          recursiveCombat(me.copy(myCard), crab.copy(otherCard))
        } else {
          if (myCard > otherCard)
            me
          else
            crab
        }
      }

      if (winner.number == me.number) {
        me.takeCards(myCard, otherCard)
      } else {
        crab.takeCards(otherCard, myCard)
      }
      winner
    }

    def recursiveCombat(me: Player, crab: Player): Player = {
      var roundsMem = Set[Seq[Int]]()
      var roundWinner = me

      while (true) {
        if (me.hasLost || crab.hasLost) {
          if (me.hasLost)
            return crab
          else
            return me
        }

        if (roundsMem.contains(me.cards.toSeq) && roundsMem.contains(crab.cards.toSeq)) {
          val player1 = if (me.number == 1) me else crab
          return player1
        } else {
          roundsMem += me.cards.toSeq
          roundsMem += crab.cards.toSeq
          roundWinner = playRound(me, crab)
        }
      }
      roundWinner
    }

    val me = players.head
    val crab = players.last

    val winner = recursiveCombat(me, crab)
    if (winner.number != me.number)
      crab.score
    else
      me.score
  }

  val sample: Seq[Player] = readFile("sample_day22")
  assert(part1(sample.map(p => p.copy)) == 306)

  val players: Seq[Player] = readFile("input_day22")
  println(s"Part 1 answer:${part1(players.map(_.copy))}")

  assert(part2(sample.map(_.copy)) == 291)
  println(s"Part 2 answer:${part2(players.map(_.copy))}")

}
