import scala.util.Random

/**
  * Game card
  */
case class Card private(suite: Suite, rank: Rank) {

  def canDefend(card: Card): Boolean = {
    this.suite == card.suite && this.rank.value > card.rank.value
  }
  override def toString: String = "[" + suite + rank + "]"
}

case class Pair(first:Option[Card], second:Option[Card] = None) {

  override def toString: String = first.mkString + "/" + (if (second.nonEmpty) second.mkString else "-")
}

abstract class Suite(pic: Char) {

  override def toString: String = pic.toString
}

sealed case class Diamonds(pic: Char) extends Suite(pic)
sealed case class Hearts(pic: Char) extends Suite(pic)
sealed case class Clubs(pic: Char) extends Suite(pic)
sealed case class Spades(pic: Char) extends Suite(pic)

case class Rank(name: Char, value: Int) {

  override def toString: String = name.toString
}

case class Deck(cards: List[Card]) {
  override def toString: String = cards.mkString
}

object Deck {

  def removeCard(deck: Deck, card:Card): Deck = {
    deck.copy(cards = deck.cards.filterNot((c: Card) => c == card))
  }

  def prepare(cards: List[Card]): List[Card] = {
    Random.shuffle(cards)
  }
}

object Card {

  def list(): List[Card] = {

    List[Card](
      Card(Diamonds('♦'), Rank('A', 14)),
      Card(Diamonds('♦'), Rank('K', 13)),
      Card(Diamonds('♦'), Rank('Q', 12)),
      Card(Diamonds('♦'), Rank('J', 11)),
      Card(Diamonds('♦'), Rank('T', 10)),
      Card(Diamonds('♦'), Rank('9', 9)),
      Card(Diamonds('♦'), Rank('8', 8)),
      Card(Diamonds('♦'), Rank('7', 7)),
      Card(Diamonds('♦'), Rank('6', 6)),
      Card(Diamonds('♦'), Rank('5', 5)),
      Card(Diamonds('♦'), Rank('4', 4)),
      Card(Diamonds('♦'), Rank('3', 3)),
      Card(Diamonds('♦'), Rank('2', 2)),

      Card(Hearts('♥'), Rank('A', 14)),
      Card(Hearts('♥'), Rank('K', 13)),
      Card(Hearts('♥'), Rank('Q', 12)),
      Card(Hearts('♥'), Rank('J', 11)),
      Card(Hearts('♥'), Rank('T', 10)),
      Card(Hearts('♥'), Rank('9', 9)),
      Card(Hearts('♥'), Rank('8', 8)),
      Card(Hearts('♥'), Rank('7', 7)),
      Card(Hearts('♥'), Rank('6', 6)),
      Card(Hearts('♥'), Rank('5', 5)),
      Card(Hearts('♥'), Rank('4', 4)),
      Card(Hearts('♥'), Rank('3', 3)),
      Card(Hearts('♥'), Rank('2', 2)),

      Card(Clubs('♣'), Rank('A', 14)),
      Card(Clubs('♣'), Rank('K', 13)),
      Card(Clubs('♣'), Rank('Q', 12)),
      Card(Clubs('♣'), Rank('J', 11)),
      Card(Clubs('♣'), Rank('T', 10)),
      Card(Clubs('♣'), Rank('9', 9)),
      Card(Clubs('♣'), Rank('8', 8)),
      Card(Clubs('♣'), Rank('7', 7)),
      Card(Clubs('♣'), Rank('6', 6)),
      Card(Clubs('♣'), Rank('5', 5)),
      Card(Clubs('♣'), Rank('4', 4)),
      Card(Clubs('♣'), Rank('3', 3)),
      Card(Clubs('♣'), Rank('2', 2)),

      Card(Spades('♠'), Rank('A', 14)),
      Card(Spades('♠'), Rank('K', 13)),
      Card(Spades('♠'), Rank('Q', 12)),
      Card(Spades('♠'), Rank('J', 11)),
      Card(Spades('♠'), Rank('T', 10)),
      Card(Spades('♠'), Rank('9', 9)),
      Card(Spades('♠'), Rank('8', 8)),
      Card(Spades('♠'), Rank('7', 7)),
      Card(Spades('♠'), Rank('6', 6)),
      Card(Spades('♠'), Rank('5', 5)),
      Card(Spades('♠'), Rank('4', 4)),
      Card(Spades('♠'), Rank('3', 3)),
      Card(Spades('♠'), Rank('2', 2))
    )
  }
}
