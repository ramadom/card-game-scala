import scala.collection.mutable.ListBuffer

/**
  * Game player
  */
case class Player(name: String, cards: List[Card] = List[Card]()) {

  override def toString: String = "Player" + name + " " + cards.mkString
}

object Player {

  val HandSize = 6

  def attack(table: Table, attacker: Player): Option[Card] = {
    if (attacker.cards.isEmpty) {
      throw new IllegalStateException("Play with no cards")
    } else {
      if (table.pairs.isEmpty) {
        Player.begin(table.trump, attacker.cards)
      } else {
        Player.continue(Table.ranks(table), attacker.cards)
      }
    }
  }

  private def begin(trump: Suite, hand: List[Card]): Option[Card] = {
    if (hand.isEmpty) {
      throw new IllegalStateException("Play with no cards")
    } else {
      val cards = hand.filter((card: Card) => card.suite != trump)
      val attack = if (cards.isEmpty) { // if normal cards empty select from all cards
        hand.min(Ordering.by((card: Card) => card.rank.value))
      } else {
        cards.min(Ordering.by((card: Card) => card.rank.value))
      }
      Option(attack)
    }
  }

  private def continue(ranks: List[Int], hand: List[Card]): Option[Card] = {
    if (hand.isEmpty) {
      throw new IllegalStateException("Play with no cards")
    } else {
      chooseByRanks(ranks, hand)
    }
  }

  def finalize(table: Table, defender: Player, attacker: Player): List[Card] = {
    var cards =  ListBuffer[Card]()
      if (defender.cards.length > cards.length && cards.length < attacker.cards.length) {
        val card = chooseByRanks(Table.ranks(table), attacker.cards)
      if (card.isDefined) {
        cards += card.get
        cards ++= finalize(table, defender, Player.removeCard(attacker, card.get))
      } else {
        List[Card]()
      }
    }
    cards.toList
  }

  private def chooseByRanks(ranks: List[Int], hand: List[Card]): Option[Card] = {
    val cards = hand.filter((card: Card) => ranks.contains(card.rank.value))
    if (cards.nonEmpty) {
      val card = cards.min(Ordering.by((card: Card) => card.rank.value))
      Option(card)
    } else
      None
  }

  def defend(table: Table, attack: Option[Card], defender: Player): Option[Card] = {
    attack match {
      case None => throw new IllegalArgumentException("Attack with no card")
      case Some(card: Card) =>
        if (defender.cards.isEmpty) {
          throw new IllegalStateException("Play with no cards")
        } else {
          if (card.suite == table.trump) {
            chooseTrump(card, defender.cards)
          } else {
            chooseCard(table.trump, card, defender.cards)
          }
        }
    }
  }

  private def chooseTrump(attack:Card, hand: List[Card]): Option[Card] = {
    val trumps = hand.filter((card: Card) => card.canDefend(attack))
    if (trumps.isEmpty) {
      None // no card to defend
    } else {
      val defence = trumps.min(Ordering.by((c: Card) => c.rank.value))
      Option(defence)
    }
  }

  private def chooseCard(trump: Suite, attack: Card, hand: List[Card]): Option[Card] = {
    val cards = hand.filter((card: Card) => card.canDefend(attack))
    if (cards.isEmpty) { // normal card not found, select trump card
      val trumps = hand.filter((card: Card) => card.suite == trump)
      if (trumps.isEmpty) {
        None
      } else {
        val defence = trumps.min(Ordering.by((card: Card) => card.rank.value))
        Option(defence)
      }
    } else { // select normal card
      val defence = cards.min(Ordering.by((card: Card) => card.rank.value))
      Option(defence)
    }
  }

  def removeCard(player: Player, card: Card): Player = {
    player.copy(cards = player.cards.filterNot({c:Card => c.equals(card)}))
  }

  def removeCards(player: Player, cards: List[Card]): Player = {
    player.copy(cards = player.cards.filterNot(cards.toSet[Card]))
  }

  def addCards(player: Player, table: Table): Player = {
    player.copy(cards = player.cards ::: Table.cards(table))
  }

  def addCard(player: Player, card: Card): Player = {
    player.copy(cards = player.cards ::: List[Card](card))
  }

}
