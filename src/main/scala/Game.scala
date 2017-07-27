/**
  * Durak game
  */
case class Turn(attacker: Player, defender: Player, table: Table) {
  override def toString: String = "P1 " + attacker + " P2 " + defender + " table " + table
}

object Game {

  def main(args: Array[String]): Unit = {

    val deck:Deck = Deck(Deck.prepare(Card.list()))
    val trump = deck.cards.last
    val trumps = trump.suite

    println("Play trumps: " + trumps + " deck: " + deck)

    play(Player("1"), Player("2"), trumps, deck, 1)
  }

  def play(player1: Player, player2: Player, trump: Suite, cards: Deck, r: Int): Unit = {

    var attacker = player1
    var defender = player2
    var deck = cards

    if (r > 100) { // Game will not end, unwinnable card config
      throw new IllegalStateException("Too many turns, unwinnable cards " + attacker + " " + defender)
    }
    if (deck.cards.nonEmpty) {
      while ((attacker.cards.length < Player.HandSize || defender.cards.length < Player.HandSize) && deck.cards.nonEmpty) {

        if (attacker.cards.length < Player.HandSize) {
          val card = deck.cards.head
          attacker = Player.addCard(attacker, card)
          deck = Deck.removeCard(deck, card)
        }
        if (defender.cards.length < Player.HandSize && deck.cards.nonEmpty) {
          val card = deck.cards.head
          defender = Player.addCard(defender, card)
          deck = Deck.removeCard(deck, card)
        }
      }
    }

    val turn: Turn = playTurn(attacker, defender, Table(trump), 1)

    if (turn.attacker.cards.isEmpty && turn.defender.cards.isEmpty) { // both players have no cards
      println("DRAW")

    } else if (turn.attacker.cards.isEmpty) {
      println("WINNER Player " + turn.attacker.name)

    } else if (turn.defender.cards.isEmpty) {
      println("WINNER Player " + turn.defender.name)

    } else { // no winner, switch player and continue
      play(defender.copy(cards = turn.defender.cards), attacker.copy(cards = turn.attacker.cards), trump, deck.copy(cards = deck.cards), r + 1)
    }
  }

  def playTurn(turnAttacker: Player, turnDefender: Player, turnTable: Table, t: Int): Turn = {

    val state: Turn = {

      println("play turn attack " +  t + " on table " + turnTable)

      if (turnDefender.cards.nonEmpty && turnAttacker.cards.nonEmpty) { // if defender and attacker has cards left

        var attacker = turnAttacker
        var defender = turnDefender
        var table = turnTable

        print(attacker + " attack ")
        val attack = Player.attack(table, attacker)
        println(attack)

        if (attack.isDefined) { // card to attack with
          attacker = Player.removeCard(attacker, attack.get)

          print(defender + " defend ")
          val defence = Player.defend(table, attack, defender)
          println(defence)

          table = Table.addPair(table, Pair(attack, defence))

          if (defence.isEmpty) { // no card to defend with
            if (defender.cards.nonEmpty) { // add more cards

              val cards = Player.finalize(table, defender, attacker)
              attacker = Player.removeCards(attacker, cards)
              table = Table.addCards(table, cards)
              println("Player" + attacker.name + " adds cards " + (if (cards.isEmpty) None else cards))
              defender = Player.addCards(defender, table)
              println("Player" + defender.name + " takes cards " + table)
            }
            Turn(attacker, defender, table)

          } else { // defended a hit, continue turn
            defender = Player.removeCard(defender, defence.get)
            playTurn(attacker, defender, table, t + 1)
          }
        } else { // no card for attack
          Turn(attacker, defender, table)
        }
      } else { // player has no cards, end turn
        Turn(turnAttacker, turnDefender, turnTable)
      }
    }
    state
  }

}
