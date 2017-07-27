/**
  * Table of cards
  */
case class Table(trump: Suite, pairs:List[Pair] = List[Pair]()) {
  override def toString: String = pairs.mkString(" ")
}

object Table {

  def ranks(table: Table):List[Int] = {
    table.pairs.flatMap(p =>
      if (p.second.nonEmpty)
        List[Int](p.first.get.rank.value, p.second.get.rank.value)
      else List[Int](p.first.get.rank.value)
    )
  }

  def cards(table: Table): List[Card] = {
    table.pairs.flatMap{p:Pair => {
      if (p.second.isEmpty)
        List(p.first)
      else
      List(p.first, p.second)
    }}.flatten
  }

  def removePairs(table: Table, pairs:List[Pair]): Table = {
    table.copy(pairs = pairs.filterNot(pairs.toSet[Pair]))
  }

  def addPair(table: Table, pair: Pair): Table = {
    table.copy(pairs = table.pairs ::: List[Pair](pair))
  }

  def addPairs(table: Table, pairs:List[Pair]): Table = {
    table.copy(pairs = table.pairs ::: pairs)
  }

  def addCards(table: Table, cards:List[Card]): Table = {
    table.copy(pairs = table.pairs ::: cards.map{c: Card => Pair(Option(c))})
  }
}
