package misc

class MultiplicationTable(len: Int, spc: Int) {

  def makeRowSeq(row: Int) = {
    for (col <- 1 to len) yield {
      val prod = (col * row).toString;
      val padding = " " * (spc - prod.length)
      padding + prod
    }
  }

  def makeRow(row:Int) = makeRowSeq(row).mkString

  def makeTable = {
    val rows = for (row <- 1 to len) yield makeRow(row)
    rows.mkString("\n")
  }
}

object MultiplicationTable {
  def main(args: Array[String])	{

    val table = new MultiplicationTable(args(0).toInt, args(1).toInt);
    println(table.makeTable)
  }
}