package astgenerator

import scala.collection.mutable

/**
  * Created by Marin on 26/05/16.
  */
object IncludedInstructions {

    def stringToTuple(x: String): (String, Int) = {

        val elems = x.split("->").toList

        elems.head -> elems.tail.head.trim.toInt
    }

    val lines = io.Source.fromURL(getClass.getResource("/instructions.txt")).getLines().toList
    //val lines = io.Source.fromFile("/Users/Marin/Documents/instructions.txt").getLines().toList

    private[this] val instructions = mutable.HashMap[String, Int]()

    lines.map(stringToTuple).foreach{

        case (k, v) => instructions.put(k.tail.takeWhile(_ != '\"') ,v)
    }

    def getInstructions = instructions
}
