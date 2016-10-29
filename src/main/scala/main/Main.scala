package main

import astgenerator.IncludedInstructions
import lexical.MyLexical
import logogrammar.LogoParser.parse

import scala.collection.mutable
import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.combinator.token.Tokens

/**
  * Created by Marin on 28/03/16.
  */
object Main {

    def main(args: Array[String]): Unit = {

        testLexical()
    }

    def testLexical(): Unit = {

        val lexical = new MyLexical

        //lexical.delimiters ++= List("\n", "+", "-", "*", "^", "/", "(", ")", "{", "}", "[", "]", ">", "<", ">=", "<=", ",", " ")

        lexical.reserved ++= IncludedInstructions.getInstructions

        val test1 = """to test :a :b
                      |     sum 2 3
                      |end""".stripMargin

        val test2 = "12.23 1223 0.123 123. 124.0"

        val test3 = "12 + 14 * 5"

        val test4 = "print 4 + 2 >= 5 * -3"

        val test5 = "{ 4 3 2 }"

        val test6 = "[ 1 2 ]"
        val test7 = "[1+2]"

        val test8 = "[ a + 1 2 + ]"

        val test9 = "[ [ 1 2 3 ] + ] 2 + 5"

        val test10 = "2 + 4 ^ 1 * 3"

        val test11 = "\"helltopo :print \"moje"

        val test12 = "[ ]"

        val test13 =
            """
              |TO fern :size :sign
              |  if :size < 1 [ stop ]
              |  fd :size
              |  rt 70 * :sign fern :size * 0.5 :sign * -1 lt 70 * :sign
              |  fd :size
              |  lt 70 * :sign fern :size * 0.5 :sign rt 70 * :sign
              |  rt 7 * :sign fern :size - 1 :sign lt 7 * :sign
              |  bk :size * 2
              |END
              |window clearscreen pu bk 150 pd
              |fern 25 1
            """.stripMargin

        val ctest = test4

        val scanner = new lexical.Scanner(ctest)

        println(parse(ctest))
    }

    def getTokens(scanner: Scanners#Scanner): List[Tokens#Token] = scanner match {

        case xs if xs.atEnd => List()
        case xs => xs.first.asInstanceOf[Tokens#Token] :: getTokens(xs.rest)
    }
}
