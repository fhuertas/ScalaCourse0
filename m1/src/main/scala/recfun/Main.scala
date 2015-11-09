package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println("balance")
    println(balance("()()(".toList))
    println("Lista de monedas=")
    println(countChange(4,List(1,2,3)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    (c,r) match {
      case (0,_) => 1
      case (c,r) if (c == r) => 1
      case (c,_) => pascal(c-1,r-1) + pascal(c,r-1)
    }
//    r match {
//      case 0 => 1
//      case _ => {
//        c match {
//          case 0 => 1
//          case `r` => 1
//          case _ => {
//            pascal(c-1,r-1) + pascal(c,r-1)
//          }
//        }
//      }
//    }

  }

  /**
   * Exercise 2
    * A parameter can not be nil
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], count: Int) : Boolean  =
      (chars.isEmpty,count) match {
      case (true,0) if chars.isEmpty => true
      case (true,_) if chars.isEmpty => false
      case (_,_) => (chars.head) match {
        case ('(') => balance(chars.tail,count+1)
        case (')') if count > 0 => balance(chars.tail,count-1)
        case (')') => false
      }


    }
    balance(chars,0)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money,coins.isEmpty) match {
      case (0,_) => 1
      case (_,true) => 0
      case (_,_) if money < 0 => 0
      case (_,_) => countChange(money,coins.tail) + countChange(money - coins.head,coins )
    }
  }
}
