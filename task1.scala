package com.rockthejvm

import scala.util.control.Breaks.{break, breakable}
import scala.io.StdIn.readInt
object task1 extends App {

  def initial(a: Int, b: Int): Int = {
    if(b == 0) return a
    initial(b, a % b)
  }

  def pour(fromCap: Int, toCap: Int, d: Int): Int = {
    var from: Int = fromCap
    var to: Int = 0
    var step : Int = 1

    println(s"($from , $to)")
    breakable {
      while (to != d) {

        val temp = (from).min(toCap - to)

        to = to + temp
        from = from - temp
        println(s"($from , $to)")
        step = step + 1


        if (to == d) {
          break
        }
        if (from == 0) {
          from = fromCap
          step = step + 1
          println(s"($from , $to)")
        }
        if (to == toCap) {
          to = 0
          step = step + 1
          println(s"($from , $to)")
        }


      }

    }
    return step
  }

  def minStep(n: Int, m: Int, d: Int ): Int = {

    if((d % initial(n,m)) != 0) {
      println("no solution possible")
      return -1
    }

    val c: Int = pour(n, m, d)

    println("second way")
    val g: Int = pour(m,n,d)
    return (c).min(g)
  }
  print("enter the 1st jug value: ")
  var m: Int = readInt()
  print("enter 2nd jug value: ")
  var n: Int = readInt()
  print("enter required quantity: ")
  var d: Int = readInt()

  println("Minimum steps " + minStep(n,m,d))

}


