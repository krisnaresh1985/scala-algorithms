package org.scala.algorithm

object Search extends App {
  
  /**
   * Binary Search
   */
  def binarySearch(list: List[Int], value: Int, start: Int, end: Int) : Int = {
    val mid = start + (end - start)/2 
    if(end >= start) {
      value match {
        case x if(x > list(mid)) => binarySearch(list, value, mid + 1, end)
        case x if(x < list(mid)) => binarySearch(list, value, start, mid)
        case x if(x == list(mid)) => mid
      }
    } else {
      -1
    }
  }
  
  /**
   * Linear search
   */
  def linearSearch(list: List[Int], value: Int, index: Int) : Int = list match {
    case h :: t => if( value == h) index else linearSearch(t, value, index + 1)
    case Nil => -1
  }
  
  println(binarySearch(List(1,2,3,4,5,6), 6, 0, 5))
  println(linearSearch(List(1,2,3,4,5,6), 6, 0))
}