package org.scala.algorithm

object QuickSort extends App {
  
  def partition(list: List[Int]) : (List[Int], Int, List[Int]) = {
    def loop(list: List[Int], p: Int, l: List[Int], r: List[Int]) : (List[Int], Int, List[Int]) = list match {
      case h :: t => if( h < p) loop(t, p, l :+ h, r) else loop(t, p, l, r :+ h)
      case Nil => (l, p, r)
    }
    loop(list.dropRight(1), list.last, Nil, Nil)
  }
  
  def quickSort(list: List[Int]) : List[Int]= partition(list) match {
    case (Nil, p, r) => List(p) ::: r
    case (l, p, Nil) => l ::: List(p)
    case (l, p, r) => quickSort(l) ::: (p :: quickSort(r))  
  }
  
  val list = List(1,23,2, 13,243,19,5,8,2,4,17)
  println(quickSort(list))
  
}