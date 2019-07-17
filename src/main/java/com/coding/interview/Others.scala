package org.scala.algorithm

object Others extends App {
  
  /**
   * Factorial
   */
  def factorial(n: Int) : Int = if(n == 1) 1 else n * factorial(n -1)
  
  /**
   * Fibonacci numbers
   */
  def fibonacci(n: Int) : Int = {
    def fib(n: Int, prev: Int, current: Int, cnt: Int) : Int = {
      if (n == cnt)  prev + current
      else if (prev == 0) {println(prev + 1); fib(n, prev + 1, current + 1, cnt + 1) }
      else { println(prev + current); fib(n, current, prev + current, cnt + 1) }
    }
    fib(10, 0, 0, 0)
  }
  
  /**
   * Find Max 
   */
  def findMaximum(list: List[Int]) = {
    def findMax(list: List[Int], max: Int) : Int = list match{
      case h :: t =>  findMax(t, if(h > max) h else max)
      case Nil => max 
    }
    findMax(list, 0)
  }
  
  /**
   * Reverse String
   */
  
  def reverseString(string: String) = {
    def reverse(string: String, reverseStr: String) : String = if(string.length() == 0) reverseStr else reverse(string.substring(1,string.length()), string.substring(0,1)+reverseStr)
    reverse(string,"")
  }
  
  
  def filter[T] (list: List[T], f: T => Boolean) : List[T] = list match {
    case h :: t => if(f(h)) h :: filter(t, f) else filter(t, f)
    case Nil => Nil
  }
  
  println(filter[Int](List(1,2,3,4,5,6,7,8,9,10), (x => (x % 2 == 0))))
  println(filter[String](List("A","B", "CAT","DOG"), (x => (x.length() == 3))))
  
//  println(factorial(5))
//  println(fibonacci(10))
//  println(findMaximum(List(1,21,3,4,51)))
//  println(reverseString("Naresh"))
}