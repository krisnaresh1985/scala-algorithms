package org.scala.algorithm

import scala.annotation.tailrec

object Sorting extends App {
  
  /**
   * Bubble Sort
   */
  def bubbleSort(list: List[Int]) : List[Int] = {
 
    @tailrec
    def sort(list: List[Int], resultList: List[Int], cnt: Int) : List[Int] = list match {
      case h1 :: h2 :: t => if(h1 > h2)  sort( h1 :: t, resultList :+ h2, cnt)  else sort(h2 :: t, resultList :+ h1, cnt)
      case h :: Nil => sort(Nil, resultList :+ h, cnt)
      case Nil => if(resultList.size == cnt) resultList else sort(resultList, Nil, cnt +1 ) 
    }
    sort(list, Nil, 1)
  }
  
  /**
   * Bubble sort - optimized
   */
  def bubbleSort2(list: List[Int]) : List[Int] = {
    @tailrec
    def sort(list: List[Int], previousResult: List[Int], resultList: List[Int], cnt: Int) : List[Int] = list match {
      case h1 :: h2 :: t => if(h1 > h2)  sort( h1 :: t, previousResult, resultList :+ h2, cnt)  else sort(h2 :: t, previousResult, resultList :+ h1, cnt)
      case h :: Nil => sort(Nil, previousResult,  resultList :+ h, cnt)
      case Nil => if(resultList  == previousResult) resultList else sort(resultList, resultList, Nil, cnt + 1)
    }
    sort(list, Nil, Nil, 1)
  }  
  
  /**
   * Selection sort
   */
  def selectionSort(list: List[Int]) : List[Int]= {
    @tailrec
    def sort(minValue: Int, list: List[Int], sortedList: List[Int], unsortedList: List[Int]) : List[Int] = list match {
      case h :: t => if(h > minValue) sort(minValue, t, sortedList, unsortedList :+ h) else sort(h,  t, sortedList, unsortedList :+ minValue)
      case Nil => if(unsortedList.isEmpty) sortedList :+ minValue else sort(unsortedList.head, unsortedList.tail, sortedList :+ minValue, Nil)
    }
    sort(list.head, list.tail, Nil, Nil)
  }
  
  /**
   * Merge Sort
   */
  def mergeSort(list: List[Int]) = {
    def sortMergeHalves(list1: List[Int], list2: List[Int], sortList: List[Int]) : List[Int] = (list1, list2) match {
      case(Nil,_) => sortList ::: list2
      case(_, Nil) => sortList ::: list1
      case (h1 :: t1, h2 :: t2) =>  if(h1 < h2) sortMergeHalves( t1 , list2, sortList :+ h1) else sortMergeHalves( list1 , t2, sortList :+ h2)
    }
    def mergeHalves(list: List[List[Int]], result: List[List[Int]]) : List[Int]= list match {
      case h1 :: h2 :: t => mergeHalves(t, result :+ sortMergeHalves(h1, h2, Nil))
      case h :: Nil => mergeHalves(Nil, result :+ h)
      case Nil =>if(result.size == 1) result(0) else mergeHalves(result, Nil)
    }
    mergeHalves(list.map(List(_)), Nil)
  }
  
  /**
   * Insertion Sort
   */
  def insertionSort(list: List[Int]) = {
    def insert(i: Int, list: List[Int], result: List[Int]) : List[Int] = list match {
      case h :: t => if(i < h) result ::: (i :: (h :: t)) else insert( i, t, result :+ h)
      case Nil => result :+ i
    }
    def sort(list: List[Int], sortedList: List[Int]) : List[Int] = list match {
      case h1 :: h2 :: t => if( h1 < h2) sort( h2 :: t, insert(h1, sortedList, Nil)) else sort( h1 :: t, insert(h2, sortedList, Nil))
      case h :: Nil =>  sort( Nil, insert(h, sortedList, Nil))
      case Nil => sortedList
    }
    sort(list, Nil)
  }
  
  /**
   * Insertion Sort
   */
  def insertionSort2(list: List[Int]) = {
    @tailrec
    def insert(i: Int, list: List[Int], result: List[Int]) : List[Int] = list match {
      case h :: t => if(i < h) result ::: (i :: (h :: t)) else insert( i, t, result :+ h)
      case Nil => result :+ i
    }
    
    @tailrec
    def sort(list: List[Int], sortedList: List[Int]) : List[Int] = list match {
      case h :: t => sort( t, insert(h, sortedList, Nil))
      case Nil => sortedList
    }
    sort(list, Nil)
  }  
  val list = List(1,2,3,4,7,6)
  val list1 = List(1,17,15,13,204,2,3)
  
 
  println(bubbleSort(list1))
  println(bubbleSort2(list1))
  println(selectionSort(list1))
  println(mergeSort(list1))
  println(insertionSort(list1))
  println(insertionSort2(list1))
  
  
}