package utils

import domain.{DomainError, Result}
import scala.annotation.tailrec

object Helper:
  /**
   * Converts a List Result into Result o List
   * @param list List[Result[A]]
   * @tparam A any type
   * @return Either[List[DomainError], List[A]]
   */
  def listResultToResultList[A](list: List[Result[A]]): Either[List[DomainError], List[A]] =
    list.partition(_.isLeft) match
      case ((Nil, success)) => Right(for(Right(element) <- success) yield element)
      case ((domainError, _)) => Left(for(Left(element) <- domainError) yield element)

  /**
   * Get the list with the smaller elements. If lists are equals then returns listA
   * @param listA List[A]
   * @param listB List[A]
   * @tparam A Any Type (String, OrderId, ProductNumber, ...)
   * @return List with the smaller elements
   */
  def getListContainsSmallerElements[A](listA: List[A], listB: List[A]): List[A] =
    val listASorted: List[A] = listA.sortBy(_.toString)
    val listBSorted: List[A] = listB.sortBy(_.toString)
    val minSize: Int = if (listA.size < listB.size) listA.size else listB.size

    @tailrec
    def getListContainsSmallerElementsRec(index: Int): List[A] =
      if(index == minSize)
        listA
      else
        val elementA: String = listASorted(index).toString
        val elementB: String = listBSorted(index).toString

        if(elementA == elementB) getListContainsSmallerElementsRec(index + 1)
        else if(elementA < elementB)
          listA
        else listB

    getListContainsSmallerElementsRec(0)
