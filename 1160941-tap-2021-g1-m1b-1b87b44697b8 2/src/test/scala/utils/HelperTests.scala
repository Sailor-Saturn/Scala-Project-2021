package utils

import domain.SimpleTypes.{OrderId, ProductNumber}
import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions
import domain.{DomainError, Result}

class HelperTests extends AnyFunSuite:

  test("listResultToResultList Right") {
    var listResult: List[Result[String]] = List(Right("A"), Right("B"), Right("C"), Right("D"))
    var resultListExpected: Either[List[DomainError], List[String]] = Right(List("A", "B", "C", "D"))
    var resultListResult: Either[List[DomainError], List[String]] = Helper.listResultToResultList(listResult)
    assert(resultListExpected.isRight, resultListResult.isRight)
    assert((resultListExpected.toSeq diff resultListResult.toSeq).size == 0)
  }

  test("listResultToResultList Left") {
    var listResult: List[Result[String]] = List(Right("A"), Left(DomainError.IOFileProblem("B")), Right("C"), Right("D"))
    var resultListExpected: Either[List[DomainError], List[String]] = Left(List(DomainError.IOFileProblem("B")))
    var resultListResult: Either[List[DomainError], List[String]] = Helper.listResultToResultList(listResult)
    assert(resultListExpected.isLeft, resultListResult.isLeft)
    assert(resultListResult.fold(errorResult => resultListExpected.fold(errorExpected => errorResult === errorExpected, _ => false) , _ => false))
  }

  test("listResultToResultList Multiple Left") {
    var listResult: List[Result[String]] = List(Right("A"), Left(DomainError.IOFileProblem("B")), Left(DomainError.IOFileProblem("C")), Right("D"))
    var resultListExpected: Either[List[DomainError], List[String]] = Left(List(DomainError.IOFileProblem("B"), DomainError.IOFileProblem("C")))
    var resultListResult: Either[List[DomainError], List[String]] = Helper.listResultToResultList(listResult)
    assert(resultListExpected.isLeft, resultListResult.isLeft)
    assert(resultListResult.fold(errorResult => resultListExpected.fold(errorExpected => errorResult === errorExpected, _ => false) , _ => false))
  }

  test("getListContainsSmallerElements should return first list if both are equal - List of Strings"){
    val listA: List[String] = List("Element1", "Element2")
    val listB: List[String] = listA
    val expected = listA
    val result = Helper.getListContainsSmallerElements(listA, listB)

    assert(expected === result)
  }

  test("getListContainsSmallerElements should return first list if lists contains the same elements - List of Strings"){
    val listA: List[String] = List("Element1", "Element2")
    val listB: List[String] = List("Element2", "Element1")
    val expected = listA
    val result = Helper.getListContainsSmallerElements(listA, listB)

    assert(expected === result)
  }

  test("getListContainsSmallerElements should return first since it contains the smaller elements - List of Strings"){
    val listA: List[String] = List("Element1", "Element2")
    val listB: List[String] = List("Element3", "Element1")
    val expected = listA
    val result = Helper.getListContainsSmallerElements(listA, listB)

    assert(expected === result)
  }

  test("getListContainsSmallerElements should return second since it contains the smaller elements - List of Strings"){
    val listA: List[String] = List("Element3", "Element1")
    val listB: List[String] = List("Element1", "Element2")
    val expected = listB
    val result = Helper.getListContainsSmallerElements(listA, listB)

    assert(expected === result)
  }

  test("getListContainsSmallerElements should return first list if both are equal - List of OrderId"){
    for {
      order1 <- OrderId.from("ORD_1")
      order2 <- OrderId.from("ORD_2")

      listA: List[OrderId] = List(order1, order2)
      listB: List[OrderId] = List(order1, order2)
      expected = listA
      result = Helper.getListContainsSmallerElements(listA, listB)
    } yield assert(expected === result)
  }

  test("getListContainsSmallerElements should return first list if lists contains the same elements - List of OrderId"){
    for {
      order1 <- OrderId.from("ORD_1")
      order2 <- OrderId.from("ORD_2")

      listA: List[OrderId] = List(order1, order2)
      listB: List[OrderId] = List(order2, order1)
      expected = listA
      result = Helper.getListContainsSmallerElements(listA, listB)
    } yield assert(expected === result)
  }

  test("getListContainsSmallerElements should return first list since it contains the smaller elements - List of OrderId"){
    for {
      order1 <- OrderId.from("ORD_1")
      order2 <- OrderId.from("ORD_2")
      order3 <- OrderId.from("ORD_3")

      listA: List[OrderId] = List(order1, order3)
      listB: List[OrderId] = List(order2, order3)
      expected = listA
      result = Helper.getListContainsSmallerElements(listA, listB)
    } yield assert(expected === result)
  }

  test("getListContainsSmallerElements should return second since it contains the smaller elements - List of OrderId"){
    for {
      order1 <- OrderId.from("ORD_1")
      order2 <- OrderId.from("ORD_2")
      order3 <- OrderId.from("ORD_3")

      listA: List[OrderId] = List(order2, order3)
      listB: List[OrderId] = List(order2, order1)
      expected = listB
      result = Helper.getListContainsSmallerElements(listA, listB)
    } yield assert(expected === result)
  }

  test("getListContainsSmallerElements should return first list if both are equal - List of ProductNumber"){
    for {
      productNumber1 <- ProductNumber.from(1)
      productNumber2 <- ProductNumber.from(2)

      listA: List[ProductNumber] = List(productNumber1, productNumber2)
      listB: List[ProductNumber] = List(productNumber1, productNumber2)
      expected = listA
      result = Helper.getListContainsSmallerElements(listA, listB)
    } yield assert(expected === result)
  }

  test("getListContainsSmallerElements should return first list if lists contains the same elements - List of ProductNumber"){
    for {
      productNumber1 <- ProductNumber.from(1)
      productNumber2 <- ProductNumber.from(2)

      listA: List[ProductNumber] = List(productNumber1, productNumber2)
      listB: List[ProductNumber] = List(productNumber2, productNumber1)
      expected = listA
      result = Helper.getListContainsSmallerElements(listA, listB)
    } yield assert(expected === result)
  }

  test("getListContainsSmallerElements should return first list since it contains the smaller elements - List of ProductNumber"){
    for {
      productNumber1 <- ProductNumber.from(1)
      productNumber2 <- ProductNumber.from(2)
      productNumber3 <- ProductNumber.from(3)

      listA: List[ProductNumber] = List(productNumber1, productNumber3)
      listB: List[ProductNumber] = List(productNumber2, productNumber3)

      expected = listA
      result = Helper.getListContainsSmallerElements(listA, listB)
    } yield assert(expected === result)
  }

  test("getListContainsSmallerElements should return second since it contains the smaller elements - List of ProductNumber"){
    for {
      productNumber1 <- ProductNumber.from(1)
      productNumber2 <- ProductNumber.from(2)
      productNumber3 <- ProductNumber.from(3)

      listA: List[ProductNumber] = List(productNumber2, productNumber3)
      listB: List[ProductNumber] = List(productNumber2, productNumber1)
      expected = listB
      result = Helper.getListContainsSmallerElements(listA, listB)
    } yield assert(expected === result)
}