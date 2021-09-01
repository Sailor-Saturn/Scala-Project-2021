package domain

import domain.SimpleTypes.{ProductId, ProductName, TaskId, Time}
import domain.DomainError.{InvalidProductId, InvalidProductName, TaskDoesNotExist}
import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions

class ProductTests extends AnyFunSuite:
  test("Product should be created with no errors"){
    val xml = scala.xml.XML.loadString("<Product id=\"PRD_4\" name=\"Product 4\"><Process tskref=\"TSK_8\"/><Process tskref=\"TSK_7\"/></Product>")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_8")
      time1     <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId2   <- TaskId.from("TSK_7")
      time2     <- Time.from("100")
    } yield Task(taskId2, time2, List())

    for {
      id     <- ProductId.from("PRD_4")
      name   <- ProductName.from("Product 4")
      t1     <- task1
      t2     <- task2
      result <- Product.from(List(t1, t2))(xml)
    } yield assert(result.productId === id && result.productName === name && result.processes === List(t1, t2))

    val result = for {
      id     <- ProductId.from("PRD_4")
      name   <- ProductName.from("Product 4")
      t1     <- task1
      t2     <- task2
      result <- Product.from(List(t1, t2))(xml)
    } yield result

    assert(result.isRight)
  }

  test("Product should be created with errors on ProductId"){
    val xml = scala.xml.XML.loadString("<Product id=\"PRD4\" name=\"Product 4\"><Process tskref=\"TSK_8\"/><Process tskref=\"TSK_7\"/></Product>")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_8")
      time1     <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId2   <- TaskId.from("TSK_7")
      time2     <- Time.from("100")
    } yield Task(taskId2, time2, List())

    for {
      id     <- ProductId.from("PRD4")
      name   <- ProductName.from("Product 4")
      t1     <- task1
      t2     <- task2
      result <- Product.from(List(t1, t2))(xml)
    } yield assert(result.productId != id && result.productName === name && result.processes === List(t1, t2))

    val result = for {
      id     <- ProductId.from("PRD4")
      name   <- ProductName.from("Product 4")
      t1     <- task1
      t2     <- task2
      result <- Product.from(List(t1, t2))(xml)
    } yield result

    assert(result.isLeft)
    assert(result.fold(error => error === InvalidProductId("PRD4"), _ => false))

  }

  test("Product should be created with errors on ProductName"){
    val xml = scala.xml.XML.loadString("<Product id=\"PRD4\" name=\"\"><Process tskref=\"TSK_8\"/><Process tskref=\"TSK_7\"/></Product>")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_8")
      time1     <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId2   <- TaskId.from("TSK_7")
      time2     <- Time.from("100")
    } yield Task(taskId2, time2, List())

    for {
      id     <- ProductId.from("PRD_4")
      name   <- ProductName.from("")
      t1     <- task1
      t2     <- task2
      result <- Product.from(List(t1, t2))(xml)
    } yield assert(result.productId === id && result.productName != name && result.processes === List(t1, t2))

    val result = for {
      id     <- ProductId.from("PRD_4")
      name   <- ProductName.from("")
      t1     <- task1
      t2     <- task2
      result <- Product.from(List(t1, t2))(xml)
    } yield result

    assert(result.isLeft)
    assert(result.fold(error => error === InvalidProductName(""), _ => false))

  }

  test("Product should be created with errors on List[Task] (Refers a non existent task)"){
    val xml = scala.xml.XML.loadString("<Product id=\"PRD_4\" name=\"Product 4\"><Process tskref=\"TSK_8\"/><Process tskref=\"TSK_7\"/></Product>")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_8")
      time1     <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId2   <- TaskId.from("TSK_33")
      time2     <- Time.from("100")
    } yield Task(taskId2, time2, List())

    for {
      id     <- ProductId.from("PRD_4")
      name   <- ProductName.from("Product 4")
      t1     <- task1
      t2     <- task2
      result <- Product.from(List(t1, t2))(xml)
    } yield assert(result.productId === id && result.productName === name && result.processes != List(t1, t2))

    val result = for {
      id     <- ProductId.from("PRD_4")
      name   <- ProductName.from("Product 4")
      t1     <- task1
      t2     <- task2
      result <- Product.from(List(t1, t2))(xml)
    } yield result

    assert(result.isLeft)
    assert(result.fold(error => error === TaskDoesNotExist("TSK_7"), _ => false))
  }