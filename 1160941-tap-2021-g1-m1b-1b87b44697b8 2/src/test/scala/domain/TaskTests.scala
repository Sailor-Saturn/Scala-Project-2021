package domain

import domain.SimpleTypes.{TaskId, Time, ProductName, Quantity}
import domain.DomainError.{InvalidTaskId, InvalidTime, InvalidResourceType, TaskUsesNonExistentPRT, TaskDoesNotExist}
import org.scalatest.funsuite.AnyFunSuite
import scala.language.adhocExtensions

class TaskTests extends AnyFunSuite:
  test("Task should be created with no errors"){
    val xml = scala.xml.XML.loadString({
      <Task id="TSK_1" time="100">
        <PhysicalResource type="PRST 1"/>
        <PhysicalResource type="PRST 2"/>
        <PhysicalResource type="PRST 3"/>
      </Task>
    }.toString)
  
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")
    
    for {
      id <- TaskId.from("TSK_1")
      time <- Time.from("100")
      r1 <-  resourceType1
      r2 <-  resourceType2
      r3 <-  resourceType3
      result <- Task.from(List(r1, r2, r3))(xml)
    } yield assert(result.taskId === id && result.time === time && result.resourceTypes === List(r1, r2, r3))
  
    val result = for {
      r1 <-  resourceType1
      r2 <-  resourceType2
      r3 <-  resourceType3
      result <- Task.from(List(r1, r2, r3))(xml)
    } yield result
    assert(result.isRight)
  }

  test("Task should be created with error on TaskId"){
    val xml = scala.xml.XML.loadString({
      <Task id="TSK1" time="100">
        <PhysicalResource type="PRST 1"/>
        <PhysicalResource type="PRST 2"/>
        <PhysicalResource type="PRST 3"/>
      </Task>
    }.toString)

    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    for {
      id <- TaskId.from("TSK1")
      time <- Time.from("100")
      r1 <-  resourceType1
      r2 <-  resourceType2
      r3 <-  resourceType3
      result <- Task.from(List(r1, r2, r3))(xml)
    } yield assert(result.taskId != id && result.time === time && result.resourceTypes === List(r1, r2, r3))

    val result = for {
      r1 <-  resourceType1
      r2 <-  resourceType2
      r3 <-  resourceType3
      result <- Task.from(List(r1, r2, r3))(xml)
    } yield result
    assert(result.isLeft)
    assert(result.fold(error => error === InvalidTaskId("TSK1"), _ => false))
  }

  test("Task should be created with error on Time"){
    val xml = scala.xml.XML.loadString({
      <Task id="TSK_1" time="-1">
        <PhysicalResource type="PRST 1"/>
        <PhysicalResource type="PRST 2"/>
        <PhysicalResource type="PRST 3"/>
      </Task>
    }.toString)

    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    for {
      id <- TaskId.from("TSK_1")
      time <- Time.from("-1")
      r1 <-  resourceType1
      r2 <-  resourceType2
      r3 <-  resourceType3
      result <- Task.from(List(r1, r2, r3))(xml)
    } yield assert(result.taskId === id && result.time != time && result.resourceTypes === List(r1, r2, r3))

    val result = for {
      r1 <-  resourceType1
      r2 <-  resourceType2
      r3 <-  resourceType3
      result <- Task.from(List(r1, r2, r3))(xml)
    } yield result
    assert(result.isLeft)
    assert(result.fold(error => error === InvalidTime(-1), _ => false))
  }

  test("Task should be created with error on ResourceType"){
    val xml = scala.xml.XML.loadString({
      <Task id="TSK_1" time="100">
        <PhysicalResource type="PRS_1"/>
        <PhysicalResource type="PRST 2"/>
        <PhysicalResource type="PRST 3"/>
      </Task>
    }.toString)

    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    for {
      id <- TaskId.from("TSK_1")
      time <- Time.from("100")
      r1 <-  resourceType1
      r2 <-  resourceType2
      r3 <-  resourceType3
      result <- Task.from(List(r1, r2, r3))(xml)
    } yield assert(result.taskId === id && result.time === time && result.resourceTypes != List(r1, r2, r3))

    val result = for {
      r1 <-  resourceType1
      r2 <-  resourceType2
      r3 <-  resourceType3
      result <- Task.from(List(r1, r2, r3))(xml)
    } yield result
    assert(result.isLeft)
    assert(result.fold(error => error === InvalidResourceType("PRS_1"), _ => false))
  }
  
  test("Task should be created with error on ResourceType (Refers a non existent Resource Type)"){
    val xml = scala.xml.XML.loadString({
      <Task id="TSK_1" time="100">
        <PhysicalResource type="PRST 1"/>
        <PhysicalResource type="PRST 2"/>
        <PhysicalResource type="PRST 3"/>
      </Task>
    }.toString)

    val resourceType1 = ResourceType.from("PRST 4")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    for {
      id <- TaskId.from("TSK_1")
      time <- Time.from("100")
      r1 <-  resourceType1
      r2 <-  resourceType2
      r3 <-  resourceType3
      result <- Task.from(List(r1, r2, r3))(xml)
    } yield assert(result.taskId === id && result.time === time && result.resourceTypes != List(r1, r2, r3))

    val result = for {
      r1 <-  resourceType1
      r2 <-  resourceType2
      r3 <-  resourceType3
      result <- Task.from(List(r1, r2, r3))(xml)
    } yield result
    assert(result.isLeft)
    assert(result.fold(error => error === TaskUsesNonExistentPRT("PRST 1"), _ => false))
  }

  test("Task should be returned with no errors"){
    val xml = scala.xml.XML.loadString("<Process tskref=\"TSK_8\"/>")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_8")
      time1     <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId2   <- TaskId.from("TSK_7")
      time2     <- Time.from("100")
    } yield Task(taskId2, time2, List())

    for {
      taskId1   <- TaskId.from("TSK_8")
      time1     <- Time.from("100")
      t1     <- task1
      t2     <- task2
      result <- Task.findTask(List(t1, t2))(xml)
    } yield assert(result.taskId === taskId1 && result.time === time1 && result.resourceTypes === List())

    val result = for {
      t1     <- task1
      t2     <- task2
      result <- Task.findTask(List(t1, t2))(xml)
    } yield result
    assert(result.isRight)
  }

  test("Task should be returned with errors on TaskId"){
    val xml = scala.xml.XML.loadString("<Process tskref=\"TSK8\"/>")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_8")
      time1     <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId2   <- TaskId.from("TSK_7")
      time2     <- Time.from("100")
    } yield Task(taskId2, time2, List())

    for {
      taskId1   <- TaskId.from("TSK8")
      time1     <- Time.from("100")
      t1     <- task1
      t2     <- task2
      result <- Task.findTask(List(t1, t2))(xml)
    } yield assert(result.taskId != taskId1 && result.time === time1 && result.resourceTypes === List())

    val result = for {
      t1     <- task1
      t2     <- task2
      result <- Task.findTask(List(t1, t2))(xml)
    } yield result
    assert(result.isLeft)
    assert(result.fold(error => error === InvalidTaskId("TSK8"), _ => false))
  }

  test("Task should be returned with errors since it refers to a non existent Task)"){
    val xml = scala.xml.XML.loadString("<Process tskref=\"TSK_1\"/>")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_8")
      time1     <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId2   <- TaskId.from("TSK_7")
      time2     <- Time.from("100")
    } yield Task(taskId2, time2, List())

    for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      t1     <- task1
      t2     <- task2
      result <- Task.findTask(List(t1, t2))(xml)
    } yield assert(result.taskId != taskId1 && result.time === time1 && result.resourceTypes === List())

    val result = for {
      t1     <- task1
      t2     <- task2
      result <- Task.findTask(List(t1, t2))(xml)
    } yield result
    assert(result.isLeft)
    assert(result.fold(error => error === TaskDoesNotExist("TSK_1"), _ => false))
  }