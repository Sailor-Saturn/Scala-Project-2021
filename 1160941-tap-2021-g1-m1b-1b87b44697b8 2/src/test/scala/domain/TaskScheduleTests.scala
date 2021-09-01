package domain

import domain.SimpleTypes.{EndTime, OrderId, ProductNumber, StartTime, TaskId}
import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions

class TaskScheduleTests extends AnyFunSuite:
  test("TaskSchedule should be created with no errors (physicalResources empty, humanResources empty)"){
    val taskSchedule: Result[TaskSchedule] = for {
      orderId <- OrderId.from("ORD_1")
      pn <- ProductNumber.from(1)
      taskId <- TaskId.from("TSK_1")
      startTime <- StartTime.from(0)
      endTime <- EndTime.from(100)
      taskSchedule <- TaskSchedule.from(orderId, pn, taskId, startTime, endTime, List(), List())
    } yield taskSchedule
    assert(taskSchedule.isRight)
  }
  
  test("TaskSchedule should be created with no errors (humanResources empty)"){

    val xmlPhysical = scala.xml.XML.loadString("<Physical id=\"PRS_1\" type=\"PRST 1\"/>")
    val physical = PhysicalResource.from(xmlPhysical)
    
    val taskSchedule: Result[TaskSchedule] = for {
      orderId <- OrderId.from("ORD_1")
      pn <- ProductNumber.from(1)
      taskId <- TaskId.from("TSK_1")
      startTime <- StartTime.from(0)
      endTime <- EndTime.from(100)
      taskSchedule <- TaskSchedule.from(orderId, pn, taskId, startTime, endTime, List(physical), List())
    } yield taskSchedule
    assert(taskSchedule.isRight)
  }

  test("TaskSchedule should be created with no errors (physicalResources empty)"){

    val xmlHuman = scala.xml.XML.loadString({
      <Human id="HRS_1" name="Antonio">
        <Handles type="PRST 1"/>
        <Handles type="PRST 2"/>
      </Human>
    }.toString)
    val human = HumanResource.from(xmlHuman)

    val taskSchedule: Result[TaskSchedule] = for {
      orderId <- OrderId.from("ORD_1")
      pn <- ProductNumber.from(1)
      taskId <- TaskId.from("TSK_1")
      startTime <- StartTime.from(0)
      endTime <- EndTime.from(100)
      taskSchedule <- TaskSchedule.from(orderId, pn, taskId, startTime, endTime, List(), List(human))
    } yield taskSchedule
    assert(taskSchedule.isRight)
  }

  test("TaskSchedule should be created with no errors"){

    val xmlPhysical = scala.xml.XML.loadString("<Physical id=\"PRS_1\" type=\"PRST 1\"/>")
    val physical = PhysicalResource.from(xmlPhysical)

    val xmlHuman = scala.xml.XML.loadString({
      <Human id="HRS_1" name="Antonio">
        <Handles type="PRST 1"/>
        <Handles type="PRST 2"/>
      </Human>
    }.toString)
    val human = HumanResource.from(xmlHuman)

    val taskSchedule: Result[TaskSchedule] = for {
      orderId <- OrderId.from("ORD_1")
      pn <- ProductNumber.from(1)
      taskId <- TaskId.from("TSK_1")
      startTime <- StartTime.from(0)
      endTime <- EndTime.from(100)
      taskSchedule <- TaskSchedule.from(orderId, pn, taskId, startTime, endTime, List(physical), List(human))
    } yield taskSchedule
    assert(taskSchedule.isRight)
  }

  test("TaskSchedule should be created with errors (physicalResources with errors, humanResources empty)"){

    val physical = Left(DomainError.ResourceUnavailable("Test"))
  
    val taskSchedule: Result[TaskSchedule] = for {
      orderId <- OrderId.from("ORD_1")
      pn <- ProductNumber.from(1)
      taskId <- TaskId.from("TSK_1")
      startTime <- StartTime.from(0)
      endTime <- EndTime.from(100)
      taskSchedule <- TaskSchedule.from(orderId, pn, taskId, startTime, endTime, List(physical), List())
    } yield taskSchedule
    assert(taskSchedule.isLeft)
    assert(taskSchedule.fold(error => error === physical.value, _ => false))
  }

  test("TaskSchedule should be created with errors (physicalResources empty, humanResources with errors)"){

    val human = Left(DomainError.ResourceUnavailable("Test"))

    val taskSchedule: Result[TaskSchedule] = for {
      orderId <- OrderId.from("ORD_1")
      pn <- ProductNumber.from(1)
      taskId <- TaskId.from("TSK_1")
      startTime <- StartTime.from(0)
      endTime <- EndTime.from(100)
      taskSchedule <- TaskSchedule.from(orderId, pn, taskId, startTime, endTime, List(), List(human))
    } yield taskSchedule
    assert(taskSchedule.isLeft)
    assert(taskSchedule.fold(error => error === human.value, _ => false))
  }

  test("TaskSchedule should be created with errors (physicalResources with errors, humanResources with errors)"){

    val physical = Left(DomainError.ResourceUnavailable("Error on physical"))
    val human = Left(DomainError.ResourceUnavailable("Error on human"))

    val taskSchedule: Result[TaskSchedule] = for {
      orderId <- OrderId.from("ORD_1")
      pn <- ProductNumber.from(1)
      taskId <- TaskId.from("TSK_1")
      startTime <- StartTime.from(0)
      endTime <- EndTime.from(100)
      taskSchedule <- TaskSchedule.from(orderId, pn, taskId, startTime, endTime, List(physical), List(human))
    } yield taskSchedule
    assert(taskSchedule.isLeft)
    assert(taskSchedule.fold(error => error === human.value, _ => false))
  }
