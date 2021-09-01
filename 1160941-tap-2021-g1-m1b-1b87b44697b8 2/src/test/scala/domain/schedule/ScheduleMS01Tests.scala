package domain.schedule

import domain.{DomainError, SimpleTypes, Result}
import domain.schedule.{MS01Scheduler, ScheduleMS01}
import io.FileIO.load
import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions
import scala.xml.{Elem, Utility}

class ScheduleMS01Tests extends AnyFunSuite:
  test("MS01Scheduler with success"){
    val schedule: Result[Boolean] = for {
      xml <- load("files/assessment/ms01/testFiles/MS01SchedulerSuccess-in.xml")
      scheduleResult <- ScheduleMS01.create(xml)
      scheduleExpected <- load("files/assessment/ms01/testFiles/MS01SchedulerSuccess-out.xml")
    } yield Utility.trim(scheduleResult) === Utility.trim(scheduleExpected)
    assert(schedule.fold(_ => false, b => b))
  }

  test("MS01Scheduler with error on human resources"){
     val schedule: Result[Elem] = for {
       xml <- load("files/assessment/ms01/testFiles/MS01SchedulerErrorHumanResources-in.xml")
       scheduleResult <- ScheduleMS01.create(xml)
       scheduleExpected <- load("files/assessment/ms01/testFiles/MS01SchedulerErrorHumanResources-out.xml")
     } yield scheduleResult
     assert(schedule.fold(error => error === DomainError.ResourceUnavailable("TSK_3,PRST 5"), b => false))
  }

  test("MS01Scheduler with error on physical resources"){
    val schedule: Result[Elem] = for {
      xml <- load("files/assessment/ms01/testFiles/MS01SchedulerErrorPhysicalResources-in.xml")
      scheduleResult <- ScheduleMS01.create(xml)
      scheduleExpected <- load("files/assessment/ms01/testFiles/MS01SchedulerErrorPhysicalResources-out.xml")
    } yield scheduleResult
    assert(schedule.fold(error => error === DomainError.ResourceUnavailable("TSK_3,PRST 1"), b => false))
  }

  test("MS01Scheduler ResourceUnavailable-physical"){
    val schedule: Result[Elem] = for {
      xml <- load("files/assessment/ms01/testFiles/MS01SchedulerResourceUnavailable-physical-in.xml")
      scheduleResult <- ScheduleMS01.create(xml)
      scheduleExpected <- load("files/assessment/ms01/testFiles/MS01SchedulerResourceUnavailable-physical-out.xml")
    } yield scheduleResult
    assert(schedule.fold(error => error === DomainError.ResourceUnavailable("TSK_1,PRST 2"), b => false))
  }

  test("MS01Scheduler ResourceUnavailable-human"){
    val schedule: Result[Elem] = for {
      xml <- load("files/assessment/ms01/testFiles/MS01SchedulerResourceUnavailable-human-in.xml")
      scheduleResult <- ScheduleMS01.create(xml)
      scheduleExpected <- load("files/assessment/ms01/testFiles/MS01SchedulerResourceUnavailable-human-out.xml")
    } yield scheduleResult
    assert(schedule.fold(error => error === DomainError.ResourceUnavailable("TSK_1,PRST 2"), b => false))
  }

  test("MS01Scheduler success - repeated Resources"){
    val schedule: Result[Boolean] = for {
      xml <- load("files/assessment/ms01/testFiles/MS01SchedulerSuccess-repeatedResources-in.xml")
      scheduleResult <- ScheduleMS01.create(xml)
      scheduleExpected <- load("files/assessment/ms01/testFiles/MS01SchedulerSuccess-repeatedResources-out.xml")
    } yield
      Utility.trim(scheduleResult) === Utility.trim(scheduleExpected)
    assert(schedule.fold(_ => false, b => b))
  }


