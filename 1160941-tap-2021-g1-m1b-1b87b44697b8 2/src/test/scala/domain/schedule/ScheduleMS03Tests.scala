package domain.schedule

import io.FileIO.load
import org.scalatest.funsuite.AnyFunSuite
import domain.{DomainError, Result}

import scala.xml.Utility
import scala.language.adhocExtensions

class ScheduleMS03Tests extends AnyFunSuite:

  test("ScheduleMS03Tests success - validAgenda_1TaskPerOrder"){
    val fileName = "validAgenda_1TaskPerOrder"

    val schedule: Result[Boolean] = for {
      xml <- load(s"files/assessment/ms03/testFiles/${fileName}_in.xml")
      scheduleResult <- ScheduleMS03.create(xml)
      scheduleExpected <- load(s"files/assessment/ms03/testFiles/${fileName}_out.xml")
    } yield Utility.trim(scheduleResult) === Utility.trim(scheduleExpected)

    assert(schedule.fold(_ => false, b => b))
  }

  test("ScheduleMS03Tests success - validAgenda_MultipleQuantity"){
    val fileName = "validAgenda_MultipleQuantity"

    val schedule: Result[Boolean] = for {
      xml <- load(s"files/assessment/ms03/testFiles/${fileName}_in.xml")
      scheduleResult <- ScheduleMS03.create(xml)
      scheduleExpected <- load(s"files/assessment/ms03/testFiles/${fileName}_out.xml")
    } yield Utility.trim(scheduleResult) === Utility.trim(scheduleExpected)

    assert(schedule.fold(_ => false, b => b))
  }

  test("ScheduleMS03Tests success - validAgenda_PreferenceOrderInitiated"){
    val fileName = "validAgenda_PreferenceOrderInitiated"

    val schedule: Result[Boolean] = for {
      xml <- load(s"files/assessment/ms03/testFiles/${fileName}_in.xml")
      scheduleResult <- ScheduleMS03.create(xml)
      scheduleExpected <- load(s"files/assessment/ms03/testFiles/${fileName}_out.xml")
    } yield Utility.trim(scheduleResult) === Utility.trim(scheduleExpected)

    assert(schedule.fold(_ => false, b => b))
  }

  test("ScheduleMS03Tests ImpossibleSchedule - invalidAgenda_NoEnoughPhysicals"){
    val fileName = "invalidAgenda_NoEnoughPhysicals"

    val schedule: Result[Boolean] = for {
      xml <- load(s"files/assessment/ms03/testFiles/${fileName}_in.xml")
      scheduleResult <- ScheduleMS03.create(xml)
      scheduleExpected <- load(s"files/assessment/ms03/testFiles/${fileName}_out.xml")
    } yield Utility.trim(scheduleResult) === Utility.trim(scheduleExpected)

    assert(schedule.fold(error => error == DomainError.ImpossibleSchedule, b => false))
  }

  test("ScheduleMS03Tests ImpossibleSchedule - invalidAgenda_NoEnoughHumans"){
    val fileName = "invalidAgenda_NoEnoughHumans"

    val schedule: Result[Boolean] = for {
      xml <- load(s"files/assessment/ms03/testFiles/${fileName}_in.xml")
      scheduleResult <- ScheduleMS03.create(xml)
      scheduleExpected <- load(s"files/assessment/ms03/testFiles/${fileName}_out.xml")
    } yield Utility.trim(scheduleResult) === Utility.trim(scheduleExpected)

    assert(schedule.fold(error => error == DomainError.ImpossibleSchedule, b => false))
  }
