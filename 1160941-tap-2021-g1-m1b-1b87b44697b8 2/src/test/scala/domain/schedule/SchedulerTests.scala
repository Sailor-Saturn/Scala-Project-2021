package domain.schedule

import io.FileIO.load
import org.scalatest.funsuite.AnyFunSuite
import domain.{Production, Result}

import scala.xml.Utility
import scala.language.adhocExtensions

class SchedulerTests extends AnyFunSuite:

  test("MS03Scheduler should provide a production time equal to or smaller than MS01Scheduler - MS01SchedulerSuccess"){
    val fileName = "MS01SchedulerSuccess"
    val folder = "ms01"
    val schedule: Result[Boolean] = for {
      xml <- load(s"files/assessment/${folder}/testFiles/${fileName}-in.xml")
      production <- Production.from(xml)
      scheduleMS01 <- MS01Scheduler.scheduleProduction(production)
      scheduleMS03 <- MS03Scheduler.scheduleProduction(production)
    } yield scheduleMS01.map(_.end.to).max <= scheduleMS03.map(_.end.to).max

    assert(schedule.fold(_ => false, b => b))
  }

  test("MS03Scheduler should provide a production time equal to or smaller than MS01Scheduler - MS01SchedulerSuccess-repeatedResources"){
    val fileName = "MS01SchedulerSuccess-repeatedResources"
    val folder = "ms01"
    val schedule: Result[Boolean] = for {
      xml <- load(s"files/assessment/${folder}/testFiles/${fileName}-in.xml")
      production <- Production.from(xml)
      scheduleMS01 <- MS01Scheduler.scheduleProduction(production)
      scheduleMS03 <- MS03Scheduler.scheduleProduction(production)
    } yield scheduleMS01.map(_.end.to).max <= scheduleMS03.map(_.end.to).max

    assert(schedule.fold(_ => false, b => b))
  }
