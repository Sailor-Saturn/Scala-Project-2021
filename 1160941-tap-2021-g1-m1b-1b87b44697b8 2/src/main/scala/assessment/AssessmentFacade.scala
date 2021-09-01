package assessment

import scala.xml.Elem
import domain.Result
import domain.schedule.{Schedule, ScheduleMS01, ScheduleMS03}

object AssessmentMS01 extends Schedule:
  def create(xml: Elem): Result[Elem] = ScheduleMS01.create(xml)

object AssessmentMS03 extends Schedule:
  def create(xml: Elem): Result[Elem] = ScheduleMS03.create(xml)
