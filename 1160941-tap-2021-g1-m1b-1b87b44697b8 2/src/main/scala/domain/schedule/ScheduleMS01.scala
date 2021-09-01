package domain.schedule

import scala.xml.Elem
import domain.{HumanResource,DomainError,Order,PhysicalResource,Product,Production,ResourceType,SimpleTypes,Task,TaskSchedule,Result}
import xml.XML.traverse

object ScheduleMS01 extends Schedule:
  /**
   * Creates Milestone 1 (MS01) Production Schedule
   * @param xml XML Elem
   * @return Result[Elem]
   */
  def create(xml: Elem): Result[Elem] =
    for {
      result <- SchedulerResolver.applyScheduler(xml, production => MS01Scheduler.scheduleProduction(production))
    } yield result
