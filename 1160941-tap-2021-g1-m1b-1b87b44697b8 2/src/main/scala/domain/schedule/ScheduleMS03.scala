package domain.schedule

import scala.xml.Elem
import domain.{Result, DomainError}

object ScheduleMS03 extends Schedule:

  /**
   * Creates Milestone 3 (MS03) Production Schedule
   * @param xml XML Elem
   *  @return Result[Elem]
   */
  def create(xml: Elem): Result[Elem] =
    for {
      result <- SchedulerResolver.applyScheduler(xml, production => {
        MS03Scheduler.scheduleProduction(production)
      })
    } yield result
