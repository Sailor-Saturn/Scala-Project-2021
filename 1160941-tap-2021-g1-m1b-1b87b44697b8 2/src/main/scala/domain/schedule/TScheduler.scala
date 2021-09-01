package domain.schedule

import domain.{Production, TaskSchedule, Result}

trait TScheduler:
  /**
   * Scheduling Algorithm
   * @param production Production
   * @return Result[List[TaskSchedule]]
   */
  def scheduleProduction(production: Production): Result[List[TaskSchedule]]
