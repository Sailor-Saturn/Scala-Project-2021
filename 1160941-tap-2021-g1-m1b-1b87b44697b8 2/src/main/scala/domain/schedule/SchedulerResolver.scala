package domain.schedule

import domain.{DomainError, HumanResource, PhysicalResource, Production, Result, TaskSchedule}
import xml.XMLParser
import scala.xml.Elem

object SchedulerResolver:

  /**
   * Higher order function that deserializes input XML, runs the algorithm and serializes output XML 
   * 
   * @param xml XML Elem
   * @param scheduler Production => Result[List[TaskSchedule]] - Algorithm 
   * @return Result[Elem] - If algorithm is run without errors then returns the output XML Elem, otherwise returns the respective domain error
   */
  def applyScheduler(xml: Elem, scheduler: Production => Result[List[TaskSchedule]]): Result[Elem] =
    val production: Result[Production] = Production.from(xml)
    production match
      case Right(production) => {
        val schedule: Result[List[TaskSchedule]] = scheduler(production)
        schedule.fold(error => Left(error/*DomainError.XMLResultError(XMLParser.serializeError(error.toString))*/), success => Right(XMLParser.serialize(success)))
      }
      case Left(error) => {
        Left(error/*DomainError.XMLResultError(XMLParser.serializeError(error.toString))*/)
      }
