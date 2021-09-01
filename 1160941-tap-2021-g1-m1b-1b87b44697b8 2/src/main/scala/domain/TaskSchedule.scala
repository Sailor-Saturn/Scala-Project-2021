package domain

import domain.SimpleTypes.{OrderId, ProductNumber, TaskId, StartTime, EndTime}
import domain.DomainError.ResourceUnavailable
import utils.Helper

final case class TaskSchedule(order: OrderId,
                        productNumber: ProductNumber,
                        taskId: TaskId,
                        start: StartTime,
                        end: EndTime,
                        physicalResources: List[PhysicalResource],
                        humanResources: List[HumanResource])

object TaskSchedule:

  /**
   * Creates TaskSchedule
   * 
   * @param order OrderId
   * @param productNumber ProductNumber
   * @param taskId TaskId
   * @param start StartTime
   * @param end EndTime
   * @param physicalResources List[Result[PhysicalResource]]
   * @param humanResources List[Result[HumanResource]]
   * @return Result[TaskSchedule] - If it is a valid TaskSchedule then returns it otherwise returns the respective domain error
   */
  def from(order: OrderId,
           productNumber: ProductNumber,
           taskId: TaskId,
           start: StartTime,
           end: EndTime,
           physicalResources: List[Result[PhysicalResource]],
           humanResources: List[Result[HumanResource]]): Result[TaskSchedule] =

    val hrResult: Either[List[DomainError], List[HumanResource]] = Helper.listResultToResultList(humanResources)
    
    hrResult.fold[Result[TaskSchedule]](error => {
      Left(error.head)
    }, listHuman => {

      val prResult: Either[List[DomainError], List[PhysicalResource]] = Helper.listResultToResultList(physicalResources)
      prResult.fold[Result[TaskSchedule]](error => {
        Left(error.head)
      }, listPhysical => {
        Right(TaskSchedule(order, productNumber, taskId, start, end, listPhysical, listHuman))
      })
    })

  /**
   * Creates TaskSchedule
   *
   * @param order OrderId
   * @param productNumber ProductNumber
   * @param taskId TaskId
   * @param start StartTime
   * @param end EndTime
   * @param physicalResources List[PhysicalResource]
   * @param humanResources List[HumanResource]
   * @return Result[TaskSchedule] - If it is a valid TaskSchedule then returns it otherwise returns the respective domain error
   */
  def from(productNumber: ProductNumber,
           taskId: TaskId,
           start: StartTime,
           end: EndTime,
           order: OrderId,
           physicalResources: List[PhysicalResource],
           humanResources: List[HumanResource]): TaskSchedule =

    TaskSchedule(order, productNumber, taskId, start, end, physicalResources, humanResources)
