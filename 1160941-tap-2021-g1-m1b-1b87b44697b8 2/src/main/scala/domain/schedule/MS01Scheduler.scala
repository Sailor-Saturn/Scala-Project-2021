package domain.schedule

import domain.SimpleTypes.{EndTime, OrderId, ProductNumber, StartTime}
import domain.{HumanResource, Order, PhysicalResource, Product, Production, ResourceType, Result, TaskSchedule}
import domain.DomainError
import domain.DomainError.ResourceUnavailable
import utils.Helper

import scala.annotation.tailrec

object MS01Scheduler extends TScheduler:

  /**
   * Milestone 1 (MS01) Scheduling Algorithm
   * @param production Production
   *  @return Result[List[TaskSchedule]]
   */
  override def scheduleProduction(production: Production): Result[List[TaskSchedule]] =
    val taskSchedules: List[Result[TaskSchedule]] = production.orders.foldLeft[(List[Result[TaskSchedule]], Int)](List(), 0) {
      case ((list, startTime), order) => {
        val orderTaskSchedule: (List[Result[TaskSchedule]], Int) = orderToTaskSchedule(order, production, startTime)
        (orderTaskSchedule._1 ::: list, orderTaskSchedule._2)
      }
    }._1.reverse
    val result: Either[List[DomainError], List[TaskSchedule]] = Helper.listResultToResultList(taskSchedules)
    result.fold[Result[List[TaskSchedule]]](error => Left(error.head), success => Right(success))

  private def orderToTaskSchedule(order: Order, production: Production, startTime: Int): (List[Result[TaskSchedule]], Int) =
    
    @tailrec
    def orderToTaskSchedule(productNumber: Int,
                            orderId: OrderId,
                            quantity: Int, 
                            product: Product, 
                            physicalResources: List[PhysicalResource], 
                            humanResources: List[HumanResource], 
                            start: Int, 
                            listTaskSchedule: List[Result[TaskSchedule]] ) : (List[Result[TaskSchedule]], Int) =
      
      if(productNumber > quantity) (listTaskSchedule, start)
      else
        val taskSchedule: (List[Result[TaskSchedule]], Int) = product.processes.foldLeft[(List[Result[TaskSchedule]], Int)]((List(), start)){
          case((list, start), task) => {
            
            val physical: List[Result[PhysicalResource]] = findPhysicalResources(physicalResources, task.taskId.to, task.resourceTypes)
            val human: List[Result[HumanResource]] = findHumanResources(humanResources, task.taskId.to, task.resourceTypes)

            val finalTime: Int = start + task.time.to
            
            val taskSchedule: Result[TaskSchedule] = for {
              pn: ProductNumber <- ProductNumber.from(productNumber)
              startTime: StartTime <- StartTime.from(start)
              endTime: EndTime <- EndTime.from(finalTime)
              taskSchedule: TaskSchedule <- TaskSchedule.from(orderId, pn, task.taskId, startTime, endTime, physical, human)
            } yield taskSchedule
            
            (taskSchedule :: list, finalTime)
          }
        }
        orderToTaskSchedule(productNumber + 1, 
          orderId, 
          quantity, 
          product, 
          physicalResources, 
          humanResources, 
          taskSchedule._2, 
          taskSchedule._1 ::: listTaskSchedule)


    orderToTaskSchedule(1,
      order.id,  
      order.quantity.to, 
      order.product, 
      production.physicalResources, 
      production.humanResources,
      startTime,
      List())
  
  /**
   * Finds all Physical Resources inherent to Task 
   * 
   * @param allPhysicalResources
   * @param taskResourceTypes
   * @return List[PhysicalResource]
   */
  private def findPhysicalResources(allPhysicalResources: List[PhysicalResource], 
                                    taskId: String, 
                                    taskResourceTypes: List[ResourceType]): List[Result[PhysicalResource]] =

    taskResourceTypes.foldLeft[(List[Result[PhysicalResource]], List[PhysicalResource])](List(), allPhysicalResources){
      case ((list, allPhysicalAvailable), resource) => {
        val physical: Result[PhysicalResource] = findPhysical(allPhysicalAvailable, taskId, resource)
        
        physical match
          case Right(p) => {
            ((physical :: list), allPhysicalAvailable.filterNot(_.physicalId == p.physicalId))
          }
          case Left(physicalError) => {
            ((physical :: List()), allPhysicalAvailable)
          }
      }
    }._1.reverse

  /**
   * Finds the Physical Resource by needed Task ResourceType* 
   * @param allPhysicalResources
   * @param taskId
   * @param resourceType
   * @return PhysicalResource
   */
  private def findPhysical(allPhysicalResources: List[PhysicalResource], 
                           taskId: String, 
                           resourceType: ResourceType): Result[PhysicalResource] =
    allPhysicalResources match
      case Nil => {
        Left(ResourceUnavailable(taskId + "," + resourceType))
      }
      case element::tail => {
        if(element.resourceType == resourceType) Right(element) 
        else findPhysical(tail, taskId, resourceType)
      }

  /**
   * Finds all Human Resources inherent to Task 
   * 
   * @param humanResources
   * @param taskId
   * @param resourceTypes
   * @return List[HumanResource]
   */
  private def findHumanResources(allHumanResources: List[HumanResource], 
                                 taskId: String, 
                                 taskResourceTypes: List[ResourceType]): List[Result[HumanResource]] =
  
    taskResourceTypes.foldLeft[(List[Result[HumanResource]], List[HumanResource])](List(), allHumanResources){
      case ((listResult, allHumanAvailable), resource) => {
        val human: Result[HumanResource] = findHuman(allHumanAvailable, taskId, resource)
        human match
          case Right(h) => {
            ((human :: listResult), allHumanAvailable.filterNot(_.humanId == h.humanId))
          }
          case Left(humanError) => {
            ((human :: List()), allHumanAvailable)
          }
      }
    }._1.reverse

  /**
   * Finds the Human Resource by needed Task ResourceType
   * 
   * @param allHumanResources
   * @param taskId
   * @param resource
   * @return HumanResource
   */
  private def findHuman(allHumanResources: List[HumanResource], taskId: String, resourceType: ResourceType): Result[HumanResource] = {
    allHumanResources match
      case Nil => {
        Left(ResourceUnavailable(taskId + "," + resourceType))
      }
      case element :: tail => {
        if(element.resourceTypes.exists(_ == resourceType)) Right(element)
        else findHuman(tail, taskId, resourceType)
      }
  }