package property

import domain.DomainError.ResourceUnavailable
import domain.SimpleTypes.{OrderId, ProductId, ProductNumber, Quantity, TaskId, Time}
import domain.schedule.{MS01Scheduler, TScheduler}
import domain.{HumanResource, Order, PhysicalResource, Production, ResourceType, Task, TaskSchedule}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.Test.Parameters

import scala.language.adhocExtensions

object ScheduleService extends Properties("ScheduleService") :

  private val scheduler: TScheduler = MS01Scheduler
  override def overrideParameters(prms: Parameters): Parameters =
    prms.withMinSuccessfulTests(1000)

  property("1.The same resource cannot be used at the same time by two tasks.") =
    forAll(Generators.productionGenerator)( (production: Production) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => false, listTaskSchedule => {
        listTaskSchedule.forall(ts1 => {
          val overlappings: List[TaskSchedule] =  listTaskSchedule.filter(ts2 => ts2 != ts1 && ts1.start.to < ts2.end.to && ts2.start.to < ts1.end.to)
          overlappings.forall( ts2 =>
            val hrlIntersect: List[HumanResource] = ts2.humanResources.toSeq intersect ts1.humanResources.toSeq
            val prIntersect: List[PhysicalResource] = ts2.physicalResources.toSeq intersect ts1.physicalResources.toSeq
            hrlIntersect.size == 0 && prIntersect.size == 0
          )
        })
      }
    ))

  property("2.The complete schedule must schedule all the tasks of all the *Products* needed.") =
    forAll(Generators.productionGenerator)( (production: Production) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => {
        false
      }, listTaskSchedule => {
        val allScheduleTasks: List[String] = listTaskSchedule.map[String](o => o.taskId.to).toList

        val allProductionTasksWithQuantity: List[(List[String], Int)] 
          = production.orders.map[(List[String], Int)](o => o.product.processes.map(_.taskId.to) -> o.quantity.to)
          
        val allProductionTasks: List[String] 
          = allProductionTasksWithQuantity.map[List[String]](x => x._1.flatMap{ List.fill(x._2) }).flatten

        allScheduleTasks.length == allProductionTasks.length && allScheduleTasks.sorted == allProductionTasks.sorted
      })
    )
  
  property("3. The number of TaskSchedules must be the same as the result of the multiplication between the Tasks and the quantity of the Product in an Order.") =
    forAll(Generators.productionGenerator)( (production: Production) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => false, listTaskSchedule => {
        val sum: Int = production.orders.map[Int](o => o.product.processes.length * o.quantity.to).sum
        sum == listTaskSchedule.length
      }
    ))

  property("4. The end time of the last TaskSchedule must be equal to the multiplication between the sum time of the Tasks and the quantity of the Product in each Order.") =
    forAll(Generators.productionGenerator)( (production: Production) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => {
        false
      }, listTaskSchedule => {
        val allTasksTime: Int = production.orders.map[Int](o => o.quantity.to * o.product.processes.map(_.time.to).sum).sum
        allTasksTime == listTaskSchedule.last.end.to
      })
    )
  
  property("5. The schedule should not be implemented when there are not enough resources (*PhysicalResource* and *HumanResource*) to process the Task Schedule. - Physical Resource") =
    forAll(Generators.productionGeneratorPhysicalResourceUnavailable)( (production: Production, taskId: TaskId, resourceType: ResourceType) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => error == ResourceUnavailable(s"${taskId},${resourceType}"), listTaskSchedule => false
    ))

  property("5. The schedule should not be implemented when there are not enough resources (*PhysicalResource* and *HumanResource*) to process the Task Schedule. - Human Resource") =
    forAll(Generators.productionGeneratorHumanResourceUnavailable)( (production: Production, taskId: TaskId, resourceType: ResourceType) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => error == ResourceUnavailable(s"${taskId},${resourceType}"), listTaskSchedule => false
      ))
  
  property("6. The sum of the TaskSchedule time of an order should be equal to sum of the times of the Task multiplied by the quantity.") =
    forAll(Generators.productionGenerator)( (production: Production) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => {
        false
      }, listTaskSchedule => {
        val taskScheduleOrderGroup: List[(OrderId, Int)] = listTaskSchedule.groupBy(_.order).map[(OrderId, Int)](o => o._1 -> (o._2.last.end.to - o._2.head.start.to)).toList
        val productionOrderGroup: List[(OrderId, Int)] = production.orders.map[(OrderId, Int)](o => o.id -> o.product.processes.map(_.time.to).sum * o.quantity.to)

        taskScheduleOrderGroup.size == productionOrderGroup.size
          && taskScheduleOrderGroup.sortBy(_._1.to) == productionOrderGroup.sortBy(_._1.to)
      })
    )
  
  property("7. The TaskSchedules can't contain PhysicalResource that is not valid.") =
    forAll(Generators.productionGenerator)( (production: Production) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => false, listTaskSchedule => {
        listTaskSchedule.forall(ts1 => {
          val prIntersect: List[PhysicalResource] = ts1.physicalResources.toSeq intersect production.physicalResources
          prIntersect.size == ts1.physicalResources.size
        })
      }
      ))

  property("8. The TaskSchedules can't contain HumanResource that is not valid.") =
    forAll(Generators.productionGenerator)( (production: Production) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => {
        false
      }, listTaskSchedule => {
        listTaskSchedule.forall(ts => {
          val hrIntersect: List[HumanResource] = ts.humanResources.toSeq intersect production.humanResources
          hrIntersect.size == ts.humanResources.size
        })
      })
    )
  
  property("9. The highest *productNumber* for each *Order* should be equal to the *quantity* to that *Order*") =
    forAll(Generators.productionGenerator)( (production: Production) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => false, listTaskSchedule => {

        val orderIdProductNumber: Map[OrderId, ProductNumber] = listTaskSchedule.foldLeft[Map[OrderId, ProductNumber]](Map()){
          case(map, taskSchedule) => {
            if(!map.contains(taskSchedule.order)){
              map + (taskSchedule.order -> taskSchedule.productNumber)
            } else if(map(taskSchedule.order).to < taskSchedule.productNumber.to) {
              map + (taskSchedule.order -> taskSchedule.productNumber)
            } else {
              map
            }
          }
        }
        production.orders.forall((order: Order) => order.quantity.to == orderIdProductNumber(order.id).to)
      })
    )

  property("10. For a given *TaskSchedule*, the list of *PhysicalResource* can't have repeated elements") =
    forAll(Generators.productionGenerator)( (production: Production) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => false, listTaskSchedule => {
        listTaskSchedule.forall((taskSchedule: TaskSchedule) =>
          taskSchedule.physicalResources.distinct.length == taskSchedule.physicalResources.length
        )
      })
    )

  property("11. For a given *TaskSchedule*, the list of *HumanResource* can't have repeated elements") =
    forAll(Generators.productionGenerator)( (production: Production) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => false, listTaskSchedule => {
        listTaskSchedule.forall((taskSchedule: TaskSchedule) =>
          taskSchedule.humanResources.distinct.length == taskSchedule.humanResources.length)
      })
    )

  property("12. For a given *TaskSchedule*, the number of *PhysicalResources* should be equal to the number of *HumanResources*") =
    forAll(Generators.productionGenerator)( (production: Production) =>
      scheduler.scheduleProduction(production).fold[Boolean](error => {
        false
      }, listTaskSchedule => {
        listTaskSchedule.forall((taskSchedule: TaskSchedule) =>
          taskSchedule.humanResources.length == taskSchedule.physicalResources.length)
      })
    )
