package models

import domain.SimpleTypes.{OrderId, ProductNumber}
import domain.{Order, Result, Task, TaskSchedule}

import scala.annotation.tailrec

final case class TaskStateModelList(list: List[TaskStateModel])

final private case class TaskStateModel(task: Task, state: TaskStateEnum.TaskStateEnum)

object TaskStateModelList:

  /**
   * Maps each Order to a tuple with the OrderId, ProductNumber (dependes on Quantity), and list of Task with the corresponding State
   * @param listOrders List[Order]
   * @return List[(OrderId, ProductNumber, TaskStateModelList)]
   */
  def getAllOrdersTasks(listOrders: List[Order]): List[(OrderId, ProductNumber, TaskStateModelList)] =
    listOrders.map[List[(OrderId, ProductNumber, TaskStateModelList)]](order => getOrderTasksByQuantity(order)).flatten.sortBy(_._1.to)

  /**
   * Recursive method to replicate tasks of Order by ProductNumber
   * @param order order from production
   * @return List[(OrderId, ProductNumber, TaskStateModelList)]
   */
  private def getOrderTasksByQuantity(order: Order): List[(OrderId, ProductNumber, TaskStateModelList)] =
    @tailrec
    def getOrderTasksByQuantityRec(productNumber: Int,
                                   result: List[(OrderId, ProductNumber, TaskStateModelList)]): List[(OrderId, ProductNumber, TaskStateModelList)] =

      if(productNumber > order.quantity.to)
        result.sortBy(_._2.to)
      else
        val number: Result[ProductNumber] = ProductNumber.from(productNumber)
        number match
          case Right(n) => {
            val orderNumberTasks: (OrderId, ProductNumber, TaskStateModelList) =
              (order.id, n, TaskStateModelList(order.product.processes.map[TaskStateModel](task => TaskStateModel(task, TaskStateEnum.NotStarted))))

            getOrderTasksByQuantityRec(productNumber + 1, orderNumberTasks :: result)
          }
          case Left(error) => {
            getOrderTasksByQuantityRec(productNumber, result)
          }

    getOrderTasksByQuantityRec(1, List())

  /**
   * Verifies if all tasks are already processed
   * @param orderInfo List[(OrderId, ProductNumber, TaskStateModelList)]
   * @return Boolean (true if all tasks processes, otherwise false)
   */
  def allTasksProcessed(orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]): Boolean =
    orderInfo.flatMap(_._3.list).filterNot(_.state == TaskStateEnum.Processed).size == 0

  /**
   * Returns the Tasks that can be processed
   * @param orderInfo List[(OrderId, ProductNumber, TaskStateModelList)]
   * @return List[(OrderId, ProductNumber, Task)]
   */
  def getOrdersTasksToProcess(orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]): List[(OrderId, ProductNumber, Task)] =
    val orderInfoWithoutTasksInProcess: List[(OrderId, ProductNumber, TaskStateModelList)] = orderInfo.filterNot(_._3.list.map(_.state).contains(TaskStateEnum.InProcess))
    orderInfoWithoutTasksInProcess.foldLeft[List[(OrderId, ProductNumber, Task)]](List()){
      case(result, (orderId, productNumber, taskStateModelList)) => {
        val taskStateModelWithTasksNotStarted: List[TaskStateModel] = taskStateModelList.list.filter(_.state == TaskStateEnum.NotStarted)
        if(taskStateModelWithTasksNotStarted.isEmpty)
          result
        else
          result ::: List((orderId, productNumber, taskStateModelWithTasksNotStarted.head.task))
      }
    }.sortBy(_._2.to).sortBy(_._1.to)

  /**
   * Returns the Tasks that are in process
   * @param orderInfo List[(OrderId, ProductNumber, TaskStateModelList)]
   * @return List[(OrderId, ProductNumber, Task)]
   */
  def getOrdersTasksInProcess(orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]): List[(OrderId, ProductNumber, Task)] =
    getOrdersTasksByState(orderInfo, TaskStateEnum.InProcess)

  /**
   * Returns the Tasks that are processed
   * @param orderInfo List[(OrderId, ProductNumber, TaskStateModelList)]
   * @return List[(OrderId, ProductNumber, Task)]
   */
  def getOrdersTasksProcessed(orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]): List[(OrderId, ProductNumber, Task)] =
    getOrdersTasksByState(orderInfo, TaskStateEnum.Processed)

  /**
   * Returns the Tasks whose State is the one received
   * @param orderInfo List[(OrderId, ProductNumber, TaskStateModelList)]
   * @param state TaskStateEnum
   * @return List[(OrderId, ProductNumber, Task)]
   */
  private def getOrdersTasksByState(orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)],
                                    state: TaskStateEnum.TaskStateEnum): List[(OrderId, ProductNumber, Task)] =
    orderInfo.foldLeft[List[(OrderId, ProductNumber, Task)]](List()){
      case(result, (orderId, productNumber, taskStateModelList)) => {
        val tasksNotStarted = taskStateModelList.list.filter(_.state == state)
        if(tasksNotStarted.isEmpty)
          result
        else
          result ::: List((orderId, productNumber, tasksNotStarted.head.task))
      }
    }

  /**
   * Changes the State of the Tasks (the ones that iniciated and the ones that finished)
   * @param orderInfo List[(OrderId, ProductNumber, TaskStateModelList)]
   * @param time Int
   * @param listTaskSchedule List[TaskSchedule]
   * @return List[(OrderId, ProductNumber, TaskStateModelList)]
   */
  def setTaskState(orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)],
                   listTaskSchedule: List[TaskSchedule],
                   time: Int): List[(OrderId, ProductNumber, TaskStateModelList)] =
    if(listTaskSchedule.isEmpty)
      orderInfo
    else
      orderInfo.map[(OrderId, ProductNumber, TaskStateModelList)](orderInfo =>

        val listTaskStateModel: List[TaskStateModel] = orderInfo._3.list.foldRight[List[TaskStateModel]](List()) {
          case (taskStateModel, result) => {
            val task: Task = taskStateModel.task
            val listTaskScheduleFiltered: List[TaskSchedule] = listTaskSchedule.filter(taskSchedule => taskSchedule.order == orderInfo._1
              && taskSchedule.productNumber == orderInfo._2
              && taskSchedule.taskId == task.taskId)
  
            if (listTaskScheduleFiltered.isEmpty) {
              taskStateModel :: result
            } else {
              val taskSchedule: TaskSchedule = listTaskScheduleFiltered.head
              if (taskSchedule.start.to == time) {
                TaskStateModel(task, TaskStateEnum.InProcess) :: result
              } else if(taskSchedule.end.to == time) {
                TaskStateModel(task, TaskStateEnum.Processed) :: result
              } else {
                taskStateModel :: result
              }
            }
          }
        }

        val result: TaskStateModelList = TaskStateModelList(listTaskStateModel)

        (orderInfo._1, orderInfo._2, result)
    )
