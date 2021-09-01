package domain.schedule.secundaryscheduler

import domain.{DomainError, HumanResource, Order, PhysicalResource, Production, ResourceType, Result, Task, TaskSchedule}
import domain.DomainError.ResourceUnavailable
import domain.SimpleTypes.{EndTime, OrderId, ProductNumber, StartTime, TaskId}
import domain.schedule.TScheduler
import models.{ResourceStateEnum, TaskStateModelList}
import utils.Helper
import scala.annotation.tailrec

object MS03SchedulerV2 extends TScheduler:

  /**
   * Milestone 3 (MS03) Scheduling Algorithm
   * @param production Production
   *  @return Result[List[TaskSchedule]]
   */
  override def scheduleProduction(production: Production): Result[List[TaskSchedule]] =
    val orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)] = TaskStateModelList.getAllOrdersTasks(production.orders)

    val physicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)]
    = production.physicalResources.map[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)]((_, ResourceStateEnum.Ready))

    val humanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)]
    = production.humanResources.map[(HumanResource, ResourceStateEnum.ResourceStateEnum)]((_, ResourceStateEnum.Ready))

    Right(orderToTaskSchedule(orderInfo, physicalResources, humanResources))


  /**
   *
   * @param orderInfo List[(OrderId, ProductNumber, TaskStateModelList)]
   * @return List[TasksSchedule]
   */
  private def orderToTaskSchedule(orderInfo1: List[(OrderId, ProductNumber, TaskStateModelList)],
                                  physicalResources1: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)],
                                  humanResources1: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)]): List[TaskSchedule] =
    @tailrec
    def orderToTaskSchedule(orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)],
                            physicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)],
                            humanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)],
                            currentTime: Int,
                            resultTaskScheduleList: List[TaskSchedule]): List[TaskSchedule] =

      if(TaskStateModelList.allTasksProcessed(orderInfo))
        resultTaskScheduleList
      else
        //Changes Tasks state
        val orderInfoUpdated: List[(OrderId, ProductNumber, TaskStateModelList)]
        = TaskStateModelList.setTaskState(orderInfo, resultTaskScheduleList, currentTime)

        //Get tasks to process
        val tasksToProcess: List[(OrderId, ProductNumber, Task)]
        = TaskStateModelList.getOrdersTasksToProcess(orderInfoUpdated)

        if(tasksToProcess.isEmpty)
          //No tasks to process at this time, so go to next one
          val nextCurrentTime = calculateNextCurrentTime(resultTaskScheduleList, currentTime)

          orderToTaskSchedule(orderInfoUpdated,
            physicalResources,
            humanResources,
            nextCurrentTime,
            resultTaskScheduleList)
        else
          //Get resources ready
          val physicalResourcesReady: List[PhysicalResource]
          = getPhysicalResourceReady(orderInfoUpdated, resultTaskScheduleList, physicalResources.map(_._1))
          val humanResourcesReady: List[HumanResource]
          = getHumanResourceReady(orderInfoUpdated, resultTaskScheduleList, humanResources.map(_._1))

          if (physicalResourcesReady.isEmpty || humanResourcesReady.isEmpty)
            // TODO: Provavelmente nunca passa aqui porque j? libertei resources

            val tasksFinished = resultTaskScheduleList.filter(_.end.to == currentTime)

            val newPhysicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] = setPhysicalResourceState(physicalResources, resultTaskScheduleList, currentTime)

            val newHumanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] = setHumanResourceState(humanResources, resultTaskScheduleList, currentTime)

            orderToTaskSchedule(orderInfoUpdated,
              newPhysicalResources,
              newHumanResources,
              currentTime,
              resultTaskScheduleList)

          else
            //orders already initiated (task in Process or Processed)
            val orderAlreadyInitiated: List[(OrderId, ProductNumber)]
            = (TaskStateModelList.getOrdersTasksInProcess(orderInfoUpdated) ::: TaskStateModelList.getOrdersTasksProcessed(orderInfoUpdated)).map((orderId, productNumber, _) => (orderId, productNumber))

            val tasksInitiated: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]
            = assignResourcesToTasks(orderAlreadyInitiated, tasksToProcess, humanResourcesReady, physicalResourcesReady)

            if (!tasksInitiated.isEmpty)
              val newTaskScheduleInitiatedList: List[TaskSchedule]
              = createTasksSchedules(tasksInitiated, currentTime)

              val newResultTaskScheduleList: List[TaskSchedule]
              = resultTaskScheduleList ::: newTaskScheduleInitiatedList

              val newOrderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]
              = TaskStateModelList.setTaskState(orderInfoUpdated, newResultTaskScheduleList, currentTime)

              val newPhysicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)]
              = setPhysicalResourceState(physicalResources, newTaskScheduleInitiatedList, currentTime)

              val newHumanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)]
              = setHumanResourceState(humanResources, newTaskScheduleInitiatedList, currentTime)

              val nextCurrentTime: Int = calculateNextCurrentTime(newResultTaskScheduleList, currentTime)

              orderToTaskSchedule(newOrderInfo, newPhysicalResources, newHumanResources, nextCurrentTime, newResultTaskScheduleList)
            else
              // Liberta resources e avan?a no tempo
              val newOrderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]
              = TaskStateModelList.setTaskState(orderInfoUpdated, resultTaskScheduleList, currentTime)

              val tasksFinished = resultTaskScheduleList.filter(_.end.to == currentTime)

              val newPhysicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] = setPhysicalResourceState(physicalResources, resultTaskScheduleList, currentTime)

              val newHumanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] = setHumanResourceState(humanResources, resultTaskScheduleList, currentTime)

              val nextCurrentTime = calculateNextCurrentTime(resultTaskScheduleList, currentTime)

              orderToTaskSchedule(newOrderInfo,
                newPhysicalResources,
                newHumanResources,
                nextCurrentTime,
                resultTaskScheduleList)

    orderToTaskSchedule(orderInfo1, physicalResources1, humanResources1, 0, List())


  // TODO: Remove toOption and get() -> Result
  def createTasksSchedules(tasksInitiated: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])],
                           time: Int): List[TaskSchedule] =
    tasksInitiated.map((orderId, productNumber, task, humanResource, physicalResource) =>
      val ts: Result[TaskSchedule] = for {
        sTime: StartTime <- StartTime.from(time)
        endTimeVal: Int = task.time.to + time
        eTime: EndTime <- EndTime.from(endTimeVal)
      } yield TaskSchedule.from(productNumber, task.taskId, sTime, eTime,orderId, physicalResource, humanResource)
  
      ts.toOption.get
  )

  /**
   * Calculates the new current time
   * @param currentTime Int
   * @return End Time of the TaskSchedule whose value is the min
   */
  def calculateNextCurrentTime(taskSchedules: List[TaskSchedule],
                               currentTime: Int): Int =
    //TODO: Test with tasksSchedulesInProcess empty
    val tasksSchedulesInProcess: List[TaskSchedule] = taskSchedules.filter(taskSchedule => taskSchedule.end.to > currentTime)
    if(!tasksSchedulesInProcess.isEmpty)
      val minTaskSchedule: TaskSchedule = tasksSchedulesInProcess.minBy(_.end.to)
      minTaskSchedule.end.to
    else currentTime

  /**
   * Assigns to each Task the needed Physical Resource.
   * @param tasks List[(OrderId, ProductNumber, Task, List[HumanResource])]
   * @param listPhysical List[PhysicalResource]
   * @return List[(OrderId, ProductNumber, TaskId, List[HumanResource], List[PhysicalResource])]
   */
//  private def assignPhysicalResourceToTask(tasks: List[(OrderId, ProductNumber, Task, List[HumanResource])],
//                                           listPhysical: List[PhysicalResource]): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] =
//
//    tasks.foldLeft[(List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])], List[PhysicalResource])](List(), listPhysical) {
//      case((result, availablePhysicals), task) => {
//        val prList: List[PhysicalResource] = findPhysicalResources(availablePhysicals, task._3.taskId.to, task._3.resourceTypes)
//        if(prList.isEmpty)
//          (result, availablePhysicals)
//        else
//          val newAvailablePhysicals = availablePhysicals diff prList
//          ((task._1, task._2, task._3, task._4, prList) :: result, newAvailablePhysicals)
//      }
//    }._1
  private def findPhysicalResourcesPerTask(tasks: List[(OrderId, ProductNumber, Task, List[HumanResource])],
                                           listPhysical: List[PhysicalResource]): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] =
    @tailrec
    def findPhysicalResourcesRec(numTries: Int,
                                 taskIndex: Int,
                                 availablePhysicals: List[PhysicalResource],
                                 result: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] =

      if(taskIndex == tasks.size && result.size != tasks.size && numTries != listPhysical.size)
        val newAvailablePhysicals = listPhysical.drop(1) ::: listPhysical.take(1)
        findPhysicalResourcesRec(numTries + 1, 0, newAvailablePhysicals, List())
      else if(taskIndex == tasks.size || numTries == listPhysical.size)
        result
      else
        val task: Task = tasks.map(_._3)(taskIndex)
        val prList: List[PhysicalResource] = findPhysicalResources(availablePhysicals, task.taskId.to, task.resourceTypes)

        if(prList.isEmpty || prList.size != task.resourceTypes.size)
          val newAvailablePhysicals = listPhysical.drop(1) ::: listPhysical.take(1)
          findPhysicalResourcesRec(numTries + 1, 0, newAvailablePhysicals, List())
        else
          val orderId: OrderId = tasks.map(_._1)(taskIndex)
          val productNumber: ProductNumber = tasks.map(_._2)(taskIndex)
          val humans: List[HumanResource] = tasks.map(_._4)(taskIndex)
          val newAvailablePhysicals = availablePhysicals diff prList
          findPhysicalResourcesRec(numTries, taskIndex + 1, newAvailablePhysicals, result ::: List((orderId, productNumber, task, humans, prList)))

    findPhysicalResourcesRec(0, 0, listPhysical, List())



  private def findPhysicalResources(allPhysicalResources: List[PhysicalResource],
                                    taskId: String,
                                    taskResourceTypes: List[ResourceType]): List[PhysicalResource] =

    taskResourceTypes.foldLeft[(List[PhysicalResource], List[PhysicalResource])](List(), allPhysicalResources){
      case ((list, allPhysicalAvailable), resource) => {
        val physical: Result[PhysicalResource] = findPhysical(allPhysicalAvailable, taskId, resource)

        physical match
          case Right(p) => {
            ((p :: list), allPhysicalAvailable.filterNot(_.physicalId == p.physicalId))
          }
          case Left(physicalError) => {
            ((List()), allPhysicalAvailable)
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

//  def assignResourcesToTasks(ordersInitiated: List[(OrderId, ProductNumber)],
//                             tasksList: List[(OrderId, ProductNumber, Task)],
//                             listHumans: List[HumanResource],
//                             listPhysicals: List[PhysicalResource]): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] =
//
//    val taskPermutations: List[List[(OrderId, ProductNumber, Task)]]
//    = tasksList.permutations.toList
//
//    @tailrec
//    def assignResourcesToTasksRec(result: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])],
//                                  index: Int): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] =
//
//      if(result.size == tasksList.size || index == taskPermutations.size)
//        result.sortBy(_._3.taskId.to).sortBy(_._2.to).sortBy(_._1.to)
//      else
//        val tasks: List[(OrderId, ProductNumber, Task)] = taskPermutations(index)
//
//        val humansAssigned: List[(OrderId, ProductNumber, Task, List[HumanResource])] = findHumanResourcesPerTask(tasks, listHumans).filter(_._4.size != 0)/*.sortBy(_._3.taskId.to).sortBy(_._2.to).sortBy(_._1.to)*/
//        val physicalsAssigned: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = assignPhysicalResourceToTask(humansAssigned/*.sortBy(_._3.taskId.to).sortBy(_._2.to).sortBy(_._1.to)*/, listPhysicals)
//        val possibleSolution: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = physicalsAssigned
//
//        if(isBetterSolution(ordersInitiated, result, possibleSolution))
//          assignResourcesToTasksRec(possibleSolution, index + 1)
//        else
//          assignResourcesToTasksRec(result, index + 1)
//
//    assignResourcesToTasksRec(List(), 0)


  def assignResourcesToTasks(ordersInitiated: List[(OrderId, ProductNumber)],
                             tasksList: List[(OrderId, ProductNumber, Task)],
                             listHumans: List[HumanResource],
                             listPhysicals: List[PhysicalResource]): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] =

    val taskCombinations: List[List[(OrderId, ProductNumber, Task)]] = tasksList.toSet[(OrderId, ProductNumber, Task)].subsets.map(_.toList).toList.filter(_.size > 0)
    @tailrec
    def assignResourcesToTasksRec(result: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])],
                                  index: Int): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] =

      if(result.size == tasksList.size || index == taskCombinations.size)
        result.sortBy(_._3.taskId.to).sortBy(_._2.to).sortBy(_._1.to)
      else
        val tasks: List[(OrderId, ProductNumber, Task)] = taskCombinations(index)

        val humansAssigned: List[(OrderId, ProductNumber, Task, List[HumanResource])] = findHumanResourcesPerTask(tasks, listHumans).filter(_._4.size != 0)/*.sortBy(_._3.taskId.to).sortBy(_._2.to).sortBy(_._1.to)*/
        val physicalsAssigned: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = findPhysicalResourcesPerTask(humansAssigned/*.sortBy(_._3.taskId.to).sortBy(_._2.to).sortBy(_._1.to)*/, listPhysicals)
        val possibleSolution: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = physicalsAssigned

        if(isBetterSolution(ordersInitiated, result, possibleSolution))
          assignResourcesToTasksRec(possibleSolution, index + 1)
        else
          assignResourcesToTasksRec(result, index + 1)

    assignResourcesToTasksRec(List(), 0)



  def isBetterSolution(ordersInitiated: List[(OrderId, ProductNumber)],
                       actualSolution: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])],
                       newSolution: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]): Boolean =

  // TODO: Improve this (duplicated code)
    if(actualSolution.isEmpty) true
    else if(newSolution.size > actualSolution.size) true
    else if(newSolution.size < actualSolution.size) false
    // if are the same solution
    else if(actualSolution.equals(newSolution))
      false
    else if(ordersInitiated.isEmpty)

      //if have the same orderids
      if(actualSolution.map(_._1.to).sorted.equals(newSolution.map(_._1.to).sorted))

      //if have the same product number
        if(actualSolution.map(_._2.to).sorted.equals(newSolution.map(_._2.to).sorted))
          if(Helper.getListContainsSmallerElements(actualSolution.map(_._2), newSolution.map(_._2)).map(_.to).equals(actualSolution.map(_._2.to))) false
          // if have the same human resources
          else if(actualSolution.flatMap(_._4).map(_.humanId.to).sorted.equals(newSolution.flatMap(_._4).map(_.humanId.to).sorted))
            if(Helper.getListContainsSmallerElements(actualSolution.flatMap(_._4), newSolution.flatMap(_._4)).equals(actualSolution.flatMap(_._4))) false
            // if have same physical resources
            else if(actualSolution.flatMap(_._5).map(_.physicalId.to).sorted.equals(newSolution.flatMap(_._5).map(_.physicalId.to).sorted))
              if(Helper.getListContainsSmallerElements(actualSolution.flatMap(_._5), newSolution.flatMap(_._5)).equals(actualSolution.flatMap(_._5))) false
              else true
            else false
          else false
        else false
      //if have different orderids then validate them
      else if(Helper.getListContainsSmallerElements(actualSolution.map(_._1), newSolution.map(_._1)).equals(actualSolution.map(_._1))) false
      else true
    else
      val newSolutionContainsOrdersInitiated: Boolean = newSolution.map((order, productNumber, _, _, _) => (order, productNumber)).exists(ordersInitiated.contains)
      val actualSolutionContainsOrdersInitiated: Boolean = actualSolution.map((order, productNumber, _, _, _) => (order, productNumber)).exists(ordersInitiated.contains)
      val newSolutionContainsAllOrdersInitiated: Boolean = newSolution.map((order, productNumber, _, _, _) => (order, productNumber)).forall(ordersInitiated.contains)
      val actualSolutionContainsAllOrdersInitiated: Boolean = actualSolution.map((order, productNumber, _, _, _) => (order, productNumber)).forall(ordersInitiated.contains)

      //New solution cantains all order initiated
      if(newSolutionContainsAllOrdersInitiated && !actualSolutionContainsAllOrdersInitiated)
        true
      // New solution does not contains any order initiated but actual solution does
      else if(!newSolutionContainsOrdersInitiated && actualSolutionContainsOrdersInitiated)
        false
      // New solution does contains some orders initiated but actual solution does not
      else if(newSolutionContainsOrdersInitiated && !actualSolutionContainsOrdersInitiated)
        true
      // New solution does contains some orders initiated but actual solution does as well
      else
      //if have the same orderids
        if(actualSolution.map(_._1.to).sorted.equals(newSolution.map(_._1.to).sorted))
        //if have the same product number
          if(actualSolution.map(_._2.to).sorted.equals(newSolution.map(_._2.to).sorted))
            if(Helper.getListContainsSmallerElements(actualSolution.map(_._2), newSolution.map(_._2)).map(_.to).equals(actualSolution.map(_._2.to))) false
            // if have the same human resources
            else if(actualSolution.flatMap(_._4).map(_.humanId.to).sorted.equals(newSolution.flatMap(_._4).map(_.humanId.to).sorted))
              if(Helper.getListContainsSmallerElements(actualSolution.flatMap(_._4), newSolution.flatMap(_._4)).equals(actualSolution.flatMap(_._4))) false
              // if have same physical resources
              else if(actualSolution.flatMap(_._5).map(_.physicalId.to).sorted.equals(newSolution.flatMap(_._5).map(_.physicalId.to).sorted))
                if(Helper.getListContainsSmallerElements(actualSolution.flatMap(_._5), newSolution.flatMap(_._5)).equals(actualSolution.flatMap(_._5))) false
                else true
              else false
            else false
          else false
        //if have different orderids then validate them
        else if(Helper.getListContainsSmallerElements(actualSolution.map(_._1), newSolution.map(_._1)).equals(actualSolution.map(_._1))) false
        else false


  /**
   * Finds the corresponding Human Resource for each Task.
   * (Here the tasks should not have the same Human Resource)
   * @param tasks List[(OrderId, ProductNumber, Task)]
   * @param listHumans List[HumanResource]
   * @return List[((OrderId, ProductNumber, Task, List[HumanResource])]
   */
  private def findHumanResourcesPerTask(tasks: List[(OrderId, ProductNumber, Task)],
                                        listHumans: List[HumanResource]): List[(OrderId, ProductNumber, Task, List[HumanResource])] =
    @tailrec
    def findHumanResourcesRec(numTries: Int,
                              taskIndex: Int,
                              humansAvailability: List[HumanResource],
                              result: List[(OrderId, ProductNumber, Task, List[HumanResource])]): List[(OrderId, ProductNumber, Task, List[HumanResource])] =

      val numTries2 = numTries
      val taskIndex2 = taskIndex
      val humansAvailability2 = humansAvailability
      val result2 = result

      if(taskIndex == tasks.size && result.size != tasks.size && numTries != listHumans.size)
        val newHumansAvailability = listHumans.drop(1) ::: listHumans.take(1)
        findHumanResourcesRec(numTries + 1, 0, newHumansAvailability, List())
      else if(taskIndex == tasks.size || numTries == listHumans.size)
        result
      else
        val orderId: OrderId = tasks.map(_._1)(taskIndex)
        val productNumber: ProductNumber = tasks.map(_._2)(taskIndex)
        val task: Task = tasks.map(_._3)(taskIndex)
        val listHumansOfTask: List[HumanResource] = findHumanResources(humansAvailability, task.taskId.to, task.resourceTypes)

  //        if(listHumansOfTask.isEmpty)
//          //Could not assign any humans to this task, it will never be possible
//          findHumanResourcesRec(humansAvailability.size, tasks.size, humansAvailability, result)
        /*else*/ if(listHumansOfTask.isEmpty || listHumansOfTask.size != task.resourceTypes.size)
          // Try again to assign an human but now the list of humansAvailability is different
//          val newHumansAvailability = humansAvailability.drop(1) ::: humansAvailability.take(1)
//          findHumanResourcesRec(numTries + 1, taskIndex, newHumansAvailability, result)
              val newHumansAvailability = listHumans.drop(numTries+1) ::: listHumans.take(numTries+1)
              findHumanResourcesRec(numTries + 1, 0, newHumansAvailability, List())
        else
          val newHumansAvailability = humansAvailability diff listHumansOfTask
          findHumanResourcesRec(numTries, taskIndex + 1, newHumansAvailability, result ::: List((orderId, productNumber, task, listHumansOfTask)))

    findHumanResourcesRec(0, 0, listHumans, List())


  private def findHumanResources(allHumanResources: List[HumanResource],
                                 taskId: String,
                                 taskResourceTypes: List[ResourceType]): List[HumanResource] =

    taskResourceTypes.foldLeft[(List[HumanResource], List[HumanResource])](List(), allHumanResources){
      case ((listResult, allHumanAvailable), resource) => {
        val human: Result[HumanResource] = findHuman(allHumanAvailable, taskId, resource)
        human match
          case Right(h) => {
            ((h :: listResult), allHumanAvailable.filterNot(_.humanId == h.humanId))
          }
          case Left(humanError) => {
            ((List()), allHumanAvailable)
          }
      }
    }._1.reverse


  // TODO: Maybe place findHuman and findHumanResource in a Util (maybe on ShedulerResolver) since its used by both schedulers
  private def findHuman(allHumanResources: List[HumanResource], taskId: String, resourceType: ResourceType): Result[HumanResource] =
    allHumanResources match
      case Nil => {
        Left(ResourceUnavailable(""))
      }
      case element :: tail => {
        if(element.resourceTypes.exists(_ == resourceType)) Right(element)
        else findHuman(tail, taskId, resourceType)
      }


  /**
   * Get list of HumanResource with state Ready
   * @param humansList List of HumanResources with current State
   * @return List of HumanResource
   */
  private def getReadyHumanResources(humansList: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)]): List[HumanResource] =
    humansList.filter(_._2 == ResourceStateEnum.Ready).map(_._1)


  /**
   * Change the State of HumanResource based on TaskSchedule time
   * @param humansList List of human resources
   * @param listTaskSchedule Atual list of task schedules
   * @param time Current time
   * @return List[(HumanResource, ResourceStateEnum.ResourceStateEnum)]
   */
  def setHumanResourceState(humansList: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)],
                            listTaskSchedule: List[TaskSchedule],
                            time: Int): List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] =

    setResourceState(humansList, listTaskSchedule, (taskSchedule) => taskSchedule.humanResources, time)

  /**
   * Get list of PhysicalResource with state Ready
   * @param physicalsList List of PhysicalResources with current State
   * @return List of PhysicalResource
   */
  private def getReadyPhysicalResources(physicalsList: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)]): List[PhysicalResource] =
    physicalsList.filter(_._2 == ResourceStateEnum.Ready).map(_._1)

  /**
   * Change the State of PhysicalResource based on TaskSchedule time
   * @param physicalsList List of physical resources
   * @param listTaskSchedule Atual list of task schedules
   * @param time Current time
   * @return List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)]
   */
  def setPhysicalResourceState(physicalsList: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)],
                               listTaskSchedule: List[TaskSchedule],
                               time: Int): List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] =
  // TODO: create test based valid_agenda_02 (same human- 1 finished 1 started)
    setResourceState(physicalsList, listTaskSchedule, (taskSchedule) => taskSchedule.physicalResources, time)

  /**
   * Generic function that changes the State of resources based on TaskSchedule time
   * @param resourcesList List of resources
   * @param listTaskSchedule Atual list of task schedules
   * @param taskScheduleResourceList Function that converts TaskSchedule into List[A]
   * @param time Current time
   * @tparam A can be PhysicalResource or HumanResource
   * @return List[(A, ResourceStateEnum.ResourceStateEnum)]
   */
  private def setResourceState[A](resourcesList: List[(A, ResourceStateEnum.ResourceStateEnum)],
                                  listTaskSchedule: List[TaskSchedule],
                                  taskScheduleResourceList: TaskSchedule => List[A],
                                  time: Int): List[(A, ResourceStateEnum.ResourceStateEnum)] =

    resourcesList.map[(A, ResourceStateEnum.ResourceStateEnum)](resourceState =>
      val listResourceFinded = listTaskSchedule.filter(schedule => taskScheduleResourceList(schedule).contains(resourceState._1))

      val result: (A, ResourceStateEnum.ResourceStateEnum) =
        if(listResourceFinded.isEmpty){
          resourceState
        } else {
          val taskSchedule: TaskSchedule = listResourceFinded.maxBy(_.end.to)
          if(taskSchedule.start.to == time){
            (resourceState._1, ResourceStateEnum.Occupied)
          } else if(taskSchedule.end.to == time){
            (resourceState._1, ResourceStateEnum.Ready)
          } else {
            resourceState
          }
        }
      result
  )

  private def setResourceState[A](listTaskSchedule: List[TaskSchedule],
                                  taskScheduleResourceList: TaskSchedule => List[A],
                                  resourceState: ResourceStateEnum.ResourceStateEnum): List[(A, ResourceStateEnum.ResourceStateEnum)] =
    listTaskSchedule.map(taskSchedule => taskScheduleResourceList(taskSchedule).map(resource => (resource, resourceState))).flatten

  /**
   * Gets the list of physical resources that are ready to be use, this is, the ones that are not being used by that tasks that are in process
   * @param orderInfo List[(OrderId, ProductNumber, TaskStateModelList)]
   * @param listTaskSchedule Atual list of task schedules
   * @param listPhysicals List of all physical resources
   * @return List[PhysicalResource]
   */
  def getPhysicalResourceReady(orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)],
                               listTaskSchedule: List[TaskSchedule],
                               listPhysicals: List[PhysicalResource]): List[PhysicalResource] =
    getResourcesReady(orderInfo, listTaskSchedule, listPhysicals, (taskSchedule) => taskSchedule.physicalResources)


  /**
   * Gets the list of human resources that are ready to be use, this is, the ones that are not being used by that tasks that are in process
   * @param orderInfo List[(OrderId, ProductNumber, TaskStateModelList)]
   * @param listTaskSchedule Atual list of task schedules
   * @param listHumans List of all human resources
   * @return List[HumanResource]
   */
  def getHumanResourceReady(orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)],
                            listTaskSchedule: List[TaskSchedule],
                            listHumans: List[HumanResource]): List[HumanResource] =
    getResourcesReady(orderInfo, listTaskSchedule, listHumans, (taskSchedule) => taskSchedule.humanResources)

  /**
   * Genetic funtion that returns the resource ready to be used
   * @param orderInfo List[(OrderId, ProductNumber, TaskStateModelList)]
   * @param listTaskSchedule Atual list of task schedules
   * @param listResources List of resources
   * @param taskScheduleResourceList Function that converts TaskSchedule into List[A]
   * @tparam A can be PhysicalResource or HumanResource
   * @return List[A]
   */
  def getResourcesReady[A](orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)],
                        listTaskSchedule: List[TaskSchedule],
                        listResources: List[A],
                        taskScheduleResourceList: TaskSchedule => List[A]): List[A] ={

    val tasksInProgress: List[(OrderId, ProductNumber, Task)] = TaskStateModelList.getOrdersTasksInProcess(orderInfo)

    val resourcesOccupied: List[A] = tasksInProgress.foldLeft[List[A]](List()){
      case(result, (orderId, productNumber, task)) =>
        val orderTaskSchedule: List[TaskSchedule] = listTaskSchedule.filter(taskSchesule => taskSchesule.order == orderId
          && taskSchesule.productNumber == productNumber
          && taskSchesule.taskId == task.taskId)
        if(orderTaskSchedule.isEmpty || orderTaskSchedule.size > 1)
        // This should never happen
          result
        else
          val taskSchedule: TaskSchedule = orderTaskSchedule.head
          taskScheduleResourceList(taskSchedule) ::: result
    }

    listResources diff resourcesOccupied
  }