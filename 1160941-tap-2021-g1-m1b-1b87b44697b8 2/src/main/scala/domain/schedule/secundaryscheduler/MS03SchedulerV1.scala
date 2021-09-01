package domain.schedule.secundaryscheduler

import domain.{DomainError, HumanResource, Order, PhysicalResource, Production, ResourceType, Result, Task, TaskSchedule}
import domain.DomainError.ResourceUnavailable
import domain.SimpleTypes.{EndTime, OrderId, ProductNumber, StartTime, TaskId}
import domain.schedule.TScheduler
import models.{ResourceStateEnum, TaskStateModelList}
import scala.annotation.tailrec

object MS03SchedulerV1 extends TScheduler:

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
      val tasksToProcess: List[(OrderId, ProductNumber, Task)] = TaskStateModelList.getOrdersTasksToProcess(orderInfo)

      println(s"\n\norderInfo - ${orderInfo}")
      println(s"physicalResources - ${physicalResources}")
      println(s"humanResources - ${humanResources}")
      println(s"currentTime - ${currentTime}")
      println(s"resultTaskScheduleList - ${resultTaskScheduleList}")
      if (TaskStateModelList.allTasksProcessed(orderInfo))
        resultTaskScheduleList
      else
        if(tasksToProcess.isEmpty)
          val newOrderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]
          = TaskStateModelList.setTaskState(orderInfo, resultTaskScheduleList, currentTime)
          val nextCurrentTime = calculateNextCurrentTime(resultTaskScheduleList, currentTime)
          orderToTaskSchedule(newOrderInfo,
            physicalResources,
            humanResources,
            nextCurrentTime,
            resultTaskScheduleList)
        else
          //        val physicalResourcesReady: List[PhysicalResource] = getReadyPhysicalResources(physicalResources)
          //        val humanResourcesReady: List[HumanResource] = getReadyHumanResources(humanResources)
          val tasksInProgress: List[(OrderId, ProductNumber, Task)] = TaskStateModelList.getOrdersTasksInProcess(orderInfo)
          val resourcesTypesOccupied: List[ResourceType] = tasksInProgress.flatMap(_._3.resourceTypes).distinctBy(_.to)

          val physicalResourcesReady: List[PhysicalResource] = physicalResources.map(_._1).filterNot(resource => resourcesTypesOccupied.contains(resource.resourceType))
          val humanResourcesReady: List[HumanResource] = humanResources.map(_._1).filterNot(resource => resourcesTypesOccupied.contains(resource.resourceTypes))

          if (physicalResourcesReady.isEmpty || humanResourcesReady.isEmpty)
            // Libertar resources
            val newOrderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]
            = TaskStateModelList.setTaskState(orderInfo, resultTaskScheduleList, currentTime)


            val tasksFinished = resultTaskScheduleList.filter(_.end.to == currentTime)

            val newPhysicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] = setPhysicalResourceState(physicalResources, resultTaskScheduleList, currentTime)
            //            val newPhysicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] =
            //              setResourceState(tasksFinished, (taskSchedule) => taskSchedule.physicalResources, ResourceStateEnum.Ready)

            val newHumanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] = setHumanResourceState(humanResources, resultTaskScheduleList, currentTime)
            //            val newHumanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] =
            //              setResourceState(tasksFinished, (taskSchedule) => taskSchedule.humanResources, ResourceStateEnum.Ready)

            //val nextCurrentTime = calculateNextCurrentTime(resultTaskScheduleList, currentTime)

            orderToTaskSchedule(newOrderInfo,
              newPhysicalResources,
              newHumanResources,
              currentTime,
              resultTaskScheduleList)

          else
            // Fazer assign de physical e humans
            val assignedPhysicalResource: List[(Task, List[PhysicalResource])] = assignPhysicalResourceToTask(tasksToProcess.map[Task](_._3), physicalResourcesReady)

            if (!assignedPhysicalResource.isEmpty)
              val assignedHumanResource: List[(Task, List[HumanResource])] = assignHumanResourceToTasks(tasksToProcess.map[Task](_._3), humanResourcesReady)

              if (!assignedHumanResource.isEmpty)
                val tasksInitiated: List[(OrderId, ProductNumber, Task)]
                = tasksToProcess.filter((_, _, task) => assignedHumanResource.map(_._1).contains(task) && assignedPhysicalResource.map(_._1).contains(task))

                val newTaskScheduleInitiatedList: List[TaskSchedule]
                = createTasksSchedules(assignedPhysicalResource, assignedHumanResource, tasksInitiated, currentTime)

                val newResultTaskScheduleList: List[TaskSchedule]
                = resultTaskScheduleList ::: newTaskScheduleInitiatedList

                val newOrderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]
                = TaskStateModelList.setTaskState(orderInfo, newResultTaskScheduleList, currentTime)

                val newPhysicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)]
                = setPhysicalResourceState(physicalResources, newTaskScheduleInitiatedList, currentTime)
                //            val newPhysicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)]
                //            = setResourceState(newTaskScheduleInitiatedList, (taskSchedule) => taskSchedule.physicalResources, ResourceStateEnum.Occupied)

                val newHumanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)]
                = setHumanResourceState(humanResources, newTaskScheduleInitiatedList, currentTime)
                //            val newHumanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)]
                //            = setResourceState(newTaskScheduleInitiatedList, (taskSchedule) => taskSchedule.humanResources, ResourceStateEnum.Occupied)

                val nextCurrentTime: Int = calculateNextCurrentTime(newResultTaskScheduleList, currentTime)

                orderToTaskSchedule(newOrderInfo, newPhysicalResources, newHumanResources, nextCurrentTime, newResultTaskScheduleList)
              else
                // Liberta resources e avan�a no tempo
                println("1 else")
                val newOrderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]
                = TaskStateModelList.setTaskState(orderInfo, resultTaskScheduleList, currentTime)

                val tasksFinished = resultTaskScheduleList.filter(_.end.to == currentTime)

                val newPhysicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] = setPhysicalResourceState(physicalResources, resultTaskScheduleList, currentTime)
                //            val newPhysicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] =
                //              setResourceState(tasksFinished, (taskSchedule) => taskSchedule.physicalResources, ResourceStateEnum.Ready)

                val newHumanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] = setHumanResourceState(humanResources, resultTaskScheduleList, currentTime)
                //            val newHumanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] =
                //              setResourceState(tasksFinished, (taskSchedule) => taskSchedule.humanResources, ResourceStateEnum.Ready)

                val nextCurrentTime = calculateNextCurrentTime(resultTaskScheduleList, currentTime)

                orderToTaskSchedule(newOrderInfo,
                  newPhysicalResources,
                  newHumanResources,
                  nextCurrentTime,
                  resultTaskScheduleList)
            else
              // Liberta resources e avan�a no tempo
              println("2 else")
              val newOrderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]
              = TaskStateModelList.setTaskState(orderInfo, resultTaskScheduleList, currentTime)

              val tasksFinished = resultTaskScheduleList.filter(_.end.to == currentTime)

              val newPhysicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] = setPhysicalResourceState(physicalResources, resultTaskScheduleList, currentTime)
              //          val newPhysicalResources: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] =
              //            setResourceState(tasksFinished, (taskSchedule) => taskSchedule.physicalResources, ResourceStateEnum.Ready)

              val newHumanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] = setHumanResourceState(humanResources, resultTaskScheduleList, currentTime)
              //          val newHumanResources: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] =
              //            setResourceState(tasksFinished, (taskSchedule) => taskSchedule.humanResources, ResourceStateEnum.Ready)

              val nextCurrentTime = calculateNextCurrentTime(resultTaskScheduleList, currentTime)

              orderToTaskSchedule(newOrderInfo,
                newPhysicalResources,
                newHumanResources,
                nextCurrentTime,
                resultTaskScheduleList)

    orderToTaskSchedule(orderInfo1, physicalResources1, humanResources1, 0, List())


  // TODO: Remove toOption and get() -> Result
  def createTasksSchedules(assignedPhysicalResource: List[(Task, List[PhysicalResource])],
                           assignedHumanResource: List[(Task, List[HumanResource])],
                           tasksInitiated: List[(OrderId, ProductNumber, Task)],
                           time: Int): List[TaskSchedule] =
    tasksInitiated.map((orderId, productNumber, task) =>
      val physicalResource: List[PhysicalResource]  = assignedPhysicalResource.filter((t,lpr) => t == task).head._2
      val humanResource: List[HumanResource] = assignedHumanResource.filter((t,lhr) => t == task).head._2
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
  def calculateNextCurrentTime(/*orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)],*/
                               taskSchedules: List[TaskSchedule],
                               currentTime: Int): Int =
    //TODO: Test with tasksSchedulesInProcess empty
//    val tasksInProcess: List[TaskId] = TaskStateModelList.getOrdersTasksInProcess(orderInfo).map(_._3.taskId)
//    val tasksSchedulesInProcess: List[TaskSchedule] = taskSchedules.filter(taskSchedule => tasksInProcess.contains(taskSchedule.taskId))
//    val minTaskSchedule: TaskSchedule = tasksSchedulesInProcess.minBy(_.end.to)
//    minTaskSchedule.end.to + 1
    val tasksSchedulesInProcess: List[TaskSchedule] = taskSchedules.filter(taskSchedule => taskSchedule.end.to > currentTime)
    if(!tasksSchedulesInProcess.isEmpty)
      val minTaskSchedule: TaskSchedule = tasksSchedulesInProcess.minBy(_.end.to)
      minTaskSchedule.end.to
    else currentTime

  /**
   * Assigns to each Task the needed Physical Resource.
   * @param tasks List[Task]
   * @param listPhysical List[PhysicalResource]
   * @return List[(TaskId, List[PhysicalResource])]
   */
  private def assignPhysicalResourceToTask(tasks: List[Task],
                                           listPhysical: List[PhysicalResource]): List[(Task, List[PhysicalResource])] =
    tasks.foldLeft[(List[(Task, List[PhysicalResource])], List[PhysicalResource])](List(), listPhysical) {
      case((result, availablePhysicals), task) => {
        val prList: List[PhysicalResource] = findPhysicalResources(availablePhysicals,task.taskId.to,task.resourceTypes)
        if(prList.isEmpty)
          (result, availablePhysicals)
        else
          val newAvailablePhysicals = availablePhysicals diff prList
          ((task,prList) :: result, newAvailablePhysicals)
      }
    }._1


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

  /**
   * Assigns to each Task the needed Human Resources.
   * Tries to find the best solution, this is, tries to assign human resources to the most tasks possible
   * @param tasks List[Task]
   * @param listHumans List[HumanResource]
   * @return List[(Task, List[HumanResource])]
   */
  def assignHumanResourceToTasks(tasks: List[Task], listHumans: List[HumanResource]): List[(Task, List[HumanResource])] =
    @tailrec
    def assignHumanResourceToTaskRec(tasks: List[Task],
                                     listHumans: List[HumanResource],
                                     numTries: Int,
                                     result: List[(Task, List[HumanResource])]): List[(Task, List[HumanResource])] =

      if(result.size == tasks.size || numTries == tasks.size)
        result.sortWith(_._1.taskId.to < _._1.taskId.to)
      else
        val possibleSolution: List[(Task, List[HumanResource])] = findHumanResourcesPerTask(tasks, listHumans).filter(_._2.size != 0)
        val newListTasks: List[Task] = tasks.drop(1) ::: tasks.take(1)
        if(possibleSolution.size > result.size)
          assignHumanResourceToTaskRec(newListTasks, listHumans, numTries + 1, possibleSolution)
        else
          assignHumanResourceToTaskRec(newListTasks, listHumans, numTries + 1, result)

    assignHumanResourceToTaskRec(tasks, listHumans, 0, List())

  /**
   * Finds the corresponding Human Resource for each Task.
   * (Here the tasks should not have the same Human Resource)
   * @param tasks List[Task]
   * @param listHumans List[HumanResource]
   * @return List[(Task, List[HumanResource])]
   */
  private def findHumanResourcesPerTask(tasks: List[Task], listHumans: List[HumanResource]): List[(Task, List[HumanResource])] =
    @tailrec
    def findHumanResourcesRec(taskIndex: Int,
                              humansAvailability: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)],
                              result: List[(Task, List[HumanResource])]): List[(Task, List[HumanResource])] =
      if(taskIndex == tasks.size)
        result
      else
        val task: Task = tasks(taskIndex)
        val humansAvailable: List[HumanResource] = getReadyHumanResources(humansAvailability)
        val listHumansOfTask: List[HumanResource] = findHumanResources(humansAvailable, task.taskId.to, task.resourceTypes)
        if(listHumansOfTask.isEmpty)
          findHumanResourcesRec(taskIndex + 1, humansAvailability, result)
        else
          val newHumansAvailability = humansAvailability.map((human, available) => if (listHumansOfTask.contains(human)) (human, ResourceStateEnum.Occupied) else (human, available))
          findHumanResourcesRec(taskIndex + 1, newHumansAvailability, (task, listHumansOfTask) :: result)

    findHumanResourcesRec(0, listHumans.map[(HumanResource, ResourceStateEnum.ResourceStateEnum)](human => (human, ResourceStateEnum.Ready)), List())

  
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

//    listTaskSchedule.map(taskSchedule =>
//      if(taskSchedule.start.to == time){
//        taskScheduleResourceList(taskSchedule).map(resource => (resource, ResourceStateEnum.Occupied))
//      } else if(taskSchedule.end.to == time){
//        taskScheduleResourceList(taskSchedule).map(resource => (resource, ResourceStateEnum.Occupied))
//      } else {
//        resourcesList
//      }
//    ).flatten
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
                                  resourceState: ResourceStateEnum.ResourceStateEnum): List[(A, ResourceStateEnum.ResourceStateEnum)] = {
    listTaskSchedule.map(taskSchedule => taskScheduleResourceList(taskSchedule).map(resource => (resource, resourceState))).flatten
  }