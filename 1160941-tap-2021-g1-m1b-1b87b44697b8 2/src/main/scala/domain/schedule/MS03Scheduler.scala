package domain.schedule

import domain.{DomainError, HumanResource, Order, PhysicalResource, Production, ResourceType, Result, Task, TaskSchedule}
import domain.DomainError.ResourceUnavailable
import domain.SimpleTypes.{EndTime, OrderId, ProductNumber, StartTime, TaskId}
import models.{ResourceStateEnum, TaskStateModelList}
import utils.Helper
import scala.annotation.tailrec

object MS03Scheduler extends TScheduler:

  /**
   * Milestone 3 (MS03) Scheduling Algorithm
   * @param production Production
   *  @return Result[List[TaskSchedule]]
   */
  override def scheduleProduction(production: Production): Result[List[TaskSchedule]] =
    if(!validTaskList(production))
      Left(DomainError.ImpossibleSchedule)
    else
      val orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)] = TaskStateModelList.getAllOrdersTasks(production.orders)

      orderToTaskSchedule(orderInfo, production.physicalResources, production.humanResources)


  /**
   * Validates the tasks that must be scheduled in order to infer that there are enough human and physical resources to be assigned
   * @param production Production
   * @return Boolean - true if there are enough humans and physical resources to produce the products
   */
  private def validTaskList(production: Production): Boolean =
    val listHumans: List[HumanResource] = production.humanResources
    val listPhysicals: List[PhysicalResource] = production.physicalResources
    val tasksToProcess: List[Task] = production.orders.flatMap(order => order.product.processes).distinct
    possibleToSchedule(tasksToProcess, listHumans, listPhysicals)

  private def possibleToSchedule(tasksToProcess: List[Task],
                                 listHumans: List[HumanResource],
                                 listPhysicals: List[PhysicalResource]): Boolean =

    tasksToProcess.forall(task => {
      val taskResources = task.resourceTypes
      val mapTaskResourceOccur: Map[String, Int] = taskResources.groupBy(_.toString).view.mapValues(_.size).toMap

      val enoughHumans = mapTaskResourceOccur.forall((taskResource, occur) => {
        val listHumansSameType = listHumans.flatMap(_.resourceTypes).filter(humanResource => humanResource.to == taskResource)
        occur <= listHumansSameType.size
      })
      val enoughPhysicals = mapTaskResourceOccur.forall((taskResource, occur) => {
        val listPhysicalsSameType = listPhysicals.map(_.resourceType).filter(physicalResource => physicalResource.to == taskResource)
        occur <= listPhysicalsSameType.size
      })

      taskResources.size <= listHumans.size
      && taskResources.size <= listPhysicals.size
      && enoughHumans
        && enoughPhysicals
    })

  /**
   * Algorithm that transforms orders to Task Schedule.
   * First, it's necessary to see if there are more tasks to be processed, if not then the result is returned.
   * If there are more tasks to be processed, then, it's necessary to update all the orders for the current time.
   * Then it's necessary to check what tasks are going to be implemented at the current time. If there are no tasks then a next iteration is created for
   * the next time.
   * If there are tasks to be processed at the current time then it's necessary to allocate physical and human resources.
   * If there are no humans available or physical resources available, then it's necessary to set the finished tasks and prepare for
   * the next iteration.
   * Second it's necessary to verify if there are orders that were already initiated and calculate the tasks to be initated at this
   * current time.
   * If there are tasks possible to be initated then it's necessary to generate a list of Task Schedule of the initiated tasks.
   * If the tasks to be initiated are invalid for any reason then the algorithm ends if it's valid then the created list is added
   * to the list of all the task schedules. After this it's necessary to set the state of the task schedules and prepare the next iteration.
   * If the list of tasks to be initiated is empty then it's necessary to prepare the next iteration.
   * @param orderInfo List[(OrderId, ProductNumber, TaskStateModelList)] - List of Orders with the list of task states at a given time.
   * @return List[TasksSchedule] - List of the assigned Task Schedules
   */
  private def orderToTaskSchedule(orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)],
                                  physicalResources: List[PhysicalResource],
                                  humanResources: List[HumanResource]): Result[List[TaskSchedule]] =
    @tailrec
    def orderToTaskSchedule(orderInfo: List[(OrderId, ProductNumber, TaskStateModelList)],
                            currentTime: Int,
                            resultTaskScheduleList: List[TaskSchedule]): Result[List[TaskSchedule]] =

      if (TaskStateModelList.allTasksProcessed(orderInfo))
        Right(resultTaskScheduleList)

      else
        //Changes Tasks state
        val orderInfoUpdated: List[(OrderId, ProductNumber, TaskStateModelList)]
        = TaskStateModelList.setTaskState(orderInfo, resultTaskScheduleList, currentTime)

        //Get tasks to process
        val tasksToProcess: List[(OrderId, ProductNumber, Task)]
        = TaskStateModelList.getOrdersTasksToProcess(orderInfoUpdated)

        if (tasksToProcess.isEmpty)
          //No tasks to process at this time, so go to next one
          val nextCurrentTime: Int = calculateNextCurrentTime(resultTaskScheduleList, currentTime)

          orderToTaskSchedule(orderInfoUpdated,
            nextCurrentTime,
            resultTaskScheduleList)
        else
          //Get resources ready
          val physicalResourcesReady: List[PhysicalResource]
          = getPhysicalResourceReady(orderInfoUpdated, resultTaskScheduleList, physicalResources)

          val humanResourcesReady: List[HumanResource]
          = getHumanResourceReady(orderInfoUpdated, resultTaskScheduleList, humanResources)

          if (physicalResourcesReady.isEmpty || humanResourcesReady.isEmpty)
            // Probably never passes here since the resources were already freed

            val tasksFinished: List[TaskSchedule] = resultTaskScheduleList.filter(_.end.to == currentTime)

            orderToTaskSchedule(orderInfoUpdated,
              currentTime,
              resultTaskScheduleList)

          else
            //orders already initiated (task in Process or Processed)
            val orderAlreadyInitiated: List[(OrderId, ProductNumber)]
            = (TaskStateModelList.getOrdersTasksInProcess(orderInfoUpdated) ::: TaskStateModelList.getOrdersTasksProcessed(orderInfoUpdated)).map((orderId, productNumber, _) => (orderId, productNumber))

            val tasksInitiated: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]
            = scheduleTasks(orderAlreadyInitiated, tasksToProcess, humanResourcesReady, physicalResourcesReady)

            if (!tasksInitiated.isEmpty)
              val newTaskScheduleInitiatedList: Result[List[TaskSchedule]]
              = createTasksSchedules(tasksInitiated, currentTime)

              if(newTaskScheduleInitiatedList.isLeft)
                newTaskScheduleInitiatedList
              else
                newTaskScheduleInitiatedList match
                  case Left(error) => newTaskScheduleInitiatedList
                  case Right(newTaskScheduleInitiatedList) =>
                    val newResultTaskScheduleList: List[TaskSchedule] = resultTaskScheduleList ::: newTaskScheduleInitiatedList

                    val newOrderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]
                    = TaskStateModelList.setTaskState(orderInfoUpdated, newResultTaskScheduleList, currentTime)

                    val nextCurrentTime: Int = calculateNextCurrentTime(newResultTaskScheduleList, currentTime)

                    orderToTaskSchedule(newOrderInfo, nextCurrentTime, newResultTaskScheduleList)
            else
              // Frees resources and goes to the next time unit
              val newOrderInfo: List[(OrderId, ProductNumber, TaskStateModelList)]
              = TaskStateModelList.setTaskState(orderInfoUpdated, resultTaskScheduleList, currentTime)

              val tasksFinished: List[TaskSchedule] = resultTaskScheduleList.filter(_.end.to == currentTime)

              val nextCurrentTime: Int = calculateNextCurrentTime(resultTaskScheduleList, currentTime)

              orderToTaskSchedule(newOrderInfo,
                nextCurrentTime,
                resultTaskScheduleList)

    orderToTaskSchedule(orderInfo, 0, List())


  /**
   * Generates a list of Task Schedules for the current time
   * @param tasksInitiated List of all the initiated tasks.
   * @param time start time of the tasks schedules.
   * @return List of Taks Schedules.
   */
  def createTasksSchedules(tasksInitiated: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])],
                           time: Int): Result[List[TaskSchedule]] =
    val listTasks: List[Result[TaskSchedule]] = tasksInitiated.map((orderId, productNumber, task, humanResource, physicalResource) =>
      val ts: Result[TaskSchedule] = for {
        sTime: StartTime <- StartTime.from(time)
        endTimeVal: Int = task.time.to + time
        eTime: EndTime <- EndTime.from(endTimeVal)
      } yield TaskSchedule.from(productNumber, task.taskId, sTime, eTime, orderId, physicalResource, humanResource)
      ts
    )
    val result: Either[List[DomainError], List[TaskSchedule]] = Helper.listResultToResultList(listTasks)
    result.fold[Result[List[TaskSchedule]]](error => Left(error.head), success => Right(success))

  /**
   * Calculates the new current time
   * @param currentTime Int
   * @return End Time of the TaskSchedule whose value is the min
   */
  def calculateNextCurrentTime(taskSchedules: List[TaskSchedule],
                               currentTime: Int): Int =
    val tasksSchedulesInProcess: List[TaskSchedule] = taskSchedules.filter(taskSchedule => taskSchedule.end.to > currentTime)
    if(!tasksSchedulesInProcess.isEmpty)
      val minTaskSchedule: TaskSchedule = tasksSchedulesInProcess.minBy(_.end.to)
      minTaskSchedule.end.to
    else currentTime


  /**
   * Tries to assign the humans and physical resources to the tasks.
   * First filters the tasksList (Tasks to process) and removes the ones that can not be assign, because there are no human or physical resources
   * Then creates all possible combinations of tasks and a permutation of human resouces
   * Next assigns to each combination of tasks the resources
   * And, finally validates this list of assignments to find the best solution
   *
   * @param ordersInitiated Orders that already initiated
   * @param tasksList Tasks to process
   * @param listHumans list of human resources available
   * @param listPhysicals list of physical resources available
   * @return List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]
   */
  def scheduleTasks(ordersInitiated: List[(OrderId, ProductNumber)],
                    tasksList: List[(OrderId, ProductNumber, Task)],
                    listHumans: List[HumanResource],
                    listPhysicals: List[PhysicalResource]): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] =

    val tasksFiltered: List[(OrderId, ProductNumber, Task)] = filterTasks(tasksList, listHumans, listPhysicals)

    val taskCombinations: List[List[(OrderId, ProductNumber, Task)]] = combinationOfTasks(tasksFiltered, listPhysicals)

    val humansCombinations: List[List[HumanResource]] = permutationOfHumanResources(listHumans).distinct

    val tasksAssigned: List[List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]] =
      assignResourcesToTasksCombinations(taskCombinations, humansCombinations, listPhysicals).distinct

    val mostSize: Int = tasksAssigned.maxBy(item => item.size).size

    val availableOptions: List[List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]] = tasksAssigned.filter(item => item.size == mostSize)

    findBetterSolution(ordersInitiated, availableOptions)


  /**
   * Filters the list of tasks and removes the ones that can not be processed at the same time
   * @param tasksList List tasks to be produced at the same time
   * @param listHumans list of humans available
   * @param listPhysicals list of physicals available
   * @return List[(OrderId, ProductNumber, Task)]
   */
  def filterTasks(tasksList: List[(OrderId, ProductNumber, Task)],
                  listHumans: List[HumanResource],
                  listPhysicals: List[PhysicalResource]): List[(OrderId, ProductNumber, Task)] =
    // Remove tasks that won't have enough human or physical resources
    val tasksFiltered: List[(OrderId, ProductNumber, Task)] =
      tasksList.filterNot(tasks => {
        val taskResources = tasks._3.resourceTypes
        val mapTaskResourceOccur: Map[String, Int] = taskResources.groupBy(_.toString).view.mapValues(_.size).toMap
        val enoughHumans = mapTaskResourceOccur.forall((taskResource, occur) => {
          val listHumansSameType = listHumans.flatMap(_.resourceTypes).filter(humanResource => humanResource.to == taskResource)
          occur <= listHumansSameType.size
        })
        val enoughPhysicals = mapTaskResourceOccur.forall((taskResource, occur) => {
          val listPhysicalsSameType = listPhysicals.map(_.resourceType).filter(physicalResource => physicalResource.to == taskResource)
          occur <= listPhysicalsSameType.size
        })
        !enoughHumans || !enoughPhysicals
      })

    //Map of tasks
    val mapOfTasks: List[(TaskId, List[(OrderId, ProductNumber, Task)])] = tasksFiltered.groupBy(_._3.taskId).toList
    val mapOfTasksSize: Int = mapOfTasks.size

    //Removes the tasks (of same id) that cannot be processed at the same time because there are not enough resources
    @tailrec
    def filterTasksRec(index: Int,
                       result: List[(OrderId, ProductNumber, Task)]): List[(OrderId, ProductNumber, Task)] =
      if(index == mapOfTasksSize)
        result
      else
        val tasks: List[(OrderId, ProductNumber, Task)] = mapOfTasks(index)._2
        val resourcesOfTask: List[String] = tasks(0)._3.resourceTypes.map(_.to)

        val physicalResource: List[String] = listPhysicals.map(_.resourceType.to)

        //Map where Key is the resource type and Values is the number of occurences on the same task (task nedded x - Value - resources - Key)
        val mapTaskResourceOccur: Map[String, Int] = resourcesOfTask.groupBy(_.toString).view.mapValues(_.size).toMap

        // Max elements of each resource type
        /*
            resource type - how many needs to produce the task  - how many available  - how many can be done at the same time
            PRST_4        - 3                                   - 4                   - 1 (4/3)
            PRST_4        - 1                                   - 2                   - 2 (2/1)
         */
        val maxResourcesSameTypeSameTime: Map[String, Int] = mapTaskResourceOccur.map[(String, Int)]((taskResource, occur) => {
          val listPhysicalsSameType: List[String] = physicalResource.filter(physicalResource => physicalResource == taskResource)
          if(listPhysicalsSameType.isEmpty)
            (taskResource, 0)
          else
            val maxResourceSameTime: Int = listPhysicalsSameType.size / occur
            (taskResource, maxResourceSameTime)
        }).toMap

        //Keeps only the first n tasks (where n is the minimun number of physicals of all the resources of task)
        val elementsKeep: List[(OrderId, ProductNumber, Task)] = tasks.take(maxResourcesSameTypeSameTime.map(_._2).min)

        //Removes the remaining elements
        val elementsRemove: List[(OrderId, ProductNumber, Task)] = tasks.filterNot(elementsKeep.contains)

        filterTasksRec(index + 1, result ++ elementsRemove)

    val elementsRemove = filterTasksRec(0, List())
    tasksList.filterNot(elementsRemove.contains)


  /**
   * Creates a combination of Orders Info.
   * Filtes the combinations and removes the ones that won't have enough resources.
   *
   * @param ordersInfo List[(OrderId, ProductNumber, Task)]
   * @param listPhysicals list of Physical Resources available
   * @return List[List[(OrderId, ProductNumber, Task)]]
   */
  private def combinationOfTasks(ordersInfo: List[(OrderId, ProductNumber, Task)],
                                 listPhysicals: List[PhysicalResource]): List[List[(OrderId, ProductNumber, Task)]] =

    @tailrec
    def combinationOfTasksRec(listOfCombinations: List[List[(OrderId, ProductNumber, Task)]],
                              index: Int): List[List[(OrderId, ProductNumber, Task)]] =
      if(index == ordersInfo.size + 1)
        listOfCombinations
      else
        combinationOfTasksRec(ordersInfo.combinations(index).toList ::: listOfCombinations, index + 1)

    combinationOfTasksRec(List(), 0).filter(taskCombination => combinationPossible(taskCombination, listPhysicals))

  /**
   * Validates if the task are possible to be done at the same time (if there are enough resources to do so)
   * @param taskCombination Tasks to be processed at the same time (attemptive)
   * @param listPhysicals list of available Physical Resources
   * @return Boolean
   */
  def combinationPossible(taskCombination: List[(OrderId, ProductNumber, Task)],
                          listPhysicals: List[PhysicalResource]): Boolean =

    val taskResources: List[ResourceType] = taskCombination.flatMap(_._3.resourceTypes)
    val resourcesOfPhysicals: List[ResourceType] = listPhysicals.map(_.resourceType)
    val taskResourcesSize = taskResources.size

    @tailrec
    def combinationPossibleRec(index: Int, resourcesOfPhysicals: List[ResourceType]): Boolean =
      if (index == taskResourcesSize)
        true
      else
        val resource: ResourceType = taskResources(index)
        if (resourcesOfPhysicals.contains(resource))
          // List of resources types equal to the resource of task without the first element
          val listResourcesOfPhysicalsWithoutFirstElement = resourcesOfPhysicals.filter(item => item == resource).drop(1)

          // List without resources types equal to the resource of task
          val listResourcesOfPhysicalsWithoutTaskResource = resourcesOfPhysicals.filter(item => item != resource)

          combinationPossibleRec(index + 1, listResourcesOfPhysicalsWithoutFirstElement ++ listResourcesOfPhysicalsWithoutTaskResource)
        else false

    combinationPossibleRec(0, resourcesOfPhysicals)


  /**
   * Sets a list of resources to a given list of tasks.
   * @param taskCombinations all the possible combinations between the tasks.
   * @param humansPermutations all the human resources permutations
   * @param listPhysicals all the physical resources available
   * @return List of assigned resources for a given tasks
   */
  private def assignResourcesToTasksCombinations(taskCombinations: List[List[(OrderId, ProductNumber, Task)]],
                                                 humansPermutations: List[List[HumanResource]],
                                                 listPhysicals: List[PhysicalResource]): List[List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]] =

    @tailrec
    def assignResourcesToTasksRec(result: List[List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]],
                                  index: Int): List[List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]] =

      if(index == taskCombinations.size)
        result
      else
        val tasks: List[(OrderId, ProductNumber, Task)] = taskCombinations(index)
        val tasksAssigned: List[List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]] =
          humansPermutations.map(humans => assignResources(tasks, listPhysicals, humans))

        assignResourcesToTasksRec(result ::: tasksAssigned, index + 1)

    assignResourcesToTasksRec(List(), 0)


  /**
   * Method that tries to find the better combination of tasks to be processed at the same time.
   * @param ordersInitiated List of Orders that are already initiated
   * @param availableOptions  List of combinations of the tasks that can be allocated
   * @return Solution that contains all the tasks that are allocated at that time
   */
  private def findBetterSolution(ordersInitiated: List[(OrderId, ProductNumber)],
                                 availableOptions: List[List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]]): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] =
    @tailrec
    def findBetterSolutionRec(actualSolution: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])],
                              index: Int): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]=
      if(index >= availableOptions.size)
        actualSolution
      else
        val nextIndex = index + 1
        val possibleSolution = availableOptions(index)
        if(isBetterSolution(ordersInitiated, actualSolution, possibleSolution))
          findBetterSolutionRec(possibleSolution, nextIndex)
        else
          findBetterSolutionRec(actualSolution, nextIndex)

    findBetterSolutionRec(availableOptions(0), 1)


  /**
   * Assigns to a given tasks list the available physical resources and human resources.
   * @param tasksList List of tasks to be assigned (one element of the combinations)
   * @param listPhysicals List of available Physical Resources.
   * @param listHumans List of available Human Resources (one element of the permutations)
   * @return List of tasks with human resource and physical resources that were possible to be assigned.
   */
  private def assignResources(tasksList: List[(OrderId, ProductNumber, Task)],
                              listPhysicals: List[PhysicalResource],
                              listHumans: List[HumanResource]): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] =
    @tailrec
    def assignResourcesRec(tasksList: List[(OrderId, ProductNumber, Task)],
                           listPhysicals: List[PhysicalResource],
                           listHumans: List[HumanResource],
                           result: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]): List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] =

      if(tasksList.isEmpty)
        result
      else
        val task = tasksList(0)
        val physicalsAssigned: List[PhysicalResource] = findPhysicalResources(task._3.resourceTypes, listPhysicals)
        val humansAssigned: List[HumanResource] = findHumanResource(listHumans, physicalsAssigned.map(_.resourceType))

        if(physicalsAssigned.isEmpty || humansAssigned.isEmpty || physicalsAssigned.size != humansAssigned.size)
          result
        else
          val newResult = result :+ (task._1, task._2, task._3, humansAssigned, physicalsAssigned)
          val newListPhysicals = listPhysicals diff physicalsAssigned
          val newListHumans = listHumans diff humansAssigned
          val newTasksList = tasksList.filter(item => item != task)
          assignResourcesRec(newTasksList, newListPhysicals, newListHumans, newResult)

    assignResourcesRec(tasksList, listPhysicals, listHumans, List())


  /**
   * Finds all Physical Resources inherent to Task
   * @param listResources resource types of a task
   * @param physicals all physical resources available
   * @return List[PhysicalResource]
   */
  private def findPhysicalResources(listResources: List[ResourceType], physicals: List[PhysicalResource]): List[PhysicalResource] =
    @tailrec
    def findPhysicalResourcesRec(physicals: List[PhysicalResource], result: List[PhysicalResource], index: Int): List[PhysicalResource] =
      if(result.size == listResources.size)
        result
      else
        val resourceType: ResourceType = listResources(index)
        val physicalsAvailable: List[PhysicalResource] = physicals.filter(p => p.resourceType.to == resourceType.to)
        if(!physicalsAvailable.isEmpty)
          val physicalResource = physicalsAvailable(0)
          val newlistPhysicals = physicals.filter(p => p.physicalId.to != physicalResource.physicalId.to)
          findPhysicalResourcesRec(newlistPhysicals, result :+ physicalResource, index + 1)
        else
          List()
    findPhysicalResourcesRec(physicals, List(), 0)

  /**
   * Assigns humans to the given physical resources
   * @param humans list of available human resources
   * @param resourcesTypes list of Resource Type that most be assigned
   * @return list of allocated human resources empty if there are no enough humans to be allocated to all resource types
   */
  private def findHumanResource(humans: List[HumanResource], resourcesTypes: List[ResourceType]): List[HumanResource] =
    @tailrec
    def findHumanRec(humans: List[HumanResource], lstHumans: List[HumanResource], index: Int): List[HumanResource] =
      if(lstHumans.size == resourcesTypes.size)
        lstHumans
      else
        val physicalType: ResourceType = resourcesTypes(index)

        val humansAvailable = humans.filter(human => {
          human.resourceTypes.map(item => item.to).contains(physicalType.to)
        })
        if(!humansAvailable.isEmpty)
          val humanResource = humansAvailable(0)
          val newlistHumans = humans.filter(h => h.humanId.to != humanResource.humanId.to)
          findHumanRec(newlistHumans, lstHumans :+ humanResource, index + 1)
        else
          List()
    findHumanRec(humans, List(), 0)

  /**
   * Creates a permutation of available human resources.
   * @param listHumans list of available human resources
   * @return list of all the possible permutations of human resources
   */
  private def permutationOfHumanResources(listHumans: List[HumanResource]): List[List[HumanResource]] =
    @tailrec
    def permutationOfHumanResourcesRec(listHumans: List[HumanResource], combinations: List[List[HumanResource]]): List[List[HumanResource]] =
      if(combinations.size == listHumans.size + 1)
        combinations
      else
        val item: HumanResource = listHumans(0)
        permutationOfHumanResourcesRec(listHumans.filter(human => human != item) :+ item, combinations :+ listHumans)
    permutationOfHumanResourcesRec(listHumans, List())

  /**
   * Validates if newSolution is "better" than actualSolution
   * To find the Better solution the next condition where considered:
   *      - To start the tasks, the set with the smallest task id(s) must be used.
   *      - When assigning physical resources to tasks, they are sorted by ID and assigned sequentially.
   *      - When assigning human resources to tasks, they are sorted by ID and assigned sequentially, finding the first permutation that works.
   *      - Try to start tasks from instances of products that are already partially in production, before trying new instances of products.
   * @param ordersInitiated List[(OrderId, ProductNumber)]
   * @param actualSolution List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]
   * @param newSolution List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]
   * @return Boolean (true if newSolution is "better" than actualSolution)
   */
  def isBetterSolution(ordersInitiated: List[(OrderId, ProductNumber)],
                       actualSolution: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])],
                       newSolution: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]): Boolean =

    def isBetterSolution(): Boolean =
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

    if(actualSolution.isEmpty) true
    else if(newSolution.size > actualSolution.size) true
    else if(newSolution.size < actualSolution.size) false
    // if are the same solution
    else if(actualSolution.equals(newSolution))
      false
    else if(ordersInitiated.isEmpty)
      isBetterSolution()
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
        isBetterSolution()

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