package domain.schedule

import domain.SimpleTypes.{EndTime, HumanId, HumanName, OrderId, PhysicalId, ProductId, ProductName, ProductNumber, Quantity, StartTime, TaskId, Time}
import domain.schedule.MS03Scheduler
import domain.schedule.secundaryscheduler.{MS03SchedulerV1}
import models.ResourceStateEnum
import domain.{HumanResource, Order, PhysicalResource, Product, ResourceType, Result, Task, TaskSchedule}
import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions

class MS03SchedulerTests extends AnyFunSuite:

  test("scheduleTasks finds the best possible result the first time"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
    } yield Task(taskId1, time1, List(r1))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("100")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1))

    val human2 = for {
      humanId <- HumanId.from("HRS_2")
      name <- HumanName.from("Maria")
      r2 <- resourceType2
    } yield HumanResource(humanId,name,List(r2))

    val human3 = for {
      humanId <- HumanId.from("HRS_3")
      name <- HumanName.from("Joao")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      h1 <- human1
      h2 <- human2
      h3 <- human3
      humanResources = List(h1, h2, h3)

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3

      physicals = List(pr1,pr2,pr3)

      t1 <- task1
      t3 <- task3
      t5 <- task5

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")
      orderId3 <- OrderId.from("ORD_3")

      productNumber1 <- ProductNumber.from(1)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber1, t3),
        (orderId3, productNumber1, t5)
      )
      ordersInitiated = List()

      result04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = MS03Scheduler.scheduleTasks(List(), tasks, humanResources, physicals)
      //assignHumanResourceToTasks(ordersInitiated, tasks, humanResources)
      expected04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]  = List(
        (tasks(0)._1, tasks(0)._2, tasks(0)._3, List(h1), List(pr1)),
        (tasks(1)._1, tasks(1)._2, tasks(1)._3, List(h2), List(pr2)),
        (tasks(2)._1, tasks(2)._2, tasks(2)._3, List(h3), List(pr3))
      )
    }yield assert(expected04 == result04 && expected04.diff(result04).size == 0 && expected04.intersect(result04).size == expected04.size)

  }

  test("scheduleTasks finds the best possible result not in the first time"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
    } yield Task(taskId1, time1, List(r1))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("100")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val human2 = for {
      humanId <- HumanId.from("HRS_2")
      name <- HumanName.from("Maria")
      r2 <- resourceType2
    } yield HumanResource(humanId,name,List(r2))

    val human3 = for {
      humanId <- HumanId.from("HRS_3")
      name <- HumanName.from("Joao")
      r1 <- resourceType1
    } yield HumanResource(humanId,name,List(r1))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      h1 <- human1
      h2 <- human2
      h3 <- human3
      humanResources = List(h1, h2, h3)

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3

      physicals = List(pr1,pr2,pr3)

      t1 <- task1
      t3 <- task3
      t5 <- task5
      tasks = List(t1, t3, t5)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")
      orderId3 <- OrderId.from("ORD_3")

      productNumber1 <- ProductNumber.from(1)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber1, t3),
        (orderId3, productNumber1, t5)
      )
      ordersInitiated = List()

      result04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = MS03Scheduler.scheduleTasks(List(),tasks,humanResources,physicals)

      expected04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]  = List(
        (tasks(0)._1, tasks(0)._2, tasks(0)._3, List(h3), List(pr1)),
        (tasks(1)._1, tasks(1)._2, tasks(1)._3, List(h2), List(pr2)),
        (tasks(2)._1, tasks(2)._2, tasks(2)._3, List(h1), List(pr3))
      )
    }yield assert(expected04 == result04 && expected04.diff(result04).size == 0 && expected04.intersect(result04).size == expected04.size)

  }

  test("scheduleTasks finds the best possible result that does not assign all tasks"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")
    val resourceType4 = ResourceType.from("PRST 4")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
    } yield Task(taskId1, time1, List(r1))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("100")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val human2 = for {
      humanId <- HumanId.from("HRS_2")
      name <- HumanName.from("Maria")
      r2 <- resourceType2
    } yield HumanResource(humanId,name,List(r2))

    val human3 = for {
      humanId <- HumanId.from("HRS_3")
      name <- HumanName.from("Joao")
      r4 <- resourceType4
    } yield HumanResource(humanId,name,List(r4))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      h1 <- human1
      h2 <- human2
      h3 <- human3
      humanResources = List(h1, h2, h3)

      t1 <- task1
      t3 <- task3
      t5 <- task5

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3

      physicals = List(pr1,pr2,pr3)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")
      orderId3 <- OrderId.from("ORD_3")

      productNumber1 <- ProductNumber.from(1)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber1, t3),
        (orderId3, productNumber1, t5)
      )
      ordersInitiated = List()

      result04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = MS03Scheduler.scheduleTasks(List(),tasks,humanResources,physicals)
      expected04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]  = List(
        (tasks(0)._1, tasks(0)._2, tasks(0)._3, List(h1), List(pr1)),
        (tasks(1)._1, tasks(1)._2, tasks(1)._3, List(h2), List(pr2))
      )
    }yield assert(expected04 == result04 && expected04.diff(result04).size == 0 && expected04.intersect(result04).size == expected04.size)
  }

  test("scheduleTasks does not assign any tasks to the human resources"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")
    val resourceType4 = ResourceType.from("PRST 4")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
    } yield Task(taskId1, time1, List(r1))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("100")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r4 <- resourceType4
    } yield HumanResource(humanId,name,List(r4))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      h1 <- human1
      humanResources = List(h1)

      t1 <- task1
      t3 <- task3
      t5 <- task5

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3

      physicals = List(pr1,pr2,pr3)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")
      orderId3 <- OrderId.from("ORD_3")

      productNumber1 <- ProductNumber.from(1)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber1, t3),
        (orderId3, productNumber1, t5)
      )
      ordersInitiated = List()

      result04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = MS03Scheduler.scheduleTasks(List(),tasks,humanResources,physicals)
      expected04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = List()

    }yield assert(expected04 == result04 && expected04.diff(result04).size == 0 && expected04.intersect(result04).size == expected04.size)
  }

  test("scheduleTasks can assign when a task has more than one resource type"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("100")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1))

    val human2 = for {
      humanId <- HumanId.from("HRS_2")
      name <- HumanName.from("Maria")
      r2 <- resourceType2
    } yield HumanResource(humanId,name,List(r2))

    val human3 = for {
      humanId <- HumanId.from("HRS_3")
      name <- HumanName.from("Joao")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      h1 <- human1
      h2 <- human2
      h3 <- human3
      humanResources = List(h1, h2, h3)
      t1 <- task1
      t3 <- task3
      t5 <- task5

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3

      physicals = List(pr1,pr2,pr3)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")
      orderId3 <- OrderId.from("ORD_3")

      productNumber1 <- ProductNumber.from(1)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber1, t3),
        (orderId3, productNumber1, t5)
      )
      result04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = MS03Scheduler.scheduleTasks(List(),tasks,humanResources,physicals)
      expected04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]  = List(
        (tasks(0)._1, tasks(0)._2, tasks(0)._3, List(h1, h2), List(pr1, pr2)),
        (tasks(2)._1, tasks(2)._2, tasks(2)._3, List(h3), List(pr3))
      )

    }yield assert(expected04 == result04 && expected04.diff(result04).size == 0 && expected04.intersect(result04).size == expected04.size)
  }

  test("scheduleTasks can assign when a task has more than one resource type - same product number"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("100")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val task6 = for {
      taskId2   <- TaskId.from("TSK_6")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1))

    val human2 = for {
      humanId <- HumanId.from("HRS_2")
      name <- HumanName.from("Maria")
      r2 <- resourceType2
    } yield HumanResource(humanId,name,List(r2))

    val human3 = for {
      humanId <- HumanId.from("HRS_3")
      name <- HumanName.from("Joao")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      h1 <- human1
      h2 <- human2
      h3 <- human3
      humanResources = List(h1, h2, h3)
      t1 <- task1
      t3 <- task3
      t5 <- task5
      t6 <- task6

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3

      physicals = List(pr1,pr2,pr3)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")
      orderId3 <- OrderId.from("ORD_3")
      orderId4 <- OrderId.from("ORD_4")

      productNumber1 <- ProductNumber.from(1)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber1, t3),
        (orderId3, productNumber1, t5),
        (orderId4, productNumber1, t6)
      )

      result04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = MS03Scheduler.scheduleTasks(List(),tasks,humanResources,physicals)
      expected04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]  = List(
        (tasks(0)._1, tasks(0)._2, tasks(0)._3, List(h1, h2), List(pr1, pr2)),
        (tasks(2)._1, tasks(2)._2, tasks(2)._3, List(h3), List(pr3))
      )

    }yield assert(expected04 == result04 && expected04.diff(result04).size == 0 && expected04.intersect(result04).size == expected04.size)
  }

  test("scheduleTasks can assign when a task has more than one resource type - same order id - different product number"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1))

    val human2 = for {
      humanId <- HumanId.from("HRS_2")
      name <- HumanName.from("Maria")
      r2 <- resourceType2
    } yield HumanResource(humanId,name,List(r2))

    val human3 = for {
      humanId <- HumanId.from("HRS_3")
      name <- HumanName.from("Joao")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      h1 <- human1
      h2 <- human2
      h3 <- human3
      humanResources = List(h1, h2, h3)
      t1 <- task1
      t5 <- task5

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3

      physicals = List(pr1,pr2,pr3)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")

      productNumber1 <- ProductNumber.from(1)
      productNumber2 <- ProductNumber.from(2)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber2, t5),
        (orderId2, productNumber1, t5),
      )
      ordersInitiated = List()

      result04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = MS03Scheduler.scheduleTasks(List(),tasks,humanResources,physicals)
      expected04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]  = List(
        (tasks(0)._1, tasks(0)._2, tasks(0)._3, List(h1, h2), List(pr1, pr2)),
        (tasks(1)._1, tasks(1)._2, tasks(1)._3, List(h3), List(pr3))
      )

    }yield assert(expected04 == result04 && expected04.diff(result04).size == 0 && expected04.intersect(result04).size == expected04.size)
  }

  test("scheduleTasks can assign based on file validAgend_08_in.xml"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")
    val resourceType4 = ResourceType.from("PRST 4")
    val resourceType5 = ResourceType.from("PRST 5")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
      r4 <- resourceType4
    } yield Task(taskId1, time1, List(r1,r4))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("100")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1, r3))

    val human2 = for {
      humanId <- HumanId.from("HRS_2")
      name <- HumanName.from("Maria")
      r2 <- resourceType2
      r4 <- resourceType4
    } yield HumanResource(humanId,name,List(r2, r4))

    val human3 = for {
      humanId <- HumanId.from("HRS_3")
      name <- HumanName.from("Joao")
      r4 <- resourceType4
      r5 <- resourceType5
    } yield HumanResource(humanId,name,List(r4,r5))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    val physicalr4 = for {
      id <- PhysicalId.from("PRS_4")
      r4 <- resourceType4
    } yield PhysicalResource(id,r4)

    val physicalr5 = for {
      id <- PhysicalId.from("PRS_5")
      r5 <- resourceType5
    } yield PhysicalResource(id,r5)

    val physicalr6 = for {
      id <- PhysicalId.from("PRS_6")
      r4 <- resourceType4
    } yield PhysicalResource(id,r4)

    for {
      h1 <- human1
      h2 <- human2
      h3 <- human3
      humanResources = List(h1, h2, h3)
      t1 <- task1
      t3 <- task3
      t5 <- task5

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")
      orderId3 <- OrderId.from("ORD_3")

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3
      pr4 <- physicalr4
      pr5 <- physicalr5
      pr6 <- physicalr6

      physicals = List(pr1,pr2,pr3,pr4,pr5,pr6)

      productNumber1 <- ProductNumber.from(1)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber1, t3),
        (orderId3, productNumber1, t5),
      )

      result04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = MS03Scheduler.scheduleTasks(List(),tasks,humanResources,physicals)
      expected04: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])]  = List(
        (tasks(0)._1, tasks(0)._2, tasks(0)._3, List(h1, h3), List(pr1, pr4)),
        (tasks(1)._1, tasks(1)._2, tasks(1)._3, List(h2), List(pr2))
      )

    }yield assert(expected04 == result04 && expected04.diff(result04).size == 0 && expected04.intersect(result04).size == expected04.size)
  }

  test("calculateNextCurrentTime - currentTime = 0 - min = 2 (1 task)") {
    val task1 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("2")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId1 <- TaskId.from("TSK_2")
      time1 <- Time.from("5")
    } yield Task(taskId1, time1, List())

    val task3 = for {
      taskId1 <- TaskId.from("TSK_3")
      time1 <- Time.from("4")
    } yield Task(taskId1, time1, List())

    val product1 = for {
      id <- ProductId.from("PRD_1")
      name <- ProductName.from("Product 1")
      t1 <- task1
      t2 <- task2
    } yield Product(id, name, List(t1, t2))

    val product2 = for {
      id <- ProductId.from("PRD_2")
      name <- ProductName.from("Product 2")
      t3 <- task3
    } yield Product(id, name, List(t3))

    val order1 = for {
      id <- OrderId.from("ORD_1")
      quantity <- Quantity.from("1")
      p1 <- product1
    } yield Order(id, p1, quantity)

    val order2 = for {
      id <- OrderId.from("ORD_2")
      quantity <- Quantity.from("1")
      p2 <- product2
    } yield Order(id, p2, quantity)

    for {
      p1 <- ProductNumber.from(1)
      o1 <- order1
      o2 <- order2
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      taskSchedules = List(TaskSchedule(o1.id, p1, t1.taskId, s1, e1, List(), List()))

      result = MS03Scheduler.calculateNextCurrentTime(taskSchedules, currentTime = 0)
      expected = 2
    } yield assert(result === expected)
  }

  test("calculateNextCurrentTime - currentTime = 0 - min = 2 (2 task)") {
    val task1 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("2")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId1 <- TaskId.from("TSK_2")
      time1 <- Time.from("5")
    } yield Task(taskId1, time1, List())

    val task3 = for {
      taskId1 <- TaskId.from("TSK_3")
      time1 <- Time.from("4")
    } yield Task(taskId1, time1, List())

    val product1 = for {
      id <- ProductId.from("PRD_1")
      name <- ProductName.from("Product 1")
      t1 <- task1
      t2 <- task2
    } yield Product(id, name, List(t1, t2))

    val product2 = for {
      id <- ProductId.from("PRD_2")
      name <- ProductName.from("Product 2")
      t3 <- task3
    } yield Product(id, name, List(t3))

    val order1 = for {
      id <- OrderId.from("ORD_1")
      quantity <- Quantity.from("1")
      p1 <- product1
    } yield Order(id, p1, quantity)

    val order2 = for {
      id <- OrderId.from("ORD_2")
      quantity <- Quantity.from("1")
      p2 <- product2
    } yield Order(id, p2, quantity)

    for {
      p1 <- ProductNumber.from(1)
      o1 <- order1
      o2 <- order2
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      e2 <- EndTime.from(t3.time.to)
      taskSchedules = List(
        TaskSchedule(o1.id, p1, t1.taskId, s1, e1, List(), List()),
        TaskSchedule(o2.id, p1, t3.taskId, s1, e2, List(), List())
      )

      result = MS03Scheduler.calculateNextCurrentTime(taskSchedules, currentTime = 0)
      expected = 2
    } yield assert(result === expected)
  }

  test("calculateNextCurrentTime - currentTime = 2 - min = 4 (2 task)") {
    val task1 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("2")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId1 <- TaskId.from("TSK_2")
      time1 <- Time.from("5")
    } yield Task(taskId1, time1, List())

    val task3 = for {
      taskId1 <- TaskId.from("TSK_3")
      time1 <- Time.from("4")
    } yield Task(taskId1, time1, List())

    val product1 = for {
      id <- ProductId.from("PRD_1")
      name <- ProductName.from("Product 1")
      t1 <- task1
      t2 <- task2
    } yield Product(id, name, List(t1, t2))

    val product2 = for {
      id <- ProductId.from("PRD_2")
      name <- ProductName.from("Product 2")
      t3 <- task3
    } yield Product(id, name, List(t3))

    val order1 = for {
      id <- OrderId.from("ORD_1")
      quantity <- Quantity.from("1")
      p1 <- product1
    } yield Order(id, p1, quantity)

    val order2 = for {
      id <- OrderId.from("ORD_2")
      quantity <- Quantity.from("1")
      p2 <- product2
    } yield Order(id, p2, quantity)

    for {
      p1 <- ProductNumber.from(1)
      o1 <- order1
      o2 <- order2
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      e2 <- EndTime.from(t2.time.to)
      e3 <- EndTime.from(t3.time.to)
      taskSchedules = List(
        TaskSchedule(o1.id, p1, t1.taskId, s1, e1, List(), List()),
        TaskSchedule(o1.id, p1, t2.taskId, s1, e2, List(), List()),
        TaskSchedule(o2.id, p1, t3.taskId, s1, e3, List(), List())
      )

      result = MS03Scheduler.calculateNextCurrentTime(taskSchedules, currentTime = 2)
      expected = 4
    } yield assert(result === expected)
  }

  test("calculateNextCurrentTime - currentTime = 2 - min = 5 (2 task)") {
    val task1 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("2")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId1 <- TaskId.from("TSK_2")
      time1 <- Time.from("4")
    } yield Task(taskId1, time1, List())

    val task3 = for {
      taskId1 <- TaskId.from("TSK_3")
      time1 <- Time.from("5")
    } yield Task(taskId1, time1, List())

    val product1 = for {
      id <- ProductId.from("PRD_1")
      name <- ProductName.from("Product 1")
      t1 <- task1
      t2 <- task2
    } yield Product(id, name, List(t1, t2))

    val product2 = for {
      id <- ProductId.from("PRD_2")
      name <- ProductName.from("Product 2")
      t3 <- task3
    } yield Product(id, name, List(t3))

    val order1 = for {
      id <- OrderId.from("ORD_1")
      quantity <- Quantity.from("1")
      p1 <- product1
    } yield Order(id, p1, quantity)

    val order2 = for {
      id <- OrderId.from("ORD_2")
      quantity <- Quantity.from("1")
      p2 <- product2
    } yield Order(id, p2, quantity)

    for {
      p1 <- ProductNumber.from(1)
      o1 <- order1
      o2 <- order2
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      s2 <- StartTime.from(2)
      e1 <- EndTime.from(t1.time.to + s1.to)
      e2 <- EndTime.from(t2.time.to + s2.to)
      e3 <- EndTime.from(t3.time.to + s1.to)
      taskSchedules = List(
        TaskSchedule(o1.id, p1, t1.taskId, s1, e1, List(), List()),
        TaskSchedule(o1.id, p1, t2.taskId, s2, e2, List(), List()),
        TaskSchedule(o2.id, p1, t3.taskId, s1, e3, List(), List())
      )

      result = MS03Scheduler.calculateNextCurrentTime(taskSchedules, currentTime = 2)
      expected = 5
    } yield
      assert(result === expected)
  }

  //TODO: setPhysicalResourceState não existe em V3
  test("setPhysicalResourceState with a list of available physical resources returns a list of physical resources that will be used by the tasks"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("2")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("5")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("4")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      t1 <- task1
      t3 <- task3
      t5 <- task5
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      pr1 <- ProductNumber.from(1)
      p1 <- physicalr1
      p2 <- physicalr2
      p3 <- physicalr3

      s1 <- StartTime.from(0)
      s2 <- StartTime.from(2)
      e1 <- EndTime.from(t1.time.to + s1.to)
      e2 <- EndTime.from(t3.time.to + s2.to)
      e3 <- EndTime.from(t5.time.to + s1.to)
      physicalList = List(
        (p1,ResourceStateEnum.Ready),
        (p2,ResourceStateEnum.Ready),
        (p3,ResourceStateEnum.Ready),
      )

      taskSchedules = List(
        TaskSchedule(o1, pr1, t1.taskId, s1, e1, List(p1), List()),
        TaskSchedule(o2, pr1, t5.taskId, s1, e3, List(p3), List())
      )

      result: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] = MS03SchedulerV1.setPhysicalResourceState(physicalList,taskSchedules,0)
      expected: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] = List(
        (p1,ResourceStateEnum.Occupied),
        (p2,ResourceStateEnum.Ready),
        (p3,ResourceStateEnum.Occupied),
      )
    }yield assert(expected === result)
  }

  test("setPhysicalResourceState with a list of occupied physical resources returns a list of some available physical resources "){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("2")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("5")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("4")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      t1 <- task1
      t3 <- task3
      t5 <- task5
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      pr1 <- ProductNumber.from(1)
      p1 <- physicalr1
      p2 <- physicalr2
      p3 <- physicalr3

      s1 <- StartTime.from(0)
      s2 <- StartTime.from(2)
      e1 <- EndTime.from(t1.time.to + s1.to)
      e2 <- EndTime.from(t3.time.to + s2.to)
      e3 <- EndTime.from(t5.time.to + s1.to)
      physicalList = List(
        (p1,ResourceStateEnum.Occupied),
        (p2,ResourceStateEnum.Ready),
        (p3,ResourceStateEnum.Occupied),
      )

      taskSchedules = List(
        TaskSchedule(o1, pr1, t1.taskId, s1, e1, List(p1), List()),
        TaskSchedule(o1, pr1, t3.taskId, s2, e2, List(p2), List()),
        TaskSchedule(o2, pr1, t5.taskId, s1, e3, List(p3), List())
      )

      result: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] = MS03SchedulerV1.setPhysicalResourceState(physicalList,taskSchedules,2)
      expected: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] = List(
        (p1,ResourceStateEnum.Ready),
        (p2,ResourceStateEnum.Occupied),
        (p3,ResourceStateEnum.Occupied),
      )
    }yield assert(expected === result)
  }

  test("setPhysicalResourceState with a list of occupied physical resources returns a list of some none physical resources "){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("2")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("5")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("4")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      t1 <- task1
      t3 <- task3
      t5 <- task5
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      pr1 <- ProductNumber.from(1)
      p1 <- physicalr1
      p2 <- physicalr2
      p3 <- physicalr3

      s1 <- StartTime.from(0)
      s2 <- StartTime.from(2)
      e1 <- EndTime.from(t1.time.to + s1.to)
      e2 <- EndTime.from(t3.time.to + s2.to)
      e3 <- EndTime.from(t5.time.to + s1.to)
      physicalList = List(
        (p1,ResourceStateEnum.Occupied),
        (p2,ResourceStateEnum.Ready),
        (p3,ResourceStateEnum.Occupied),
      )

      taskSchedules = List(
        TaskSchedule(o1, pr1, t1.taskId, s1, e1, List(p1), List()),
        TaskSchedule(o1, pr1, t3.taskId, s2, e2, List(p2), List()),
        TaskSchedule(o2, pr1, t5.taskId, s1, e3, List(p3, p1), List())
      )

      result: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] = MS03SchedulerV1.setPhysicalResourceState(physicalList,taskSchedules,2)
      expected: List[(PhysicalResource, ResourceStateEnum.ResourceStateEnum)] = List(
        (p1,ResourceStateEnum.Occupied),
        (p2,ResourceStateEnum.Occupied),
        (p3,ResourceStateEnum.Occupied),
      )
    }yield assert(expected === result)
  }

  test("setHumanResourceState with a list of available human resources returns a list of human resources that will be used by the tasks"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("2")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("5")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("4")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val humanr1 = for {
      id      <- HumanId.from("HRS_1")
      name    <- HumanName.from("Antonio")
      r1      <-  resourceType1
      r2      <-  resourceType2
    } yield HumanResource(id,name,List(r1,r2))

    val humanr2 = for {
      id      <- HumanId.from("HRS_2")
      name    <- HumanName.from("Maria")
      r2      <-  resourceType2
    } yield HumanResource(id,name,List(r2))

    val humanr3 = for {
      id      <- HumanId.from("HRS_3")
      name    <- HumanName.from("Joao")
      r3      <-  resourceType3
    } yield HumanResource(id,name,List(r3))

    for {
      t1 <- task1
      t3 <- task3
      t5 <- task5
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      pr1 <- ProductNumber.from(1)
      h1 <- humanr1
      h2 <- humanr2
      h3 <- humanr3

      s1 <- StartTime.from(0)
      s2 <- StartTime.from(2)
      e1 <- EndTime.from(t1.time.to + s1.to)
      e2 <- EndTime.from(t3.time.to + s2.to)
      e3 <- EndTime.from(t5.time.to + s1.to)
      humanList = List(
        (h1,ResourceStateEnum.Ready),
        (h2,ResourceStateEnum.Ready),
        (h3,ResourceStateEnum.Ready),
      )

      taskSchedules = List(
        TaskSchedule(o1, pr1, t1.taskId, s1, e1, List(), List(h1)),
        TaskSchedule(o2, pr1, t5.taskId, s1, e3, List(), List(h3))
      )

      result: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] = MS03SchedulerV1.setHumanResourceState(humanList,taskSchedules,0)
      expected: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] = List(
        (h1,ResourceStateEnum.Occupied),
        (h2,ResourceStateEnum.Ready),
        (h3,ResourceStateEnum.Occupied),
      )
    }yield assert(expected === result)
  }

  test("setHumanResourceState with a list of occupied human resources returns a list of some available human resources "){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("2")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task3 = for {
      taskId2   <- TaskId.from("TSK_3")
      time2     <- Time.from("5")
      r2 <-  resourceType2
    } yield Task(taskId2, time2, List(r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("4")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val humanr1 = for {
      id      <- HumanId.from("HRS_1")
      name    <- HumanName.from("Antonio")
      r1      <-  resourceType1
      r2      <-  resourceType2
    } yield HumanResource(id,name,List(r1,r2))

    val humanr2 = for {
      id      <- HumanId.from("HRS_2")
      name    <- HumanName.from("Maria")
      r2      <-  resourceType2
    } yield HumanResource(id,name,List(r2))

    val humanr3 = for {
      id      <- HumanId.from("HRS_3")
      name    <- HumanName.from("Joao")
      r3      <-  resourceType3
    } yield HumanResource(id,name,List(r3))

    for {
      t1 <- task1
      t3 <- task3
      t5 <- task5
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      pr1 <- ProductNumber.from(1)
      h1 <- humanr1
      h2 <- humanr2
      h3 <- humanr3

      s1 <- StartTime.from(0)
      s2 <- StartTime.from(2)
      e1 <- EndTime.from(t1.time.to + s1.to)
      e2 <- EndTime.from(t3.time.to + s2.to)
      e3 <- EndTime.from(t5.time.to + s1.to)
      humanList = List(
        (h1,ResourceStateEnum.Occupied),
        (h2,ResourceStateEnum.Ready),
        (h3,ResourceStateEnum.Occupied),
      )

      taskSchedules = List(
        TaskSchedule(o1, pr1, t1.taskId, s1, e1, List(), List(h1)),
        TaskSchedule(o1, pr1, t3.taskId, s2, e2, List(), List(h2)),
        TaskSchedule(o2, pr1, t5.taskId, s1, e3, List(), List(h3))
      )

      result: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] = MS03SchedulerV1.setHumanResourceState(humanList,taskSchedules,2)
      expected: List[(HumanResource, ResourceStateEnum.ResourceStateEnum)] = List(
        (h1,ResourceStateEnum.Ready),
        (h2,ResourceStateEnum.Occupied),
        (h3,ResourceStateEnum.Occupied),
      )
    }yield assert(expected === result)
  }

  test("isBetterSolution should return true since actualSolution is Empty"){
    for {
      order1 <- OrderId.from("ORD_1")
      order2 <- OrderId.from("ORD_2")
      productNumber1 <- ProductNumber.from(1)

      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("2")
      r1 <-  ResourceType.from("PRST 1")
      task1 = Task(taskId1, time1, List(r1))

      taskId2 <- TaskId.from("TSK_2")
      time2 <- Time.from("3")
      r1 <-  ResourceType.from("PRST 1")
      task2 = Task(taskId2, time2, List(r1))

      actualSolution = List()
      newSolution = List(
        (order1, productNumber1, task1, List(), List()),
        (order2, productNumber1, task2, List(), List())
      )
      ordersInitiated = List()

      expected = true
      result = MS03Scheduler.isBetterSolution(ordersInitiated, actualSolution, newSolution)
    } yield assert(result == expected)
  }

  test("isBetterSolution should return true since actualSolution has less elements"){
    for {
      order1 <- OrderId.from("ORD_1")
      order2 <- OrderId.from("ORD_2")
      productNumber1 <- ProductNumber.from(1)

      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("2")
      r1 <-  ResourceType.from("PRST 1")
      task1 = Task(taskId1, time1, List(r1))

      taskId2 <- TaskId.from("TSK_2")
      time2 <- Time.from("3")
      r1 <-  ResourceType.from("PRST 1")
      task2 = Task(taskId2, time2, List(r1))

      actualSolution = List(
        (order1, productNumber1, task1, List(), List())
      )
      newSolution = List(
        (order1, productNumber1, task1, List(), List()),
        (order2, productNumber1, task2, List(), List())
      )
      ordersInitiated = List()

      expected = true
      result = MS03Scheduler.isBetterSolution(ordersInitiated, actualSolution, newSolution)
    } yield assert(result == expected)

  }

  test("isBetterSolution should return false since actualSolution has more elements"){
    for {
      order1 <- OrderId.from("ORD_1")
      order2 <- OrderId.from("ORD_2")
      productNumber1 <- ProductNumber.from(1)

      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("2")
      r1 <-  ResourceType.from("PRST 1")
      task1 = Task(taskId1, time1, List(r1))

      taskId2 <- TaskId.from("TSK_2")
      time2 <- Time.from("3")
      r1 <-  ResourceType.from("PRST 1")
      task2 = Task(taskId2, time2, List(r1))

      actualSolution = List(
        (order1, productNumber1, task1, List(), List()),
        (order2, productNumber1, task2, List(), List())
      )
      newSolution = List(
        (order1, productNumber1, task1, List(), List())
      )
      ordersInitiated = List()

      expected = false
      result = MS03Scheduler.isBetterSolution(ordersInitiated, actualSolution, newSolution)
    } yield assert(result == expected)

  }

  test("isBetterSolution should return false since actualSolution has the same elements has newSolution"){
    for {
      order1 <- OrderId.from("ORD_1")
      order2 <- OrderId.from("ORD_2")
      productNumber1 <- ProductNumber.from(1)

      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("2")
      r1 <-  ResourceType.from("PRST 1")
      task1 = Task(taskId1, time1, List(r1))

      taskId2 <- TaskId.from("TSK_2")
      time2 <- Time.from("3")
      r1 <-  ResourceType.from("PRST 1")
      task2 = Task(taskId2, time2, List(r1))

      actualSolution = List(
        (order1, productNumber1, task1, List(), List()),
        (order2, productNumber1, task2, List(), List())
      )
      newSolution = List(
        (order1, productNumber1, task1, List(), List()),
        (order2, productNumber1, task2, List(), List())
      )
      ordersInitiated = List()

      expected = false
      result = MS03Scheduler.isBetterSolution(ordersInitiated, actualSolution, newSolution)
    } yield assert(result == expected)

  }

  test("isBetterSolution should return false since actualSolution has the same orderids but smaller product numbers"){
    for {
      order1 <- OrderId.from("ORD_1")
      order2 <- OrderId.from("ORD_2")
      productNumber1 <- ProductNumber.from(1)
      productNumber2 <- ProductNumber.from(2)

      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("2")
      r1 <-  ResourceType.from("PRST 1")
      task1 = Task(taskId1, time1, List(r1))

      taskId2 <- TaskId.from("TSK_2")
      time2 <- Time.from("3")
      r1 <-  ResourceType.from("PRST 1")
      task2 = Task(taskId2, time2, List(r1))

      actualSolution = List(
        (order1, productNumber1, task1, List(), List()),
        (order2, productNumber1, task2, List(), List())
      )
      newSolution = List(
        (order1, productNumber1, task1, List(), List()),
        (order2, productNumber2, task2, List(), List())
      )
      ordersInitiated = List()

      expected = false
      result = MS03Scheduler.isBetterSolution(ordersInitiated, actualSolution, newSolution)
    } yield assert(result == expected)

  }

  test("isBetterSolution should return true since actualSolution has bigger orderids"){
    for {
      order1 <- OrderId.from("ORD_1")
      order2 <- OrderId.from("ORD_2")
      order3 <- OrderId.from("ORD_3")
      productNumber1 <- ProductNumber.from(1)

      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("2")
      r1 <-  ResourceType.from("PRST 1")
      task1 = Task(taskId1, time1, List(r1))

      taskId2 <- TaskId.from("TSK_2")
      time2 <- Time.from("3")
      r1 <-  ResourceType.from("PRST 1")
      task2 = Task(taskId2, time2, List(r1))

      actualSolution = List(
        (order1, productNumber1, task1, List(), List()),
        (order3, productNumber1, task2, List(), List())
      )
      newSolution = List(
        (order1, productNumber1, task1, List(), List()),
        (order2, productNumber1, task2, List(), List())
      )
      ordersInitiated = List()

      expected = true
      result = MS03Scheduler.isBetterSolution(ordersInitiated, actualSolution, newSolution)
    } yield assert(result == expected)

  }

  test("isBetterSolution should return false since actualSolution has smaller orderids"){
    for {
      order1 <- OrderId.from("ORD_1")
      order2 <- OrderId.from("ORD_2")
      order3 <- OrderId.from("ORD_3")
      productNumber1 <- ProductNumber.from(1)

      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("2")
      r1 <-  ResourceType.from("PRST 1")
      task1 = Task(taskId1, time1, List(r1))

      taskId2 <- TaskId.from("TSK_2")
      time2 <- Time.from("3")
      r1 <-  ResourceType.from("PRST 1")
      task2 = Task(taskId2, time2, List(r1))

      actualSolution = List(
        (order1, productNumber1, task1, List(), List()),
        (order3, productNumber1, task2, List(), List())
      )
      newSolution = List(
        (order2, productNumber1, task1, List(), List()),
        (order3, productNumber1, task2, List(), List())
      )
      ordersInitiated = List()

      expected = false
      result = MS03Scheduler.isBetterSolution(ordersInitiated, actualSolution, newSolution)
    } yield assert(result == expected)

  }

  test("filterTasks should return the same list"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1))

    val human2 = for {
      humanId <- HumanId.from("HRS_2")
      name <- HumanName.from("Maria")
      r2 <- resourceType2
    } yield HumanResource(humanId,name,List(r2))

    val human3 = for {
      humanId <- HumanId.from("HRS_3")
      name <- HumanName.from("Joao")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val human4 = for {
      humanId <- HumanId.from("HRS_4")
      name <- HumanName.from("Manuel")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    val physicalr4 = for {
      id <- PhysicalId.from("PRS_4")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      h1 <- human1
      h2 <- human2
      h3 <- human3
      h4 <- human4
      humanResources = List(h1, h2, h3, h4)
      t1 <- task1
      t5 <- task5

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3
      pr4 <- physicalr4
      physicals = List(pr1, pr2, pr3, pr4)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")

      productNumber1 <- ProductNumber.from(1)
      productNumber2 <- ProductNumber.from(2)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber2, t5),
        (orderId2, productNumber1, t5),
      )

      result04: List[(OrderId, ProductNumber, Task)] = MS03Scheduler.filterTasks(tasks, humanResources, physicals)
      expected04: List[(OrderId, ProductNumber, Task)]  = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber2, t5),
        (orderId2, productNumber1, t5),
      )

    }yield assert(expected04 == result04 && expected04.diff(result04).size == 0 && expected04.intersect(result04).size == expected04.size)

  }

  test("filterTasks should return list with less elements since there are not enough Human resources"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1))

    val human2 = for {
      humanId <- HumanId.from("HRS_2")
      name <- HumanName.from("Maria")
      r2 <- resourceType2
    } yield HumanResource(humanId,name,List(r2))

    val human3 = for {
      humanId <- HumanId.from("HRS_3")
      name <- HumanName.from("Joao")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      h1 <- human1
      h2 <- human2
      h3 <- human3
      humanResources = List(h1, h2, h3)
      t1 <- task1
      t5 <- task5

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3

      physicals = List(pr1,pr2,pr3)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")

      productNumber1 <- ProductNumber.from(1)
      productNumber2 <- ProductNumber.from(2)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber2, t5),
        (orderId2, productNumber1, t5),
      )

      result04: List[(OrderId, ProductNumber, Task)] = MS03Scheduler.filterTasks(tasks, humanResources, physicals)
      expected04: List[(OrderId, ProductNumber, Task)]  = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber2, t5),
      )

    }yield assert(expected04 == result04 && expected04.diff(result04).size == 0 && expected04.intersect(result04).size == expected04.size)
  }

  test("filterTasks should return list with less elements since there are not enough Physical resources"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1))

    val human2 = for {
      humanId <- HumanId.from("HRS_2")
      name <- HumanName.from("Maria")
      r2 <- resourceType2
    } yield HumanResource(humanId,name,List(r2))

    val human3 = for {
      humanId <- HumanId.from("HRS_3")
      name <- HumanName.from("Joao")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val human4 = for {
      humanId <- HumanId.from("HRS_4")
      name <- HumanName.from("Manuel")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      h1 <- human1
      h2 <- human2
      h3 <- human3
      h4 <- human4
      humanResources = List(h1, h2, h3, h4)
      t1 <- task1
      t5 <- task5

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3
      physicals = List(pr1, pr2, pr3)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")

      productNumber1 <- ProductNumber.from(1)
      productNumber2 <- ProductNumber.from(2)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber1, t5),
        (orderId2, productNumber2, t5),
      )

      result04: List[(OrderId, ProductNumber, Task)] = MS03Scheduler.filterTasks(tasks, humanResources, physicals)
      expected04: List[(OrderId, ProductNumber, Task)]  = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber1, t5),
      )

    }yield assert(expected04 == result04 && expected04.diff(result04).size == 0 && expected04.intersect(result04).size == expected04.size)

  }

  test("combinationPossible should true"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    val physicalr4 = for {
      id <- PhysicalId.from("PRS_4")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      t1 <- task1
      t5 <- task5

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3
      pr4 <- physicalr4
      physicals = List(pr1, pr2, pr3, pr4)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")

      productNumber1 <- ProductNumber.from(1)
      productNumber2 <- ProductNumber.from(2)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber2, t5),
        (orderId2, productNumber1, t5),
      )

      result04: Boolean = MS03Scheduler.combinationPossible(tasks, physicals)
      expected04: Boolean = true

    }yield assert(expected04 == result04)

  }

  test("combinationPossible should false since there are not enough Physical resources"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      t1 <- task1
      t5 <- task5

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3
      physicals = List(pr1, pr2, pr3)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")

      productNumber1 <- ProductNumber.from(1)
      productNumber2 <- ProductNumber.from(2)

      tasks: List[(OrderId, ProductNumber, Task)] = List(
        (orderId1, productNumber1, t1),
        (orderId2, productNumber1, t5),
        (orderId2, productNumber2, t5),
      )

      result04: Boolean = MS03Scheduler.combinationPossible(tasks, physicals)
      expected04: Boolean = false

    }yield assert(expected04 == result04)

  }

  test("createTasksSchedules should create the TasksSchedules of each tasksInitiated"){
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    val resourceType3 = ResourceType.from("PRST 3")

    val task1 = for {
      taskId1   <- TaskId.from("TSK_1")
      time1     <- Time.from("100")
      r1 <-  resourceType1
      r2 <- resourceType2
    } yield Task(taskId1, time1, List(r1,r2))

    val task5 = for {
      taskId2   <- TaskId.from("TSK_5")
      time2     <- Time.from("100")
      r3 <-  resourceType3
    } yield Task(taskId2, time2, List(r3))

    val human1 = for {
      humanId <- HumanId.from("HRS_1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1))

    val human2 = for {
      humanId <- HumanId.from("HRS_2")
      name <- HumanName.from("Maria")
      r2 <- resourceType2
    } yield HumanResource(humanId,name,List(r2))

    val human3 = for {
      humanId <- HumanId.from("HRS_3")
      name <- HumanName.from("Joao")
      r1 <- resourceType1
      r3 <- resourceType3
    } yield HumanResource(humanId,name,List(r1,r3))

    val physicalr1 = for {
      id <- PhysicalId.from("PRS_1")
      r1 <- resourceType1
    } yield PhysicalResource(id,r1)

    val physicalr2 = for {
      id <- PhysicalId.from("PRS_2")
      r2 <- resourceType2
    } yield PhysicalResource(id,r2)

    val physicalr3 = for {
      id <- PhysicalId.from("PRS_3")
      r3 <- resourceType3
    } yield PhysicalResource(id,r3)

    for {
      h1 <- human1
      h2 <- human2
      h3 <- human3
      humanResources = List(h1, h2, h3)

      t1 <- task1
      t5 <- task5

      pr1 <- physicalr1
      pr2 <- physicalr2
      pr3 <- physicalr3
      physicals = List(pr1,pr2,pr3)

      orderId1 <- OrderId.from("ORD_1")
      orderId2 <- OrderId.from("ORD_2")

      productNumber1 <- ProductNumber.from(1)
      sTime <- StartTime.from(10)
      endTimeT1 <- EndTime.from(sTime.to + t1.time.to)
      endTimeT5 <- EndTime.from(sTime.to + t5.time.to)

      tasksInitiated: List[(OrderId, ProductNumber, Task, List[HumanResource], List[PhysicalResource])] = List(
        (orderId1, productNumber1, t1, List(h1, h2), List(pr1, pr2)),
        (orderId2, productNumber1, t5, List(h3), List(pr3)),
      )

      result04: Result[List[TaskSchedule]] = MS03Scheduler.createTasksSchedules(tasksInitiated, sTime.to)
      expected04: Result[List[TaskSchedule]] = Right(List(
        TaskSchedule(orderId1, productNumber1, t1.taskId, sTime, endTimeT1, List(pr1, pr2), List(h1, h2)),
        TaskSchedule(orderId2, productNumber1, t5.taskId, sTime, endTimeT5, List(pr3), List(h3)),

      ))

    }yield assert(expected04 === result04)
  }
