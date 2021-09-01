package models
import domain.SimpleTypes.{EndTime, OrderId, ProductId, ProductName, ProductNumber, Quantity, StartTime, TaskId, Time}
import domain.{Order, Product, Task, TaskSchedule}
import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions

class TaskStateModelListTests extends AnyFunSuite:
  test("getAllOrdersTask should return tasks of each order with product number 1") {
    val task1 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task3 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
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
      l: List[Order] = List(o1, o2)
      result = TaskStateModelList.getAllOrdersTasks(l)
      expected = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.NotStarted),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )
    } yield assert(result === expected)
  }

  test("getAllOrdersTask should return tasks of each order with product number 2") {
    val task1 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task3 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
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
      quantity <- Quantity.from("2")
      p2 <- product2
    } yield Order(id, p2, quantity)

    for {
      p1 <- ProductNumber.from(1)
      p2 <- ProductNumber.from(2)
      o1 <- order1
      o2 <- order2
      t1 <- task1
      t2 <- task2
      t3 <- task3
      l: List[Order] = List(o1, o2)
      result = TaskStateModelList.getAllOrdersTasks(l)
      expected = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.NotStarted),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted)))),
        (o2.id, p2, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )
    } yield assert(result === expected)
  }

  test("getAllOrdersTask should return tasks of both orders that have more than one product") {
    val task1 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task3 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
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
      quantity <- Quantity.from("2")
      p1 <- product1
    } yield Order(id, p1, quantity)

    val order2 = for {
      id <- OrderId.from("ORD_2")
      quantity <- Quantity.from("2")
      p2 <- product2
    } yield Order(id, p2, quantity)

    for {
      p1 <- ProductNumber.from(1)
      p2 <- ProductNumber.from(2)
      o1 <- order1
      o2 <- order2
      t1 <- task1
      t2 <- task2
      t3 <- task3
      l: List[Order] = List(o1, o2)
      result = TaskStateModelList.getAllOrdersTasks(l)
      expected = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.NotStarted),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o1.id, p2, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.NotStarted),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted)))),
        (o2.id, p2, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )
    } yield assert(result === expected)
  }

  test("allTasksProcessed should return true"){
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

    for {
      p1 <- ProductNumber.from(1)
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      orderInfo = List(
        (o1, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.Processed),TaskStateModel(t2, TaskStateEnum.Processed)))),
        (o2, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.Processed))))
      )

      result = TaskStateModelList.allTasksProcessed(orderInfo)
      expected = true
    } yield assert(result === expected)
  }

  test("allTasksProcessed should return false since contains tasks Not Started"){
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

    for {
      p1 <- ProductNumber.from(1)
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      orderInfo = List(
        (o1, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.Processed),TaskStateModel(t2, TaskStateEnum.Processed)))),
        (o2, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )

      result = TaskStateModelList.allTasksProcessed(orderInfo)
      expected = false
    } yield assert(result === expected)

  }

  test("allTasksProcessed should return false since contains tasks in process"){
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

    for {
      p1 <- ProductNumber.from(1)
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      orderInfo = List(
        (o1, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.Processed),TaskStateModel(t2, TaskStateEnum.Processed)))),
        (o2, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.InProcess))))
      )

      result = TaskStateModelList.allTasksProcessed(orderInfo)
      expected = false
    } yield assert(result === expected)

  }

  test("allTasksProcessed should return false since contains tasks Not Started and in process"){
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

    for {
      p1 <- ProductNumber.from(1)
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      orderInfo = List(
        (o1, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.InProcess),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.Processed))))
      )

      result = TaskStateModelList.allTasksProcessed(orderInfo)
      expected = false
    } yield assert(result === expected)

  }

  test("getOrdersTasksToProcess returns the first task of two orders"){
    val task1 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId1 <- TaskId.from("TSK_2")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task3 = for {
      taskId1 <- TaskId.from("TSK_3")
      time1 <- Time.from("100")
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
      quantity <- Quantity.from("2")
      p1 <- product1
    } yield Order(id, p1, quantity)

    val order2 = for {
      id <- OrderId.from("ORD_2")
      quantity <- Quantity.from("2")
      p2 <- product2
    } yield Order(id, p2, quantity)

    for {
      p1 <- ProductNumber.from(1)
      p2 <- ProductNumber.from(2)
      o1 <- order1
      o2 <- order2
      t1 <- task1
      t2 <- task2
      t3 <- task3
      l: List[Order] = List(o1, o2)
      orderInfo = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.NotStarted),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o1.id, p2, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.NotStarted),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted)))),
        (o2.id, p2, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )

      result = TaskStateModelList.getOrdersTasksToProcess(orderInfo)
      expected = List(
        (o1.id, p1, t1),
        (o1.id, p2, t1),
        (o2.id, p1, t3),
        (o2.id, p2, t3)
      )
    } yield assert(result === expected)
  }

  test("getOrdersTasksToProcess returns the second task of two orders due to the first task is in started state"){
    val task1 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId1 <- TaskId.from("TSK_2")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task3 = for {
      taskId1 <- TaskId.from("TSK_3")
      time1 <- Time.from("100")
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
      quantity <- Quantity.from("2")
      p1 <- product1
    } yield Order(id, p1, quantity)

    val order2 = for {
      id <- OrderId.from("ORD_2")
      quantity <- Quantity.from("2")
      p2 <- product2
    } yield Order(id, p2, quantity)

    for {
      p1 <- ProductNumber.from(1)
      p2 <- ProductNumber.from(2)
      o1 <- order1
      o2 <- order2
      t1 <- task1
      t2 <- task2
      t3 <- task3
      l: List[Order] = List(o1, o2)
      orderInfo = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.InProcess),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o1.id, p2, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.InProcess),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.InProcess)))),
        (o2.id, p2, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )

      result = TaskStateModelList.getOrdersTasksToProcess(orderInfo)
      expected = List(
        (o2.id, p2, t3)
      )
    } yield assert(result === expected)
  }

  test("getOrdersTasksToProcess returns the second task of one orders due to the first task is in started state and one order is already finished"){
    val task1 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId1 <- TaskId.from("TSK_2")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task3 = for {
      taskId1 <- TaskId.from("TSK_3")
      time1 <- Time.from("100")
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
      quantity <- Quantity.from("2")
      p1 <- product1
    } yield Order(id, p1, quantity)

    val order2 = for {
      id <- OrderId.from("ORD_2")
      quantity <- Quantity.from("2")
      p2 <- product2
    } yield Order(id, p2, quantity)

    for {
      p1 <- ProductNumber.from(1)
      p2 <- ProductNumber.from(2)
      o1 <- order1
      o2 <- order2
      t1 <- task1
      t2 <- task2
      t3 <- task3
      l: List[Order] = List(o1, o2)
      orderInfo = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.Processed),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o1.id, p2, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.InProcess),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.Processed)))),
        (o2.id, p2, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.InProcess))))
      )

      result = TaskStateModelList.getOrdersTasksToProcess(orderInfo)
      expected = List(
        (o1.id, p1, t2)
      )
    } yield assert(result === expected)
  }

  test("getOrdersTasksToProcess returns empty list since there are no tasks in not started state"){
    val task1 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task2 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
    } yield Task(taskId1, time1, List())

    val task3 = for {
      taskId1 <- TaskId.from("TSK_1")
      time1 <- Time.from("100")
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
      quantity <- Quantity.from("2")
      p1 <- product1
    } yield Order(id, p1, quantity)

    val order2 = for {
      id <- OrderId.from("ORD_2")
      quantity <- Quantity.from("2")
      p2 <- product2
    } yield Order(id, p2, quantity)

    for {
      p1 <- ProductNumber.from(1)
      p2 <- ProductNumber.from(2)
      o1 <- order1
      o2 <- order2
      t1 <- task1
      t2 <- task2
      t3 <- task3
      l: List[Order] = List(o1, o2)
      orderInfo = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.Processed),TaskStateModel(t2, TaskStateEnum.InProcess)))),
        (o1.id, p2, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.Processed),TaskStateModel(t2, TaskStateEnum.InProcess)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.InProcess)))),
        (o2.id, p2, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.InProcess))))
      )

      result = TaskStateModelList.getOrdersTasksToProcess(orderInfo)
      expected = List()
    } yield assert(result === expected)
  }

  test("getOrdersTasksInProcess should return empty list since there are no tasks In Process"){
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

    for {
      p1 <- ProductNumber.from(1)
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      orderInfo = List(
        (o1, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.NotStarted),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.Processed))))
      )

      result = TaskStateModelList.getOrdersTasksInProcess(orderInfo)
      expected = List()
    } yield assert(result === expected)

  }

  test("getOrdersTasksInProcess should return list elements since there are tasks In Process"){
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

    for {
      p1 <- ProductNumber.from(1)
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      o3 <- OrderId.from("ORD_3")
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      orderInfo = List(
        (o1, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.NotStarted),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.InProcess)))),
        (o3, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.InProcess),TaskStateModel(t2, TaskStateEnum.NotStarted))))
      )

      result = TaskStateModelList.getOrdersTasksInProcess(orderInfo)
      expected = List(
        (o2, p1, t3),
        (o3, p1, t1)
      )
    } yield assert(result === expected)

  }

  test("getOrdersTasksProcessed should return empty list since there are no tasks Processed"){
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

    for {
      p1 <- ProductNumber.from(1)
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      orderInfo = List(
        (o1, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.NotStarted),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.InProcess))))
      )

      result = TaskStateModelList.getOrdersTasksProcessed(orderInfo)
      expected = List()
    } yield assert(result === expected)

  }

  test("getOrdersTasksProcessed should return list elements since there are tasks Processed"){
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

    for {
      p1 <- ProductNumber.from(1)
      o1 <- OrderId.from("ORD_1")
      o2 <- OrderId.from("ORD_2")
      o3 <- OrderId.from("ORD_3")
      t1 <- task1
      t2 <- task2
      t3 <- task3
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      orderInfo = List(
        (o1, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.NotStarted),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.Processed)))),
        (o3, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.Processed),TaskStateModel(t2, TaskStateEnum.NotStarted))))
      )

      result = TaskStateModelList.getOrdersTasksProcessed(orderInfo)
      expected = List(
        (o2, p1, t3),
        (o3, p1, t1)
      )
    } yield assert(result === expected)

  }

  test("setTaskState - only 1 TaskSchedule started "){
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
      orderInfo = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.NotStarted),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )
      taskSchedules = List(TaskSchedule(o1.id, p1, t1.taskId, s1, e1, List(), List()))

      result = TaskStateModelList.setTaskState(orderInfo, taskSchedules, s1.to)
      expected = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.InProcess),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )
    } yield assert(result === expected)
  }

  test("setTaskState - 1 TaskSchedule ended and 1 started different orders"){
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
      s2 <- StartTime.from(e1.to)
      e2 <- EndTime.from(t3.time.to)
      orderInfo = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.InProcess),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )
      taskSchedules = List(
        TaskSchedule(o1.id, p1, t1.taskId, s1, e1, List(), List()),
        TaskSchedule(o2.id, p1, t3.taskId, s2, e2, List(), List())
      )

      result = TaskStateModelList.setTaskState(orderInfo, taskSchedules, e1.to)
      expected = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.Processed),TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.InProcess))))
      )
    } yield assert(result === expected)
  }

  test("setTaskState - 1 TaskSchedule ended and 1 started same order") {
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
      s2 <- StartTime.from(e1.to)
      e2 <- EndTime.from(t2.time.to)
      orderInfo = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.InProcess), TaskStateModel(t2, TaskStateEnum.NotStarted)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )
      taskSchedules = List(
        TaskSchedule(o1.id, p1, t1.taskId, s1, e1, List(), List()),
        TaskSchedule(o1.id, p1, t2.taskId, s2, e2, List(), List())
      )

      result = TaskStateModelList.setTaskState(orderInfo, taskSchedules, e1.to)
      expected = List(
        (o1.id, p1, TaskStateModelList(List(TaskStateModel(t1, TaskStateEnum.Processed), TaskStateModel(t2, TaskStateEnum.InProcess)))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )
    } yield assert(result === expected)
  }

  test("setTaskState - 1 TaskSchedule ended and 1 started same order and one keept the same state"){
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

    val task4 = for {
      taskId1 <- TaskId.from("TSK_4")
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
      t4 <- task4
      s1 <- StartTime.from(0)
      e1 <- EndTime.from(t1.time.to)
      s2 <- StartTime.from(e1.to)
      e2 <- EndTime.from(t2.time.to)
      orderInfo = List(
        (o1.id, p1, TaskStateModelList(List(
          TaskStateModel(t1, TaskStateEnum.InProcess),
          TaskStateModel(t2, TaskStateEnum.NotStarted),
          TaskStateModel(t4, TaskStateEnum.NotStarted)
        ))),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )
      taskSchedules = List(
        TaskSchedule(o1.id, p1, t1.taskId, s1, e1, List(), List()),
        TaskSchedule(o1.id, p1, t2.taskId, s2, e2, List(), List())
      )

      result = TaskStateModelList.setTaskState(orderInfo, taskSchedules, e1.to)
      expected = List(
        (o1.id, p1, TaskStateModelList(List(
          TaskStateModel(t1, TaskStateEnum.Processed),
          TaskStateModel(t2, TaskStateEnum.InProcess),
          TaskStateModel(t4, TaskStateEnum.NotStarted)))
        ),
        (o2.id, p1, TaskStateModelList(List(TaskStateModel(t3, TaskStateEnum.NotStarted))))
      )
    } yield assert(result === expected)
  }
