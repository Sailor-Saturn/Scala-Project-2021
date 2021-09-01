package property

import scala.language.adhocExtensions
import org.scalacheck.*
import org.scalacheck.Test.Parameters
import org.scalacheck.Prop.forAll
import domain.SimpleTypes.{HumanId, HumanName, OrderId, PhysicalId, ProductId, ProductName, Quantity, TaskId, Time}
import domain.{HumanResource, Order, PhysicalResource, Product, Production, ResourceType, Task}
import domain.Result
import org.scalacheck.Properties
import scala.annotation.tailrec

object Generators extends Properties("Generators"):

  val MIN_LENGTH_NAME = 10
  val MAX_ID_NUMBER = 100
  val MAX_QUANTITY = 10
  val MIN_DOMAIN_CONCEPTS = 1
  val MAX_LENGTH_NAME = 10
  val MIN_TASK_TIME = 1
  val MAX_TASK_TIME = 100
  val MIN_LIST_SIZE = 1
  val MAX_LIST_SIZE = 20
  val PREFIX_PHYSICALID = "PRS_"
  val PREFIX_RESOURCETYPE = "PRST "
  val PREFIX_HUMANID = "HRS_"
  val PREFIX_TASKID = "TSK_"
  val PREFIX_PRODUCTID = "PRD_"
  val PREFIX_ORDERID = "ORD_"


  // Generic Generators

  /**
   * Generates an ID (prefix and a generated number) of Type A using function getId to convert String into A
   *
   * @param prefix prefix of the ID
   * @param getId  String => Result[A] (normally function from of the SimpleType)
   * @tparam A (SimpleType)
   * @return Gen[A]
   */
  private def idGenerator[A](prefix: String, getId: String => Result[A]): Gen[A] =
    for
      number: Int <- Gen.chooseNum[Int](1, MAX_ID_NUMBER)
      id: A       <- idGenerator(prefix, number, getId)
    yield id

  /**
   * Generates an ID (prefix and number) of Type A using function getId to convert String into A
   *
   * @param prefix prefix of the ID
   * @param number number
   * @param getId String => Result[A] (normally function from of the SimpleType)
   * @tparam A (SimpleType)
   * @return Gen[A]
   */
  private def idGenerator[A](prefix: String, number: Int, getId: String => Result[A]): Gen[A] =
    for
      id: A <- getId(prefix + number).fold(_ => Gen.fail, id => Gen.const(id))
    yield id

  private def nameGenerator[A](getName: String => Result[A]): Gen[A] =
    for
      nameSize  <- Gen.chooseNum(MIN_LENGTH_NAME, MAX_LENGTH_NAME)
      nameChars <- Gen.listOfN(nameSize, Gen.alphaChar)
      name      <- getName(nameChars.mkString).fold(_ => Gen.fail, hn => Gen.const(hn))
    yield name

  private def filteredListGenerator[A](list: List[A]): Gen[List[A]] =
    for
      conceptsSize <- Gen.chooseNum(MIN_DOMAIN_CONCEPTS, list.length)
      concepts     <- Gen.pick(conceptsSize, list)
    yield concepts.toList

  /**
   * Generates a list of A whose size is also generated
   *
   * @param generator Int => Gen[A] (generator of A which receives an Int as param)
   * @tparam A Type of the result
   * @return Gen[List[A]]
   */
  private def listGenerator[A](generator: Int => Gen[A]): Gen[List[A]] =
    for
      size: Int  <- Gen.chooseNum(MIN_LIST_SIZE, MAX_LIST_SIZE)
      list: List[A] <- listGenerator(size, generator)
    yield list

  /**
   * Generates a list of A whose size is received as param (size)
   *
   * @param size size of the generated list
   * @param generator Int => Gen[A]
   * @tparam A Type of the result
   * @return Gen[List[A]]
   */
  private def listGenerator[A](size: Int, generator: Int => Gen[A]): Gen[List[A]] =
    @tailrec
    def recursiveListGenerator(id: Int, resultList: List[Gen[A]]): Gen[List[A]] =
      if(id == size + 1)
        Gen.sequence(resultList)
      else
        val newElement: Gen[A] = generator(id)
        recursiveListGenerator(id + 1, newElement :: resultList)

    recursiveListGenerator(1, List())

  // PhysicalResource Generators
  private def physicalResourceGenerator(id: Int): Gen[PhysicalResource] =
    for
      id <- idGenerator(PREFIX_PHYSICALID, id, value => PhysicalId.from(value))
      rt <- idGenerator(PREFIX_RESOURCETYPE, value => ResourceType.from(value))
    yield PhysicalResource(id, rt)

  private def listPhysicalResourceGenerator: Gen[List[PhysicalResource]] =
    listGenerator(physicalResourceGenerator)

  // HumanResource Generators
  private def humanResourceGenerator(resourceTypesList: List[ResourceType])(id: Int): Gen[HumanResource] =
    for
      id    <- idGenerator(PREFIX_HUMANID, id, value => HumanId.from(value))
      name  <- nameGenerator(value => HumanName.from(value))
    yield HumanResource(id, name, resourceTypesList)

  /**
   * Generates a list of HumanResource without repeated IDs (the size of the list is the same as the size of the ist received as param (resourceTypesList)
   * Each HumanResource has as resourceTypes the list received as param (resourceTypesList)
   * @param resourceTypesList List[ResourceType]
   * @return Gen[List[HumanResource]]
   */
  private def listHumanResourceGenerator(resourceTypesList: List[ResourceType]): Gen[List[HumanResource]] =
    listGenerator(resourceTypesList.size, humanResourceGenerator(resourceTypesList))

  // Task Generators
  private def taskGenerator(resourceTypesList: List[ResourceType])(id: Int): Gen[Task] =
    for
      id      <- idGenerator(PREFIX_TASKID, id, value => TaskId.from(value))
      timeInt <- Gen.chooseNum[Int](MIN_TASK_TIME, MAX_TASK_TIME)
      time    <- Time.from(timeInt.toString).fold(_ => Gen.fail, t => Gen.const(t))
      types   <- filteredListGenerator(resourceTypesList)
    yield Task(id, time, types)

  private def listTaskGenerator(resourceTypesList: List[ResourceType]): Gen[List[Task]] =
    listGenerator(taskGenerator(resourceTypesList))

  // Product Generators
  private def productGenerator(taskList: List[Task])(id: Int): Gen[Product] =
    for
      id    <- idGenerator(PREFIX_PRODUCTID, id, value => ProductId.from(value))
      name  <- nameGenerator(value => ProductName.from(value))
      tasks <- filteredListGenerator(taskList)
    yield Product(id, name, tasks)

  private def listProductGenerator(taskList: List[Task]): Gen[List[Product]] =
    listGenerator(productGenerator(taskList))

  // Order Generators
  private def orderGenerator(productList: List[Product])(id: Int): Gen[Order] =
    for
      id          <- idGenerator(PREFIX_ORDERID, id, value => OrderId.from(value))
      product     <- Gen.oneOf(productList)
      quantityInt <- Gen.chooseNum[Int](1, MAX_QUANTITY)
      quantity    <- Quantity.from(quantityInt).fold(_ => Gen.fail, q => Gen.const(q))
    yield Order(id, product, quantity)

  private def listOrderGenerator(productList: List[Product]): Gen[List[Order]] =
    listGenerator(orderGenerator(productList))

  // Production Generators
  def productionGenerator: Gen[Production] =
    for
      physicals: List[PhysicalResource] <- listPhysicalResourceGenerator
      types: List[ResourceType]         = physicals.map(p => p.resourceType)
      humans: List[HumanResource]       <- listHumanResourceGenerator(types)
      tasks: List[Task]                 <- listTaskGenerator(types)
      products: List[Product]           <- listProductGenerator(tasks)
      orders: List[Order]               <- listOrderGenerator(products)
    yield Production(tasks, products, orders, physicals, humans, types)

  /**
   * Generates a production where there are not enough PhysicalResources to complete a task of an order
   * @return Gen[(Production, TaskId, ResourceType)]
   */
  def productionGeneratorPhysicalResourceUnavailable: Gen[(Production, TaskId, ResourceType)] =
    for
      physicals: List[PhysicalResource] <- listPhysicalResourceGenerator
      types: List[ResourceType]         = physicals.map(p => p.resourceType)
      humans: List[HumanResource]       <- listHumanResourceGenerator(types)
      tasks: List[Task]                 <- listTaskGenerator(types)
      products: List[Product]           <- listProductGenerator(tasks)
      orders: List[Order]               <- listOrderGenerator(products)
      firstTaskToBeScheduled: Task      = orders.head.product.processes.head
      resourceTypeToDrop : ResourceType = firstTaskToBeScheduled.resourceTypes.head
      physicalsError: List[PhysicalResource] = physicals.filterNot(physical => physical.resourceType.to == resourceTypeToDrop.to)
    yield (Production(tasks, products, orders, physicalsError, humans, types), firstTaskToBeScheduled.taskId, resourceTypeToDrop)

  /**
   * Generates a production where there are not enough HumanResources to complete a task of an order
   * @return Gen[(Production, TaskId, ResourceType)]
   */
  def productionGeneratorHumanResourceUnavailable: Gen[(Production, TaskId, ResourceType)] =
    for
      physicals: List[PhysicalResource] <- listPhysicalResourceGenerator
      types: List[ResourceType]         = physicals.map(p => p.resourceType)
      tasks: List[Task]                 <- listTaskGenerator(types)
      products: List[Product]           <- listProductGenerator(tasks)
      orders: List[Order]               <- listOrderGenerator(products)
      firstTaskToBeScheduled: Task      = orders.head.product.processes.head
      resourceTypeToDrop : ResourceType = firstTaskToBeScheduled.resourceTypes.head
      humansTypes: List[ResourceType]   = types.filterNot(resource => resource.to == resourceTypeToDrop.to)
      humansError: List[HumanResource]  <- listHumanResourceGenerator(humansTypes)
    yield (Production(tasks, products, orders, physicals, humansError, types), firstTaskToBeScheduled.taskId, resourceTypeToDrop)

  def listProductionGenerator: Gen[List[Production]] =
    for
      productionSize: Int           <- Gen.chooseNum(MIN_LIST_SIZE, MAX_LIST_SIZE)
      productions: List[Production] <- Gen.listOfN(productionSize, productionGenerator)
    yield productions


  // Properties to validate the generators
  property("1. listPhysicalResourceGenerator should not generate list with repeated ids.") =
    forAll(listPhysicalResourceGenerator)( (physicals: List[PhysicalResource]) =>
        physicals.map(physical => physical.physicalId).toSeq.size == physicals.size
  )

  property("2. listHumanResourceGenerator should not generate list with repeated ids.") =
    forAll(listPhysicalResourceGenerator)(physicals =>
      val types : List[ResourceType] = physicals.map(p => p.resourceType)
      forAll(listHumanResourceGenerator(types))( (humans: List[HumanResource]) =>
        humans.map(human => human.humanId).toSeq.size == humans.size
      )
  )

  property("3. listTaskGenerator should not generate list with repeated ids.") =
    forAll(listPhysicalResourceGenerator)(physicals =>
      val types : List[ResourceType] = physicals.map(p => p.resourceType)
      forAll(listTaskGenerator(types))( (tasks: List[Task]) =>
        tasks.map(task => task.taskId).toSeq.size == tasks.size
      )
    )

  property("4. listProductGenerator should not generate list with repeated ids.") =
    forAll(listPhysicalResourceGenerator)(physicals =>
      val types : List[ResourceType] = physicals.map(p => p.resourceType)
      forAll(listTaskGenerator(types))(tasks =>
        forAll(listProductGenerator(tasks))( (products: List[Product]) =>
          products.map(product => product.productId).toSeq.size == products.size
        )
      )
    )

  property("5. listOrderGenerator should not generate list with repeated ids.") =
    forAll(listPhysicalResourceGenerator)(physicals =>
      val types : List[ResourceType] = physicals.map(p => p.resourceType)
        forAll(listTaskGenerator(types))(tasks =>
        forAll(listProductGenerator(tasks))( (products: List[Product]) =>
          forAll(listOrderGenerator(products))( (orders: List[Order]) =>
            orders.map(order => order.id).toSeq.size == orders.size
          )
        )
      )
    )

  property("6. listPhysicalResourceGenerator should generate list within MIN_LIST_SIZE e MAX_LIST_SIZE.") =
    forAll(listPhysicalResourceGenerator)( physicals =>
      physicals.size <= MAX_LIST_SIZE && physicals.size >= MIN_LIST_SIZE
    )

  property("7. listTaskGenerator should generate list within MIN_LIST_SIZE e MAX_LIST_SIZE.") =
    forAll(listPhysicalResourceGenerator)(physicals =>
      val types : List[ResourceType] = physicals.map(p => p.resourceType)
        forAll(listTaskGenerator(types))( (tasks: List[Task]) =>
          tasks.size <= MAX_LIST_SIZE && tasks.size >= MIN_LIST_SIZE
      )
    )

  property("8. listProductGenerator should generate list within MIN_LIST_SIZE e MAX_LIST_SIZE.") =
    forAll(listPhysicalResourceGenerator)(physicals =>
      val types : List[ResourceType] = physicals.map(p => p.resourceType)
        forAll(listTaskGenerator(types))(tasks =>
        forAll(listProductGenerator(tasks))( (products: List[Product]) =>
          products.size <= MAX_LIST_SIZE && products.size >= MIN_LIST_SIZE
        )
      )
    )

  property("9. listOrderGenerator should generate list within MIN_LIST_SIZE e MAX_LIST_SIZE.") =
    forAll(listPhysicalResourceGenerator)(physicals =>
      val types : List[ResourceType] = physicals.map(p => p.resourceType)
      forAll(listTaskGenerator(types))(tasks =>
        forAll(listProductGenerator(tasks))( (products: List[Product]) =>
          forAll(listOrderGenerator(products))( (orders: List[Order]) =>
            orders.size <= MAX_LIST_SIZE && orders.size >= MIN_LIST_SIZE
          )
        )
      )
    )
  
  property("10. listHumanResourceGenerator should generate a list with the same number of elements of the ResourceType list.") =
    forAll(listPhysicalResourceGenerator)(physicals =>
      val types : List[ResourceType] = physicals.map(p => p.resourceType)
      forAll(listHumanResourceGenerator(types))((humanResources: List[HumanResource]) =>
        humanResources.size == types.size
      )
    )