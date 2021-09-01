package domain
import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions
import domain.DomainError.*
import domain.SimpleTypes.*

class SimpleTypesTests extends AnyFunSuite:
  /* Order */
  test("Order Id ORD_1 should be valid") {
    val s = "ORD_1"
    val result = OrderId.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }

  test("Order Id ORD_100 should be valid") {
    val s = "ORD_100"
    val result = OrderId.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }

  test("Order Id 100 should be invalid") {
    val s = "100"
    val result = OrderId.from(s)

    assert(result.fold(de => de == InvalidOrderId(s),m => false))
  }

  test("Order Id ORD 100 should be valid") {
    val s = "ORD 100"
    val result = OrderId.from(s)

    assert(result.fold(de => false,m =>  m.to == s))
  }

  test("Order Id ORD_0 should be valid") {
    val s = "ORD_0"
    val result = OrderId.from(s)

    assert(result.fold(de => false,m =>  m.to == s))
  }

  test("Order Id ord_10 should be invalid") {
    val s = "ord_10"
    val result = OrderId.from(s)

    assert(result.fold(de => de == InvalidOrderId(s),m => false))
  }

  test("Order Id ORD_ should be invalid") {
    val s = "ORD_"
    val result = OrderId.from(s)

    assert(result.fold(de => de == InvalidOrderId(s), m => false))
  }

  test("Empty Order Id should be invalid") {
    val s = ""
    val result = OrderId.from(s)

    assert(result.fold(de => de == InvalidOrderId(s), m => false))
  }

  test("Quantity positive integer should be valid") {
    val value = 123
    val result = Quantity.from(value)

    assert(result.fold(_ => false, quantity => quantity.to == value))
  }

  test("Quantity negative integer should be invalid") {
    val value = -456
    val result = Quantity.from(value)

    assert(result.fold(error => error == InvalidQuantity(value), _ => false))
  }

  test("Quantity zero should be invalid") {
    val value = 0
    val result = Quantity.from(value)

    assert(result.fold(error => error == InvalidQuantity(value), _ => false))
  }
  
  test("Quantity positive integer on string should be valid") {
    val value = 123
    val result = Quantity.from(value.toString())

    assert(result.fold(_ => false, quantity => quantity.to == value))
  }

  test("Quantity negative integer on string should be invalid") {
    val value = -456
    val result = Quantity.from(value.toString())

    assert(result.fold(error => error == InvalidQuantity(value), _ => false))
  }

  test("Quantity zero on string should be invalid") {
    val value = 0
    val result = Quantity.from(value.toString())

    assert(result.fold(error => error == InvalidQuantity(value), _ => false))
  }

  test("Quantity a should be invalid") {
    val value = "a"
    val result = Quantity.from(value)

    assert(result.fold(error => error == InvalidQuantityConversion(value), _ => false))
  }

  /*HumanResource*/
  test("Human Resource Id HRS_1 should be valid") {
    val s = "HRS_1"
    val result = HumanId.from(s)

    assert(result.fold(_ => false, humanId => humanId.to == s))
  }

  test("Human Resource Id HRS_100 should be valid") {
    val s = "HRS_100"
    val result = HumanId.from(s)

    assert(result.fold(_ => false, humanId => humanId.to == s))
  }

  test("Human Resource Id 100 should be invalid") {
    val s = "100"
    val result = HumanId.from(s)

    assert(result.fold(error => error == InvalidHumanId(s), _ => false))
  }

  test("Human Resource Id HRS 100 should be valid") {
    val s = "HRS 100"
    val result = HumanId.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }

  test("Human Resource Id HRS_0 should be valid") {
    val s = "HRS_0"
    val result = HumanId.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }

  test("Human Resource Id Empty should be invalid") {
    val s = ""
    val result = HumanId.from(s)

    assert(result.fold(error => error == InvalidHumanId(s), _ => false))
  }

  test("Human Resource name Luis should be valid") {
    val s = "Luis"
    val result = HumanName.from(s)

    assert(result.fold(_ => false, humanName => humanName.to == s))
  }

  test("Human Resource empty name should be invalid") {
    val s = ""
    val result = HumanName.from(s)

    assert(result.fold(error => error == InvalidHumanName(s), _ => false))
  }

  test("Human Resource null name should be invalidvalid") {
    val s = null
    val result = HumanName.from(s)

    assert(result.fold(error => error == InvalidHumanName(s), _ => false))
  }

  /* Product */
  test("Product Id PRD_1 should be valid") {
    val s = "PRD_1"
    val result = ProductId.from(s)

    assert(result.fold(_ => false, productId => productId.to == s))
  }

  test("Product Id PRD_100 should be valid") {
    val s = "PRD_100"
    val result = ProductId.from(s)

    assert(result.fold(_ => false, productId => productId.to == s))
  }

  test("Product Id 100 should be invalid") {
    val s = "100"
    val result = ProductId.from(s)

    assert(result.fold(error => error == InvalidProductId(s), _ => false))
  }

  test("Product Id PRD 100 should be valid") {
    val s = "PRD 100"
    val result = ProductId.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }

  test("Product Id PRD_0 should be valid") {
    val s = "PRD_0"
    val result = ProductId.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }

  test("Product Id prd_10 should be invalid") {
    val s = "prd_10"
    val result = ProductId.from(s)

    assert(result.fold(error => error == InvalidProductId(s), _ => false))
  }

  test("Product Id PRD_ should be invalid") {
    val s = "PRD_"
    val result = ProductId.from(s)

    assert(result.fold(error => error == InvalidProductId(s), _ => false))
  }

  test("Empty Product Id should be invalid") {
    val s = ""
    val result = ProductId.from(s)

    assert(result.fold(error => error == InvalidProductId(s), _ => false))
  }

  test("Product Name product 1 should be valid") {
    val s = "product 1"
    val result = ProductName.from(s)

    assert(result.fold(_ => false, productName => productName.to == s))
  }
  
  test("Empty Product Name should be invalid") {
    val s = ""
    val result = ProductName.from(s)

    assert(result.fold(error => error == InvalidProductName(s), _ => false))
  }

  test("Null Product Name should be invalid") {
    val s = null
    val result = ProductName.from(s)

    assert(result.fold(error => error == InvalidProductName(s), _ => false))
  }

  /* Physical Resource*/
  test("Physical Id PRS_1 should be valid") {
    val s = "PRS_1"
    val result = PhysicalId.from(s)

    assert(result.fold(_ => false, physicalId => physicalId.to == s))
  }

  test("Physical Id PRS_100 should be valid") {
    val s = "PRS_100"
    val result = PhysicalId.from(s)

    assert(result.fold(_ => false, physicalId => physicalId.to == s))
  }

  test("Physical Id 100 should be invalid") {
    val s = "100"
    val result = PhysicalId.from(s)

    assert(result.fold(error => error == InvalidPhysicalId(s), _ => false))
  }

  test("Physical Id PRS 100 should be valid") {
    val s = "PRS 100"
    val result = PhysicalId.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }

  test("Physical Id PRS_0 should be valid") {
    val s = "PRS_0"
    val result = PhysicalId.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }

  test("Physical Id prd_10 should be invalid") {
    val s = "prd_10"
    val result = PhysicalId.from(s)

    assert(result.fold(error => error == InvalidPhysicalId(s), _ => false))
  }

  test("Physical Id PRS_ should be invalid") {
    val s = "PRS_"
    val result = PhysicalId.from(s)

    assert(result.fold(error => error == InvalidPhysicalId(s), _ => false))
  }

  test("Empty Physical Id should be invalid") {
    val s = ""
    val result = PhysicalId.from(s)

    assert(result.fold(error => error == InvalidPhysicalId(s), _ => false))
  }

  /* TaskSchedule */
  test("ProductNumber positive integer should be valid") {
    val value = 123
    val result = ProductNumber.from(value)

    assert(result.fold(_ => false, productNumber => productNumber.to == value))
    assert(result.fold(_ => false, productNumber => productNumber.toString == value.toString))
  }

  test("ProductNumber negative integer should be invalid") {
    val value = -456
    val result = ProductNumber.from(value)

    assert(result.fold(error => error == InvalidProductNumber(value), _ => false))
  }
  
  test("ProductNumber zero should be invalid") {
    val value = 0
    val result = ProductNumber.from(value)

    assert(result.fold(error => error == InvalidProductNumber(value), _ => false))
  }

  test("StartTime positive integer should be valid") {
    val value = 123
    val result = StartTime.from(value)

    assert(result.fold(_ => false, startTime => startTime.to == value))
    assert(result.fold(_ => false, startTime => startTime.toString == value.toString))
  }

  test("StartTime negative integer should be invalid") {
    val value = -123
    val result = StartTime.from(value)

    assert(result.fold(error => error == InvalidStartTime(value), _ => false))
  }

  test("StartTime zero should be valid") {
    val value = 0
    val result = StartTime.from(value)

    assert(result.fold(_ => false, startTime => startTime.to == value))
    assert(result.fold(_ => false, startTime => startTime.toString == value.toString))
  }

  test("EndTime positive integer should be valid") {
    val value = 123
    val result = EndTime.from(value)

    assert(result.fold(_ => false, endTime => endTime.to == value))
    assert(result.fold(_ => false, startTime => startTime.toString == value.toString))
  }

  test("EndTime negative integer should be invalid") {
    val value = -123
    val result = EndTime.from(value)

    assert(result.fold(error => error == InvalidEndTime(value), _ => false))
  }

  test("EndTime zero should be invalid") {
    val value = 0
    val result = EndTime.from(value)

    assert(result.fold(error => error == InvalidEndTime(value), _ => false))
  }

  /*Task*/
  test("Task Id TSK_100 should be valid") {
    val s = "TSK_100"
    val result = TaskId.from(s)

    assert(result.fold(_ => false, v => v.to == s))
  }

  test("Task Id TSK_2 should be valid") {
    val s = "TSK_2"
    val result = TaskId.from(s)

    assert(result.fold(_ => false, v => v.to == s))
  }

  test("Task Id TSK_ should be invalid") {
    val s = "TSK_"
    val result = TaskId.from(s)

    assert(result.fold(error => error == InvalidTaskId(s), _ => false))
  }

  test("Task Id TSK 100 should be valid") {
    val s = "TSK 100"
    val result = TaskId.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }
  
  test("Task Id 100 should be invalid") {
    val s = "100"
    val result = TaskId.from(s)

    assert(result.fold(error => error == InvalidTaskId(s), _ => false))
  }

  test("Task Id Empty should be invalid") {
    val s = ""
    val result = TaskId.from(s)

    assert(result.fold(error => error == InvalidTaskId(s), _ => false))
  }
  
  test("Time 100 should be valid") {
    val s = "100"
    val result = Time.from(s)

    assert(result.fold(_ => false, v => v.to == 100))
  }

  test("Time -100 should be invalid") {
    val s = "-100"
    val result = Time.from(s)

    assert(result.fold(error => error == InvalidTime(-100), _ => false))
  }

  test("Time 0 should be invalid") {
    val s = "0"
    val result = Time.from(s)

    assert(result.fold(error => error == InvalidTime(0), _ => false))
  }
  
  test("Time a should be invalid") {
  val s = "a"
  val result = Time.from(s)

  assert(result.fold(error => error == InvalidTimeConversion(s), _ => false))
}
