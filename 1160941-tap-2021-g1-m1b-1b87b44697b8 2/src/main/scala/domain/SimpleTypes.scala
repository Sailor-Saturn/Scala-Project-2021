package domain

import domain.DomainError.*

import scala.annotation.targetName
import scala.util.Try

object SimpleTypes:
  // Order
  opaque type OrderId = String
  object OrderId:
    def from(value: String): Result[OrderId] =
      val regex = "ORD(_| )[0-9]+".r
      if(regex.matches(value)) Right(value) else Left(InvalidOrderId(value))
  
  extension (orderId: OrderId)
    @targetName("orderTo")
    def to: String = orderId
  
  opaque type Quantity = Int
  object Quantity:
    def from(value: Int): Result[Quantity] =
      if(value > 0) Right(value) else Left(InvalidQuantity(value))
    def from(value: String): Result[Quantity] =
      Try(value.toInt).fold(_ => Left(InvalidQuantityConversion(value)), i => {
        if(i > 0) Right(i) else Left(InvalidQuantity(i))
      })

  extension (quantity: Quantity)
    @targetName("quantityTo")
    def to: Int = quantity


  // Product
  opaque type ProductId = String
  object ProductId:
    def from(value: String): Result[ProductId] =
      val regex = "PRD(_| )[0-9]+".r
      if(regex.matches(value)) Right(value) else Left(InvalidProductId(value))

  extension (productId: ProductId)
    @targetName("productIdTo")
    def to: String = productId

  opaque type ProductName = String
  object ProductName:
    def from(value: String): Result[ProductName] =
      if(value != null && !value.isEmpty) Right(value) else Left(InvalidProductName(value))

  extension (productName: ProductName)
    @targetName("productNameTo")
    def to: String = productName

  // Human Resource
  opaque type HumanId = String 
  object HumanId:
    def from(value: String): Result[HumanId] =
      val regex = "HRS(_| )[0-9]+".r
      if(regex.matches(value)) Right(value) else Left(InvalidHumanId(value))
  
  extension (humanId: HumanId)
    @targetName("humanIdTo")
    def to: String = humanId

  opaque type HumanName = String
  object HumanName:
    def from(value: String): Result[HumanName] =
      if(value != null && !value.isEmpty) Right(value) else Left(InvalidHumanName(value))

  extension (humanName: HumanName)
    @targetName("humanNameTo")
    def to: String = humanName


  // Physical Resource
  opaque type PhysicalId = String
  object PhysicalId:
    def from(value: String): Result[PhysicalId] =
      val regex = "PRS(_| )[0-9]+".r
      if(regex.matches(value)) Right(value) else Left(InvalidPhysicalId(value))
  
  extension (physicalId: PhysicalId)
    @targetName("PhysicalIdTo")
    def to: String = physicalId
    
  
  // Task
  opaque type TaskId = String
  object TaskId:
    def from(value: String): Result[TaskId] =
      val regex = "TSK(_| )[0-9]+".r
      if(regex.matches(value)) Right(value) else Left(InvalidTaskId(value))

  extension (taskId: TaskId)
    @targetName("taskIdTo")
    def to: String = taskId

  opaque type Time = Int
  object Time:
    def from(time: String): Result[Time] =
      Try(time.toInt).fold(_ => Left(InvalidTimeConversion(time)), result => if(result > 0) Right(result) else Left(InvalidTime(result)))

  extension (time: Time)
    @targetName("timeTo")
    def to: Int = time


  // TaskSchedule
  opaque type ProductNumber = Int
  object ProductNumber:
    def from(value: Int): Result[ProductNumber] =
      if(value > 0) Right(value) else Left(InvalidProductNumber(value))

  extension (productNumber: ProductNumber)
    @targetName("productNumberTo")
    def to: Int = productNumber
    
    @targetName("productNumberToString")
    def toString: String = productNumber.toString

  opaque type StartTime = Int
  object StartTime:
    def from(value: Int): Result[StartTime] =
      if(value >= 0) Right(value) else Left(InvalidStartTime(value))

  extension (startTime: StartTime)
    @targetName("startTimeTo")
    def to: Int = startTime
    
    @targetName("startTimeToString")
    def toString: String = startTime.toString

  opaque type EndTime = Int
  object EndTime:
    def from(value: Int): Result[EndTime] =
    if(value > 0) Right(value) else Left(InvalidEndTime(value))

  extension (endTime: EndTime)
    @targetName("endTimeTo")
    def to: Int = endTime
    
    @targetName("endTimeToString")
    def toString: String = endTime.toString
