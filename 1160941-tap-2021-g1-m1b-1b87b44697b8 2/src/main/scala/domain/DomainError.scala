package domain

import scala.xml.Elem

type Result[A] = Either[DomainError,A]

enum DomainError:
  case IOFileProblem(error: String)
  case XMLError(error: String)
  case InvalidResourceType(error: String)
  case InvalidOrderId(error: String)
  case InvalidProductId(error: String)
  case InvalidProductName(error: String)
  case InvalidQuantity(error: Int)
  case InvalidQuantityConversion(error: String)
  case InvalidList(error: String)
  case InvalidHumanId(error: String)
  case InvalidHumanName(error: String)
  case InvalidPhysicalId(error: String)
  case InvalidTaskId(error: String)
  case InvalidTime(error: Int)
  case InvalidProductNumber(error: Int)
  case InvalidStartTime(error: Int)
  case InvalidEndTime(error: Int) 
  case ProductDoesNotExist(error: String)
  case InvalidTimeConversion(error: String)
  case TaskUsesNonExistentPRT(error: String)
  case TaskDoesNotExist(error: String)
  case ResourceUnavailable(error: String)
  case ImpossibleSchedule
  case XMLResultError(error: Elem)
