package domain
import scala.xml.Node
import domain.DomainError.*
import xml.XML.*

opaque type ResourceType = String

object ResourceType:
  
  /**
   * Validates if the String is a valid ResourceType
   * 
   * @param s String
   * @return Result[ResourceType] - If it is a valid Resource Type then returns it otherwise returns InvalidResourceType
   */
  def from(s: String): Result[ResourceType] =
    val regex = "PRST(_| )[0-9]+".r
    if(regex.matches(s)) Right(s) else Left(InvalidResourceType(s))

  /**
   * Creates ResourceType
   * 
   * @param n XML Node
   * @return Result[ResourceType] - If it is a valid then return it otherwise return InvalidResourceType
   */
  def from(n: Node): Result[ResourceType] =
    for
      sType <- fromAttribute(n, "type")
      finalType <-  from(sType)
    yield(finalType)

  /**
   * Finds ResourceType in the list of valid ResourceType
   * 
   * @param validTypes Valid ResourceType
   * @param n XML Node
   * @return Result[ResourceType] - If ResourceType is in the list validTypes then returns it, otherwise returns the domain error TaskUsesNonExistentPRT 
   */
  def from(validTypes: List[ResourceType])(n: Node): Result[ResourceType] =
    for
      sType <- fromAttribute(n, "type")
      finalType <- from(sType).fold(error => Left(error), value => {
        validTypes.find(resourceType => resourceType == sType).fold(Left(TaskUsesNonExistentPRT(sType)))(resourceType => Right(resourceType))
      })
    yield finalType
  
  extension (resourceType: ResourceType)
    def to: String = resourceType


