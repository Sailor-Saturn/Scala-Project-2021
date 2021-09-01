package domain

import domain.SimpleTypes.PhysicalId
import scala.xml.Node
import xml.XML.fromAttribute

final case class PhysicalResource(physicalId: PhysicalId, resourceType: ResourceType)

object PhysicalResource:

  /**
   * Creates PhysicalResource
   * 
   * @param xml XML Node
   * @return Result[PhysicalResource] - If it is a valid PhysicalResource then returns it otherwise returns the respective domain error
   */
  def from(xml: Node): Result[PhysicalResource] =
    for {
      sId   <- fromAttribute(xml, "id")
      id    <- PhysicalId.from(sId)
      sType <- fromAttribute(xml, "type")
      rType <- ResourceType.from(sType)
    } yield PhysicalResource(id, rType)