package domain

import domain.DomainError
import domain.SimpleTypes.*
import scala.xml.Node
import xml.XML.{fromAttribute, traverse}

final case class HumanResource(humanId: HumanId, name: HumanName, resourceTypes: List[ResourceType])


object HumanResource:

  /**
   * Creates HumanResource
   * 
   * @param xml XML Node
   * @return Result[Task] - If it is a valid Task then returns it otherwise returns the respective domain error
   */
  def from(xml: Node): Result[HumanResource] =
    for
      stId  <-  fromAttribute(xml, "id")
      tId   <-  HumanId.from(stId)
      shn   <-  fromAttribute(xml, "name")
      hn    <-  HumanName.from(shn)
      pr    <-  traverse((xml \\ "Handles"), ResourceType.from)
    yield HumanResource(tId,hn,pr)
