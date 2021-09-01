package domain

import domain.SimpleTypes.ProductId

import scala.xml.Node
import domain.{HumanResource, Order, PhysicalResource, Product, ResourceType, Task}
import xml.XML.{fromAttribute, fromNode, traverse}

final case class Production(tasks: List[Task],
                            products: List[Product],
                            orders: List[Order],
                            physicalResources: List[PhysicalResource],
                            humanResources: List[HumanResource],
                            resourceTypes: List[ResourceType])

object Production:

  /**
   * Creates Production
   * 
   * @param xml XML Node
   * @return Result[Production] - If it is a valid Prodution then returns it otherwise returns the respective domain error
   */
  def from(xml: Node): Result[Production] = 
    for {
      physicalResources <- traverse( (xml \\ "Physical"), PhysicalResource.from )
      resourceTypes <- traverse( (xml \\ "Physical"), ResourceType.from )
      humanResources <- traverse( (xml \\ "Human"), HumanResource.from )
      tasks <- traverse( (xml \\ "Task"), Task.from(resourceTypes.distinct) ) 
      products <- traverse( (xml \\ "Product"), Product.from(tasks) )
      orders <- traverse( (xml \\ "Order"), Order.from(products) ) 
      
    } yield Production(tasks, products, orders, physicalResources, humanResources, resourceTypes.distinct)
