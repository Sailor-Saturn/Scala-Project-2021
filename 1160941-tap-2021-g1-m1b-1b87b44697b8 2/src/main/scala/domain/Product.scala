package domain

import domain.SimpleTypes.{ProductId, ProductName}
import scala.xml.Node
import xml.XML.{fromAttribute, traverse}

final case class Product(productId: ProductId, productName: ProductName, processes: List[Task])

object Product:

  /**
   * Creates Product
   * 
   * @param existingTasks existing Tasks - To validate if the Product references existing Tasks
   * @param xml XML Node
   * @return Result[Product] - If it is a valid Product then returns it otherwise returns the respective domain error
   */
  def from(existingTasks: List[Task])(xml: Node): Result[Product] =
    for {
      sId       <- fromAttribute(xml, "id")
      id        <- ProductId.from(sId)
      sname     <- fromAttribute(xml, "name")
      name      <- ProductName.from(sname)
      processes <- traverse( (xml \\ "Process"), Task.findTask(existingTasks) )
    } yield Product(id, name, processes)