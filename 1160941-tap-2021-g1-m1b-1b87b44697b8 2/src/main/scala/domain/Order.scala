package domain

import domain.SimpleTypes.{OrderId, ProductId, Quantity}
import domain.DomainError.ProductDoesNotExist
import scala.xml.Node
import xml.XML.fromAttribute

final case class Order(id: OrderId, product: Product, quantity: Quantity)

object Order:

  /**
   * Creates Order
   * 
   * @param lp Existing Products - To validate if the Order references a valid product
   * @param xml XML Node
   * @return Result[Order] - If it is a valid Order then returns it otherwise returns the respective domain error
   */
  def from(lp: List[Product])(xml: Node): Result[Order] =
    for {
      sId       <- fromAttribute(xml, "id")
      id        <- OrderId.from(sId)
      sPrdref   <- fromAttribute(xml, "prdref")
      prdref    <- ProductId.from(sPrdref)
      p         <- lp.find(p => p.productId == prdref).fold(Left(ProductDoesNotExist(sPrdref)))(p => Right(p))
      sQuantity <- fromAttribute(xml, "quantity")
      quantity  <- Quantity.from(sQuantity)
    } yield Order(id, p, quantity)