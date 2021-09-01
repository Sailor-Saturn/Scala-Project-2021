package domain

import domain.SimpleTypes.{OrderId, ProductId, ProductName, Quantity}
import domain.DomainError.{InvalidOrderId, InvalidProductId, ProductDoesNotExist, InvalidQuantity, InvalidQuantityConversion}
import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions

class OrderTests extends AnyFunSuite:
  test("Order should be created with no errors"){
    val xml = scala.xml.XML.loadString("<Order id=\"ORD_1\" prdref=\"PRD_1\" quantity=\"1\"/>")

    val product1 = for {
      id     <- ProductId.from("PRD_1")
      name  <- ProductName.from("Product 1")
    } yield Product(id, name, List())
    
    val product2 = for {
      id     <- ProductId.from("PRD_2")
      name  <- ProductName.from("Product 2")
    } yield Product(id, name, List())
    
    for {
      id <- OrderId.from("ORD_1")
      product <- ProductId.from("PRD_1")
      quantity <- Quantity.from("1")
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield assert(result.id === id && result.product.productId === product && result.quantity === quantity)

    val result = for {
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield result
    assert(result.isRight)
  }

  test("Order should be created with errors on OrderId"){
    val xml = scala.xml.XML.loadString("<Order id=\"ORD1\" prdref=\"PRD_1\" quantity=\"1\"/>")

    val product1 = for {
      id     <- ProductId.from("PRD_1")
      name  <- ProductName.from("Product 1")
    } yield Product(id, name, List())

    val product2 = for {
      id     <- ProductId.from("PRD_2")
      name  <- ProductName.from("Product 2")
    } yield Product(id, name, List())
    
    for {
      id <- OrderId.from("ORD1")
      product <- ProductId.from("PRD_1")
      quantity <- Quantity.from("1")
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield assert(result.id != id && result.product.productId === product && result.quantity === quantity)

    val result = for {
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield result
    assert(result.isLeft)
    assert(result.fold(error => error == InvalidOrderId("ORD1"), _ => false))
  }
  
  test("Order should be created with errors on Product"){
    val xml = scala.xml.XML.loadString("<Order id=\"ORD_1\" prdref=\"PRD1\" quantity=\"1\"/>")

    val product1 = for {
      id     <- ProductId.from("PRD_1")
      name  <- ProductName.from("Product 1")
    } yield Product(id, name, List())

    val product2 = for {
      id     <- ProductId.from("PRD_2")
      name  <- ProductName.from("Product 2")
    } yield Product(id, name, List())
    
    for {
      id <- OrderId.from("ORD_1")
      product <- ProductId.from("PRD1")
      quantity <- Quantity.from("1")
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield assert(result.id === id && result.product.productId != product && result.quantity === quantity)

    val result = for {
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield result
    assert(result.isLeft)
    assert(result.fold(error => error == InvalidProductId("PRD1"), _ => false))
  }

  test("Order should be created with errors on Product (Refers a non existent product)"){
    val xml = scala.xml.XML.loadString("<Order id=\"ORD_1\" prdref=\"PRD_1\" quantity=\"1\"/>")

    val product1 = for {
      id     <- ProductId.from("PRD_2")
      name  <- ProductName.from("Product 2")
    } yield Product(id, name, List())

    val product2 = for {
      id     <- ProductId.from("PRD_5")
      name  <- ProductName.from("Product 5")
    } yield Product(id, name, List())

    for {
      id <- OrderId.from("ORD_1")
      product <- ProductId.from("PRD_1")
      quantity <- Quantity.from("1")
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield assert(result.id === id && result.product.productId != product && result.quantity === quantity)

    val result = for {
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield result
    assert(result.isLeft)
    assert(result.fold(error => error == ProductDoesNotExist("PRD_1"), _ => false))
  }

  test("Order should be created with errors on Quantity"){
    val xml = scala.xml.XML.loadString("<Order id=\"ORD_1\" prdref=\"PRD_1\" quantity=\"0\"/>")

    val product1 = for {
      id     <- ProductId.from("PRD_1")
      name  <- ProductName.from("Product 1")
    } yield Product(id, name, List())

    val product2 = for {
      id     <- ProductId.from("PRD_2")
      name  <- ProductName.from("Product 2")
    } yield Product(id, name, List())
    
    for {
      id <- OrderId.from("ORD_1")
      product <- ProductId.from("PRD_1")
      quantity <- Quantity.from("0")
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield assert(result.id === id && result.product.productId === product && result.quantity != quantity)

    val result = for {
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield result
    assert(result.fold(error => error == InvalidQuantity(0), _ => false))
  }

  test("Order should be created with errors on Quantity Conversion"){
    val xml = scala.xml.XML.loadString("<Order id=\"ORD_1\" prdref=\"PRD_1\" quantity=\"a\"/>")

    val product1 = for {
      id     <- ProductId.from("PRD_1")
      name  <- ProductName.from("Product 1")
    } yield Product(id, name, List())

    val product2 = for {
      id     <- ProductId.from("PRD_2")
      name  <- ProductName.from("Product 2")
    } yield Product(id, name, List())
    
    for {
      id <- OrderId.from("ORD_1")
      product <- ProductId.from("PRD_1")
      quantity <- Quantity.from("a")
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield assert(result.id === id && result.product.productId === product && result.quantity != quantity)
    
    val result = for {
      p1 <-  product1
      p2 <-  product2
      result <- Order.from(List(p1, p2))(xml)
    } yield result
    assert(result.isLeft)
    assert(result.fold(error => error == InvalidQuantityConversion("a"), _ => false))
  }


