package domain

import domain.SimpleTypes.{HumanId, HumanName}
import domain.DomainError.{InvalidHumanId, InvalidHumanName}
import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions

class HumanResourceTests extends AnyFunSuite:
  test("HumanResource should be created with no errors"){
    val xml = scala.xml.XML.loadString({
      <Human id="HRS_1" name="Antonio">
        <Handles type="PRST 1"/>
        <Handles type="PRST 2"/>
      </Human>
    }.toString)
      
    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    for {
      id      <- HumanId.from("HRS_1")
      name    <- HumanName.from("Antonio")
      r1      <-  resourceType1
      r2      <-  resourceType2
      result  <- HumanResource.from(xml)
    } yield assert(result.humanId === id && result.name === name && result.resourceTypes === List(r1, r2))
      
    val result = for {
      result <- HumanResource.from(xml)
    } yield result
    assert(result.isRight)
  }

  test("HumanResource should be created with errors on HumanId") {
    val xml = scala.xml.XML.loadString({
      <Human id="HRS1" name="Antonio">
        <Handles type="PRST 1"/>
        <Handles type="PRST 2"/>
      </Human>
    }.toString)

    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    for {
      id <- HumanId.from("HRS1")
      name <- HumanName.from("Antonio")
      r1 <- resourceType1
      r2 <- resourceType2
      result <- HumanResource.from(xml)
    } yield assert(result.humanId != id && result.name === name && result.resourceTypes === List(r1, r2))

    val result = for {
      result <- HumanResource.from(xml)
    } yield result
    assert(result.isLeft)
    assert(result.fold(error => error === InvalidHumanId("HRS1"), _ => false))
  }

  test("HumanResource should be created with errors on HumanName") {
    val xml = scala.xml.XML.loadString({
      <Human id="HRS_1" name="">
        <Handles type="PRST 1"/>
        <Handles type="PRST 2"/>
      </Human>
    }.toString)

    val resourceType1 = ResourceType.from("PRST 1")
    val resourceType2 = ResourceType.from("PRST 2")
    for {
      id <- HumanId.from("HRS_1")
      name <- HumanName.from("")
      r1 <- resourceType1
      r2 <- resourceType2
      result <- HumanResource.from(xml)
    } yield assert(result.humanId === id && result.name != name && result.resourceTypes === List(r1, r2))

    val result = for {
      result <- HumanResource.from(xml)
    } yield result
    assert(result.isLeft)
  }