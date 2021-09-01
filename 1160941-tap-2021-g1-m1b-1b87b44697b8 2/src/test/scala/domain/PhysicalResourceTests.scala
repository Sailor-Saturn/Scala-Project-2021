package domain

import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions
import domain.DomainError.*
import domain.PhysicalResource
import domain.SimpleTypes.*

class PhysicalResourceTests extends AnyFunSuite:
  test("PhysicalResource should be created with no errors"){
    val xml = scala.xml.XML.loadString("<Physical id=\"PRS_1\" type=\"PRST 1\"/>")
    for {
      id <- PhysicalId.from("PRS_1")
      rType <- ResourceType.from("PRST 1")
      result <- PhysicalResource.from(xml)
    } yield assert(result.physicalId === id && result.resourceType === rType)

    val result = PhysicalResource.from(xml)
    assert(result.isRight)
  }

  test("PhysicalResource should be created with errors (Not valid ResourceType)"){
    val xml = scala.xml.XML.loadString("<Physical id=\"PRS_1\" type=\"PRST2\"/>")
    for {
      id <- PhysicalId.from("PRS_1")
      rType <- ResourceType.from("PRST2")
      result <- PhysicalResource.from(xml)
    } yield assert(result.physicalId === id && result.resourceType != rType)

    val result = PhysicalResource.from(xml)
    assert(result.isLeft)
    assert(result.fold(error => error == InvalidResourceType("PRST2"), _ => false))
  }
  
  test("PhysicalResource should be created with error on Physical Id"){
    val xml = scala.xml.XML.loadString("<Physical id=\"PRS1\" type=\"PRST 1\"/>")
    for {
      id <- PhysicalId.from("PRS1")
      rType <- ResourceType.from("PRST 1")
      result <- PhysicalResource.from(xml)
    } yield assert(result.physicalId != id && result.resourceType === rType)
    
    val result = PhysicalResource.from(xml)
    assert(result.isLeft)
    assert(result.fold(error => error == InvalidPhysicalId("PRS1"), _ => false))
  }

