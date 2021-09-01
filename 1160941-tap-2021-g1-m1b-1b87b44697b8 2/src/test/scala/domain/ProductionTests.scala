package domain

import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions
import domain.DomainError.*
import domain.Production
import domain.SimpleTypes.*
import xml.XML.traverse

class ProductionTests extends AnyFunSuite:
  test("Production should be created with no errors"){
    val xml = scala.xml.XML.loadString({
      <Production>
        <PhysicalResources>
          <Physical id="PRS_1" type="PRST 1"/>
          <Physical id="PRS_2" type="PRST 2"/>
        </PhysicalResources>
        <Tasks>
          <Task id="TSK_1" time="100">
            <PhysicalResource type="PRST 1"/>
            <PhysicalResource type="PRST 2"/>
          </Task>
        </Tasks>
        <HumanResources>
          <Human id="HRS_1" name="Antonio">
            <Handles type="PRST 1"/>
            <Handles type="PRST 2"/>
          </Human>
        </HumanResources>
        <Products>
          <Product id="PRD_1" name="Product 1">
            <Process tskref="TSK_1"/>
          </Product>
        </Products>
        <Orders>
          <Order id="ORD_1" prdref="PRD_1" quantity="1"/>
        </Orders>
      </Production>}.toString)

    val result = Production.from(xml)
    assert(result.isRight)
  }

  test("Production should be created with errors on Products (invalid task )"){
    val xml = scala.xml.XML.loadString({
      <Production>
        <PhysicalResources>
          <Physical id="PRS_1" type="PRST 1"/>
          <Physical id="PRS_2" type="PRST 2"/>
        </PhysicalResources>
        <Tasks>
          <Task id="TSK_1" time="100">
            <PhysicalResource type="PRST 1"/>
            <PhysicalResource type="PRST 2"/>
          </Task>
        </Tasks>
        <HumanResources>
          <Human id="HRS_1" name="Antonio">
            <Handles type="PRST 1"/>
            <Handles type="PRST 2"/>
          </Human>
        </HumanResources>
        <Products>
          <Product id="PRD_1" name="Product 1">
            <Process tskref="TSK_123"/>
          </Product>
        </Products>
        <Orders>
          <Order id="ORD_1" prdref="PRD_1" quantity="1"/>
        </Orders>
      </Production>}.toString)

    val result = Production.from(xml)
    assert(result.isLeft)
    assert(result.fold(error => error == TaskDoesNotExist("TSK_123"), _ => false))
  }

  test("Production should be created with errors on Tasks (invalid resourceType on task type)"){
    val xml = scala.xml.XML.loadString({
      <Production>
        <PhysicalResources>
          <Physical id="PRS_1" type="PRST 1"/>
          <Physical id="PRS_2" type="PRST 2"/>
        </PhysicalResources>
        <Tasks>
          <Task id="TSK_1" time="100">
            <PhysicalResource type="PRST 123"/>
            <PhysicalResource type="PRST 2"/>
          </Task>
        </Tasks>
        <HumanResources>
          <Human id="HRS_1" name="Antonio">
            <Handles type="PRST 1"/>
            <Handles type="PRST 2"/>
          </Human>
        </HumanResources>
        <Products>
          <Product id="PRD_1" name="Product 1">
            <Process tskref="TSK_1"/>
          </Product>
        </Products>
        <Orders>
          <Order id="ORD_1" prdref="PRD_1" quantity="1"/>
        </Orders>
      </Production>}.toString)      

    val result = Production.from(xml)
    assert(result.isLeft)
    assert(result.fold(error => error == TaskUsesNonExistentPRT("PRST 123"), _ => false))
  }

  test("Production should be created with errors on Order (invalid product)"){
    val xml = scala.xml.XML.loadString({
      <Production>
        <PhysicalResources>
          <Physical id="PRS_1" type="PRST 1"/>
          <Physical id="PRS_2" type="PRST 2"/>
        </PhysicalResources>
        <Tasks>
          <Task id="TSK_1" time="100">
            <PhysicalResource type="PRST 1"/>
            <PhysicalResource type="PRST 2"/>
          </Task>
        </Tasks>
        <HumanResources>
          <Human id="HRS_1" name="Antonio">
            <Handles type="PRST 1"/>
            <Handles type="PRST 2"/>
          </Human>
        </HumanResources>
        <Products>
          <Product id="PRD_1" name="Product 1">
            <Process tskref="TSK_1"/>
          </Product>
        </Products>
        <Orders>
          <Order id="ORD_1" prdref="PRD_456" quantity="1"/>
        </Orders>
      </Production>}.toString)
  
    val result = Production.from(xml)
    assert(result.isLeft)
    assert(result.fold(error => error == ProductDoesNotExist("PRD_456"), _ => false))
  }
