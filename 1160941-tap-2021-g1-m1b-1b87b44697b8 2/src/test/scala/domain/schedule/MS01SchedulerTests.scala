package domain.schedule

import domain.DomainError.ResourceUnavailable
import domain.Production
import domain.schedule.MS01Scheduler
import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions

class MS01SchedulerTests extends AnyFunSuite:

  test("MS01Scheduler with success"){
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
          <Human id="HRS_2" name="Maria">
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

    val schedule = for {
      production <- Production.from(xml)
    } yield MS01Scheduler.scheduleProduction(production)  
    
    assert(schedule.isRight)
    assert(schedule.fold(_ => false, schedule => schedule.fold(_ => false, list => (list(0).order.to === "ORD_1" && list(0).taskId.to === "TSK_1"))))
  }
  
  test("MS01Scheduler with error on human resources"){
    val xml = scala.xml.XML.loadString({
        <Production xmlns="http://www.dei.isep.ipp.pt/tap-2021" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.dei.isep.ipp.pt/tap-2021 ../../production.xsd ">
          <PhysicalResources>
            <Physical id="PRS_1" type="PRST 1"/>
            <Physical id="PRS_2" type="PRST 2"/>
            <Physical id="PRS_3" type="PRST 3"/>
            <Physical id="PRS_4" type="PRST 4"/>
            <Physical id="PRS_5" type="PRST 5"/>
          </PhysicalResources>
          <Tasks>
            <Task id="TSK_1" time="100">
              <PhysicalResource type="PRST 1"/>
            </Task>
            <Task id="TSK_2" time="80">
              <PhysicalResource type="PRST 3"/>
            </Task>
            <Task id="TSK_3" time="60">
              <PhysicalResource type="PRST 1"/>
              <PhysicalResource type="PRST 3"/>
              <PhysicalResource type="PRST 5"/>
            </Task>
          </Tasks>
          <HumanResources>
            <Human id="HRS_1" name="Antonio">
              <Handles type="PRST 1"/>
              <Handles type="PRST 2"/>
            </Human>
            <Human id="HRS_2" name="Maria">
              <Handles type="PRST 3"/>
              <Handles type="PRST 4"/>
              <Handles type="PRST 5"/>
            </Human>
          </HumanResources>
          <Products>
            <Product id="PRD_1" name="Product 1">
              <Process tskref="TSK_1"/>
              <Process tskref="TSK_2"/>
            </Product>
            <Product id="PRD_2" name="Product 2">
              <Process tskref="TSK_3"/>
            </Product>
          </Products>
          <Orders>
            <Order id="ORD_2" prdref="PRD_2" quantity="1"/>
          </Orders>
        </Production>}.toString)

    val schedule = for {
      production <- Production.from(xml)
    } yield MS01Scheduler.scheduleProduction(production)

    assert(schedule.fold(error => false, schedule => schedule.fold(error => error === ResourceUnavailable("TSK_3,PRST 5"), _ => false)))
  }
  
  test("MS01Scheduler with error on physical resources"){
    val xml = scala.xml.XML.loadString({
      <Production xmlns="http://www.dei.isep.ipp.pt/tap-2021" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.dei.isep.ipp.pt/tap-2021 ../../production.xsd ">
        <PhysicalResources>
          <Physical id="PRS_1" type="PRST 1"/>
          <Physical id="PRS_2" type="PRST 2"/>
          <Physical id="PRS_3" type="PRST 3"/>
          <Physical id="PRS_4" type="PRST 4"/>
          <Physical id="PRS_5" type="PRST 5"/>
        </PhysicalResources>
        <Tasks>
          <Task id="TSK_1" time="100">
            <PhysicalResource type="PRST 1"/>
          </Task>
          <Task id="TSK_2" time="80">
            <PhysicalResource type="PRST 3"/>
          </Task>
          <Task id="TSK_3" time="60">
            <PhysicalResource type="PRST 1"/>
            <PhysicalResource type="PRST 1"/>
          </Task>
        </Tasks>
        <HumanResources>
          <Human id="HRS_1" name="Antonio">
            <Handles type="PRST 1"/>
            <Handles type="PRST 2"/>
          </Human>
          <Human id="HRS_2" name="Maria">
            <Handles type="PRST 3"/>
            <Handles type="PRST 4"/>
            <Handles type="PRST 5"/>
          </Human>
        </HumanResources>
        <Products>
          <Product id="PRD_1" name="Product 1">
            <Process tskref="TSK_1"/>
            <Process tskref="TSK_2"/>
          </Product>
          <Product id="PRD_2" name="Product 2">
            <Process tskref="TSK_3"/>
          </Product>
        </Products>
        <Orders>
          <Order id="ORD_2" prdref="PRD_2" quantity="1"/>
        </Orders>
        </Production>}.toString)

    val schedule = for {
      production <- Production.from(xml)
    } yield MS01Scheduler.scheduleProduction(production)

    assert(schedule.fold(error => false, schedule => schedule.fold(error => error === ResourceUnavailable("TSK_3,PRST 1"), _ => false)))
  }