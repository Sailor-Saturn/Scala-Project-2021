package assessment

import domain.DomainError
import domain.schedule
import domain.SimpleTypes
import domain.schedule.ScheduleMS03
import io.FileIO
import io.FileIO.load
import xml.XML
import domain.Result
import scala.xml.{Utility}
import scala.xml.Elem

object Main:
  def main(args: Array[String]) =
/*    val fileNameIn = "invalidHumanId_in.xml"
    val fileNameout = "files/assessment/ms01/generated/invalidHumanId_out.xml"
    val productionElem = load("files/assessment/ms01/" + fileNameIn)
      val validResourceTypes = for {
        physicalResources <- productionElem.flatMap(xml => traverse( (xml \\ "Physical"), PhysicalResource.from ))
      } yield physicalResources.map(physicalResource => physicalResource.resourceType).distinct
  
    val result = for {
      xmlElem <- productionElem
      xmlResult <- AssessmentMS01.create(xmlElem)
    } yield save(fileNameout, xmlResult)*/
    val schedule = for {
      xml <- load("files/assessment/ms03/validAgenda_02_in.xml")
      scheduleResult <- ScheduleMS03.create(xml)
      scheduleExpected <- load("files/assessment/ms03/validAgenda_02_out.xml")
    } yield
      println(scheduleResult)
      println(scheduleExpected)
      scheduleResult == scheduleExpected

    
    
    
