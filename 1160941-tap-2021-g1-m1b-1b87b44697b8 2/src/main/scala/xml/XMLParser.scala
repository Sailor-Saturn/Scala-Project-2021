package xml

import domain.{HumanResource, PhysicalResource, TaskSchedule}
import scala.xml.Elem

object XMLParser:

  def serializeError(value: String): Elem =
//    val result = <ScheduleError xmlns="http://www.dei.isep.ipp.pt/tap-2021"
//                                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
//                                xsi:schemaLocation="http://www.dei.isep.ipp.pt/tap-2021 ../../scheduleError.xsd "
//                                message={ value }/>
    val result = <ScheduleError message={ value }/>

    result

  def serialize(schedule: List[TaskSchedule]): Elem =
    val taskSchedules = schedule.map(taskSchedule => serializeTaskSchedule(taskSchedule))
    val result = <Schedule xmlns="http://www.dei.isep.ipp.pt/tap-2021" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.dei.isep.ipp.pt/tap-2021 ../../schedule.xsd ">
      { taskSchedules }
    </Schedule>

    result

  private def serializeTaskSchedule(taskSchedule: TaskSchedule): Elem =
    val result = <TaskSchedule order={ taskSchedule.order.to } productNumber={ taskSchedule.productNumber.toString } task={ taskSchedule.taskId.to } start={ taskSchedule.start.toString } end={ taskSchedule.end.toString }>
      { serializePhysicalResources(taskSchedule.physicalResources) }
      { serializeHumanResources(taskSchedule.humanResources) }
    </TaskSchedule>

    result

  private def serializePhysicalResources(list: List[PhysicalResource]): Elem =
    val physicals = list.map(physicalResource => <Physical id={ physicalResource.physicalId.to } />)
    val result = <PhysicalResources>
      { physicals }
    </PhysicalResources>

    result

  private def serializeHumanResources(list: List[HumanResource]): Elem =
    val humans = list.map(humanResource => <Human name={ humanResource.name.to } />)
    val result = <HumanResources>
      { humans }
    </HumanResources>

    result

