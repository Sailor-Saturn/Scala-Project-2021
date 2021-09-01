

import domain.schedule.secundaryscheduler.MS03SchedulerV1
import io.FileIO.*
import xml.XML.*
import domain.{PhysicalResource, Production, ResourceType, Result, SimpleTypes}

val productionElem = load("C:/Users/rute_/OneDrive/Documentos/TAP/TAP-2021-G1-M1B/files/assessment/ms03/testFiles/validAgenda_04_in.xml")
val productionResult: Result[List[Production]] = productionElem.flatMap(xml => traverse( (xml \\ "Production"), Production.from ))
val production = productionResult.toOption.get.head

val humanResources = production.humanResources
val tasks = production.orders.map(_.product.processes).flatten

val result04: List[(domain.Task, List[domain.HumanResource])] = MS03SchedulerV1.assignHumanResourceToTasks(tasks, humanResources)
val expected04: List[(domain.Task, List[domain.HumanResource])] = List(
  (tasks(0), List(humanResources.filter(_.humanId.to == "HRS_3")).flatten),
  (tasks(1), List(humanResources.filter(_.humanId.to == "HRS_2")).flatten),
  (tasks(2), List(humanResources.filter(_.humanId.to == "HRS_1")).flatten)
)
assert(expected04.intersect(result04).size == expected04.size)
assert(expected04.diff(result04).size == 0)
assert(expected04 == result04)