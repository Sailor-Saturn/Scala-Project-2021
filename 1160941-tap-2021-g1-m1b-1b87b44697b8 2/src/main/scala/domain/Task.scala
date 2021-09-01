package domain
import domain.DomainError.*
import domain.SimpleTypes.*
import scala.xml.Node
import xml.XML.*

final case class Task(taskId: TaskId, time: Time, resourceTypes: List[ResourceType])

object Task:

  /**
   * Creates Task
   * 
   * @param validTypes Valid ResourceType - To validate if the task references existing Resources Types
   * @param xml XML Node
   * @return Result[Task] - If it is a valid Task then returns it otherwise returns the respective domain error
   */
  def from(validTypes: List[ResourceType])(xml: Node): Result[Task] =
    for
      stId  <- fromAttribute(xml,"id")
      tId   <- TaskId.from(stId)
      sTime <- fromAttribute(xml,"time")
      t     <- Time.from(sTime)
      pr    <- traverse((xml \\ "PhysicalResource"), ResourceType.from(validTypes))
    yield Task(tId,t,pr)

  /**
   * Finds the Task in the list of existing Tasks
   * 
   * @param existingTasks Existing Tasks
   * @param xml XML Node
   * @return Result[Task] - If Task is in the list of existing tasks then returns it, otherwise returns the domain error TaskDoesNotExist 
   */
  def findTask(existingTasks: List[Task])(xml: Node): Result[Task] =
    for
      stId  <- fromAttribute(xml, "tskref")
      task  <- TaskId.from(stId)fold(error => Left(error), value => {
        existingTasks.find(taskElement => taskElement.taskId.to == stId).fold(Left(TaskDoesNotExist(stId)))(taskElement => Right(taskElement))
      }) 
    yield task
