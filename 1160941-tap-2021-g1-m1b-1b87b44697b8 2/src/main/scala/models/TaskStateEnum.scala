package models

/**
 * State of a Task
 */
object TaskStateEnum extends Enumeration:
  type TaskStateEnum = Value
  val Processed, InProcess, NotStarted = Value
