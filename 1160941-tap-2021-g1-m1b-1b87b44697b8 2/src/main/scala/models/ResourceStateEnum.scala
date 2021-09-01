package models

/**
 * State of a Resource (Human or Physical)
 */
object ResourceStateEnum extends Enumeration:
  type ResourceStateEnum = Value
  val Ready, Occupied = Value
