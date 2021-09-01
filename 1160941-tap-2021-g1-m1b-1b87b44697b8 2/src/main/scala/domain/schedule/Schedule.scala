package domain.schedule

import scala.xml.Elem
import domain.Result

trait Schedule:
  /**
   * Creates a Production Schedule
   * 
   * @param xml XML Elem
   * @return Result[Elem]
   */
  def create(xml: Elem): Result[Elem]
