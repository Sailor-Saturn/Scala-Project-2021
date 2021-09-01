package domain
import org.scalatest.funsuite.AnyFunSuite
import scala.language.adhocExtensions
import domain.DomainError.*


class ResourceTypeTests extends AnyFunSuite:
  test("Resource type PRST 1 should be valid") {
    val s = "PRST 1"
    val result = ResourceType.from(s)
    
    assert(result.fold(de => false,m => m.to == s))
  }

  test("Resource type PRST 0 should be valid") {
    val s = "PRST 0"
    val result = ResourceType.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }

  test("Resource type PRST_0 should be valid") {
    val s = "PRST_0"
    val result = ResourceType.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }

  test("Resource type dassfds should be invalid") {
    val s = "dassfds"
    val result = ResourceType.from(s)

    assert(result.fold(de => de == InvalidResourceType(s),_ => false))
  }

  test("Resource type prst 200 should be invalid") {
    val s = "prst 200"
    val result = ResourceType.from(s)

    assert(result.fold(de => de == InvalidResourceType(s),_ => false))
  }

  test("Resource type PRST100 should be invalid") {
    val s = "PRST100"
    val result = ResourceType.from(s)

    assert(result.fold(de => de == InvalidResourceType(s),_ => false))
  }

  test("Resource type PRST 1222 should be valid") {
    val s = "PRST 1222"
    val result = ResourceType.from(s)

    assert(result.fold(de => false,m => m.to == s))
  }

  test("Resource type PRST  should be invalid") {
    val s = "PRST "
    val result = ResourceType.from(s)

    assert(result.fold(de => de == InvalidResourceType(s),_ => false))
  }

  test("Empty Resource type should be invalid") {
    val s = ""
    val result = ResourceType.from(s)

    assert(result.fold(de => de == InvalidResourceType(s),_ => false))
  }