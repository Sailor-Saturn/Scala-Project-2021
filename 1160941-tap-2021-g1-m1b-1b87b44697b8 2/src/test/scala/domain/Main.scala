package domain
import domain.schedule.MS01Scheduler
import property.{Generators,ScheduleService}
object Main:
  def main(args: Array[String]) =
    val result = for {
      production <- Generators.productionGenerator.sample
    } yield MS01Scheduler.scheduleProduction(production)
    ScheduleService.properties(0)._2.check()
    println(result)