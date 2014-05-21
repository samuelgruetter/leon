/* Copyright 2009-2014 EPFL, Lausanne */

package leon
package evaluators

import purescala.Trees._
import purescala.Definitions.Program

object RunnerPhase extends LeonPhase[Program, Unit] {
  val name = "Runner"
  val description = "Run function specified as argument"

  def run(ctx: LeonContext)(pgm: Program) = {
    val reporter = ctx.reporter

    println(pgm.asString(ctx))

    ctx.settings.run match {
      case Some(fname) =>

        val found = pgm.modules.flatMap { m => 
          m.definedFunctions.find(_.id.name == fname) match {
            case Some(fd) =>
              Some((m, fd))  
            case None =>
              None
          }
        }
        if (found.isEmpty) {
          reporter.error(s"Function $fname not found!")
        } else {
          val (module, fd) = found.head
          reporter.info(s"running ${fd.id} from ${module.id}")

          if (fd.params.size > 0) {
            reporter.fatalError(s"${fd.id} takes arguments, cannot run.")
          }

          val res = if (fd.annotations("extern")) {
            val se = new ScalacEvaluator(ctx, pgm)
            se.call(fd.typed, Nil)
          } else {
            val e = new DefaultEvaluator(ctx, pgm)
            e.eval(FunctionInvocation(fd.typed, Nil))
          }

          reporter.info(s" => $res")
        }

      case None =>
        ctx.reporter.warning("Nothing to run?!?")
    }
  }
}
