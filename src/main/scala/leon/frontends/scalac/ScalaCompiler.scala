/* Copyright 2009-2014 EPFL, Lausanne */

package leon
package frontends.scalac

import scala.tools.nsc.{Global,Settings=>NSCSettings}
import scala.tools.nsc.interactive.RangePositions

class ScalaCompiler(settings : NSCSettings, ctx: LeonContext) extends Global(settings, new SimpleReporter(settings, ctx.reporter)) with RangePositions {

  object leonExtraction extends {
    val global: ScalaCompiler.this.type = ScalaCompiler.this
    val runsAfter = List[String]("refchecks")
    val runsRightAfter = None
    val ctx = ScalaCompiler.this.ctx
  } with LeonExtraction

  override protected def computeInternalPhases() : Unit = {
    val phs = List(
      syntaxAnalyzer          -> "parse source into ASTs, perform simple desugaring",
      analyzer.namerFactory   -> "resolve names, attach symbols to named trees",
      analyzer.packageObjects -> "load package objects",
      analyzer.typerFactory   -> "the meat and potatoes: type the trees",
      patmat                  -> "translate match expressions",
      superAccessors          -> "add super accessors in traits and nested classes",
      extensionMethods        -> "add extension methods for inline classes",
      pickler                 -> "serialize symbol tables",
      refChecks               -> "reference/override checking, translate nested objects",
      leonExtraction          -> "extracts leon trees out of scala trees"
    )
    phs foreach { phasesSet += _._1 }
  }
}

class FullScalaCompiler(settings : NSCSettings, ctx: LeonContext) extends Global(settings, new SimpleReporter(settings, ctx.reporter)) with RangePositions {

  override protected def computeInternalPhases() : Unit = {
    val phs = List(
      syntaxAnalyzer          -> "parse source into ASTs, perform simple desugaring",
      analyzer.namerFactory   -> "resolve names, attach symbols to named trees",
      analyzer.packageObjects -> "load package objects",
      analyzer.typerFactory   -> "the meat and potatoes: type the trees",
      patmat                  -> "translate match expressions",
      superAccessors          -> "add super accessors in traits and nested classes",
      extensionMethods        -> "add extension methods for inline classes",
      pickler                 -> "serialize symbol tables",
      refChecks               -> "reference/override checking, translate nested objects",
      uncurry                 -> "uncurry, translate function values to anonymous classes",
      tailCalls               -> "replace tail calls by jumps",
      specializeTypes         -> "@specialized-driven class and method specialization",
      explicitOuter           -> "this refs to outer pointers, translate patterns",
      erasure                 -> "erase types, add interfaces for traits",
      postErasure             -> "clean up erased inline classes",
      lazyVals                -> "allocate bitmaps, translate lazy vals into lazified defs",
      lambdaLift              -> "move nested functions to top level",
      constructors            -> "move field definitions into constructors",
      mixer                   -> "mixin composition",
      cleanup                 -> "platform-specific cleanups, generate reflective calls",
      genicode                -> "generate portable intermediate code",
      inliner                 -> "optimization: do inlining",
      inlineExceptionHandlers -> "optimization: inline exception handlers",
      closureElimination      -> "optimization: eliminate uncalled closures",
      deadCode                -> "optimization: eliminate dead code",
      terminal                -> "The last phase in the compiler chain"
    )
    phs foreach { phasesSet += _._1 }
  }
}
