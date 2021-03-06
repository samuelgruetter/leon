/* Copyright 2009-2014 EPFL, Lausanne */

package leon
package termination

import purescala.Trees._
import purescala.TreeOps._
import purescala.Extractors._
import purescala.Common._
import purescala.Definitions._

import scala.collection.mutable.{Map => MutableMap}

final case class Relation(funDef: FunDef, path: Seq[Expr], call: FunctionInvocation, inLambda: Boolean) {
  override def toString : String = "Relation(" + funDef.id + "," + path + ", " + call.tfd.id + call.args.mkString("(",",",")") + "," + inLambda + ")"
}

trait RelationBuilder { self: TerminationChecker with Strengthener =>

  protected type RelationSignature = (FunDef, Option[Expr], Option[Expr], Option[Expr], Boolean, Set[(FunDef, Boolean)])

  protected def funDefRelationSignature(fd: FunDef): RelationSignature = {
    val strengthenedCallees = self.program.callGraph.callees(fd).map(fd => fd -> strengthened(fd))
    (fd, fd.precondition, fd.body, fd.postcondition.map(_._2), self.terminates(fd).isGuaranteed, strengthenedCallees)
  }

  private val relationCache : MutableMap[FunDef, (Set[Relation], RelationSignature)] = MutableMap.empty

  def getRelations(funDef: FunDef): Set[Relation] = relationCache.get(funDef) match {
    case Some((relations, signature)) if signature == funDefRelationSignature(funDef) => relations
    case _ => {
      val collector = new CollectorWithPaths[Relation] {
        var inLambda: Boolean = false

        override def rec(e: Expr, path: Seq[Expr]): Expr = e match {
          case l : Lambda =>
            val old = inLambda
            inLambda = true
            val res = super.rec(e, path)
            inLambda = old
            res
          case _ =>
            super.rec(e, path)
        }

        def collect(e: Expr, path: Seq[Expr]): Option[Relation] = e match {
          case fi @ FunctionInvocation(f, args) if self.functions(f.fd) =>
            Some(Relation(funDef, path, fi, inLambda))
          case _ => None
        }

        override def walk(e: Expr, path: Seq[Expr]) = e match {
          case FunctionInvocation(tfd, args) =>
            val funDef = tfd.fd
            Some(FunctionInvocation(tfd, (funDef.params.map(_.id) zip args) map { case (id, arg) =>
              rec(arg, register(self.applicationConstraint(funDef, id, arg, args), path))
            }))
          case _ => None
        }
      }

      val relations = collector.traverse(funDef).toSet
      relationCache(funDef) = (relations, funDefRelationSignature(funDef))
      relations
    }
  }
}
