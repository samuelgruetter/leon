/* Copyright 2009-2014 EPFL, Lausanne */

package leon
package evaluators

import purescala.Definitions._
import purescala.Trees._
import purescala.TypeTrees._
import java.io.File
import java.nio.file.Files
import java.net.{URL, URLClassLoader}
import java.lang.reflect.{Constructor, Method}

import frontends.scalac.FullScalaCompiler

import scala.tools.nsc.{Settings=>NSCSettings,CompilerCommand}

class ScalacEvaluator(ctx: LeonContext, pgm: Program) extends LeonComponent {
  val name = "Evaluator of external functions"
  val description = "Evaluator for non-purescala functions"

  val outDir = new File("out/")

  implicit val debugSection = utils.DebugSectionEvaluation


  protected def definingModule(fd: FunDef): Option[ModuleDef] = {
    pgm.modules.find(_.definedFunctions contains fd)
  }

  protected def definingModule(cd: ClassDef): Option[ModuleDef] = {
    pgm.modules.find(_.definedClasses contains cd)
  }


  var classPaths: List[File] = Nil

  def methodCall(tfd: TypedFunDef, o: Any, args: Seq[Expr]): Expr = {
    assert(tfd.fd.wasMethod)

    val cd         = tfd.params(0).getType.asInstanceOf[ClassType].classDef
    val methodName = tfd.fd.methodName.get
    val className   = compiledName(cd)

    ctx.reporter.debug("Calling "+className+"."+methodName)

    o.getClass.getMethods().filter(_.getName() == methodName).toList match {
      case meth :: Nil =>
        val argsObjs = args.map(leonToScalac)
        scalacToLeon(meth.invoke(o, argsObjs : _*), tfd.returnType)

      case Nil =>
        ctx.reporter.fatalError("Unable to find "+className+"."+methodName)

      case ms =>
        println(ms.mkString("\n"))
        ctx.reporter.fatalError("Ambiguous reference to "+className+"."+methodName)
    }
  }

  def call(tfd: TypedFunDef, args: Seq[Expr]): Expr = {
    assert(tfd.fd.annotations("extern"))
    if (classPaths.isEmpty) {
      compile()
    }

    val module        = definingModule(tfd.fd).get
    val containerName = module.id.name
    val methodName    = tfd.id.name

    ctx.reporter.debug(s"Calling $containerName.${tfd.signature}(${args.mkString(",")})")

    try {
      val cl = loadClass(containerName)

      val meth: Method = cl.getMethods().find(_.getName() == methodName).get

      val res = if (args.isEmpty) {
        meth.invoke(null)
      } else {
        val argsObjs = args.map(leonToScalac)
        meth.invoke(null, argsObjs : _*)
      }

      scalacToLeon(res, tfd.fd.returnType)
    } catch {
      case t: Throwable =>
        t.printStackTrace
        null
    }
  }

  lazy val localClassLoader = {
    classOf[ScalacEvaluator].getClassLoader()
  }

  lazy val compiledClassLoader = {
    assert(classPaths.nonEmpty, "Not compiled ?!?")
    val classUrls = classPaths.map(_.toURI.toURL)
    println(classUrls)
    new URLClassLoader(classUrls.toArray, localClassLoader)
  }

  def loadClass(classname: String): Class[_] = {
    compiledClassLoader.loadClass(classname)
  }

  def compiledName(cd: ClassDef): String = {
    definingModule(cd) match {
      case Some(md) =>
        val prefix = if (md.isPackage) {
          md.id.name+"."
        } else {
          md.id.name+"$"
        }

        if (cd.isCaseObject) {
          prefix+cd.id.name+"$"
        } else {
          prefix+cd.id.name
        }
      case _ =>
        ctx.reporter.fatalError("Unable to find module corresponding to "+cd.id.name)
    }
  }

  def leonToScalac(e: Expr): AnyRef = e match {
    case eo: ExternalObject =>
      eo

    case IntLiteral(v) =>
      new java.lang.Integer(v)

    case BooleanLiteral(v) =>
      new java.lang.Boolean(v)

    case Tuple(exprs) =>
      val name = f"scala.Tuple${exprs.size}%d"

      val cl = localClassLoader.loadClass(name)
      val constr = cl.getConstructors().head.asInstanceOf[Constructor[AnyRef]]

      constr.newInstance(exprs.map(leonToScalac) : _*)

    case FiniteSet(exprs) =>

      Set(exprs.map(leonToScalac) : _*)

    case CaseClass(cct, fields) =>

      val name   = compiledName(cct.classDef)

      val cl = loadClass(name)
      val constr = cl.getConstructors().head.asInstanceOf[Constructor[AnyRef]]
      constr.newInstance(fields.map(leonToScalac) : _*)


    case t: Terminal =>
      ctx.reporter.error("Unhandled conversion to scala runtime: "+t)
      null

    case _ =>
      ctx.reporter.error("Trying to convert non-terminal: "+e)
      null
  }

  def productToElems(p: Product, tps: Seq[TypeTree]): Seq[Expr] = {
    for ((t,i) <- tps.zipWithIndex) yield {
      scalacToLeon(p.productElement(i), t)
    }
  }

  def scalacToLeon(o: Any, t: TypeTree): Expr = t match {
    case BooleanType =>
      BooleanLiteral(o.asInstanceOf[Boolean].booleanValue)
    case Int32Type =>
      IntLiteral(o.asInstanceOf[Integer].intValue)
    case UnitType =>
      UnitLiteral()
    case TupleType(tps) =>
      Tuple(productToElems(o.asInstanceOf[Product], tps))
    case cct: CaseClassType =>
      CaseClass(cct, productToElems(o.asInstanceOf[Product], cct.fieldsTypes))

    case act: AbstractClassType =>
      val name = o.getClass.getName.split("\\$").last

      act.knownCCDescendents.find(_.id.name == name) match {
        case Some(cct) =>
          // We refine the type, now that we know exactly what it is
          scalacToLeon(o, cct)
        case None =>
          // this wraps the external object, we can then access its fields through ScalacEvaluator
          ExternalObject(o, act)
      }

    case SetType(b) =>
      val s = o.asInstanceOf[Set[_]]
      FiniteSet(s.iterator.map(scalacToLeon(_, b)).toSeq).setType(t)

    case _ =>
      ctx.reporter.error("Unhandled conversion from scala runtime: "+t)
      null
  }

  def compile() = {
    val args = ctx.files.map(_.getPath).toList

    val tempOut = Files.createTempDirectory(outDir.toPath, "classes").toFile

    classPaths = tempOut :: ctx.settings.classPath.map(new File(_));

    ctx.reporter.debug(s"Compiling to ${tempOut.getPath}")

    val settings = new NSCSettings

    settings.classpath.value = ctx.settings.classPath.mkString(":")
    settings.usejavacp.value = false
    settings.outdir.value    = tempOut.getPath

    val libFiles = Settings.defaultLibFiles()

    val injected = if (ctx.settings.injectLibrary) {
      libFiles
    } else {
      libFiles.filter(f => f.contains("/lang/") || f.contains("/annotation/"))
    }

    val compilerOpts = injected ::: args.filterNot(_.startsWith("--"))

    val command = new CompilerCommand(compilerOpts, settings) {
      override val cmdName = "leon"
    }

    if(command.ok) {
      //new scala.tools.util.PathResolver(settings).Calculated.basis.foreach { cp =>
      //  cp.foreach( p =>
      //    classPaths ::= new File(p.toString)
      //  )
      //}

      val compiler = new FullScalaCompiler(settings, ctx)
      val run = new compiler.Run
      run.compile(command.files)
    } else {
      ctx.reporter.fatalError("No input program.")
    }
  }
}
