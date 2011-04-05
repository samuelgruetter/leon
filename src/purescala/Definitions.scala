package purescala

object Definitions {
  import Common._
  import Trees._
  import TypeTrees._

  @serializable sealed abstract class Definition {
    val id: Identifier
    override def toString: String = PrettyPrinter(this)
    override def hashCode : Int = id.hashCode
    override def equals(that : Any) : Boolean = that match {
      case t : Definition => t.id == this.id
      case _ => false
    }
    def allIdentifiers : Set[Identifier]
  }

  /** A VarDecl declares a new identifier to be of a certain type. */
  @serializable case class VarDecl(id: Identifier, tpe: TypeTree) extends Typed {
    override def getType = tpe
    override def setType(tt: TypeTree) = scala.Predef.error("Can't set type of VarDecl.")

    override def hashCode : Int = id.hashCode
    override def equals(that : Any) : Boolean = that match {
      case t : VarDecl => t.id == this.id
      case _ => false
    }

    def toVariable : Variable = Variable(id).setType(tpe)
  }

  @serializable type VarDecls = Seq[VarDecl]

  /** A wrapper for a program. For now a program is simply a single object. The
   * name is meaningless and we just use the package name as id. */
  @serializable case class Program(id: Identifier, mainObject: ObjectDef) extends Definition {
    def definedFunctions = mainObject.definedFunctions
    def definedClasses = mainObject.definedClasses
    def classHierarchyRoots = mainObject.classHierarchyRoots
    def callGraph = mainObject.callGraph
    def calls(f1: FunDef, f2: FunDef) = mainObject.calls(f1, f2)
    def callers(f1: FunDef) = mainObject.callers(f1)
    def callees(f1: FunDef) = mainObject.callees(f1)
    def transitiveCallGraph = mainObject.transitiveCallGraph
    def transitivelyCalls(f1: FunDef, f2: FunDef) = mainObject.transitivelyCalls(f1, f2)
    def transitiveCallers(f1: FunDef) = mainObject.transitiveCallers(f1)
    def transitiveCallees(f1: FunDef) = mainObject.transitiveCallees(f1)
    def isRecursive(f1: FunDef) = mainObject.isRecursive(f1)
    def isCatamorphism(f1: FunDef) = mainObject.isCatamorphism(f1)
    def caseClassDef(name: String) = mainObject.caseClassDef(name)
    def allIdentifiers : Set[Identifier] = mainObject.allIdentifiers + id
  }

  /** Objects work as containers for class definitions, functions (def's) and
   * val's. */
  @serializable case class ObjectDef(id: Identifier, defs : Seq[Definition], invariants: Seq[Expr]) extends Definition {
    lazy val definedFunctions : Seq[FunDef] = defs.filter(_.isInstanceOf[FunDef]).map(_.asInstanceOf[FunDef])

    lazy val definedClasses : Seq[ClassTypeDef] = defs.filter(_.isInstanceOf[ClassTypeDef]).map(_.asInstanceOf[ClassTypeDef])

    def caseClassDef(caseClassName : String) : CaseClassDef =
    definedClasses.find(ctd => ctd.id.name == caseClassName).getOrElse(scala.Predef.error("Asking for non-existent case class def: " + caseClassName)).asInstanceOf[CaseClassDef]

    def allIdentifiers : Set[Identifier] = {
      (defs       map (_.allIdentifiers)).foldLeft(Set[Identifier]())((a, b) => a ++ b) ++ 
      (invariants map (Trees.allIdentifiers(_))).foldLeft(Set[Identifier]())((a, b) => a ++ b) + id
    }

    lazy val classHierarchyRoots : Seq[ClassTypeDef] = defs.filter(_.isInstanceOf[ClassTypeDef]).map(_.asInstanceOf[ClassTypeDef]).filter(!_.hasParent)

    lazy val (callGraph, callers, callees) = {
      type CallGraph = Set[(FunDef,FunDef)]

      val convert: Expr=>CallGraph = (_ => Set.empty)
      val combine: (CallGraph,CallGraph)=>CallGraph = (s1,s2) => s1 ++ s2
      def compute(fd: FunDef)(e: Expr, g: CallGraph) : CallGraph = e match {
        case f @ FunctionInvocation(f2, _) => g + ((fd, f2))
        case _ => g
      }

      val resSet: CallGraph = (for(funDef <- definedFunctions) yield {
        funDef.precondition.map(treeCatamorphism[CallGraph](convert, combine, compute(funDef)_, _)).getOrElse(Set.empty) ++
        funDef.body.map(treeCatamorphism[CallGraph](convert, combine, compute(funDef)_, _)).getOrElse(Set.empty) ++
        funDef.postcondition.map(treeCatamorphism[CallGraph](convert, combine, compute(funDef)_, _)).getOrElse(Set.empty)
      }).reduceLeft(_ ++ _)

      var callers: Map[FunDef,Set[FunDef]] =
        new scala.collection.immutable.HashMap[FunDef,Set[FunDef]]
      var callees: Map[FunDef,Set[FunDef]] =
        new scala.collection.immutable.HashMap[FunDef,Set[FunDef]]

      for(funDef <- definedFunctions) {
        val clrs = resSet.filter(_._2 == funDef).map(_._1)
        val cles = resSet.filter(_._1 == funDef).map(_._2)
        callers = callers + (funDef -> clrs)
        callees = callees + (funDef -> cles)
      }

      (resSet, callers, callees)
    }

    // checks whether f1's body, pre or post contain calls to f2
    def calls(f1: FunDef, f2: FunDef) : Boolean = callGraph((f1,f2))

    lazy val (transitiveCallGraph, transitiveCallers, transitiveCallees) = {
      var resSet : Set[(FunDef,FunDef)] = callGraph
      var change = true

      while(change) {
        change = false
        for(f1 <- definedFunctions; f2 <- callers(f1); f3 <- callees(f1)) {
          if(!resSet(f2,f3)) {
            change = true
            resSet = resSet + ((f2,f3))
          }
        }
      }

      var tCallers: Map[FunDef,Set[FunDef]] =
        new scala.collection.immutable.HashMap[FunDef,Set[FunDef]]
      var tCallees: Map[FunDef,Set[FunDef]] =
        new scala.collection.immutable.HashMap[FunDef,Set[FunDef]]

      for(funDef <- definedFunctions) {
        val clrs = resSet.filter(_._2 == funDef).map(_._1)
        val cles = resSet.filter(_._1 == funDef).map(_._2)
        tCallers = tCallers + (funDef -> clrs)
        tCallees = tCallees + (funDef -> cles)
      }

      (resSet, tCallers, tCallees)
    }

    def transitivelyCalls(f1: FunDef, f2: FunDef) : Boolean = transitiveCallGraph((f1,f2))

    def isRecursive(f: FunDef) = transitivelyCalls(f, f)

    def isCatamorphism(f : FunDef) : Boolean = {
      val c = callees(f)
      if(f.hasImplementation && f.args.size == 1 && c.size == 1 && c.head == f) f.body.get match {
        case SimplePatternMatching(scrut, _, _) if (scrut == f.args.head.toVariable) => true
        case _ => false
      } else {
        false
      }
    }
  }

  /** Useful because case classes and classes are somewhat unified in some
   * patterns (of pattern-matching, that is) */
  @serializable sealed trait ClassTypeDef extends Definition {
    self =>

    val id: Identifier
    def parent: Option[AbstractClassDef]
    def setParent(parent: AbstractClassDef) : self.type
    def hasParent: Boolean = parent.isDefined
    val isAbstract: Boolean

  }

  /** Will be used at some point as a common ground for case classes (which
   * implicitely define extractors) and explicitely defined unapply methods. */
  @serializable sealed trait ExtractorTypeDef

  /** Abstract classes. */
  object AbstractClassDef {
    def unapply(acd: AbstractClassDef): Option[(Identifier,Option[AbstractClassDef])] = {
      if(acd == null) None else Some((acd.id, acd.parent))
    }
  }
  @serializable class AbstractClassDef(val id: Identifier, prnt: Option[AbstractClassDef] = None) extends ClassTypeDef {
    private var parent_ = prnt
    var fields: VarDecls = Nil
    val isAbstract = true

    private var children : List[ClassTypeDef] = Nil

    private[purescala] def registerChild(child: ClassTypeDef) : Unit = {
      children = child :: children
    }

    def allIdentifiers : Set[Identifier] = {
      fields.map(f => f.id).toSet + id
    }
      
    def knownChildren : Seq[ClassTypeDef] = {
      children
    }

    def knownDescendents : Seq[ClassTypeDef] = {
      knownChildren ++ (knownChildren.map(c => c match {
        case acd: AbstractClassDef => acd.knownDescendents
        case _ => Nil
      }).reduceLeft(_ ++ _))
    }

    def setParent(newParent: AbstractClassDef) = {
      if(parent_.isDefined) {
        scala.Predef.error("Resetting parent is forbidden.")
      }
      newParent.registerChild(this)
      parent_ = Some(newParent)
      this
    }
    def parent = parent_
  }

  /** Case classes. */
  object CaseClassDef {
    def unapply(ccd: CaseClassDef): Option[(Identifier,Option[AbstractClassDef],VarDecls)] =  {
      if(ccd == null) None else Some((ccd.id, ccd.parent, ccd.fields))
    }
  }

  @serializable class CaseClassDef(val id: Identifier, prnt: Option[AbstractClassDef] = None) extends ClassTypeDef with ExtractorTypeDef {
    private var parent_ = prnt
    var fields: VarDecls = Nil
    val isAbstract = false

    def setParent(newParent: AbstractClassDef) = {
      if(parent_.isDefined) {
        scala.Predef.error("Resetting parent is forbidden.")
      }
      newParent.registerChild(this)
      parent_ = Some(newParent)
      this
    }
    def parent = parent_

    def allIdentifiers : Set[Identifier] = {
      fields.map(f => f.id).toSet
    }

    def selectorID2Index(id: Identifier) : Int = {
      var i : Int = 0
      var found = false
      val fs = fields.size
      while(!found && i < fs) {
        if(fields(i).id == id) {
          found = true
        } else {
          i += 1
        }
      }

      if(found)
        i
      else
        scala.Predef.error("Asking for index of field that does not belong to the case class.")
    }
  }

  /** "Regular" classes */
  //class ClassDef(val id: Identifier, var parent: Option[AbstractClassDef]) extends ClassTypeDef {
  //  var fields: VarDecls = Nil
  //  val isAbstract = false
  //}
  
  /** Values */
  @serializable case class ValDef(varDecl: VarDecl, value: Expr) extends Definition {
    val id: Identifier = varDecl.id
    def allIdentifiers : Set[Identifier] = Trees.allIdentifiers(value) + id
  }

  /** Functions (= 'methods' of objects) */
  object FunDef {
    def unapply(fd: FunDef): Option[(Identifier,TypeTree,VarDecls,Option[Expr],Option[Expr],Option[Expr])] = {
      if(fd != null) {
        Some((fd.id, fd.returnType, fd.args, fd.body, fd.precondition, fd.postcondition))
      } else {
        None
      }
    }
  }
  @serializable class FunDef(val id: Identifier, val returnType: TypeTree, val args: VarDecls) extends Definition with ScalacPositional {
    var body: Option[Expr] = None
    var precondition: Option[Expr] = None
    var postcondition: Option[Expr] = None

    def hasImplementation : Boolean = body.isDefined
    def hasBody = hasImplementation
    def hasPrecondition : Boolean = precondition.isDefined
    def hasPostcondition : Boolean = postcondition.isDefined

    def allIdentifiers : Set[Identifier] = {
      args.map(_.id).toSet ++
      body.map(Trees.allIdentifiers(_)).getOrElse(Set[Identifier]()) ++
      precondition.map(Trees.allIdentifiers(_)).getOrElse(Set[Identifier]()) ++
      postcondition.map(Trees.allIdentifiers(_)).getOrElse(Set[Identifier]()) + id
    }
    
    private var annots: Set[String] = Set.empty[String]
    def addAnnotation(as: String*) : FunDef = {
      annots = annots ++ as
      this
    }
    def annotations : Set[String] = annots

    def isPrivate : Boolean = annots.contains("private")
  }
}
