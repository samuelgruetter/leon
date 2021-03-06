/* Copyright 2009-2014 EPFL, Lausanne */

package leon
package codegen

import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TreeOps.{simplestValue, matchToIfThenElse}
import purescala.TypeTrees._
import purescala.Constructors._
import purescala.TypeTreeOps.instantiateType
import utils._

import cafebabe._
import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.ClassFileTypes._
import cafebabe.Defaults.constructorName
import cafebabe.Flags._

trait CodeGeneration {
  self: CompilationUnit =>

  /** A class providing information about the status of parameters in the function that is being currently compiled.
   *  vars is a mapping from local variables/ parameters to the offset of the respective JVM local register
   *  isStatic signifies if the current method is static (a function, in Leon terms) 
   */
  case class Locals(
    vars     : Map[Identifier, Int],
    args     : Map[Identifier, Int],
    closures : Map[Identifier, (String,String,String)],
    private val isStatic : Boolean
  ) {
    /** Fetches the offset of a local variable/ parameter from its identifier */
    def varToLocal(v: Identifier): Option[Int] = vars.get(v)

    def varToArg(v: Identifier): Option[Int] = args.get(v)

    def varToClosure(v: Identifier): Option[(String,String,String)] = closures.get(v)

    /** Adds some extra variables to the mapping */
    def withVars(newVars: Map[Identifier, Int]) = Locals(vars ++ newVars, args, closures, isStatic)

    /** Adds an extra variable to the mapping */
    def withVar(nv: (Identifier, Int)) = Locals(vars + nv, args, closures, isStatic)

    def withArgs(newArgs: Map[Identifier, Int]) = Locals(vars, args ++ newArgs, closures, isStatic)

    def withClosures(newClosures: Map[Identifier,(String,String,String)]) = Locals(vars, args, closures ++ newClosures, isStatic)

    /** The index of the monitor object in this function */
    def monitorIndex = if (isStatic) 0 else 1
  }
  
  object NoLocals {
    /** Make a $Locals object without any local variables */
    def apply(isStatic : Boolean) = new Locals(Map(), Map(), Map(), isStatic)
  }

  private[codegen] val BoxedIntClass             = "java/lang/Integer"
  private[codegen] val BoxedBoolClass            = "java/lang/Boolean"
  private[codegen] val BoxedCharClass            = "java/lang/Character"
  private[codegen] val BoxedArrayClass           = "leon/codegen/runtime/ArrayBox"

  private[codegen] val TupleClass                = "leon/codegen/runtime/Tuple"
  private[codegen] val SetClass                  = "leon/codegen/runtime/Set"
  private[codegen] val MapClass                  = "leon/codegen/runtime/Map"
  private[codegen] val BigIntClass               = "leon/codegen/runtime/BigInt"
  private[codegen] val CaseClassClass            = "leon/codegen/runtime/CaseClass"
  private[codegen] val LambdaClass               = "leon/codegen/runtime/Lambda"
  private[codegen] val ErrorClass                = "leon/codegen/runtime/LeonCodeGenRuntimeException"
  private[codegen] val ImpossibleEvaluationClass = "leon/codegen/runtime/LeonCodeGenEvaluationException"
  private[codegen] val HashingClass              = "leon/codegen/runtime/LeonCodeGenRuntimeHashing"
  private[codegen] val ChooseEntryPointClass     = "leon/codegen/runtime/ChooseEntryPoint"
  private[codegen] val GenericValuesClass        = "leon/codegen/runtime/GenericValues"
  private[codegen] val MonitorClass              = "leon/codegen/runtime/LeonCodeGenRuntimeMonitor"

  def idToSafeJVMName(id: Identifier) = id.uniqueName.replaceAll("\\.", "\\$")
  def defToJVMName(d : Definition) : String = "Leon$CodeGen$" + idToSafeJVMName(d.id)

  /** Retrieve the name of the underlying lazy field from a lazy field accessor method */
  private[codegen] def underlyingField(lazyAccessor : String) = lazyAccessor + "$underlying" 
  
  /** Return the respective JVM type from a Leon type */
  def typeToJVM(tpe : TypeTree) : String = tpe match {
    case Int32Type => "I"

    case BooleanType => "Z"

    case CharType => "C"

    case UnitType => "Z"

    case c : ClassType =>
      leonClassToJVMInfo(c.classDef).map { case (n, _) => "L" + n + ";" }.getOrElse("Unsupported class " + c.id)

    case _ : TupleType =>
      "L" + TupleClass + ";"

    case _ : SetType =>
      "L" + SetClass + ";"

    case _ : MapType =>
      "L" + MapClass + ";"

    case IntegerType =>
      "L" + BigIntClass + ";"

    case _ : FunctionType =>
      "L" + LambdaClass + ";"

    case ArrayType(base) =>
      "[" + typeToJVM(base)

    case TypeParameter(_) =>
      "Ljava/lang/Object;"

    case _ => throw CompilationException("Unsupported type : " + tpe)
  }

  /** Return the respective boxed JVM type from a Leon type */
  def typeToJVMBoxed(tpe : TypeTree) : String = tpe match {
    case Int32Type              => s"L$BoxedIntClass;"
    case BooleanType | UnitType => s"L$BoxedBoolClass;"
    case CharType               => s"L$BoxedCharClass;"
    case other => typeToJVM(other)
  }
  
  /**
   * Compiles a function/method definition.
   * @param funDef The function definition to be compiled
   * @param owner The module/class that contains $funDef
   */  
  def compileFunDef(funDef : FunDef, owner : Definition) {
    
    val isStatic = owner.isInstanceOf[ModuleDef]
    
    val cf = classes(owner)
    val (_,mn,_) = leonFunDefToJVMInfo(funDef).get

    val paramsTypes = funDef.params.map(a => typeToJVM(a.tpe))

    val realParams = if (params.requireMonitor) {
      ("L" + MonitorClass + ";") +: paramsTypes
    } else {
      paramsTypes
    }

    val m = cf.addMethod(
      typeToJVM(funDef.returnType),
      mn,
      realParams : _*
    )
    m.setFlags(( 
      if (isStatic)   
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL  |
        METHOD_ACC_STATIC
      else
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL
    ).asInstanceOf[U2])
    
    val ch = m.codeHandler
   
    // An offset we introduce to the parameters:
    // 1 if this is a method, so we need "this" in position 0 of the stack
    // 1 if we are monitoring // FIXME
    val paramsOffset = Seq(!isStatic, params.requireMonitor).count(x => x)
    val newMapping = 
      funDef.params.map(_.id).zipWithIndex.toMap.mapValues(_ + paramsOffset)
      
    val body = funDef.body.getOrElse(throw CompilationException("Can't compile a FunDef without body: "+funDef.id.name))

    val bodyWithPre = if(funDef.hasPrecondition && params.checkContracts) {
      IfExpr(funDef.precondition.get, body, Error(body.getType, "Precondition failed"))
    } else {
      body
    }

    val bodyWithPost = if(funDef.hasPostcondition && params.checkContracts) {
      val Some((id, post)) = funDef.postcondition
      Let(id, bodyWithPre, IfExpr(post, Variable(id), Error(id.getType, "Postcondition failed")) )
    } else {
      bodyWithPre
    }

    if (params.recordInvocations) {
      // index of monitor object will be before the first Scala parameter
      ch << ALoad(paramsOffset-1) << InvokeVirtual(MonitorClass, "onInvoke", "()V") 
    }

    mkExpr(bodyWithPost, ch)(Locals(newMapping, Map.empty, Map.empty, isStatic))

    funDef.returnType match {
      case Int32Type | BooleanType | UnitType =>
        ch << IRETURN

      case IntegerType | _ : ClassType | _ : TupleType | _ : SetType | _ : MapType | _ : ArrayType | _ : FunctionType | _ : TypeParameter =>
        ch << ARETURN

      case other =>
        throw CompilationException("Unsupported return type : " + other.getClass)
    }

    ch.freeze
  }

  private[codegen] def mkExpr(e: Expr, ch: CodeHandler, canDelegateToMkBranch: Boolean = true)(implicit locals: Locals) {
    e match {
      case Variable(id) =>
        load(id, ch)

      case Assert(cond, oerr, body) =>
        mkExpr(IfExpr(Not(cond), Error(body.getType, oerr.getOrElse("Assertion failed @"+e.getPos)), body), ch)

      case Ensuring(body, id, post) =>
        mkExpr(Let(id, body, Assert(post, Some("Ensuring failed"), Variable(id))), ch)

      case Let(i,d,b) =>
        mkExpr(d, ch)
        val slot = ch.getFreshVar
        val instr = i.getType match {
          case Int32Type | CharType | BooleanType | UnitType => IStore(slot)
          case _ => AStore(slot)
        }
        ch << instr
        mkExpr(b, ch)(locals.withVar(i -> slot))

      case IntLiteral(v) =>
        ch << Ldc(v)

      case CharLiteral(v) =>
        ch << Ldc(v)

      case BooleanLiteral(v) =>
        ch << Ldc(if(v) 1 else 0)

      case UnitLiteral() =>
        ch << Ldc(1)

      case InfiniteIntegerLiteral(v) =>
        ch << New(BigIntClass) << DUP
        ch << Ldc(v.toString)
        ch << InvokeSpecial(BigIntClass, constructorName, "(Ljava/lang/String;)V")

      // Case classes
      case CaseClass(cct, as) =>
        val (ccName, ccApplySig) = leonClassToJVMInfo(cct.classDef).getOrElse {
          throw CompilationException("Unknown class : " + cct.id)
        }
        ch << New(ccName) << DUP
        if (params.requireMonitor) 
          ch << ALoad(locals.monitorIndex)
        for((a, vd) <- as zip cct.classDef.fields) {
          vd.tpe match {
            case TypeParameter(_) =>
              mkBoxedExpr(a, ch)
            case _ =>
              mkExpr(a, ch)
          }
        }
        ch << InvokeSpecial(ccName, constructorName, ccApplySig)

      case CaseClassInstanceOf(cct, e) =>
        val (ccName, _) = leonClassToJVMInfo(cct.classDef).getOrElse {
          throw CompilationException("Unknown class : " + cct.id)
        }
        mkExpr(e, ch)
        ch << InstanceOf(ccName)

      case CaseClassSelector(cct, e, sid) =>
        mkExpr(e, ch)
        val (ccName, _) = leonClassToJVMInfo(cct.classDef).getOrElse {
          throw CompilationException("Unknown class : " + cct.id)
        }
        ch << CheckCast(ccName)
        instrumentedGetField(ch, cct, sid)

      // Tuples (note that instanceOf checks are in mkBranch)
      case Tuple(es) =>
        ch << New(TupleClass) << DUP
        ch << Ldc(es.size)
        ch << NewArray("java/lang/Object")
        for((e,i) <- es.zipWithIndex) {
          ch << DUP
          ch << Ldc(i)
          mkBoxedExpr(e, ch)
          ch << AASTORE
        }
        ch << InvokeSpecial(TupleClass, constructorName, "([Ljava/lang/Object;)V")

      case TupleSelect(t, i) =>
        val TupleType(bs) = t.getType
        mkExpr(t,ch)
        ch << Ldc(i - 1)
        ch << InvokeVirtual(TupleClass, "get", "(I)Ljava/lang/Object;")
        mkUnbox(bs(i - 1), ch)

      // Sets
      case FiniteSet(es) =>
        ch << DefaultNew(SetClass)
        for(e <- es) {
          ch << DUP
          mkBoxedExpr(e, ch)
          ch << InvokeVirtual(SetClass, "add", "(Ljava/lang/Object;)V")
        }

      case ElementOfSet(e, s) =>
        mkExpr(s, ch)
        mkBoxedExpr(e, ch)
        ch << InvokeVirtual(SetClass, "contains", "(Ljava/lang/Object;)Z")

      case SetCardinality(s) =>
        mkExpr(s, ch)
        ch << InvokeVirtual(SetClass, "size", "()I")

      case SubsetOf(s1, s2) =>
        mkExpr(s1, ch)
        mkExpr(s2, ch)
        ch << InvokeVirtual(SetClass, "subsetOf", "(L%s;)Z".format(SetClass))

      case SetIntersection(s1, s2) =>
        mkExpr(s1, ch)
        mkExpr(s2, ch)
        ch << InvokeVirtual(SetClass, "intersect", "(L%s;)L%s;".format(SetClass,SetClass))

      case SetUnion(s1, s2) =>
        mkExpr(s1, ch)
        mkExpr(s2, ch)
        ch << InvokeVirtual(SetClass, "union", "(L%s;)L%s;".format(SetClass,SetClass))

      case SetDifference(s1, s2) =>
        mkExpr(s1, ch)
        mkExpr(s2, ch)
        ch << InvokeVirtual(SetClass, "minus", "(L%s;)L%s;".format(SetClass,SetClass))

      // Maps
      case FiniteMap(ss) =>
        ch << DefaultNew(MapClass)
        for((f,t) <- ss) {
          ch << DUP
          mkBoxedExpr(f, ch)
          mkBoxedExpr(t, ch)
          ch << InvokeVirtual(MapClass, "add", "(Ljava/lang/Object;Ljava/lang/Object;)V")
        }

      case MapGet(m, k) =>
        val MapType(_, tt) = m.getType
        mkExpr(m, ch)
        mkBoxedExpr(k, ch)
        ch << InvokeVirtual(MapClass, "get", "(Ljava/lang/Object;)Ljava/lang/Object;")
        mkUnbox(tt, ch)

      case MapIsDefinedAt(m, k) =>
        mkExpr(m, ch)
        mkBoxedExpr(k, ch)
        ch << InvokeVirtual(MapClass, "isDefinedAt", "(Ljava/lang/Object;)Z")

      case MapUnion(m1, m2) =>
        mkExpr(m1, ch)
        mkExpr(m2, ch)
        ch << InvokeVirtual(MapClass, "union", "(L%s;)L%s;".format(MapClass,MapClass))

      // Branching
      case IfExpr(c, t, e) =>
        val tl = ch.getFreshLabel("then")
        val el = ch.getFreshLabel("else")
        val al = ch.getFreshLabel("after")
        mkBranch(c, tl, el, ch)
        ch << Label(tl)
        mkExpr(t, ch)
        ch << Goto(al) << Label(el)
        mkExpr(e, ch)
        ch << Label(al)

      // Strict static fields
      case FunctionInvocation(tfd, as) if tfd.fd.canBeStrictField =>
        val (className, fieldName, _) = leonFunDefToJVMInfo(tfd.fd).getOrElse {
          throw CompilationException("Unknown method : " + tfd.id)
        }
        
        if (params.requireMonitor) {
          // index of monitor object will be before the first Scala parameter
          ch << ALoad(locals.monitorIndex) << InvokeVirtual(MonitorClass, "onInvoke", "()V") 
        }
    
        // Get static field
        ch << GetStatic(className, fieldName, typeToJVM(tfd.fd.returnType))
        
        // unbox field
        (tfd.fd.returnType, tfd.returnType) match {
          case (TypeParameter(_), tpe)  =>
            mkUnbox(tpe, ch)
          case _ =>
        }
        
      // Static lazy fields/ functions
      case FunctionInvocation(tfd, as) =>
        val (cn, mn, ms) = leonFunDefToJVMInfo(tfd.fd).getOrElse {
          throw CompilationException("Unknown method : " + tfd.id)
        }
        
        if (params.requireMonitor) {         
          ch << ALoad(locals.monitorIndex)
        }

        for((a, vd) <- as zip tfd.fd.params) {
          vd.tpe match {
            case TypeParameter(_) =>
              mkBoxedExpr(a, ch)
            case _ =>
              mkExpr(a, ch)
          }
        }

        ch << InvokeStatic(cn, mn, ms)

        (tfd.fd.returnType, tfd.returnType) match {
          case (TypeParameter(_), tpe)  =>
            mkUnbox(tpe, ch)
          case _ =>
        }
        
      // Strict fields are handled as fields
      case MethodInvocation(rec, _, tfd, _) if tfd.fd.canBeStrictField =>
        val (className, fieldName, _) = leonFunDefToJVMInfo(tfd.fd).getOrElse {
          throw CompilationException("Unknown method : " + tfd.id)
        }
        
        if (params.requireMonitor) {
          // index of monitor object will be before the first Scala parameter
          ch << ALoad(locals.monitorIndex) << InvokeVirtual(MonitorClass, "onInvoke", "()V") 
        }
        // Load receiver 
        mkExpr(rec,ch) 
        
        // Get field
        ch << GetField(className, fieldName, typeToJVM(tfd.fd.returnType))
        
        // unbox field
        (tfd.fd.returnType, tfd.returnType) match {
          case (TypeParameter(_), tpe)  =>
            mkUnbox(tpe, ch)
          case _ =>
        }
              
      // This is for lazy fields and real methods.
      // To access a lazy field, we call its accessor function.
      case MethodInvocation(rec, cd, tfd, as) =>
        val (className, methodName, sig) = leonFunDefToJVMInfo(tfd.fd).getOrElse {
          throw CompilationException("Unknown method : " + tfd.id)
        }
        
        // Receiver of the method call 
        mkExpr(rec,ch)
        
        if (params.requireMonitor) {
          ch << ALoad(locals.monitorIndex)
        }
  
        for((a, vd) <- as zip tfd.fd.params) {
          vd.tpe match {
            case TypeParameter(_) =>
              mkBoxedExpr(a, ch)
            case _ =>
              mkExpr(a, ch)
          }
        }
       
        // No dynamic dispatching/overriding in Leon, 
        // so no need to take care of own vs. "super" methods
        ch << InvokeVirtual(className, methodName, sig) 
  
        (tfd.fd.returnType, tfd.returnType) match {
          case (TypeParameter(_), tpe)  =>
            mkUnbox(tpe, ch)
          case _ =>
        }

      case app @ Application(caller, args) =>
        mkExpr(caller, ch)
        ch << Ldc(args.size) << NewArray("java/lang/Object")
        for ((arg,i) <- args.zipWithIndex) {
          ch << DUP << Ldc(i)
          mkBoxedExpr(arg, ch)
          ch << AASTORE
        }

        ch << InvokeVirtual(LambdaClass, "apply", "([Ljava/lang/Object;)Ljava/lang/Object;")
        mkUnbox(app.getType, ch)

      case l @ Lambda(args, body) =>
        val afName = "Leon$CodeGen$Lambda$" + CompilationUnit.nextLambdaId

        val cf = new ClassFile(afName, Some(LambdaClass))

        cf.setFlags((
          CLASS_ACC_SUPER |
          CLASS_ACC_PUBLIC |
          CLASS_ACC_FINAL
        ).asInstanceOf[U2])

        val closures = purescala.TreeOps.variablesOf(l).toSeq.sortBy(_.uniqueName)
        val closureTypes = closures.map(id => id.name -> typeToJVM(id.getType))

        if (closureTypes.isEmpty) {
          cf.addDefaultConstructor
        } else {
          for ((nme, jvmt) <- closureTypes) {
            val fh = cf.addField(jvmt, nme)
            fh.setFlags((
              FIELD_ACC_PUBLIC |
              FIELD_ACC_FINAL
            ).asInstanceOf[U2])
          }

          val cch = cf.addConstructor(closureTypes.map(_._2).toList).codeHandler

          cch << ALoad(0)
          cch << InvokeSpecial(LambdaClass, constructorName, "()V")

          var c = 1
          for ((nme, jvmt) <- closureTypes) {
            cch << ALoad(0)
            cch << (jvmt match {
              case "I" | "Z" => ILoad(c)
              case _ => ALoad(c)
            })
            cch << PutField(afName, nme, jvmt)
            c += 1
          }

          cch << RETURN
          cch.freeze
        }

        locally {
          val argTypes = args.map(arg => typeToJVM(arg.tpe))

          val apm = cf.addMethod("Ljava/lang/Object;", "apply", "[Ljava/lang/Object;")

          apm.setFlags((
            METHOD_ACC_PUBLIC |
            METHOD_ACC_FINAL
          ).asInstanceOf[U2])

          val argMapping = args.map(_.id).zipWithIndex.toMap
          val closureMapping = (closures zip closureTypes).map { case (id, (name, tpe)) => id -> (afName, name, tpe) }.toMap

          val newLocals = locals.withArgs(argMapping).withClosures(closureMapping)
          
          val apch = apm.codeHandler

          mkBoxedExpr(body, apch)(newLocals)

          apch << ARETURN

          apch.freeze
        }

        loader.register(cf)

        val consSig = "(" + closures.map(id => typeToJVM(id.getType)).mkString("") + ")V"

        ch << New(afName) << DUP
        for (a <- closures) {
          mkExpr(Variable(a), ch)
        }
        ch << InvokeSpecial(afName, constructorName, consSig)
        
      // Arithmetic
      /*
       * TODO: Correct code generation for infinite precision operations
       */
      case Plus(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << InvokeVirtual(BigIntClass, "add", "(L%s;)L%s;".format(BigIntClass, BigIntClass))

      case Minus(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << InvokeVirtual(BigIntClass, "sub", "(L%s;)L%s;".format(BigIntClass, BigIntClass))

      case Times(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << InvokeVirtual(BigIntClass, "mult", "(L%s;)L%s;".format(BigIntClass, BigIntClass))

      case Division(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << InvokeVirtual(BigIntClass, "div", "(L%s;)L%s;".format(BigIntClass, BigIntClass))

      case Modulo(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << InvokeVirtual(BigIntClass, "mod", "(L%s;)L%s;".format(BigIntClass, BigIntClass))

      case UMinus(e) =>
        mkExpr(e, ch)
        ch << InvokeVirtual(BigIntClass, "neg", "()L%s;".format(BigIntClass))

      //BV arithmetic
      case BVPlus(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IADD

      case BVMinus(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << ISUB

      case BVTimes(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IMUL

      case BVDivision(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IDIV

      case BVModulo(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IREM

      case BVUMinus(e) =>
        mkExpr(e, ch)
        ch << INEG

      case BVNot(e) =>
        mkExpr(e, ch)
        mkExpr(IntLiteral(-1), ch)
        ch << IXOR

      case BVAnd(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IAND

      case BVOr(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IOR

      case BVXOr(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IXOR

      case BVShiftLeft(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << ISHL

      case BVLShiftRight(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IUSHR

      case BVAShiftRight(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << ISHR

      case ArrayLength(a) =>
        mkExpr(a, ch)
        ch << ARRAYLENGTH

      case as @ ArraySelect(a,i) =>
        mkExpr(a, ch)
        mkExpr(i, ch)
        ch << (as.getType match {
          case Untyped => throw CompilationException("Cannot compile untyped array access.")
          case CharType => CALOAD
          case Int32Type => IALOAD
          case BooleanType => BALOAD
          case _ => AALOAD
        })

      case au @ ArrayUpdated(a, i, v) =>
        mkExpr(a, ch)
        ch << DUP
        ch << ARRAYLENGTH
        val storeInstr = a.getType match {
          case ArrayType(CharType) => ch << NewArray.primitive("T_CHAR"); CASTORE
          case ArrayType(Int32Type) => ch << NewArray.primitive("T_INT"); IASTORE
          case ArrayType(BooleanType) => ch << NewArray.primitive("T_BOOLEAN"); BASTORE
          case ArrayType(other) => ch << NewArray(typeToJVM(other)); AASTORE
          case other => throw CompilationException("Cannot compile finite array expression whose type is %s.".format(other))
        } 
        //srcArrary and targetArray is on the stack
        ch << DUP_X1 //insert targetArray under srcArray
        ch << Ldc(0) << SWAP //srcArray, 0, targetArray
        ch << DUP << ARRAYLENGTH //targetArray, length on stack
        ch << Ldc(0) << SWAP //final arguments: src, 0, target, 0, length
        ch << InvokeStatic("java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V")

        //targetArray remains on the stack
        ch << DUP
        mkExpr(i, ch)
        mkExpr(v, ch)
        ch << storeInstr
        //returns targetArray

      case a @ FiniteArray(elems, default, length) =>
        val IntLiteral(l) = length
        ch << Ldc(l)
        val storeInstr = a.getType match {
          case ArrayType(CharType) => ch << NewArray.primitive("T_CHAR"); CASTORE
          case ArrayType(Int32Type) => ch << NewArray.primitive("T_INT"); IASTORE
          case ArrayType(BooleanType) => ch << NewArray.primitive("T_BOOLEAN"); BASTORE
          case ArrayType(other) => ch << NewArray(typeToJVM(other)); AASTORE
          case other => throw CompilationException("Cannot compile finite array expression whose type is %s.".format(other))
        }
        for((i, e) <- elems) {
          ch << DUP << Ldc(i)
          mkExpr(e, ch) 
          ch << storeInstr
        }

      // Misc and boolean tests
      case Error(tpe, desc) =>
        ch << New(ErrorClass) << DUP
        ch << Ldc(desc)
        ch << InvokeSpecial(ErrorClass, constructorName, "(Ljava/lang/String;)V")
        ch << ATHROW

      case Choose(_, _, Some(e)) =>
        mkExpr(e, ch)

      case choose @ Choose(_, _, None) =>
        val prob = synthesis.Problem.fromChoose(choose)

        val id = runtime.ChooseEntryPoint.register(prob, this);
        ch << Ldc(id)


        ch << Ldc(prob.as.size)
        ch << NewArray("java/lang/Object")

        for ((id, i) <- prob.as.zipWithIndex) {
          ch << DUP
          ch << Ldc(i)
          mkExpr(Variable(id), ch)
          mkBox(id.getType, ch)
          ch << AASTORE
        }

        ch << InvokeStatic(ChooseEntryPointClass, "invoke", "(I[Ljava/lang/Object;)Ljava/lang/Object;")

        mkUnbox(choose.getType, ch)

      case gv @ GenericValue(tp, int) =>
        val id = runtime.GenericValues.register(gv);
        ch << Ldc(id)
        ch << InvokeStatic(GenericValuesClass, "get", "(I)Ljava/lang/Object;")
      
      case nt @ NoTree( tp@(Int32Type | BooleanType | UnitType | CharType)) =>
        println("COMPILING "+nt+" TO "+simplestValue(tp))
        mkExpr(simplestValue(tp), ch)
        
      case nt @ NoTree(_) =>
        println("COMPILING "+nt+" TO NULL")
        ch << ACONST_NULL
      
      case This(ct) =>
        ch << ALoad(0) // FIXME what if doInstrument etc
        
      case p : Passes => 
        mkExpr(matchToIfThenElse(p.asConstraint), ch)

      case m : MatchExpr => 
        mkExpr(matchToIfThenElse(m), ch)
      
      case b if b.getType == BooleanType && canDelegateToMkBranch =>
        val fl = ch.getFreshLabel("boolfalse")
        val al = ch.getFreshLabel("boolafter")
        ch << Ldc(1)
        mkBranch(b, al, fl, ch, canDelegateToMkExpr = false)
        ch << Label(fl) << POP << Ldc(0) << Label(al)

      case _ => throw CompilationException("Unsupported expr " + e + " : " + e.getClass) 
    }
  }

  // Leaves on the stack a value equal to `e`, always of a type compatible with java.lang.Object.
  private[codegen] def mkBoxedExpr(e: Expr, ch: CodeHandler)(implicit locals: Locals) {
    e.getType match {
      case Int32Type =>
        ch << New(BoxedIntClass) << DUP
        mkExpr(e, ch)
        ch << InvokeSpecial(BoxedIntClass, constructorName, "(I)V")

      case BooleanType | UnitType =>
        ch << New(BoxedBoolClass) << DUP
        mkExpr(e, ch)
        ch << InvokeSpecial(BoxedBoolClass, constructorName, "(Z)V")

      case CharType =>
        ch << New(BoxedCharClass) << DUP
        mkExpr(e, ch)
        ch << InvokeSpecial(BoxedCharClass, constructorName, "(C)V")

      case at @ ArrayType(et) =>
        ch << New(BoxedArrayClass) << DUP
        mkExpr(e, ch)
        ch << InvokeSpecial(BoxedArrayClass, constructorName, "(%s)V".format(typeToJVM(at)))

      case _ =>
        mkExpr(e, ch)
    }
  }

  // Assumes the top of the stack contains of value of the right type, and makes it
  // compatible with java.lang.Object.
  private[codegen] def mkBox(tpe: TypeTree, ch: CodeHandler)(implicit locals: Locals) {
    tpe match {
      case Int32Type =>
        ch << New(BoxedIntClass) << DUP_X1 << SWAP << InvokeSpecial(BoxedIntClass, constructorName, "(I)V")

      case BooleanType | UnitType =>
        ch << New(BoxedBoolClass) << DUP_X1 << SWAP << InvokeSpecial(BoxedBoolClass, constructorName, "(Z)V")

      case CharType =>
        ch << New(BoxedCharClass) << DUP_X1 << SWAP << InvokeSpecial(BoxedCharClass, constructorName, "(C)V")

      case at @ ArrayType(et) =>
        ch << New(BoxedArrayClass) << DUP_X1 << SWAP << InvokeSpecial(BoxedArrayClass, constructorName, "(%s)V".format(typeToJVM(at)))
      case _ =>
    }
  }

  // Assumes that the top of the stack contains a value that should be of type `tpe`, and unboxes it to the right (JVM) type.
  private[codegen] def mkUnbox(tpe: TypeTree, ch: CodeHandler)(implicit locals: Locals) {
    tpe match {
      case Int32Type =>
        ch << CheckCast(BoxedIntClass) << InvokeVirtual(BoxedIntClass, "intValue", "()I")

      case BooleanType | UnitType =>
        ch << CheckCast(BoxedBoolClass) << InvokeVirtual(BoxedBoolClass, "booleanValue", "()Z")

      case CharType =>
        ch << CheckCast(BoxedCharClass) << InvokeVirtual(BoxedCharClass, "charValue", "()C")

      case ct : ClassType =>
        val (cn, _) = leonClassToJVMInfo(ct.classDef).getOrElse {
          throw new CompilationException("Unsupported class type : " + ct)
        }
        ch << CheckCast(cn)

      case IntegerType =>
        ch << CheckCast(BigIntClass)

      case tt : TupleType =>
        ch << CheckCast(TupleClass)

      case st : SetType =>
        ch << CheckCast(SetClass)

      case mt : MapType =>
        ch << CheckCast(MapClass)

      case ft : FunctionType =>
        ch << CheckCast(LambdaClass)

      case tp : TypeParameter => 

      case tp : ArrayType =>
        ch << CheckCast(BoxedArrayClass) << InvokeVirtual(BoxedArrayClass, "arrayValue", "()%s".format(typeToJVM(tp)))
        ch << CheckCast(typeToJVM(tp))

      case _ =>
        throw new CompilationException("Unsupported type in unboxing : " + tpe)
    }
  }

  private[codegen] def mkBranch(cond: Expr, thenn: String, elze: String, ch: CodeHandler, canDelegateToMkExpr: Boolean = true)(implicit locals: Locals) {
    cond match {
      case BooleanLiteral(true) =>
        ch << Goto(thenn)

      case BooleanLiteral(false) =>
        ch << Goto(elze)

      case And(es) =>
        val fl = ch.getFreshLabel("andnext")
        mkBranch(es.head, fl, elze, ch)
        ch << Label(fl)
        mkBranch(andJoin(es.tail), thenn, elze, ch)

      case Or(es) =>
        val fl = ch.getFreshLabel("ornext")
        mkBranch(es.head, thenn, fl, ch)
        ch << Label(fl)
        mkBranch(orJoin(es.tail), thenn, elze, ch) 

      case Implies(l, r) =>
        mkBranch(or(not(l), r), thenn, elze, ch)

      case Not(c) =>
        mkBranch(c, elze, thenn, ch)

      case Variable(b) =>
        load(b, ch)
        ch << IfEq(elze) << Goto(thenn)

      case Equals(l,r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        l.getType match {
          case Int32Type | BooleanType | UnitType | CharType =>
            ch << If_ICmpEq(thenn) << Goto(elze)

          case _ =>
            ch << InvokeVirtual("java/lang/Object", "equals", "(Ljava/lang/Object;)Z")
            ch << IfEq(elze) << Goto(thenn)
        }

      case LessThan(l,r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        l.getType match {
          case Int32Type =>
            ch << If_ICmpLt(thenn) << Goto(elze) 
          case IntegerType =>
            ch << InvokeVirtual(BigIntClass, "lessThan", "(L%s;)Z".format(BigIntClass)) 
            ch << IfEq(elze) << Goto(thenn)
        }

      case GreaterThan(l,r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        l.getType match {
          case Int32Type =>
            ch << If_ICmpGt(thenn) << Goto(elze) 
          case IntegerType =>
            ch << InvokeVirtual(BigIntClass, "greaterThan", "(L%s;)Z".format(BigIntClass)) 
            ch << IfEq(elze) << Goto(thenn)
        }

      case LessEquals(l,r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        l.getType match {
          case Int32Type =>
            ch << If_ICmpLe(thenn) << Goto(elze) 
          case IntegerType =>
            ch << InvokeVirtual(BigIntClass, "lessEquals", "(L%s;)Z".format(BigIntClass)) 
            ch << IfEq(elze) << Goto(thenn)
        }

      case GreaterEquals(l,r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        l.getType match {
          case Int32Type =>
            ch << If_ICmpGe(thenn) << Goto(elze) 
          case IntegerType =>
            ch << InvokeVirtual(BigIntClass, "greaterEquals", "(L%s;)Z".format(BigIntClass)) 
            ch << IfEq(elze) << Goto(thenn)
        }
  
      case IfExpr(c, t, e) => 
        val innerThen = ch.getFreshLabel("then")
        val innerElse = ch.getFreshLabel("else")
        mkBranch(c, innerThen, innerElse, ch)
        ch << Label(innerThen)
        mkBranch(t, thenn, elze, ch)
        ch << Label(innerElse)
        mkBranch(e, thenn, elze, ch)

      case cci@CaseClassInstanceOf(cct, e) =>
        mkExpr(cci, ch)
        ch << IfEq(elze) << Goto(thenn)

      case other if canDelegateToMkExpr =>
        mkExpr(other, ch, canDelegateToMkBranch = false)
        ch << IfEq(elze) << Goto(thenn)

      case other => throw CompilationException("Unsupported branching expr. : " + other) 
    }
  }

  private def load(id: Identifier, ch: CodeHandler)(implicit locals: Locals): Unit = {
    locals.varToArg(id) match {
      case Some(slot) =>
        ch << ALoad(1) << Ldc(slot) << AALOAD
        mkUnbox(id.getType, ch)
      case None => locals.varToClosure(id) match {
        case Some((afName, nme, tpe)) =>
          ch << ALoad(0) << GetField(afName, nme, tpe)
        case None => locals.varToLocal(id) match {
          case Some(slot) =>
            val instr = id.getType match {
              case Int32Type | CharType | BooleanType | UnitType => ILoad(slot)
              case _ => ALoad(slot)
            }
            ch << instr
          case None => throw CompilationException("Unknown variable : " + id)
        }
      }
    }
  }

  /**
    * Compiles a lazy field $lzy, owned by the module/ class $owner.
    * 
    * To define a lazy field, we have to add an accessor method and an underlying field.
    * The accessor method has the name of the original (Scala) lazy field and can be public.
    * The underlying field has a different name, is private, and is of a boxed type 
    * to support null value (to signify uninitialized). 
    * 
    * @param lzy The lazy field to be compiled
    * @param owner The module/class containing $lzy
    */
  def compileLazyField(lzy : FunDef, owner : Definition) { 
    ctx.reporter.internalAssertion(lzy.canBeLazyField, s"Trying to compile non-lazy ${lzy.id.name} as a lazy field")
        
    val (_, accessorName, _ ) = leonFunDefToJVMInfo(lzy).get
    val cf = classes(owner)
    val cName = defToJVMName(owner)
    
    val isStatic = owner.isInstanceOf[ModuleDef]
    
    // Name of the underlying field
    val underlyingName = underlyingField(accessorName)
    // Underlying field is of boxed type
    val underlyingType = typeToJVMBoxed(lzy.returnType)
    
    // Underlying field. It is of a boxed type
    val fh = cf.addField(underlyingType,underlyingName)
    fh.setFlags( if (isStatic) {(
      FIELD_ACC_STATIC | 
      FIELD_ACC_PRIVATE
    ).asInstanceOf[U2] } else {
      FIELD_ACC_PRIVATE
    }) // FIXME private etc?
      
    // accessor method
    locally {
      val parameters = if (params.requireMonitor) {
        Seq("L" + MonitorClass + ";")
      } else Seq()
      
      val accM = cf.addMethod(typeToJVM(lzy.returnType), accessorName, parameters : _*)
      accM.setFlags( if (isStatic) {(
        METHOD_ACC_STATIC | // FIXME other flags? Not always public?
        METHOD_ACC_PUBLIC
      ).asInstanceOf[U2] } else {
        METHOD_ACC_PUBLIC
      }) 
      val ch = accM.codeHandler
      val body = lzy.body.getOrElse(throw CompilationException("Lazy field without body?"))
      val initLabel = ch.getFreshLabel("isInitialized")
      
      if (params.requireMonitor) {
        ch << ALoad(if (isStatic) 0 else 1) << InvokeVirtual(MonitorClass, "onInvoke", "()V") 
      }
      
      if (isStatic) {
        ch << GetStatic(cName, underlyingName, underlyingType)
      } else {
        ch << ALoad(0) << GetField(cName, underlyingName, underlyingType) // if (lzy == null)
      }
      // oldValue
      ch << DUP << IfNonNull(initLabel) 
      // null
      ch << POP
      // 
      mkBoxedExpr(body,ch)(NoLocals(isStatic)) // lzy = <expr> 
      ch << DUP
      // newValue, newValue 
      if (isStatic) {
        ch << PutStatic(cName, underlyingName, underlyingType)
        //newValue
      }
      else {
        ch << ALoad(0) << SWAP
        // newValue, object, newValue
        ch << PutField (cName, underlyingName, underlyingType)
        //newValue
      }
      ch << Label(initLabel)  // return lzy 
      //newValue
      lzy.returnType match {
        case Int32Type | BooleanType | UnitType | CharType => 
          // Since the underlying field only has boxed types, we have to unbox them to return them
          mkUnbox(lzy.returnType, ch)(NoLocals(isStatic)) 
          ch << IRETURN      
        case IntegerType | _ : ClassType | _ : TupleType | _ : SetType | _ : MapType | _ : ArrayType | _: TypeParameter => 
          ch << ARETURN
        case other => throw CompilationException("Unsupported return type : " + other.getClass)
      }
      ch.freeze 
    }
  }
    
  /** Compile the (strict) field $field which is owned by class $owner */
  def compileStrictField(field : FunDef, owner : Definition) = {

    ctx.reporter.internalAssertion(field.canBeStrictField, 
      s"Trying to compile ${field.id.name} as a strict field")
    val (_, fieldName, _) = leonFunDefToJVMInfo(field).get

    val cf = classes(owner)
    val fh = cf.addField(typeToJVM(field.returnType),fieldName)
    fh.setFlags( owner match {
      case _ : ModuleDef => (
        FIELD_ACC_STATIC | 
        FIELD_ACC_PUBLIC | // FIXME
        FIELD_ACC_FINAL
      ).asInstanceOf[U2]
      case _ => (
        FIELD_ACC_PUBLIC | // FIXME
        FIELD_ACC_FINAL
      ).asInstanceOf[U2]
    })
  }
  
  /** Initializes a lazy field to null
   *  @param ch the codehandler to add the initializing code to
   *  @param className the name of the class in which the field is initialized 
   *  @param lzy the lazy field to be initialized
   *  @param isStatic true if this is a static field
   */
  def initLazyField(ch: CodeHandler, className : String,  lzy : FunDef, isStatic: Boolean) = {
    val (_, name, _) = leonFunDefToJVMInfo(lzy).get
    val underlyingName = underlyingField(name) 
    val jvmType = typeToJVMBoxed(lzy.returnType)
    if (isStatic){
      ch << ACONST_NULL << PutStatic(className, underlyingName, jvmType)
    } else {
      ch << ALoad(0) << ACONST_NULL << PutField(className, underlyingName, jvmType)
    }
  }
  
  /** Initializes a (strict) field
   *  @param ch the codehandler to add the initializing code to
   *  @param className the name of the class in which the field is initialized 
   *  @param field the field to be initialized
   *  @param isStatic true if this is a static field
   */
  def initStrictField(ch : CodeHandler, className : String, field: FunDef,  isStatic: Boolean) { 
    val (_, name , _) = leonFunDefToJVMInfo(field).get
    val body = field.body.getOrElse(throw CompilationException("No body for field?"))
    val jvmType = typeToJVM(field.returnType)
    
    mkExpr(body, ch)(NoLocals(isStatic)) // FIXME Locals?  
    
    if (isStatic){
      ch << PutStatic(className, name, jvmType)
    } else {
      ch << ALoad(0) << SWAP << PutField (className, name, jvmType)
    }
  }   
  
  
  def compileAbstractClassDef(acd : AbstractClassDef) {    
    
    val cName = defToJVMName(acd)

    val cf  = classes(acd)

    cf.setFlags((
      CLASS_ACC_SUPER |
      CLASS_ACC_PUBLIC |
      CLASS_ACC_ABSTRACT
    ).asInstanceOf[U2])

    cf.addInterface(CaseClassClass)

    // add special monitor for method invocations 
    if (params.doInstrument) {
      val fh = cf.addField("I", instrumentedField)
      fh.setFlags(FIELD_ACC_PUBLIC)
    }
    
    val (fields, methods) = acd.methods partition { _.canBeField }
    val (strictFields, lazyFields) = fields partition { _.canBeStrictField }
    
    // Compile methods
    for (method <- methods) {
      compileFunDef(method,acd)
    }
    
    // Compile lazy fields
    for (lzy <- lazyFields) {
      compileLazyField(lzy, acd)
    }
    
    // Compile strict fields
    for (field <- strictFields) {
      compileStrictField(field, acd)
    }
    
    // definition of the constructor
    if (fields.isEmpty && !params.doInstrument && !params.requireMonitor) cf.addDefaultConstructor else {
      
      val constrParams = if (params.requireMonitor) {
        Seq("L" + MonitorClass + ";")
      } else Seq()
      
      val cch = cf.addConstructor(constrParams : _*).codeHandler
      // Abstract classes are hierarchy roots, so call java.lang.Object constructor
      cch << ALoad(0)
      cch << InvokeSpecial("java/lang/Object", constructorName, "()V")
      
      // Initialize special monitor field
      if (params.doInstrument) {
        cch << ALoad(0)
        cch << Ldc(0)
        cch << PutField(cName, instrumentedField, "I")
      }
      
      for (lzy <- lazyFields) { initLazyField(cch, cName, lzy, false) }
      for (field <- strictFields) { initStrictField(cch, cName, field, false)}
 
      cch << RETURN
      cch.freeze
    }
    
  }

  /**
   * Instrument read operations
   */
  val instrumentedField = "__read"

  def instrumentedGetField(ch: CodeHandler, cct: ClassType, id: Identifier)(implicit locals: Locals): Unit = {
    val ccd = cct.classDef
    ccd.fields.zipWithIndex.find(_._1.id == id) match {
      case Some((f, i)) =>
        val expType = cct.fields(i).tpe

        val cName = defToJVMName(ccd)
        if (params.doInstrument) {
          ch << DUP << DUP
          ch << GetField(cName, instrumentedField, "I")
          ch << Ldc(1)
          ch << Ldc(i)
          ch << ISHL
          ch << IOR
          ch << PutField(cName, instrumentedField, "I")
        }
        ch << GetField(cName, f.id.name, typeToJVM(f.tpe))

        f.tpe match {
          case TypeParameter(_) =>
            mkUnbox(expType, ch)
          case _ =>
        }
      case None =>
        throw CompilationException("Unknown field: "+ccd.id.name+"."+id)
    }
  }



  def compileCaseClassDef(ccd: CaseClassDef) {

    val cName = defToJVMName(ccd)
    val pName = ccd.parent.map(parent => defToJVMName(parent.classDef))
    // An instantiation of ccd with its own type parameters 
    val cct = CaseClassType(ccd, ccd.tparams.map(_.tp))
    
    val cf = classes(ccd)

    cf.setFlags((
      CLASS_ACC_SUPER |
      CLASS_ACC_PUBLIC |
      CLASS_ACC_FINAL
    ).asInstanceOf[U2])

    if(ccd.parent.isEmpty) {
      cf.addInterface(CaseClassClass)
    }

    locally { 
      
      val (fields, methods) = ccd.methods partition { _.canBeField }
      val (strictFields, lazyFields) = fields partition { _.canBeStrictField }
      
      // Compile methods
      for (method <- methods) {
        compileFunDef(method,ccd)
      }
      
      // Compile lazy fields
      for (lzy <- lazyFields) {
        compileLazyField(lzy, ccd) 
      }
      
      // Compile strict fields
      for (field <- strictFields) {
        compileStrictField(field, ccd) 
      }
      
      // Case class parameters
      val namesTypes = ccd.fields.map { vd => (vd.id.name, typeToJVM(vd.tpe)) }
  
      // definition of the constructor
      if(!params.doInstrument && !params.requireMonitor && ccd.fields.isEmpty && ccd.methods.filter{ _.canBeField }.isEmpty) {
        cf.addDefaultConstructor
      } else {
        for((nme, jvmt) <- namesTypes) {
          val fh = cf.addField(jvmt, nme)
          fh.setFlags((
            FIELD_ACC_PUBLIC |
            FIELD_ACC_FINAL
          ).asInstanceOf[U2])
        }
  
        if (params.doInstrument) {
          val fh = cf.addField("I", instrumentedField)
          fh.setFlags(FIELD_ACC_PUBLIC)
        }
  
        // If we are monitoring function calls, we have an extra argument on the constructor
        val realArgs = if (params.requireMonitor) {
          ("L" + MonitorClass + ";") +: (namesTypes map (_._2))
        } else (namesTypes map (_._2))
        
        // Offset of the first Scala parameter of the constructor
        val paramOffset = if (params.requireMonitor) 2 else 1
        
        val cch = cf.addConstructor(realArgs.toList).codeHandler
  
        if (params.doInstrument) {
          cch << ALoad(0)
          cch << Ldc(0)
          cch << PutField(cName, instrumentedField, "I")
        }
  
        var c = paramOffset
        for((nme, jvmt) <- namesTypes) {
          cch << ALoad(0)
          cch << (jvmt match {
            case "I" | "Z" => ILoad(c)
            case _ => ALoad(c)
          })
          cch << PutField(cName, nme, jvmt)
          c += 1
        }
        
        // Call parent constructor AFTER initializing case class parameters
        if (ccd.parent.isDefined) {
          // Load this
          cch << ALoad(0)
          // Load monitor object
          if (params.requireMonitor) cch << ALoad(1)
          val constrSig = if (params.requireMonitor) "(L" + MonitorClass + ";)V" else "()V"
          cch << InvokeSpecial(pName.get, constructorName, constrSig)
        } else {
          // Call constructor of java.lang.Object
          cch << ALoad(0)
          cch << InvokeSpecial("java/lang/Object", constructorName, "()V")
        }

        
        // Now initialize fields
        for (lzy <- lazyFields) { initLazyField(cch, cName, lzy, false)}  
        for (field <- strictFields) { initStrictField(cch, cName , field, false)}
        cch << RETURN
        cch.freeze
      }
    }

    locally {
      val pnm = cf.addMethod("I", "__getRead")
      pnm.setFlags((
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL
      ).asInstanceOf[U2])

      val pnch = pnm.codeHandler

      pnch << ALoad(0) << GetField(cName, instrumentedField, "I") << IRETURN

      pnch.freeze
    }

    locally {
      val pnm = cf.addMethod("Ljava/lang/String;", "productName")
      pnm.setFlags((
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL
      ).asInstanceOf[U2])

      val pnch = pnm.codeHandler

      pnch << Ldc(cName) << ARETURN

      pnch.freeze
    }

    locally {
      val pem = cf.addMethod("[Ljava/lang/Object;", "productElements")
      pem.setFlags((
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL
      ).asInstanceOf[U2])

      val pech = pem.codeHandler

      pech << Ldc(ccd.fields.size)
      pech << NewArray("java/lang/Object")

      for ((f, i) <- ccd.fields.zipWithIndex) {
        pech << DUP
        pech << Ldc(i)
        pech << ALoad(0)
        // WARNING: Passing NoLocals(false) is kind of a hack, 
        // since there is no monitor object anywhere in this method. 
        // We are saved because it is not used anywhere, 
        // but beware if you decide to add any mkExpr and the like.
        instrumentedGetField(pech, cct, f.id)(NoLocals(false))
        mkBox(f.tpe, pech)(NoLocals(false))
        pech << AASTORE
      }

      pech << ARETURN
      pech.freeze
    }

    // definition of equals
    locally {
      val emh = cf.addMethod("Z", "equals", "Ljava/lang/Object;")
      emh.setFlags((
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL
      ).asInstanceOf[U2])

      val ech = emh.codeHandler

      val notRefEq = ech.getFreshLabel("notrefeq")
      val notEq = ech.getFreshLabel("noteq")
      val castSlot = ech.getFreshVar

      // If references are equal, trees are equal.
      ech << ALoad(0) << ALoad(1) << If_ACmpNe(notRefEq) << Ldc(1) << IRETURN << Label(notRefEq)

      // We check the type (this also checks against null)....
      ech << ALoad(1) << InstanceOf(cName) << IfEq(notEq)

      // ...finally, we compare fields one by one, shortcircuiting on disequalities.
      if(!ccd.fields.isEmpty) {
        ech << ALoad(1) << CheckCast(cName) << AStore(castSlot)

        for(vd <- ccd.fields) {
          // WARNING: Passing NoLocals(false) is kind of a hack, 
          // since there is no monitor object anywhere in this method. 
          // We are saved because it is not used anywhere, 
          // but beware if you decide to add any mkExpr and the like.
          ech << ALoad(0)
          instrumentedGetField(ech, cct, vd.id)(NoLocals(false)) 
          ech << ALoad(castSlot)
          instrumentedGetField(ech, cct, vd.id)(NoLocals(false))

          typeToJVM(vd.getType) match {
            case "I" | "Z" =>
              ech << If_ICmpNe(notEq)

            case ot =>
              ech << InvokeVirtual("java/lang/Object", "equals", "(Ljava/lang/Object;)Z") << IfEq(notEq)
          }
        }
      } 

      ech << Ldc(1) << IRETURN << Label(notEq) << Ldc(0) << IRETURN
      ech.freeze
    }

    // definition of hashcode
    locally {
      val hashFieldName = "$leon$hashCode"
      cf.addField("I", hashFieldName).setFlags((FIELD_ACC_PRIVATE).asInstanceOf[U2])
      val hmh = cf.addMethod("I", "hashCode", "")
      hmh.setFlags((
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL
      ).asInstanceOf[U2])

      val hch = hmh.codeHandler
      
      val wasNotCached = hch.getFreshLabel("wasNotCached")

      hch << ALoad(0) << GetField(cName, hashFieldName, "I") << DUP
      hch << IfEq(wasNotCached)
      hch << IRETURN
      hch << Label(wasNotCached) << POP
      hch << ALoad(0) << InvokeVirtual(cName, "productElements", "()[Ljava/lang/Object;")
      hch << ALoad(0) << InvokeVirtual(cName, "productName", "()Ljava/lang/String;")
      hch << InvokeVirtual("java/lang/String", "hashCode", "()I")
      hch << InvokeStatic(HashingClass, "seqHash", "([Ljava/lang/Object;I)I") << DUP
      hch << ALoad(0) << SWAP << PutField(cName, hashFieldName, "I") 
      hch << IRETURN
      
      hch.freeze
    }

  }
}
