import scala.collection.Map

object SatFun {

  sealed abstract class Formula
  case class And(f1: Formula, f2: Formula) extends Formula
  case class Or(f1: Formula, f2: Formula) extends Formula
  case class Not(f: Formula) extends Formula
  case class Var(i: Int) extends Formula

  //vars are numbered from 2 to n+1, and Not(Var(n)) is represented as -n. 1 is true and -1 is false
  sealed abstract class VarList
  case class VarCons(head: Int, tail: VarList) extends VarList
  case class VarNil() extends VarList
  case class VarLit(value: Boolean) extends VarList

  sealed abstract class ClauseList
  case class ClauseCons(head: VarList, tail: ClauseList) extends ClauseList
  case class ClauseNil() extends ClauseList
  case class ClauseLit(value: Boolean) extends ClauseList

  def eval(formula: Formula, assignment: Map[Int, Boolean]): Boolean = formula match {
    case Var(n) => assignment(n)
    case Not(f) => !eval(f, assignment)
    case And(f1, f2) => eval(f1, assignment) && eval(f2, assignment)
    case Or(f1, f2) => eval(f1, assignment) || eval(f2, assignment)
  }

  def evalCnf(clauses: ClauseList, assignment: Map[Int, Boolean]): Boolean = clauses match {
    case ClauseCons(cl, cls) => evalClauseCnf(cl, assignment) && evalCnf(cls, assignment)
    case ClauseNil() => false
    case ClauseLit(b) => b
  }
  def evalDnf(clauses: ClauseList, assignment: Map[Int, Boolean]): Boolean = clauses match {
    case ClauseCons(cl, cls) => evalClauseDnf(cl, assignment) || evalDnf(cls, assignment)
    case ClauseNil() => true
    case ClauseLit(b) => b
  }
  def evalClauseCnf(clause: VarList, assignment: Map[Int, Boolean]): Boolean = clause match {
    case VarCons(v, vs) => (if(v < 0) assignment(-v) else assignment(v)) || evalClauseCnf(vs, assignment)
    case VarNil() => false
    case VarLit(b) => b
  }
  def evalClauseDnf(clause: VarList, assignment: Map[Int, Boolean]): Boolean = clause match {
    case VarCons(v, vs) => (if(v < 0) assignment(-v) else assignment(v)) && evalClauseDnf(vs, assignment)
    case VarNil() => true
    case VarLit(b) => b
  }

  def concatClauses(cll1: ClauseList, cll2: ClauseList): ClauseList = cll1 match {
    case ClauseCons(cl, tail) => ClauseCons(cl, concatClauses(tail, cll2))
    case ClauseNil() => cll2
    case ClauseLit(b) => ClauseCons(VarLit(b), cll2)
  }

  def concatVars(l1: VarList, l2: VarList): VarList = l1 match {
    case VarCons(v, vs) => VarCons(v, concatVars(vs, l2))
    case VarNil() => l2
    case VarLit(b) => if(b) VarCons(1, l2) else VarCons(-1, l2)
  }

  def distributeClause(cl: VarList, cll: ClauseList): ClauseList = cll match {
    case ClauseCons(cl2, cl2s) => ClauseCons(concatVars(cl, cl2), distributeClause(cl, cl2s))
    case ClauseNil() => ClauseNil()
    case ClauseLit(b) => if(b) ClauseCons(VarCons(1, cl), ClauseNil()) else ClauseCons(VarCons(-1, cl), ClauseNil())
  }

  def distribute(cll1: ClauseList, cll2: ClauseList): ClauseList = cll1 match {
    case ClauseCons(cl, cls) => concatClauses(distributeClause(cl, cll2), distribute(cls, cll2))
    case ClauseNil() => cll2
    case ClauseLit(b) => distributeClause(VarLit(b), cll2)
  }

  def negateClauses(cll: ClauseList): ClauseList = cll match {
    case ClauseCons(cl, cls) => ClauseCons(negateVars(cl), negateClauses(cls))
    case ClauseNil() => ClauseNil()
    case ClauseLit(b) => ClauseLit(!b)
  }

  def negateVars(lst: VarList): VarList = lst match {
    case VarCons(v, vs) => VarCons(-v, negateVars(vs))
    case VarNil() => VarNil()
    case VarLit(b) => VarLit(!b)
  }

  def cnfNaive(formula: Formula): ClauseList = formula match {
    case And(f1, f2) => {
      val cnf1 = cnfNaive(f1)
      val cnf2 = cnfNaive(f2)
      concatClauses(cnf1, cnf2)
    }
    case Or(f1, f2) => {
      val cnf1 = cnfNaive(f1)
      val cnf2 = cnfNaive(f2)
      distribute(cnf1, cnf2)
    }
    case Not(And(f1, f2)) => cnfNaive(Or(Not(f1), Not(f2)))
    case Not(Or(f1, f2)) => cnfNaive(And(Not(f1), Not(f2)))
    case Not(Not(f)) => cnfNaive(f)
    case Not(Var(n)) => ClauseCons(VarCons(-n, VarNil()), ClauseNil())
    case Var(n) => ClauseCons(VarCons(n, VarNil()), ClauseNil())
  }
  def dnfNaive(formula: Formula): ClauseList = formula match {
    case And(f1, f2) => {
      val dnf1 = dnfNaive(f1)
      val dnf2 = dnfNaive(f2)
      distribute(dnf1, dnf2)
    }
    case Or(f1, f2) => {
      val dnf1 = dnfNaive(f1)
      val dnf2 = dnfNaive(f2)
      concatClauses(dnf1, dnf2)
    }
    case Not(And(f1, f2)) => dnfNaive(Or(Not(f1), Not(f2)))
    case Not(Or(f1, f2)) => dnfNaive(And(Not(f1), Not(f2)))
    case Not(Not(f)) => dnfNaive(f)
    case Not(Var(n)) => ClauseCons(VarCons(-n, VarNil()), ClauseNil())
    case Var(n) => ClauseCons(VarCons(n, VarNil()), ClauseNil())
  }
  def property1(formula: Formula, assignment: Map[Int, Boolean]): Boolean = {
    val dnfFormula = dnfNaive(formula)
    eval(formula, assignment) == evalDnf(dnfFormula, assignment)
  } ensuring(_ == true)


  def vars(formula: Formula): Set[Int] = formula match {
    case Var(n) => Set(n)
    case Not(f) => vars(f)
    case And(f1, f2) => vars(f1) ++ vars(f2)
    case Or(f1, f2) => vars(f1) ++ vars(f2)
  }
  def isContradictory(clause: VarList, vars: Set[Int]): Boolean = clause match {
    case VarCons(v, vs) => vars.contains(-v) || vars.contains(-1) || isContradictory(vs, vars ++ Set(v))
    case VarNil() => false
    case VarLit(b) => !b
  }
  def isSatDnf(clauses: ClauseList): Boolean = clauses match {
    case ClauseCons(cl, cls) => !isContradictory(cl, Set.empty) || isSatDnf(cls)
    case ClauseNil() => false
    case ClauseLit(b) => b
  }

  //for substitute we assume we are dealing with a cnf formula
  def substitute(formula: ClauseList, variable: Int, value: Boolean): ClauseList = formula match {
    case ClauseNil() => ClauseNil()
    case ClauseCons(cl, cls) => ClauseCons(substitute(cl, variable, value), substitute(cls, variable, value))
    case ClauseLit(b) => ClauseLit(b)
  }

  def substitute(vars: VarList, variable: Int, value: Boolean): VarList = vars match {
    case VarNil() => VarNil()
    case VarLit(b) => VarLit(b)
    case VarCons(v, vs) => 
      if     (v == variable && value)   VarLit(true)
      else if(v == variable && !value)  substitute(vs, variable, value)
      else if(v == -variable && value)  substitute(vs, variable, value)
      else if(v == -variable && !value) VarLit(true)
      else                              VarCons(v, substitute(vs, variable, value))
  }

  def choose(formula: ClauseList): Int = formula match {
    case ClauseCons(varList, cls) => varList match {
      case VarCons(head, vs) => head
      case VarNil() => 0
      case VarLit(b) => 0
    }
    case ClauseNil() => 0
    case ClauseLit(b) => 0
  }

  def dpll(formula: ClauseList): Boolean = formula match {
    case ClauseNil() => true
    case ClauseLit(b) => b
    case _ => {
      val chosenVar = choose(formula)
      val lhs = dpll(substitute(formula, chosenVar, true))
      val rhs = dpll(substitute(formula, chosenVar, false))
      lhs || rhs
    }
  }



//  def main(args: Array[String]) {
//    val f1 = And(Var(1), Or(Var(1), Not(Var(2)), Var(3)), Var(2), Not(Var(3)))
//    val dnff1 = clauses2list(dnfNaive(f1))
//    val vars1 = vars(f1)
//    vars.foreach(v => {
//
//
//    })
//    println(f1 + " translated in dnf as:\n\t" + dnff1.mkString("\n\t"))
//  }

  //some non-leon functions to test the program with scala
  object False {
    def apply(): Formula = And(Var(1), Not(Var(1)))
  }
  object True {
    def apply(): Formula = Or(Var(1), Not(Var(1)))
  }
  object Or {
    def apply(fs: Formula*): Formula = fs match {
      case Seq() => False()
      case Seq(f) => f
      case fs => fs.reduceLeft((f1, f2) => Or(f1, f2))
    }
  }
  object And {
    def apply(fs: Formula*): Formula = fs match {
      case Seq() => True()
      case Seq(f) => f
      case fs => fs.reduceLeft((f1, f2) => And(f1, f2))
    }
  }
  def clause2list(cl: VarList): List[Int] = cl match {
    case VarCons(v, vs) => v :: clause2list(vs)
    case VarNil() => Nil
    case VarLit(b) => if(b) List(1) else List(-1)
  }
  def clauses2list(cll: ClauseList): List[List[Int]] = cll match {
    case ClauseCons(cl, cls) => clause2list(cl) :: clauses2list(cls)
    case ClauseNil() => Nil
    case ClauseLit(b) => if(b) List(List(1)) else List(List(-1))
  }
}
