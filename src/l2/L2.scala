//import main.Interpreter;
import scala._
package l2 {
  abstract class Type
  case class TInteger() extends Type
  case class TBoolean() extends Type
  case class TUnit() extends Type
  case class TFunction(t1: Type, t2: Type) extends Type
  case class TLists(t: Type) extends Type
  case class Empty() extends Type
  abstract class Expr
  abstract case class Value(v:Any) extends Expr
  abstract case class ValueSimple(vl:Any) extends Value(vl)
  case class Address(lab:String) extends ValueSimple
  case class N (n:scala.Int) extends ValueSimple(n)
  case class B (b:scala.Boolean) extends ValueSimple(b)
  abstract class Op
  case class AritOp extends Op
  case class CompOp extends Op
  case class BoolOp extends Op
  case class Sum extends AritOp
  case class Sub extends AritOp
  case class Mul extends AritOp
  case class Gr extends CompOp
  case class Le extends CompOp
  case class Eq extends CompOp
  case class Not extends BoolOp
  case class And extends BoolOp
  case class Or extends BoolOp
  case class Oper(e1: Expr, e2: Expr, op:Op) extends Expr
  case class If (e1: Expr, e2: Expr, e3: Expr) extends Expr
  case class Asg(l:Address, e2: Expr) extends Expr
  case class Deref(l:Address) extends Expr
  case class Skip() extends Value
  case class Seq (e1: Expr, e2: Expr) extends Expr
  case class W (e1: Expr, e2: Expr) extends Expr
  case class Fn (s:String, t: Type, e: Expr) extends Value
  case class App (e1: Expr, e2: Expr) extends Expr
  case class X (s:String) extends Expr
  case class Let (s:String, t: Type, e1: Expr, e2: Expr) extends Expr
  case class LetRec (f: String, t: Type, e1: Expr, e2: Expr) extends Expr
  case class Print(e:Expr) extends Expr
  case class Nil() extends ValueSimple
  case class Lists(e1: Expr, e2: Expr) extends Value
  case class Head(e: Expr) extends Expr
  case class Tail(e: Expr) extends Expr

  case class TypeException(val e:Expr) extends Exception
  case class ExecutionException(val e:Expr) extends Exception



//TODO: listas
//TODO: exceptions
  object L2Interpreter /*extends Interpreter*/{
    def typecheck(e:Expr, gamma: Map[String,Type]) : Type =
      e match {
        case N (_) => TInteger()
        case B (_) => TBoolean()
        case Oper(e1, e2, op) => (typecheck(e1,gamma), typecheck(e2,gamma), op) match {
            case (TInteger(), TInteger(), AritOp()) => TInteger()
            case (TInteger(), TInteger(), CompOp()) => TBoolean()
            case (TBoolean(), TBoolean(), BoolOp()) => TBoolean()
            case _ => throw TypeException(e)
          }
        case If (e1, e2, e3) => (typecheck(e1,gamma), typecheck(e2,gamma), typecheck(e3,gamma)) match {
            case (TBoolean(), t1, t2) if (t1==t2) => t1
            case _ => throw TypeException(e)
          }
        case Asg(l, e2) => (l, typecheck(e2,gamma)) match {
            case (Address(_), TInteger())=> TUnit()
            case _ => throw TypeException(e)
          }
        case Deref(Address(_)) => TInteger()
        case Skip () => TUnit()
        case Seq(e1, e2) => (typecheck(e1,gamma), typecheck(e2,gamma)) match {
            case (TUnit(), t1) => t1
            case _ => throw TypeException(e)
          }
        case W (e1, e2) => (typecheck(e1,gamma), typecheck(e2,gamma)) match {
            case (TBoolean(), TUnit()) => TUnit()
            case _ => throw TypeException(e)
          }
        case X(x) => gamma(x)
        case Fn(x, t, e) => TFunction(t, typecheck(e, gamma(x)=t))
        case App(e1, e2) => (typecheck(e1,gamma), typecheck(e2,gamma)) match {
            case (TFunction(t1, t2), t3) if t1==t3 => t2
            case _ => throw TypeException(e)
          }
        case Let(x, t, e1, e2) => (typecheck(e1, gamma), typecheck(e2, gamma(x)=t)) match {
            case (t1:Type, tl:Type) if t1==t => tl
            case _ => throw TypeException(e)
          }
        case LetRec(f, TFunction(t1, t2), e1, e2) =>
          (typecheck(e1, gamma(f)=TFunction(t1, t2)), typecheck(e2, gamma(f)=TFunction(t1, t2))) match {
            case (TFunction(t1, t2), t) => t
            case _ => throw TypeException(e)
          }
        case Print(e1) => typecheck(e1, gamma) match {
            case e => TUnit()
          }
        case Nil() => TLists(Empty())
        case Lists(e1, Nil()) => TLists(typecheck(e1, gamma))
        case Lists(e1, e2) =>
          (typecheck(e1, gamma), typecheck(e2, gamma)) match {
            case(t1, TLists(t2)) if t1 == t2 => TLists(t1)
            case _ => throw TypeException(e)
          }
        case Head(e) =>
          (typecheck(e, gamma)) match {
            case TLists(t1) => t1
            case _ => throw TypeException(e)
          }
        case Tail(e) => typecheck(e, gamma)
        case _ => throw TypeException(e)
      }
    type Memory = Map[Address, scala.Int]
    def repl(v: Value, x:String, e: Expr): Expr = e match {
      case X(y) if x==y => v //caso básico
        //recursões para quando x!=y, propagar considerando escopo
      case Fn(y, t, e) if x!=y => Fn(y, t, repl(v, x, e))
      case Let(y, t, e1, e2) if x!=y => Let(y, t, repl(v, x, e1), repl(v, x, e2))
      case LetRec(y, t, e1, e2) if x!=y => LetRec(y, t, repl(v, x, e1), repl(v, x, e2))
        //propagar recursões
      case Oper(e1, e2, op) => Oper(repl(v, x, e1), repl(v, x, e2), op)
      case App(e1, e2) => App(repl(v, x, e1), repl(v, x, e2))
      case If(e1, e2, e3) => If(repl(v, x, e1), repl(v, x, e2), repl(v, x, e3))
      case W(e1, e2) => W(repl(v, x, e1), repl(v, x, e2))
      case Asg(l, e2) => Asg(l, repl(v, x, e2))
      case Seq(e1, e2) => Seq(repl(v, x, e1), repl(v, x, e2))
      case Print(e1)=> Print(repl(v, x, e1))
        //casos sem recursão
      case e => e
    }
  
    def step(e: Expr, sigma: Memory): (Expr, Memory) = e match {
      case v:ValueSimple => (v, sigma)
      case Oper(N(v1), N(v2), Sum()) => (N(v1+v2), sigma)
      case Oper(N(v1), N(v2), Sub()) => (N(v1-v2), sigma)
      case Oper(N(v1), N(v2), Mul()) => (N(v1*v2), sigma)
      case Oper(N(v1), N(v2), Gr()) => (B(v1>=v2), sigma)
      case Oper(N(v1), N(v2), Le()) => (B(v1<=v2), sigma)
      case Oper(N(v1), N(v2), Eq()) => (B(v1==v2), sigma)
      case Oper(B(v1), B(v2), Not()) => (B((!v1)&&(!v2)), sigma)
      case Oper(B(v1), B(v2), And()) => (B(v1&&v1), sigma)
      case Oper(B(v1), B(v2), Or()) => (B(v1||v2), sigma)
      case Oper(v1:Value, v2:Value, op) => throw ExecutionException(e)
      case Oper(v1:Value, e2, op) => eval(e2,sigma) match{
          case (e2l, sigmal) => (Oper(v1, e2l, op), sigmal)
          case _ => throw ExecutionException(e)
        }
      case Oper(e1, e2, op) => eval(e1, sigma) match {
          case (e1l, sigmal) => (Oper(e1l, e2, op), sigmal)
          case _ => throw ExecutionException(e)
        }
      case If(B(true), e2, e3) => (e2, sigma)
      case If(B(false), e2, e3) => (e3, sigma)
      case If(e1, e2, e3) => eval(e1,sigma) match{
          case (e1l, sigmal) => (If(e1l, e2, e3), sigmal)
          case _ => throw ExecutionException(e)
        }
      case Seq(Skip(), e2) => (e2, sigma)
      case Seq(e1, e2) => eval(e1, sigma) match{
          case (e1l, sigmal) => (Seq(e1l, e2), sigmal)
          case _ => throw ExecutionException(e)
        }
      case Asg(l, e2) => e2 match{
          case N(n) => (Skip(), sigma(l)=n)
          case e2 => eval(e2, sigma) match {
              case (e2l, sigmal) => (Asg(l, e2l), sigmal)
              case _ => throw ExecutionException(e)
            }
        }
      case Deref(l) => (N(sigma(l)), sigma)
      case W(e1, e2) => (If(e1,Seq(e2,W(e1,e2)),Skip()), sigma)
      case App(Fn(x, t, e), v:Value) => (repl(v, x, e), sigma)
      case App(v1:Value, e2) => eval(e2,sigma) match{
          case (e2l, sigmal) => (App(v1, e2l), sigmal)
          case _ => throw ExecutionException(e)
        }
      case App(e1, e2) => eval(e1, sigma) match {
          case (e1l, sigmal) => (App(e1l, e2), sigmal)
          case _ => throw ExecutionException(e)
        }
      case Let(x, t, v : Value, e2) => (repl(v, x, e2), sigma)
      case Let(x, t, e1, e2) => eval(e1, sigma) match {
          case (e1l, sigmal) => (Let(x, t, e1l, e2), sigmal)
          case _ => throw ExecutionException(e)
        }
      case LetRec(f, TFunction(t1, t2), Fn(y, t3, e1), e2) if t1==t3 =>
        (repl(Fn(y, t1, LetRec(f, TFunction(t1, t2), Fn(y, t1, e1), e1)), f, e2), sigma)
      case Print(v:Value) => println(v.v); (Skip(), sigma)
      case Print(e1) => eval(e1, sigma) match {
          case (e1l, sigma) => (Print(e1l), sigma)
        }
      case Lists(e1:Value, e2) => eval(e2, sigma) match {
          case (e2lin, sigmalin) => (Lists(e1, e2lin), sigmalin)
          case _ => throw ExecutionException(e)
        }
      case Lists(e1, e2) => eval(e1, sigma) match {
          case (e1lin, sigmalin) => (Lists(e1lin, e2), sigmalin)
          case _ => throw ExecutionException(e)
        }
      case Head(Lists(e1:Value, e2:Value)) => (e1, sigma)
      case Head(e1) => eval(e1, sigma) match {
          case (e1lin, sigmalin) => (Head(e1lin), sigmalin)
          case _ => throw ExecutionException(e)
        }
      case Tail(Lists(e1:Value, e2:Value)) => (e2, sigma)
      case Tail(e1) => eval(e1, sigma) match {
          case (e1lin, sigmalin) => (Tail(e1lin), sigmalin)
          case _ => throw ExecutionException(e)
        }
      case _ => throw ExecutionException(e)
    }

    def eval(e: Expr, sigma:Memory): (Expr, Memory) =
      step(e,sigma) match {
        case (e:Value, sigma) => (e, sigma)
        case (e, sigma) => eval(e, sigma)
      }

    def doExpr(e:Expr, verbose:Int, sigma:Memory):Memory = {
      var t: Type = null
      var sigma_ret = sigma
      if (verbose>0)
        println("--Expression: "+e);
      try{
        t = typecheck(e, Map[String,Type]())
      }catch {
        case TypeException(e) => println("Type error in expression: "+e)
      }
      try {
        val (el:Expr, sigmal:Memory) = eval(e, sigma)
        if (verbose>0)
          println("--Sigma: "+ sigmal)
        println(":- "+t+" = "+el)
        sigma_ret=sigmal
      }catch{
        case ExecutionException(e) => println("Execution error in expression: "+e)
      }
      sigma_ret
    }

    def doExprs(es:List[Expr], verbose:Int, sigma:Memory):Memory = {
      var sigmal = sigma
      for (e <-es){
        sigmal = doExpr(e, verbose, sigmal)
        println()
      }
      sigmal
    }
  }
}