import scala.util.parsing.combinator._ ;
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
package l2 {
//TODO: verificar precedencia da aplicação e de tipos
//TODO: not

  case class ParserError(e:String) extends Exception

  object L2Parser extends StandardTokenParsers  {
    lexical.reserved += ("true", "false", "if", "then", "else", "skip",
                         "while", "do", "fn", "let", "rec", "in", "end",
                         "for", "to", "print", "int", "bool", "unit",
                         "head", "tail", "nil")
    lexical.delimiters += ("(", ")", "!", "%", "*", "/", "+", "-",
                           ">=", "<=", "=", "~", "&&", "||", ":=", ";", ":",
                           "=>", "->", ";;", " ", "~", "::")

    //Parser de tipos
    def simple_type:Parser[Type] = ("int"^^^TInteger()) | ("bool"^^^TBoolean()) | ("unit"^^^TUnit());
    def comp_type = simple_type ~ "->" ~  ttype ^^ {case t1 ~ "->" ~ t2 => TFunction(t1, t2) }
    def ttype:Parser[Type] =  "(" ~> ttype <~ ")" | comp_type | simple_type;

    def defexp : Parser[Expr] = parens | n | b | ife | deref | asg | skip | 
                                wh | id | fn | let | letRec | forTo | prt |
                                head | tail | nil;
    def exp : Parser[Expr] = (seq | let2 | letRec2 | app | defexp) ;
    def parens:Parser[Expr] = "(" ~> exp <~ ")"
        
    //Parser de Operadores
    def seq:Parser[Expr] = (listOp) * (";" ^^^ { (e1:Expr, e2:Expr) => Seq(e1, e2) } )
    def listOp:Parser[Expr] = logOp ~ "::" ~ (seq | logOp) ^^ {case e1 ~ "::" ~ e2 => Lists(e1, e2)} | logOp /*(logOp) * (
      "::" ^^^ { (e1:Expr, e2:Expr) => Lists(e1, e2) } )*/
    def logOp:Parser[Expr] = (compOp) * (
      "&&" ^^^ { (e1:Expr, e2:Expr) => Oper(e1, e2, And()) } |
      "||" ^^^ { (e1:Expr, e2:Expr) => Oper(e1, e2, Or()) } )
    def compOp:Parser[Expr] = (sumOp) * (
      ">=" ^^^ { (e1:Expr, e2:Expr) => Oper(e1, e2, Gr()) } |
      "<=" ^^^ { (e1:Expr, e2:Expr) => Oper(e1, e2, Le()) } |
      "=" ^^^ { (e1:Expr, e2:Expr) => Oper(e1, e2, Eq()) } )
    def sumOp:Parser[Expr] = (prodOp) * (
      "+" ^^^ { (e1:Expr, e2:Expr) => Oper(e1, e2, Sum()) } |
      "-" ^^^ { (e1:Expr, e2:Expr) => Oper(e1, e2, Sub()) } )
    def prodOp:Parser[Expr] = (app) * (
      "*" ^^^ { (e1:Expr, e2:Expr) => Oper(e1, e2, Mul()) } )
    def app:Parser[Expr] = (defexp) * ((" "|"%") ^^^ { (e1:Expr, e2:Expr) => App(e1, e2) })

    //Parser de Valores
    def n:Parser[N] = numericLit ^^ {case v => N(Integer.parseInt(v)) }
    def b:Parser[B] = ("true"^^^(B(true))) | ("false" ^^^ (B(false)))
    def boolNot = "~" ~ exp ^^ {case "~" ~ e => Oper(e, e, Not())}
    def ife = "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp ^^ { case "if" ~ e1 ~ "then" ~ e2 ~ "else" ~ e3 => If(e1, e2, e3) }
    def address = ident ^^ { case l => Address(l)};
    def asg = address ~ ":=" ~ (logOp) ^^ { case l ~ ":=" ~ e2 => Asg(l, e2) }
    def deref = "!" ~ address ^^ { case "!" ~ l => Deref(l) }
    def skip = "skip" ^^^ Skip()
    def wh = "while" ~ exp ~ "do" ~ exp ^^ { case "while" ~ e1 ~ "do" ~ e2 => W(e1, e2) }
    def id = ident ^^ {case x => X(x)}
    def fn:Parser[Fn] = "fn" ~ ident ~ ":" ~ ttype ~ "=>" ~ exp ^^ { case "fn" ~ x ~ ":" ~ t ~ "=>" ~ e => Fn(x, t, e) }
    def let : Parser[Expr] = "let" ~ ident ~ ":" ~ ttype ~ "=" ~ exp ~ "in" ~ exp ~ "end" ^^ { case "let" ~ x ~ ":" ~ t ~ "=" ~ e1 ~ "in" ~ e2 ~ "end" => Let(x, t, e1, e2) }
    def let2 : Parser[Expr] = "let" ~ ident ~ ":" ~ ttype ~ "=" ~ exp ~ ";" ~ exp ^^ { case "let" ~ x ~ ":" ~ t ~ "=" ~ e1 ~ ";" ~ e2 => Let(x, t, e1, e2) }
    def letRec : Parser[Expr] = "let" ~ "rec" ~ ident ~ ":" ~ ttype ~ "=" ~ exp ~ "in" ~ exp ~ "end" ^^ { case "let" ~ "rec" ~ f ~ ":" ~ t ~ "=" ~ e1 ~ "in" ~ e2 ~ "end" => LetRec(f, t, e1, e2) }
    def letRec2 : Parser[Expr] = "let" ~ "rec" ~ ident ~ ":" ~ ttype ~ "=" ~ exp ~ ";" ~ exp ^^ { case "let" ~ "rec" ~ f ~ ":" ~ t ~ "=" ~ e1 ~ ";" ~ e2 => LetRec(f, t, e1, e2) }
    def forTo: Parser[Expr] = "for" ~ address ~ ":=" ~ exp ~ "to" ~ exp ~ "do" ~ exp ^^ { case "for" ~ l ~ ":=" ~ e1 ~ "to" ~ e2 ~ "do" ~ e3 => Seq(Asg(l, e1), W(Oper(Deref(l), e2, Le()), Seq(e3, Asg(l, Oper(Deref(l), N(1), Sum()))))) }
    def prt = "print" ~ exp ^^ {case "print" ~ e => Print(e) }
    def head = "head" ~ exp ^^ {case "head" ~ e => Head(e)}
    def tail = "tail" ~ exp ^^ {case "tail" ~ e => Tail(e)}
    def nil = "nil" ^^^ Nil()
    def parse(s:String) = phrase(exp)(new lexical.Scanner(s))
    def apply(s:String) = parse(s) match {
      case Success(e, _) => e
      case e: NoSuccess => throw ParserError(""+e)
    }

    def simple_exp  = (exp) ^^ {case e => List(e)}
    def exps : Parser[List[Expr]] = (";;" ~ exps ^^ {case ";;"~ es => es}) | (";;" ^^^ List()) | (exp ~ exps ^^ {case e ~ es => e::es})
      //  | (simple_exp * (";;" ^^^ { (le1: List[Expr], le2: List[Expr]) => le1++le2 }))
    def parseM(s:String) = phrase(exps)(new lexical.Scanner(s))
    def applyM(s:String):List[Expr] = parseM(s) match {
      case Success(e, _) => e
      case e: NoSuccess => throw ParserError(""+e)
    }

    def parseFile(path:String) = phrase(exps)(new lexical.Scanner(new PagedSeqReader(PagedSeq.fromFile(path))))
    def applyFile(path:String):List[Expr] = parseFile(path) match {
      case Success(e, _) => e
      case e: NoSuccess => throw ParserError(""+e)
    }
    def p() = parseFile("/home/luisarmando/cic/semantica/input.l2");
  }
}