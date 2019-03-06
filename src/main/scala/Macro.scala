
import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions
import scala.quoted.Exprs.LiftedExpr
import scala.quoted.Toolbox.Default._

object Macro {

  class StringContextOps(strCtx: => StringContext) {
    inline def f2(args: Any*): String = ~FIntepolator('(strCtx), '(args))
    inline def s2(args: Any*): String = ~SIntepolator('(strCtx), '(args))
    inline def raw2(args: Any*): String = ~RawIntepolator('(strCtx), '(args))
    inline def foo(args: Any*): String = ~FooIntepolator('(strCtx), '(args))
  }
  implicit inline def SCOps(strCtx: => StringContext): StringContextOps = new StringContextOps(strCtx)
}

object SIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]])(implicit reflect: Reflection): Expr[String] =
    '((~strCtx.toExpr).s(~args.toExprOfList: _*))
}

object RawIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]])(implicit reflect: Reflection): Expr[String] =
    '((~strCtx.toExpr).raw(~args.toExprOfList: _*))
}

object FooIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]])(implicit reflect: Reflection): Expr[String] =
    '((~strCtx.toExpr).s(~args.map(_ => '("foo")).toExprOfList: _*))
}

object FIntepolator extends MacroStringInterpolator[String] {
  /**
    * Interpolates the given arguments to the formatted string
    * @param strCtx that contains all the chunks of the formatted string
    * @param args the list of arguments to interpolate to the string in the correct format
    * @return the expression containing the formatted and interpolated string
    * @throws TastyTypecheckError if the given format is not correct  
    */
  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]])(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    import scala.tasty.TastyTypecheckError

    /**
      * Checks if a given type is a subtype of any of the possibilities
      * @param tpe the given type 
      * @param possibilities all the types within which we want to find a super type of tpe
      * @return true if the given type is a subtype of at least one of the possibilities, false otherwise
      */
    def checkSubtype(tpe : Type, possibilities : Type*) : Option[Type] = {
      possibilities.find(tpe <:< _)
    }

    /**
      * Checks if a given character is a kind of flag for the formatting of a string
      * @param c the given character
      * @return true if the given character is a flag, false otherwise
      */
    def isFlag(c : Char) : Boolean = c match {
      case '-' | '#' | '+' | ' ' | '0' | ',' | '(' => true
      case _ => false
    }

    /**
     * Go through the whole given string until it find the formatting string 
     * and returns the corresponding index
     * @param s the given string containing the formatting string as substring
     * @param argPos the position of the argument to format, only useful to throw errors
     * @throws TastyTypecheckError if the formatting string has not the correct format
     * @return the index of the formatting string
     */
    def getFormatTypeIndex(s : String, argPos : reflect.Position) = {
      var i = 0
      val l = s.length
      while(i < l && isFlag(s.charAt(i))) {i += 1}
      while(i < l && Character.isDigit(s.charAt(i))) {i += 1}
      if(i < l && s.charAt(i) == '.') {
        i += 1
        while(i < l && Character.isDigit(s.charAt(i))) {i += 1}
      }
      if(i >= l) throw new TastyTypecheckError("Wrong parameter : " + argPos) 
      i
    }
  
    // add the default "%s" format if no format is given by the user"
    val parts2 = strCtx.parts.toList match {
      case Nil => Nil
      case p :: parts1 => p :: parts1.map(part => if(!part.startsWith("%")) "%s" + part else part)
    }

    if(parts2.size - 1 != args.size && !(parts2.isEmpty && args.isEmpty)) throw new TastyTypecheckError("Missing parameter")

    // typechecking 
    if(!parts2.isEmpty) {
      parts2.tail.zip(args.map(_.unseal)).foreach((part, arg) => { 
        val i = getFormatTypeIndex(part, arg.pos)
        part.charAt(i) match { 
            case 'b' | 'B' =>  
            case 'h' | 'H' => 
            case 's' | 'S' => 
            case 'c' | 'C' if checkSubtype(arg.tpe, definitions.CharType, definitions.ByteType, definitions.ShortType, definitions.IntType).nonEmpty => 
            case 'd' | 'o' | 'x' | 'X' if checkSubtype(arg.tpe, definitions.IntType, definitions.LongType, definitions.ShortType, definitions.ByteType, typeOf[java.math.BigInteger]).nonEmpty =>
            case 'e' | 'E' |'f' | 'g' | 'G' | 'a' | 'A' if checkSubtype(arg.tpe, definitions.DoubleType, definitions.FloatType, typeOf[java.math.BigDecimal]).nonEmpty => 
            case 't' | 'T' if checkSubtype(arg.tpe, definitions.LongType, typeOf[java.util.Calendar], typeOf[java.util.Date]).nonEmpty => 
            case '%' => 
            case 'n' => 
            case _ => 
              throw new TastyTypecheckError("Wrong parameter : " + arg.pos)
        }
      })
    }
      
    // macro expansion
    '((~parts2.mkString.toExpr).format(~args.toExprOfList: _*)) 
  }

  // /**
  //   * Interpolates the given arguments to the formatted string
  //   * @param strCtxExpr the expression that holds the StringContext which contains all the chunks of the formatted string
  //   * @param args the expression that holds the sequence of arguments to interpolate to the string in the correct format
  //   * @return the expression containing the formatted and interpolated string
  //   * @throws TastyTypecheckError if the given format is not correct
  //   */
  // override protected def interpolate(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[String] = {
  //   interpolate(getStaticStringContext(strCtxExpr), getArgsList(argsExpr)) 
  // }

  // override protected def getStaticStringContext(strCtxExpr: Expr[StringContext])(implicit reflect: Reflection): StringContext = {
  //   import reflect._
  //   getStringContext(getListOfExpr(strCtxExpr))
  // }

  /**
   * Transforms a given expression containing a StringContext into a list of expressions containing strings
   * @param strCtxExpr the given expression to convert
   * @throws NotNotStaticlyKnownError if the StringContext contained inside the given expression does not contain only
   * String literals
   * @return a list of expr of string corresponding to the parts of the given StringContext
   */
  protected def getListOfExpr(strCtxExpr : Expr[StringContext])(implicit reflect: Reflection): List[Expr[String]] = {
    import reflect._
    strCtxExpr.unseal.underlyingArgument match {
      case Term.Select(Term.Typed(Term.Apply(_, List(Term.Apply(_, List(Term.Typed(Term.Repeated(strCtxArgTrees, _), TypeTree.Inferred()))))), _), _) =>
        val strCtxArgs = strCtxArgTrees.map {
          case Term.Literal(Constant.String(str)) => str
          case tree => throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal[Any])
        }
        StringContext(strCtxArgs: _*).parts.toList.map(_.toExpr)
      case tree =>
        throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal[Any])
    }
  }

  // protected def getStringContext(listExprStr : List[Expr[String]]) : StringContext = {
  //   import reflect._
  //   //TODO 
  //   // may be done using getArgsList 
  //   // needs also to know where the error is, if one happens and to return the position in the string in which this happens
  //   new StringContext
  // }
}

// TODO put this class in the stdlib or separate project?
abstract class MacroStringInterpolator[T] {

  final def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[T] = {
    try interpolate(strCtxExpr, argsExpr)
    catch {
      case ex: NotStaticlyKnownError =>
        // TODO use ex.expr to recover the position
        throw new QuoteError(ex.getMessage)
      case ex: StringContextError =>
        // TODO use ex.idx to recover the position
        throw new QuoteError(ex.getMessage)
      case ex: ArgumentError =>
        // TODO use ex.idx to recover the position
        throw new QuoteError(ex.getMessage)
    }
  }

  protected def interpolate(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[T] =
    interpolate(getStaticStringContext(strCtxExpr), getArgsList(argsExpr)) 

  protected def interpolate(strCtx: StringContext, argExprs: List[Expr[Any]])(implicit reflect: Reflection): Expr[T]

  protected def getStaticStringContext(strCtxExpr: Expr[StringContext])(implicit reflect: Reflection): StringContext = {
    import reflect._
    strCtxExpr.unseal.underlyingArgument match {
      case Term.Select(Term.Typed(Term.Apply(_, List(Term.Apply(_, List(Term.Typed(Term.Repeated(strCtxArgTrees, _), TypeTree.Inferred()))))), _), _) =>
        val strCtxArgs = strCtxArgTrees.map {
          case Term.Literal(Constant.String(str)) => str
          case tree => throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal[Any])
        }
        StringContext(strCtxArgs: _*)
      case tree =>
        throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal[Any])
    }
  }

  protected def getArgsList(argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): List[Expr[Any]] = {
    import reflect._
    argsExpr.unseal.underlyingArgument match {
      case Term.Typed(Term.Repeated(args, _), _) => args.map(_.seal[Any])
      case tree => throw new NotStaticlyKnownError("Expected statically known argument list", tree.seal[Any])
    }
  }

  protected implicit def StringContextIsLiftable: Liftable[StringContext] = new Liftable[StringContext] {
    def toExpr(strCtx: StringContext): Expr[StringContext] = {
      // TODO define in stdlib?
      implicit def ListIsLiftable: Liftable[List[String]] = new Liftable[List[String]] {
        override def toExpr(list: List[String]): Expr[List[String]] = list match {
          case x :: xs => '(~x.toExpr :: ~toExpr(xs))
          case Nil => '(Nil)
        }
      }
      '(StringContext(~strCtx.parts.toList.toExpr: _*))
    }
  }

  protected class NotStaticlyKnownError(msg: String, expr: Expr[Any]) extends Exception(msg)
  protected class StringContextError(msg: String, idx: Int, start: Int = -1, end: Int = -1) extends Exception(msg)
  protected class ArgumentError(msg: String, idx: Int) extends Exception(msg)

}
