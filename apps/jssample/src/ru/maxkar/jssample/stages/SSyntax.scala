package ru.maxkar.jssample.stages

import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.lispy.parser.Input
import ru.maxkar.scoping.simple._
import ru.maxkar.lispy._
import ru.maxkar.jssample.att._

import ru.maxkar.collection.LazySeq



/**
 * S-Syntax analysis stage.
 * This stage checks s-expressions for well-formness and
 * provides list of s-syntax violations. This analyzer expects
 * that each S-Expression have at least location attribute.
 */
object SSyntax {
  import scala.language.implicitConversions

  /**
   * Input type.
   */
  type Input = SExpression[BaseItem]

  /**
   * Analyziz output type.
   */
  type Output = LazySeq[Error]

  /**
   * Generic location.
   */
  type Location = TextPosition

  /**
   * S-Syntax errors.
   */
  abstract sealed class Error

  /** Illegal (empty) expression. */
  final case class EmptyExpression(location : Location)
    extends Error

  /** Missing function arguments in function definition. */
  final case class MailformedFunctionArguments(
      val funDefLocation : Location)
    extends Error

  /** Mailformed function arguments in function definition. */
  final case class MailformedFunctionArgument(
      val argDefLocation : Location)
    extends Error

  /** Empty variable declaration statement. */
  final case class EmptyVarDeclaration(
      val declLocation : Location)
    extends Error

  /** Illegal variable identifier (not an ID). */
  final case class IllegalVariableIdentifier(
      val varLocation : Location)
    extends Error

  /** Illegal variable initializer for the variable.
   * @param varLocation location of the variable declaration stamement, not
   * an initializer's location and not a variable location.
   */
  final case class MailformedVariableInitializer(
      val declLocation : Location)
    extends Error

  /** Missing condition in when clause. */
  final case class MissingWhenCondition(
      val declLocation : Location)
    extends Error

  /** Missing condition in while clause. */
  final case class MissingWhileCondition(
      val declLocation : Location)
    extends Error

  /** Illegal return statement. */
  final case class MailformedReturn(
      val stmtLocation : Location)
    extends Error

  /** Missing if condition error. */
  final case class MissingIfCondition(
      val stmtLocation : Location)
    extends Error

  /** Missing if "true" statement. */
  final case class MissingIfSuccessBranch(
      val stmtLocation : Location)
    extends Error

  /** Missing if "false" statement. */
  final case class MissingIfFailureBranch(
      val stmtLocation : Location)
    extends Error

  /** Errorneous block in if statement. */
  final case class ErrorneousIfTail(
      val stmtLocation : Location)
    extends Error

  /** Missing loop iterator variable. */
  final case class MissingLoopVariable(
      val stmtLocation : Location)
    extends Error

  /** Missing loop collection. */
  final case class MissingLoopCollection(
      val stmtLocation : Location)
    extends Error

  /** Missing exception definition. */
  final case class MissingExceptionName(
      val stmtLocation : Location)
    extends Error

  /** Missing catch clause. */
  final case class MissingCatchClause(
      val stmtLocation : Location)
    extends Error

  /** Missing case expression. */
  final case class MissingCaseExpression(
      val stmtLocation : Location)
    extends Error

  /** Mailformed case clause (non-list or empty list). */
  final case class MailformedCaseClause(
      val clauseLocation : Location)
    extends Error


  // ANALYZERS //
  def analyzeExpression(input : Input) : Output =
    input match {
      case SLeaf(BaseInteger(_), _) ⇒ success
      case SLeaf(BaseFloating(_, _), _) ⇒ success
      case SLeaf(BaseString(_), _) ⇒ success
      case SLeaf(BaseId(_), _) ⇒  success
      case SList(items, _) ⇒
        items match {
          case Seq() ⇒ EmptyExpression(input)
          case items@Seq(idref@SLeaf(BaseId(id), _), tail@_*) ⇒
            id match {
              case "fun" ⇒ analyzeFunction(idref, tail)
              case _ ⇒ Σ(items map analyzeExpression)
            }
          case items ⇒ Σ(items map analyzeExpression)
        }
    }


  /** Analyzes a function definition. */
  def analyzeFunction(head : Input, defBody : Seq[Input]) : Output = {
    defBody match {
      case Seq(SList(args, _), funBody@_*) ⇒
        Σ(args map analyzeFunctionArgument) ++
        Σ(defBody map analyzeStatement)
      case _ ⇒ MailformedFunctionArguments(head)
    }
  }


  /** Analyzes a function argument. */
  def analyzeFunctionArgument(input : Input) : Output = {
    input match {
      case SLeaf(BaseId(_), _) ⇒ success
      case _ ⇒ MailformedFunctionArgument(input)
    }
  }


  /** Analyzes a statement. */
  def analyzeStatement(input : Input) : Output =
    input match {
      case SList(Seq(stmtdef@SLeaf(BaseId(id), _), tail@_*), _) ⇒
        id match {
          case "def" ⇒ analyzeFunction(stmtdef, tail)
          case "var" ⇒
            if (tail.isEmpty)
              EmptyVarDeclaration(stmtdef)
            else
              Σ(tail map analyzeVarDeclaration)
          case "ret" ⇒
            tail match {
              case Seq() ⇒ success
              case Seq(x) ⇒ analyzeExpression(x)
              case _ ⇒ MailformedReturn(stmtdef)
            }
          case "do" ⇒ Σ(tail map analyzeStatement)
          case "when" ⇒
            tail match {
              case Seq() ⇒ MissingWhenCondition(stmtdef)
              case Seq(hd, tl@ _*) ⇒
                analyzeExpression(hd) ++
                Σ(tl map analyzeStatement)
            }
          case "while" ⇒
            tail match {
              case Seq() ⇒ MissingWhileCondition(stmtdef)
              case Seq(hd, tl@ _*) ⇒
                analyzeExpression(hd) ++
                Σ(tl map analyzeStatement)
            }
          case "if" ⇒
            tail match {
              case Seq() ⇒ MissingIfCondition(stmtdef)
              case Seq(_) ⇒ MissingIfSuccessBranch(stmtdef)
              case Seq(_, _) ⇒ MissingIfFailureBranch(stmtdef)
              case Seq(cond, succ, fail) ⇒
                analyzeExpression(cond) ++
                analyzeStatement(succ) ++
                analyzeStatement(fail)
              case _ ⇒ ErrorneousIfTail(stmtdef)
            }
          case "for-array" ⇒
            tail match {
              case Seq() ⇒ MissingLoopVariable(stmtdef)
              case Seq(_) ⇒ MissingLoopCollection(stmtdef)
              case Seq(vdef, coll, stmts@_*) ⇒
                analyzeStandaloneVar(vdef) ++
                analyzeExpression(coll) ++
                Σ(stmts map analyzeStatement)
            }
          case "for-key" ⇒
            tail match {
              case Seq() ⇒ MissingLoopVariable(stmtdef)
              case Seq(_) ⇒ MissingLoopCollection(stmtdef)
              case Seq(vdef, coll, stmts@_*) ⇒
                analyzeStandaloneVar(vdef) ++
                analyzeExpression(coll) ++
                Σ(stmts map analyzeStatement)
            }
          case "on-exn" ⇒
            tail match {
              case Seq() ⇒ MissingExceptionName(stmtdef)
              case Seq(_) ⇒ MissingCatchClause(stmtdef)
              case Seq(ename, handler, body@_*) ⇒
                analyzeStandaloneVar(ename) ++
                analyzeStatement(handler) ++
                Σ(body map analyzeStatement)
            }
          case "case" ⇒
            tail match {
              case Seq() ⇒ MissingCaseExpression(stmtdef)
              case Seq(cond, clauses@_*) ⇒
                analyzeExpression(cond) ++
                Σ(clauses map analyzeCaseClause)
            }
          case _ ⇒ analyzeExpression(input)
        }
      case _ ⇒ analyzeExpression(input)
    }


  /** Analyzes a case clause. */
  private def analyzeCaseClause(input : Input) : Output =
    input match {
      case SList(Seq(cond, tail@_*), _) ⇒
        analyzeCaseCondition(cond) ++
        Σ(tail map analyzeStatement)
      case _ ⇒ MailformedCaseClause(input)
    }


  /** Analyzes a case condition. */
  private def analyzeCaseCondition(input : Input) : Output =
    input match {
      case SList(Seq(), _) ⇒ success
      case _ ⇒ analyzeExpression(input)
    }


  /** Analyzes standalone variable declaration. */
  private def analyzeStandaloneVar(input : Input) : Output =
    input match {
      case SLeaf(x, _) ⇒ analyzeVarName(input, x)
      case _ ⇒ IllegalVariableIdentifier(input)
    }


  /** Analyzes a variable declaration. */
  def analyzeVarDeclaration(decl : Input) : Output =
    decl match {
      case SLeaf(vname, _) ⇒ analyzeVarName(decl, vname)
      case SList(Seq(ndecl@SLeaf(vname, _), initializer), _) ⇒
        analyzeVarName(ndecl, vname) ++
        analyzeExpression(initializer)
      case _ ⇒ MailformedVariableInitializer(decl)
    }


  /** Analyzes a variable name. */
  private def analyzeVarName(decl : Input, vname : BaseItem) : Output =
    vname match {
      case BaseId(_) ⇒ success
      case _ ⇒ IllegalVariableIdentifier(decl)
    }


  // UTIL //

  /** Denotion of the success. */
  private val success : Output = LazySeq.empty

  /** Creates a new failure definition. */
  implicit private def fail(error : Error) : Output = LazySeq(error)

  /**
   + Location accessor. This method pretends that location
   * is set on every input element.
   */
  implicit private def locationOf(item : Input) : Location = {
    val locs = item.atts.allValues(Input.textPosition)
    if (locs.size != 1)
      throw new IllegalArgumentException("Mailformed location attribute in " + item);
    locs.head
  }

  /** Output summing function, U03a3. */
  private def Σ(items : Seq[Output]) : Output =
    LazySeq.sum(items : _*)

}
