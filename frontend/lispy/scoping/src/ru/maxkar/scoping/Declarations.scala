package ru.maxkar.scoping

import ru.maxkar.lispy._
import scala.collection.mutable.ArrayBuffer

/** Declarations definition. */
final object Declarations {


  /** Declarations processing callback. */
  trait DeclarationCallback {
    /** Handles a duplicate declaration. */
    def duplicateDeclaration(name : String, first : Attributes, next : Attributes)
  }


  /** Local declarations attribute. */
  val declaredLocals : Attribute[Set[String]] =
    new Attribute[Set[String]]("Declared members' names")


  /** Evaluates declared members of the scope. */
  private def declsOf[T](expr : SExpression[T]) : Seq[(String, Attributes)] = {
    expr match {
      case SList(Seq(SLeaf(BaseId(k), _),  args@_*), _) ⇒
        k match {
          case "const" ⇒
            val res = new ArrayBuffer[(String, Attributes)]
            args.foreach(x ⇒ x match {
                case SList(Seq(SLeaf(BaseId(k), largs), _*), _) ⇒
                  res += ((k, largs))
                case _ ⇒  ()
              })
            res.toSeq
          case "var" ⇒
            val res = new ArrayBuffer[(String, Attributes)]
            args.foreach(x ⇒ x match {
                case SList(Seq(SLeaf(BaseId(k), largs), _*), _) ⇒
                  res += ((k, largs))
                case SLeaf(BaseId(k), largs) ⇒
                  res += ((k, largs))
                case _ ⇒ ()
              })
            res.toSeq
          case "def" ⇒
            args match {
              case Seq(SLeaf(BaseId(k), largs), _*) ⇒
                Seq((k, largs))
              case _ ⇒ Seq.empty
            }
          case _ ⇒ Seq.empty
        }
      case _ ⇒  Seq.empty
    }
  }


  /** Lifts definition in default manner. */
  private def liftDefault[T](expr : SList[T], cb : DeclarationCallback)
      : SExpression[T] = {

    val buf = new scala.collection.mutable.HashMap[String, Attributes]
    def proc(name : String, atts : Attributes) {
      buf.get(name) match {
        case Some(x) ⇒ cb.duplicateDeclaration(name, x, atts)
        case None ⇒ buf.put(name, atts)
      }
    }

    expr.items.foreach(i ⇒
      declsOf(i).foreach(x ⇒ proc(x._1, x._2)))

    SList(
      expr.items.map(x ⇒ liftDeclarations(x, cb)),
      expr.atts + (declaredLocals, buf.keys.toSet))
  }


  /** Finds all internal declarations and moves
   * them to the top of the block.
   */
  def liftDeclarations[T](expr : SExpression[T], cb : DeclarationCallback)
      : SExpression[T] = {
    expr match {
      case SLeaf(_, _) ⇒ expr
      case a@SList(items, oatts) ⇒ liftDefault(a, cb)
    }
  }
}
