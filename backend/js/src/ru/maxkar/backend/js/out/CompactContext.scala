package ru.maxkar.backend.js.out

import java.io.Writer

/** Context for compact writing. */
private[js] class CompactContext(
    base : Writer,
    vars : IdScope,
    labels : IdScope,
    globals : scala.collection.Set[String]) {

  /** Writes a text into the output. */
  def write(txt : String) : Unit = base.write(txt)

  /** Writes a single character. */
  def writeChar(c : Char) : Unit = base.write(c)

  /** Ensures a global variable in the context. */
  def ensureGlobal(g : String) : Unit = {
    if (!globals(g))
      throw new IllegalArgumentException("Unknown global ref to " + g)
  }

  /** Resolves a variable in the context. */
  def resolveVariable(v : AnyRef) : String =
    vars.lookup(v)

  /** Resolves a label. */
  def resolveLabel(v : AnyRef) : String =
    labels.lookup(v)

  /** Creates a subcontext. */
  def sub(locals : Iterable[AnyRef], sublabels : Iterable[AnyRef]) : CompactContext =
    new CompactContext(base, vars.sub(locals),
      labels.sub(sublabels), globals)


  /** Checks, if character is valid identifier char. */
  def isValidIdentifierChar(c : Char) : Boolean = {
    Character.getType(c) match {
      case
        Character.UPPERCASE_LETTER |
        Character.LOWERCASE_LETTER |
        Character.TITLECASE_LETTER |
        Character.MODIFIER_LETTER |
        Character.OTHER_LETTER |
        Character.LETTER_NUMBER ⇒ true
      case _ ⇒ false
    }
  }
}


/** Compact context utilities. */
private[js] object CompactContext {
  /** Creates a new context for the writer. */
  def forWriter(w : Writer, globals : scala.collection.Set[String]) : CompactContext =
    new CompactContext(w,
      IdScope.default(x ⇒ KEYWORDS(x) || globals(x)),
      IdScope.labels(KEYWORDS.contains),
      globals)


  val KEYWORDS = Set(
    "break",
    "case",
    "catch",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "finally",
    "for",
    "function",
    "if",
    "in",
    "instanceof",
    "new",
    "return",
    "switch",
    "this",
    "throw",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "class",
    "enum",
    "export",
    "extends",
    "import",
    "super",
    "implements",
    "interface",
    "let",
    "package",
    "private",
    "protected",
    "public",
    "static",
    "yield"
  )



}
