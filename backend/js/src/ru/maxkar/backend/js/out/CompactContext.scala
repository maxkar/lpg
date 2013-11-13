package ru.maxkar.backend.js.out

import java.io.Writer

/** Context for compact writing. */
private[js] class CompactContext(
    base : Writer,
    vars : IdScope,
    labels : IdScope) {

  /** Writes a text into the output. */
  def write(txt : String) : Unit = base.write(txt)


  /** Writes a character. */
  def write(c : Char) : Unit = base.write(c)


  /** Writes a variable. */
  def writeVariable(v : AnyRef) : Unit = {
    write(vars.lookup(v))
  }


  /** Writes a label. */
  def writeLabel(v : AnyRef) : Unit =
    write(labels.lookup(v))


  /** Writes a possible-bracketed expression. */
  def bracketed(cond : Boolean, lbracket : Char, rbracket : Char,
      body : ⇒ Unit) : Unit = {
    if (cond)
      write(lbracket)
    body
    if (cond)
      write(rbracket)
  }


  /** Runs a operation with a separator. */
  def sepby[T](x : Iterable[T], sep : Char, handler : T ⇒ Unit) : Unit = {
    val itr = x.iterator
    if (itr.hasNext)
      handler(itr.next)
    while (itr.hasNext) {
      write(sep)
      handler(itr.next)
    }
  }


  /** Creates a subcontext with same labels. */
  def sub(locals : Iterable[AnyRef], sublabels : Iterable[AnyRef]) : CompactContext =
    new CompactContext(base, vars.sub(locals), labels.sub(sublabels))


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
  def forWriter(w : Writer, globals : Map[AnyRef, String]) : CompactContext =
    new CompactContext(w,
      IdScope.rootVar(KEYWORDS.apply, globals),
      IdScope.labels(KEYWORDS.contains))


  val KEYWORDS = Set(
    "arguments",

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
