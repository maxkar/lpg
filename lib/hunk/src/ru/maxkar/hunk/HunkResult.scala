package ru.maxkar.hunk

/** Hunk execution result.
 * @param R result type.
 */
abstract sealed class HunkResult[+R]


/** Hunk execution success. */
final case class HunkSuccess[+R](value : R)
  extends HunkResult[R]


/** Hunk failed with an exception. */
final case class HunkException[+R](exception : Throwable)
    extends HunkResult[R]
