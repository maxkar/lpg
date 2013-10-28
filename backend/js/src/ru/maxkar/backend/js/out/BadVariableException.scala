package ru.maxkar.backend.js.out

final class BadVariableException(v : AnyRef) extends
  Exception("Reference to invalid local variable " + v)
