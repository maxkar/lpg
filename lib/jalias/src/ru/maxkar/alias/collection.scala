package ru.maxkar.alias

/** Collection aliases. */
package object collection {
  type JSet[T] = java.util.Set[T]
  type JMap[K, V] = java.util.Map[K, V]
  type JHashSet[T] = java.util.HashSet[T]
  type JHashMap[K, V] = java.util.HashMap[K, V]
}
