package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Javascript file. */
final class JSFile private[model](
    private[model] val globals : Map[AnyRef, String],
    private[model] val writer : CompactContext ⇒  Unit)
