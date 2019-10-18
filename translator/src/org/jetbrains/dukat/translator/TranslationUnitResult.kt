package org.jetbrains.dukat.translator

import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonObject
import org.jetbrains.dukat.astCommon.NameEntity

sealed class TranslationUnitResult

data class ModuleTranslationUnit(
        val name: String,
        val fileName: String,
        val packageName: NameEntity,
        val content: String,
        val ast: JsonArray
) : TranslationUnitResult()

data class TranslationErrorInvalidFile(val fileName: String) : TranslationUnitResult()
data class TranslationErrorFileNotFound(val fileName: String) : TranslationUnitResult()
