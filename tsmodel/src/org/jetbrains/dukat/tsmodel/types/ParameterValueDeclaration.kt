package org.jetbrains.dukat.tsmodel.types

import org.jetbrains.dukat.astCommon.AstTypeEntity

interface ParameterValueDeclaration : AstTypeEntity {
    val nullable: Boolean
    var meta: ParameterValueDeclaration?
}

fun ParameterValueDeclaration.isSimpleType(str: String): Boolean {
    return this == TypeDeclaration(value = str, params = emptyList(), nullable = false, meta = null)
}