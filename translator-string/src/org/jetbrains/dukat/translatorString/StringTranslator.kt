package org.jetbrains.dukat.translatorString

import kotlinx.serialization.json.*
import org.jetbrains.dukat.astCommon.IdentifierEntity
import org.jetbrains.dukat.astCommon.NameEntity
import org.jetbrains.dukat.astModel.*
import org.jetbrains.dukat.astModel.modifiers.VisibilityModifierModel
import org.jetbrains.dukat.astModel.statements.*
import org.jetbrains.dukat.panic.raiseConcern
import org.jetbrains.dukat.translator.ModelVisitor
import org.jetbrains.dukat.translator.ROOT_PACKAGENAME

const val FORMAT_TAB = "    "

private fun String?.translateMeta(): String {
    return if (this != null) {
        " /* ${this} */"
    } else {
        ""
    }
}

private fun CommentModel.serialize() = let {
    json {
        when (it) {
            is SimpleCommentModel -> {
                "kind" to SimpleCommentModel::class.java.simpleName
            }
            is DocumentationCommentModel -> {
                "kind" to DocumentationCommentModel::class.java.simpleName
            }
        }
        "text" to it.text
    }
}

private fun CommentModel.translate(output: (String) -> Unit) {
    when (this) {
        is SimpleCommentModel -> output(text.translateMeta().trim())
        is DocumentationCommentModel -> {
            output("/**")
            output(" * $text")
            output(" */")
        }
    }
}


private fun TypeModel.translateMeta(): String {
    return when (this) {
        is TypeValueModel -> metaDescription.translateMeta()
        is FunctionTypeModel -> metaDescription.translateMeta()
        else -> ""
    }
}

private fun StatementModel.translateMeta(): String {
    return metaDescription.translateMeta()
}

private fun translateTypeParams(params: List<TypeModel>): String {
    return "<" + params.joinToString(", ") { param -> "${param.translate()}${param.translateMeta()}" } + ">"
}

fun TypeModel.translate(): String {
    return when (this) {
        is TypeValueModel -> {
            val res = mutableListOf(value.translate())
            if (isGeneric()) {
                res.add(translateTypeParameters(params))
            }
            if (nullable) {
                res.add("?")
            }
            res.joinToString("")
        }
        is FunctionTypeModel -> {
            val res = mutableListOf("(")
            val paramsList = mutableListOf<String>()
            for (param in parameters) {
                val paramNameSerialized = if (param.name != "") {
                    param.name + ": "
                } else {
                    ""
                }
                val paramSerialized = paramNameSerialized + param.type.translate() + param.type.translateMeta()
                paramsList.add(paramSerialized)
            }
            res.add(paramsList.joinToString(", ") + ")")
            res.add(" -> ${type.translate()}")
            var translated = res.joinToString("")
            if (nullable) {
                translated = "(${translated})?"
            }
            translated
        }
        else -> return "failed to translateType ${this}"
    }
}

private fun serializeParameterModel(parameterModel: ParameterModel): JsonObject = json {
    "kind" to ParameterModel::class.java.simpleName
    "name" to parameterModel.name
    "type" to parameterModel.type.serialize()
    "vararg" to parameterModel.vararg
}

private fun ParameterModel.translate(needsMeta: Boolean = true): String {
    var res = name + ": " + type.translate()
    if (vararg) {
        res = "vararg $res"
    }

    if (needsMeta) {
        res += type.translateMeta()
    }

    if (initializer != null) {
        res += " = ${initializer!!.translate()}"
        if (needsMeta) {
            res += initializer!!.translateMeta()
        }
    }

    return res
}


fun TypeParameterModel.serialize(): JsonObject = json {
    "kind" to TypeParameterModel::class.java.simpleName
    "type" to type.serialize()
    "constraints" to jsonArray { constraints.map { it.serialize() } }
    "variance" to "$variance"
}

fun serializeTypeParameterModels(typeParameters: List<TypeParameterModel>): JsonArray = jsonArray {
    typeParameters.map { it.serialize() }
}

private fun translateTypeParameters(typeParameters: List<TypeParameterModel>): String {
    return if (typeParameters.isEmpty()) {
        ""
    } else {
        "<" + typeParameters.map { typeParameter ->
            val varianceDescription = when (typeParameter.variance) {
                Variance.INVARIANT -> ""
                Variance.COVARIANT -> "out "
                Variance.CONTRAVARIANT -> "in "
            }
            val constraintDescription = if (typeParameter.constraints.isEmpty()) {
                ""
            } else {
                " : ${typeParameter.constraints[0].translate()}"
            }
            varianceDescription + typeParameter.type.translate() +
                    typeParameter.type.translateMeta() + constraintDescription
        }.joinToString(", ") + ">"
    }
}

private fun translateTypeArguments(typeParameters: List<TypeModel>): String {
    return if (typeParameters.isEmpty()) {
        ""
    } else {
        "<" + typeParameters.map {
            it.translate() + it.translateMeta()
        }.joinToString(", ") + ">"
    }
}


private fun translateParameters(parameters: List<ParameterModel>, needsMeta: Boolean = true): String {
    return parameters
            .map { parameter -> parameter.translate(needsMeta) }
            .joinToString(", ")
}

private fun serializeAnnotations(annotations: List<AnnotationModel>): JsonArray {
    return jsonArray {
        annotations.map { a ->
            json {
                "kind" to AnnotationModel::class.java.simpleName
                "name" to a.name
                "params" to jsonArray { a.params.map { p -> p.serialize() } }
            }
        }
    }
}

private fun translateAnnotations(annotations: List<AnnotationModel>): String {
    val annotationsResolved = annotations.map { annotationNode ->
        var res = "@" + annotationNode.name
        if (annotationNode.params.isNotEmpty()) {
            res = res + "(" + annotationNode.params.joinToString(", ") { "\"${it.translate()}\"" } + ")"
        }
        res
    }

    val annotationTranslated = if (annotationsResolved.isEmpty()) "" else annotationsResolved.joinToString(LINE_SEPARATOR) + LINE_SEPARATOR

    return annotationTranslated
}

private fun StatementCallModel.translate(): String {
    return "${value.translate()}${if (typeParameters.isEmpty()) "" else "<${typeParameters.joinToString(
            ", ") { it.value }}>"}${if (params == null) "" else "(${params?.joinToString(", ") { it.value }})"}"
}


private fun StatementModel.serialize() = let { s ->
    json {
        "kind" to StatementModel::class.java.simpleName
        s.metaDescription?.let { m ->
            "metaDescription" to m
        }
    }
}

private fun StatementModel.translate(): String {
    return when (this) {
        is AssignmentStatementModel -> "${left.translate()} = ${right.translate()}"
        is ChainCallModel -> "${left.translate()}.${right.translate()}"
        is ReturnStatementModel -> "return ${statement.translate()}"
        is IndexStatementModel -> "${array.translate()}[${index.translate()}]"
        is StatementCallModel -> translate()
        else -> raiseConcern("unkown StatementNode ${this}") { "" }
    }
}

private fun ClassLikeReferenceModel.serialize() = let { c ->
    json {
        "kind" to ClassLikeReferenceModel::class.java.simpleName
        "name" to c.name.serialize()
        "typeParameters" to jsonArray { c.typeParameters.forEach { +it.serialize() } }
    }
}

private fun ClassLikeReferenceModel.translate(): String {
    return name.translate() + if (typeParameters.isNotEmpty()) {
        "<${typeParameters.map { it.translate() }.joinToString(", ")}>"
    } else {
        ""
    }
}

private fun serializeFunctionModel(model: FunctionModel): JsonObject {
    return json {
        "kind" to Function::class.java.simpleName
        "name" to model.name.serialize()
        "parameters" to jsonArray { model.parameters.map { +serializeParameterModel(it) } }
        "annotations" to serializeAnnotations(model.annotations)
        "export" to JsonLiteral(model.export)
        "inline" to JsonLiteral(model.inline)
        "operator" to JsonLiteral(model.operator)
        "type" to model.type.serialize()
        "typeParameters" to serializeTypeParameterModels(model.typeParameters)
        model.body
    }
}

private fun FunctionModel.translate(padding: Int, output: (String) -> Unit) {
    val returnsUnit = (type is TypeValueModel) &&
            (type as TypeValueModel).value == IdentifierEntity("Unit")

    val returnClause = if (returnsUnit) "" else ": ${type.translate()}"

    var typeParams = translateTypeParameters(typeParameters)
    if (typeParams.isNotEmpty()) {
        typeParams = " " + typeParams
    }

    val modifier = if (inline) "inline" else KOTLIN_EXTERNAL_KEYWORD
    val operator = if (operator) " operator" else ""

    val bodyFirstLine = if (body.isEmpty()) {
        ""
    } else if (body.size == 1) {
        if (body[0] is ReturnStatementModel) {
            " = ${(body[0] as ReturnStatementModel).statement.translate()}"
        } else {
            " { ${body[0].translate()} }"
        }
    } else {
        " {"
    }

    val funName = if (extend == null) {
        name.translate()
    } else {
        extend?.translate() + "." + name.translate()
    }

    output(FORMAT_TAB.repeat(padding) +
            "${translateAnnotations(annotations)}${visibilityModifier.asClause()}${modifier}${operator} fun${typeParams} ${funName}(${translateParameters(
                    parameters)})${returnClause}${type.translateMeta()}${bodyFirstLine}")

    if (body.size > 1) {
        body.forEach { statement ->
            output(FORMAT_TAB.repeat(padding + 1) + statement.translate())
        }
        output(FORMAT_TAB.repeat(padding) + "}")
    }
}

private fun MethodModel.translate(): List<String> {
    val returnsUnit = (type is TypeValueModel) &&
            ((type as TypeValueModel).value == IdentifierEntity("@@None")
                    || (type as TypeValueModel).value == IdentifierEntity("Unit"))
    val returnClause = if (returnsUnit) "" else ": ${type.translate()}"

    var typeParams = translateTypeParameters(typeParameters)
    if (typeParams.isNotEmpty()) {
        typeParams = " " + typeParams
    }

    val operatorModifier = if (operator) "operator " else ""
    val annotations = annotations.map { "@${it.name}" }

    val open = !static && open
    val overrideClause = if (override) "override " else if (open) "open " else ""

    val metaClause = type.translateMeta()
    return annotations + listOf(
            "${overrideClause}${operatorModifier}fun${typeParams} ${name.translate()}(${translateParameters(parameters)})${returnClause}$metaClause")
}

private fun ConstructorModel.serialize() = let {
    json {
        "kind" to ConstructorModel::class.java.simpleName
        "generated" to it.generated
        "parameters" to jsonArray { it.parameters.forEach { p -> +serializeParameterModel(p) } }
        "typeParameters" to serializeTypeParameterModels(it.typeParameters)
    }
}

private fun ConstructorModel.translate(): List<String> {
    val typeParams = translateTypeParameters(typeParameters)
    return listOf("constructor${typeParams}(${translateParameters(parameters, false)})")
}

private fun TypeAliasModel.translate(): String {
    return "typealias ${name.translate()}${translateTypeParameters(typeParameters)} = ${typeReference.translate()}"
}

private fun TypeAliasModel.serialize(): JsonObject {
    return json {
        "kind" to TypeAliasModel::class.java.simpleName
        "name" to name.serialize()
        "typeParameters" to typeParameters.map { it.serialize() }
    }
}


private fun VariableModel.serialize() = let { v ->
    json {
        "kind" to VariableModel::class.java.simpleName
        "annotations" to serializeAnnotations(v.annotations)
        v.get?.let { g ->
            "get" to g.serialize()
        }
        v.set?.let { s ->
            "set" to s.serialize()
        }
        "immutable" to v.immutable
        v.initializer?.let { i -> "initializer" to i.serialize() }
        "inline" to v.inline
        "type" to v.type.serialize()
        "typeParameters" to serializeTypeParameterModels(v.typeParameters)
        v.extend?.let { e ->
            "extend" to e.serialize()
        }

    }
}

private fun VariableModel.translate(): String {
    val variableKeyword = if (immutable) "val" else "var"
    val modifier = if (inline) "inline" else KOTLIN_EXTERNAL_KEYWORD

    val body = if (initializer != null) {
        " = ${initializer?.translate()}"
    } else if ((get != null) && (set != null)) {
        val getter = "get() = ${get?.translate()};"
        val setter = "set(value) { ${set?.translate()} }"
        " ${getter} ${setter}"
    } else if (get != null) {
        val getter = "get() = ${get?.translate()}"
        " ${getter}"
    } else ""

    val typeParams = if (typeParameters.isEmpty()) {
        ""
    } else {
        " ${translateTypeParameters(typeParameters)}"
    }

    val varName = if (extend == null) {
        name.translate()
    } else {
        extend?.translate() + "." + name.translate()
    }
    return "${translateAnnotations(
            annotations)}${visibilityModifier.asClause()}${modifier} ${variableKeyword}${typeParams} ${varName}: ${type.translate()}${type.translateMeta()}${body}"
}

private fun EnumModel.serialize() = let { s ->
    json {
        "kind" to EnumModel::class.java.simpleName
        "name" to s.name.serialize()
        "values" to jsonArray {
            s.values.forEach { t ->
                +json {
                    "kind" to EnumTokenModel::class.java.simpleName
                    "meta" to t.meta
                    "value" to t.value
                }
            }
        }
        "visibilityModifier" to "${s.visibilityModifier}"

    }
}

private fun EnumModel.translate(): String {
    val res = mutableListOf("${KOTLIN_EXTERNAL_KEYWORD} enum class ${name.translate()} {")
    res.add(values.map { value ->
        val metaClause = if (value.meta.isEmpty()) "" else " /* = ${value.meta} */"
        "    ${value.value}${metaClause}"
    }.joinToString(",${LINE_SEPARATOR}"))
    res.add("}")
    return res.joinToString(LINE_SEPARATOR)
}

private fun PropertyModel.translate(): String {
    val open = !static && open
    val modifier = if (override) "override " else if (open) "open " else ""
    val varModifier = if (getter && !setter) "val" else "var"

    return "$modifier$varModifier ${name.translate()}: ${type.translate()}${type.translateMeta()}"
}

private fun MemberModel.translate(): List<String> {
    return when (this) {
        is MethodModel -> translate()
        is PropertyModel -> listOf(translate())
        is ConstructorModel -> translate()
        is ClassModel -> listOf(translate(1))
        is InterfaceModel -> listOf(translate(1))
        else -> raiseConcern("can not translate MemberModel ${this}") { listOf("") }
    }
}

private fun PropertyModel.translateSignature(): List<String> {
    val varModifier = if (getter && !setter) "val" else "var"
    val overrideClause = if (override) "override " else ""


    var typeParams = translateTypeParameters(typeParameters)
    if (typeParams.isNotEmpty()) {
        typeParams = " " + typeParams
    }
    val metaClause = type.translateMeta()
    val res = mutableListOf(
            "${overrideClause}${varModifier}${typeParams} ${name.translate()}: ${type.translate()}${metaClause}"
    )
    if (type.nullable || (type is TypeValueModel && (type as TypeValueModel).value == IdentifierEntity("dynamic"))) {
        if (getter) {
            res.add(FORMAT_TAB + "get() = definedExternally")
        }
        if (setter) {
            res.add(FORMAT_TAB + "set(value) = definedExternally")
        }
    }
    return res
}

private fun MethodModel.translateSignature(): List<String> {
    var typeParams = translateTypeParameters(typeParameters)
    if (typeParams.isNotEmpty()) {
        typeParams = " " + typeParams
    }

    val operatorModifier = if (operator) "operator " else ""
    val annotations = annotations.map { "@${it.name}" }

    val returnsUnit = (type is TypeValueModel) && ((type as TypeValueModel).value == IdentifierEntity("Unit"))
    val returnClause = if (returnsUnit) "" else ": ${type.translate()}"
    val overrideClause = if (override) "override " else ""

    val metaClause = type.translateMeta()
    val methodNodeTranslation = "${overrideClause}${operatorModifier}fun${typeParams} ${name.translate()}(${translateParameters(
            parameters)})${returnClause}$metaClause"
    return annotations + listOf(methodNodeTranslation)
}

private fun MemberModel.translateSignature(): List<String> {
    return when (this) {
        is MethodModel -> translateSignature()
        is PropertyModel -> translateSignature()
        is ClassModel -> listOf(translate(1))
        is InterfaceModel -> listOf(translate(1))
        else -> raiseConcern("can not translate signature ${this}") { emptyList<String>() }
    }
}

private fun translateHeritagModels(parentEntities: List<HeritageModel>): String {
    val parents = if (parentEntities.isNotEmpty()) {
        " : " + parentEntities.map { parentEntity ->
            "${parentEntity.value.translate()}${translateTypeArguments(parentEntity.typeParams)}"
        }.joinToString(", ")
    } else ""

    return parents
}

private fun FunctionTypeModel.serialize(): JsonObject = let { f ->
    json {
        "kind" to FunctionTypeModel::class.java.simpleName
        f.metaDescription?.let {
            "metaDescription" to f.metaDescription
        }
        "parameters" to jsonArray { f.parameters.forEach { +serializeParameterModel(it) } }
        "type" to f.type.serialize()
    }
}

private fun TypeValueModel.serialize() = let { t ->
    json {
        "kind" to TypeValueModel::class.java.simpleName
        t.metaDescription?.let {
            "metaDescription" to t.metaDescription
        }
        "params" to serializeTypeParameterModels(t.params)
        "value" to t.value.serialize()
    }
}


private fun TypeModel.serialize() = let { t ->
    when (t) {
        is FunctionTypeModel -> t.serialize()
        is TypeValueModel -> t.serialize()
        else -> throw RuntimeException("Unknown TypeModel ${this}")
    }
}

private fun TypeModel.translateAsHeritageClause(): String {
    return when (this) {
        is FunctionTypeModel -> translate()
        is TypeValueModel -> {
            val typeParams = if (params.isEmpty()) {
                ""
            } else {
                "<${params.joinToString("::") { it.type.translateAsHeritageClause() }}>"
            }

            when (value) {
                is IdentifierEntity -> "${(value as IdentifierEntity).value}${typeParams}"
                else -> raiseConcern("unknown NameEntity ${value}") { "" }
            }
        }
        else -> ""
    }
}


private fun DelegationModel.serialize(): JsonObject = let { d ->
    when (d) {
        is ClassModel -> d.serialize()
        is ExternalDelegationModel -> d.serialize()
        else -> throw RuntimeException("Unknown DelegationModel ${d}")
    }
}

private fun DelegationModel.translate(): String {
    return when (this) {
        is ClassModel -> name.translate()
        is ExternalDelegationModel -> "definedExternally"
        else -> ""
    }
}

private fun HeritageModel.translateAsHeritageClause(): String {
    val delegationClause = delegateTo?.let { " by ${it.translate()}" } ?: ""
    return "${value.translateAsHeritageClause()}${delegationClause}"
}

private fun HeritageModel.serialize() = let { h ->
    json {
        "kind" to HeritageModel::class.java.simpleName
        "value" to h.value.serialize()
        "typeParams" to jsonArray { h.typeParams.forEach { +it.serialize() } }
        h.delegateTo?.let {
            "delegateTo" to it.serialize()
        }
    }

}


private fun ClassModel.translate(depth: Int): String {
    val res = mutableListOf<String>()
    translate(depth) { res.add(it) }
    return res.joinToString(LINE_SEPARATOR)
}


private fun VisibilityModifierModel.translate(): String? {
    return when (this) {
        VisibilityModifierModel.PUBLIC -> "public"
        VisibilityModifierModel.INTERNAL -> "internal"
        VisibilityModifierModel.PRIVATE -> "private"
        VisibilityModifierModel.PROTECTED -> "protected"
        VisibilityModifierModel.DEFAULT -> null
    }
}

private fun VisibilityModifierModel.asClause(): String {
    return translate()?.let { "$it " } ?: ""
}

private fun ClassModel.serialize() = let { c ->
    println("Serializing ClassModel")
    json {
        "kind" to ClassModel::class.java.simpleName
        "abstract" to c.abstract
        "annotations" to serializeAnnotations(c.annotations)
        c.comment?.let { c ->
            "comment" to c.serialize()
        }
        "external" to c.external
        c.primaryConstructor?.let { c ->
            "primaryConstructor" to c.serialize()
        }
        "typeParameters" to serializeTypeParameterModels(c.typeParameters)
        "members" to jsonArray { c.members.forEach { +it.serialize() } }
        c.companionObject?.let { o ->
            "companionObject" to o.serialize()
        }
        "name" to c.name.serialize()
        "parentEntities" to jsonArray { c.parentEntities.forEach { +it.serialize() } }
        "visibilityModifier" to "${c.visibilityModifier}"
    }
}


private fun ClassModel.translate(depth: Int, output: (String) -> Unit) {
    val primaryConstructor = primaryConstructor
    val hasSecondaryConstructors = members.any { it is ConstructorModel }

    comment?.translate(output)

    val parents = translateHeritagModels(parentEntities)
    val externalClause = if (external) "${KOTLIN_EXTERNAL_KEYWORD} " else ""
    val params = if (primaryConstructor == null) "" else
        if (primaryConstructor.parameters.isEmpty() && !hasSecondaryConstructors) "" else "(${translateParameters(primaryConstructor.parameters)})"

    val openClause = if (abstract) "abstract" else "open"

    val classDeclaration = "${translateAnnotations(
            annotations)}${visibilityModifier.asClause()}${externalClause}${openClause} class ${name.translate()}${translateTypeParameters(
            typeParameters)}${params}${parents}"

    val members = members
    val staticMembers = companionObject?.members.orEmpty()

    val hasMembers = members.isNotEmpty()
    val hasStaticMembers = staticMembers.isNotEmpty()
    val isBlock = hasMembers || hasStaticMembers

    output(classDeclaration + if (isBlock) " {" else "")

    if (hasMembers) {
        members.flatMap { it.translate() }.map({ FORMAT_TAB.repeat(depth + 1) + it }).forEach {
            output(it)
        }
    }

    if (companionObject != null) {
        if (hasMembers) {
            output("")
        }
        output(FORMAT_TAB.repeat(depth + 1) + "companion object${if (!hasStaticMembers) "" else " {"}")
    }
    if (hasStaticMembers) {
        staticMembers.flatMap { it.translate() }.map({ FORMAT_TAB.repeat(depth + 2) + it }).forEach {
            output(it)
        }
        output(FORMAT_TAB.repeat(depth + 1) + "}")
    }

    if (isBlock) {
        output(FORMAT_TAB.repeat(depth) + "}")
    }
}

private fun InterfaceModel.translate(padding: Int): String {
    val res = mutableListOf<String>()
    translate(padding) { res.add(it) }
    return res.joinToString(LINE_SEPARATOR)
}

private fun InterfaceModel.serialize() = let { i ->
    json {
        "kind" to InterfaceModel::class.java.simpleName
        "annotations" to serializeAnnotations(i.annotations)
        i.comment?.let {
            "comment" to it.serialize()
        }
        "external" to i.external
        "typeParameters" to jsonArray { i.typeParameters.forEach { +it.serialize() } }
    }
}

fun InterfaceModel.translate(padding: Int, output: (String) -> Unit) {

    comment?.translate(output)

    val hasMembers = members.isNotEmpty()
    val staticMembers = companionObject?.members.orEmpty()

    val isBlock = hasMembers || staticMembers.isNotEmpty() || companionObject != null
    val parents = translateHeritagModels(parentEntities)

    val externalClause = if (external) "${KOTLIN_EXTERNAL_KEYWORD} " else ""
    output("${translateAnnotations(annotations)}${visibilityModifier.asClause()}${externalClause}interface ${name.translate()}${translateTypeParameters(
            typeParameters)}${parents}" + if (isBlock) " {" else "")
    if (isBlock) {
        members.flatMap { it.translateSignature() }.map { FORMAT_TAB.repeat(padding + 1) + it }.forEach { output(it) }

        if (companionObject != null) {
            val parentsResolved = if (companionObject!!.parentEntities.isEmpty()) {
                ""
            } else {
                " : ${companionObject!!.parentEntities.map { it.translateAsHeritageClause() }.joinToString(", ")}"
            }

            if (hasMembers) {
                output("")
            }
            output("${FORMAT_TAB.repeat(padding + 1)}companion object${parentsResolved}${if (staticMembers.isEmpty()) "" else " {"}")

            if (staticMembers.isNotEmpty()) {
                staticMembers.flatMap { it.translate() }.map { "${FORMAT_TAB.repeat(padding + 2)}${it}" }.forEach { output(it) }
                output("${FORMAT_TAB.repeat(padding + 1)}}")
            }
        }

        output("${FORMAT_TAB.repeat(padding)}}")
    }
}

fun MethodModel.serialize() = let { m ->
    json {
        "kind" to MethodModel::class.java.simpleName
        "annotations" to serializeAnnotations(m.annotations)
        "name" to m.name.serialize()
        "open" to m.open
        "operator" to m.operator
        "override" to m.override
        "static" to m.static
        "type" to m.type.serialize()
        "typeParameters" to serializeTypeParameterModels(m.typeParameters)
    }
}

fun PropertyModel.serialize() = let { p ->
    json {
        "kind" to PropertyModel::class.java.simpleName
        "getter" to p.getter
        "name" to p.name.serialize()
        "open" to p.open
        "override" to p.override
        "setter" to p.setter
        "static" to p.static
        "type" to p.type.serialize()
        "typeParameters" to serializeTypeParameterModels(p.typeParameters)
    }
}


fun MemberModel.serialize(): JsonObject = let { s ->
    when (s) {
        is MethodModel -> s.serialize()
        is PropertyModel -> s.serialize()
        is ConstructorModel -> s.serialize()
        is ClassModel -> s.serialize()
        is InterfaceModel -> s.serialize()
        else -> throw RuntimeException("Unknown MemberModel ${this}")
    }
}

fun ObjectModel.serialize() = let { o ->
    json {
        "kind" to ObjectModel::class.java.simpleName
        "name" to o.name.serialize()
        "visibilityModifier" to "${o.visibilityModifier}"
        "members" to jsonArray { o.members.forEach { it.serialize() } }
        "parentEntities" to jsonArray { o.parentEntities.forEach { it.serialize() } }
    }
}

@UseExperimental(kotlinx.serialization.UnstableDefault::class)
class StringTranslator : ModelVisitor {
    private var myOutput: MutableList<String> = mutableListOf()

    private val ast = mutableListOf<JsonObject>()
    private fun addOutput(fragment: String) {
        myOutput.add(fragment)
    }


    fun output(): Pair<String, JsonArray> {
        return myOutput.joinToString(LINE_SEPARATOR) to jsonArray { ast.forEach { +it } }
    }

    override fun visitTypeAlias(typeAlias: TypeAliasModel) {
        addOutput("")
        addOutput(typeAlias.translate())
        ast.add(typeAlias.serialize())

    }

    override fun visitVariable(variable: VariableModel) {
        addOutput("")
        addOutput(variable.translate())
        ast.add(variable.serialize())
    }

    override fun visitFunction(function: FunctionModel) {
        addOutput("")
        function.translate(0, ::addOutput)
        ast.add(serializeFunctionModel(function))
    }

    override fun visitObject(objectNode: ObjectModel) {
        addOutput("")
        val objectModel = "${objectNode.visibilityModifier.asClause()}${KOTLIN_EXTERNAL_KEYWORD} object ${objectNode.name.translate()}"

        val members = objectNode.members

        val hasMembers = members.isNotEmpty()

        addOutput(objectModel + " {")

        if (hasMembers) {
            members.flatMap { it.translate() }.map({ "    " + it }).forEach {
                addOutput(it)
            }

        }

        addOutput("}")
        // TODO: Finish this model
        ast.add(objectNode.serialize())
    }

    override fun visitEnum(enumNode: EnumModel) {
        addOutput("")
        addOutput(enumNode.translate())
        ast.add(enumNode.serialize())
    }

    override fun visitInterface(interfaceModel: InterfaceModel) {
        addOutput("")
        interfaceModel.translate(0, ::addOutput)
        ast.add(interfaceModel.serialize())
    }

    override fun visitClass(classModel: ClassModel) {
        addOutput("")
        classModel.translate(0, ::addOutput)
        ast.add(classModel.serialize())
    }

    fun visitImport(import: NameEntity) {
        addOutput("import ${import.translate()}")
    }

    private fun ModuleModel.serialize(): JsonObject = let { m ->
        json {
            "kind" to ModuleModel::class.java.simpleName
            "annotations" to serializeAnnotations(m.annotations)
            "name" to m.name.serialize()
            "imports" to jsonArray { m.imports.map { importNode -> +importNode.serialize() } }
            "shortName" to m.shortName.serialize()
            "submodules" to jsonArray { m.submodules.forEach { +it.serialize() } }
        }
    }

    override fun visitModule(moduleModel: ModuleModel) {
        if (moduleModel.declarations.isEmpty() && moduleModel.submodules.isEmpty()) {
            return
        }

        val containsSomethingExceptDocRoot = moduleModel.declarations.isNotEmpty()

        if (containsSomethingExceptDocRoot) {
            val translateAnnotations = translateAnnotations(moduleModel.annotations)

            if (moduleModel.name != ROOT_PACKAGENAME) {
                addOutput("${translateAnnotations}package ${moduleModel.name.translate()}")
                addOutput("")
            } else {
                if (translateAnnotations.isNotEmpty()) {
                    addOutput(translateAnnotations)
                }
            }
        }

        moduleModel.imports.forEachIndexed { _, importNode ->
            visitImport(importNode)
        }

        ast.add(moduleModel.serialize())

    }

}
