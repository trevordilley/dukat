package org.jetbrains.dukat.compiler.tests.extended

import org.jetbrains.dukat.compiler.tests.CliTranslator
import org.jetbrains.dukat.compiler.tests.CompileMessageCollector
import org.jetbrains.dukat.compiler.tests.createStandardCliTranslator
import org.jetbrains.kotlin.cli.common.ExitCode
import org.jetbrains.kotlin.cli.common.arguments.K2JSCompilerArguments
import org.jetbrains.kotlin.cli.js.K2JSCompiler
import org.jetbrains.kotlin.config.Services
import java.io.File
import kotlin.test.assertEquals

abstract class CompilationTests {

    private fun getTranslator(): CliTranslator = createStandardCliTranslator()

    abstract fun runTests(
            descriptor: String,
            sourcePath: String
    )

    protected fun compile(sources: List<String>, targetPath: String): ExitCode {

        val options =
                K2JSCompilerArguments().apply {
                    outputFile = targetPath
                    metaInfo = false
                    sourceMap = false
                    noStdlib = true
                    libraries = listOf(
                            "./build/kotlinHome/kotlin-stdlib-js.jar"
                    ).joinToString(File.pathSeparator)
                }

        options.freeArgs = sources

        return K2JSCompiler().exec(
                CompileMessageCollector(),
                Services.EMPTY,
                options
        )
    }

    companion object {
        val COMPILATION_ERROR_ASSERTION = "COMPILATION ERROR"
        val FILE_NOT_FIND_ASSERTION = "FILE NOT FOUND"
    }

    protected fun assertContentCompiles(
            descriptor: String,
            sourcePath: String
    ) {

        val targetPath = "./build/tests/compiled/${descriptor}"
        val targetDir = File("./build/tests/compiled/${descriptor}")

        getTranslator().translate(sourcePath, targetPath)
        val outSource = "${targetPath}/${descriptor}.js"

        val sources = targetDir.walk().map { it.absolutePath }.toList()

        assert(sources.isNotEmpty()) { "$FILE_NOT_FIND_ASSERTION: $targetPath" }

        assertEquals(
                ExitCode.OK,
                compile(
                        sources,
                        outSource
                ), COMPILATION_ERROR_ASSERTION
        )
    }

}