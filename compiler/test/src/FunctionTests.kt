import org.jetbrains.dukat.compiler.Translator
import org.jetbrains.dukat.compiler.compile
import org.jetbrains.dukat.compiler.createV8Translator
import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals


class FunctionTests {

    companion object {
        val translator: Translator

        init {
            translator = createV8Translator()
//            translator = createNashornTranslator()
        }

    }


    private fun assertContentEquals(name: String) {

        val resourceDirectory = File("./test/data")
        val fileNameSource = resourceDirectory.resolve("${name}.d.ts").absolutePath
        val fileNameTarget = resourceDirectory.resolve("${name}.d.kt")

        assertEquals(
                compile(fileNameSource, translator),
                fileNameTarget.readText().trimEnd()
        )
    }

    @Test
    fun testFunctions() {
        assertContentEquals("functions")
    }

    @Test
    fun testFunctionsWithDefaultArguments() {
        assertContentEquals("functionsWithDefaultArguments")
    }

    @Test
    fun functionsWithOptionalFunctionType() {
        assertContentEquals("functionsWithOptionalFunctionType")
    }

    @Test
    fun functionsWithOptionalParameter() {
        assertContentEquals("functionsWithOptionalParameter")
    }

}