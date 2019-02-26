package org.jetbrains.dukat.compiler.tests.thisType

import org.jetbrains.dukat.compiler.tests.StandardTests
import org.junit.Test


class ThisTypeTests : StandardTests() {

    @Test
    fun inClass() {
        assertContentEquals("thisType/inClass")
    }

    @Test
    fun inInterface() {
        assertContentEquals("thisType/inInterface")
    }

    @Test
    fun inObjectType() {
        assertContentEquals("thisType/inObjectType")
    }

    @Test
    fun withGenericParameters() {
        assertContentEquals("thisType/withGenericParameters")
    }

}