/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.ide.inspections

import org.intellij.lang.annotations.Language

class RsConstantConditionInspectionTest : RsInspectionsTestBase(RsConstantConditionInspection()) {
    fun `test declaration from integer constant`() = checkDeclaration("42", "{42}")

    fun `test declaration from unary expression`() = checkDeclaration("-42", "{-42}")

    fun `test declaration from binary expression`() = checkDeclaration("21 + 21", "{42}")

    fun `test declaration from boolean constant`() = checkDeclaration("true", "{true}")

    private fun checkDeclaration(expression: String, value: String) = checkWithExpandValues("""
        fn main() {
            let x/*$value*/ = $expression;
        }
    """)

    fun `test declaration from arguments`() = checkWithExpandValues("""
        fn foo(a/*{-2147483648..2147483647}*/: i32, b/*{?}*/: bool, c/*{0..255}*/: u8) { }
    """)

    fun `test declaration from tuple of 2 elements`() = checkWithExpandValues("""
        fn main() {
            let (x/*{42}*/, y/*{24}*/) = (42, 24);
        }
    """)

    fun `test declaration from tuple of 3 elements`() = checkWithExpandValues("""
        fn main() {
            let (x/*{1}*/, y/*{true}*/, z/*{-3}*/) = (1, true , -3);
        }
    """)

    fun `test declaration from function`() = checkWithExpandValues("""
        fn foo() -> i8 { 42 }
        fn main() {
            let x/*{-128..127}*/ = foo();
        }
    """)

    private fun checkWithExpandValues(@Language("Rust") text: String) = checkByText(text.expandValues, checkWeakWarn = true)

    private val String.expandValues: String
        get() = setOfValuesRegex.replace(this) {
            val variableName = it.value.substringBefore('/')
            val setOfValue = it.value.substringAfter("/*").substringBefore("*/")
            "<weak_warning descr=\"Value is '$setOfValue'\">$variableName</weak_warning>"
        }

    companion object {
        private val setOfValuesRegex = Regex("([_a-zA-Z0-9])*(/[*]).*?([*]/)")
    }
}
