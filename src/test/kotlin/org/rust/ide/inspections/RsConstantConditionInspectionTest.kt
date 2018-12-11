/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.ide.inspections

import org.intellij.lang.annotations.Language
import org.rust.lang.core.dfa.DataFlowRunner
import org.rust.lang.core.dfa.RunnerResult
import org.rust.lang.core.psi.RsFunction
import org.rust.lang.core.psi.RsPatBinding
import org.rust.lang.core.psi.ext.descendantsOfType

class RsConstantConditionInspectionTest : RsInspectionsTestBase(RsConstantConditionInspection()) {
    fun `test declaration from integer constant`() = checkDeclaration("42", "{42}")

    fun `test declaration from integer constant with suffix`() = checkDeclaration("42u16", "{42}")

    fun `test declaration from integer constant with separator`() = checkDeclaration("1_000__000", "{1000000}")

    fun `test declaration from unary expression`() = checkDeclaration("-42", "{-42}")

    fun `test declaration from binary expression`() = checkDeclaration("21 + 21", "{42}")

    fun `test declaration from boolean constant`() = checkDeclaration("true", "{true}")

    private fun checkDeclaration(expression: String, value: String) = checkWithExpandValues("""
        fn main() {
            let x/*$value*/: i32 = $expression;
        }
    """)

    fun `test declaration from overflow expression`() = checkWithExpandValues("""
        fn main() {
            let x/*{200}*/: u8 = 200;
            let y/*{!}*/: u8 = x * 2u8;
        }
    """)

    fun `test declaration from arguments`() = checkWithExpandValues("""
        fn foo(
            a/*{-2147483648..2147483647}*/: i32,
            b/*{?}*/: bool,
            c/*{0..255}*/: u8,
            d/*{-9223372036854775808..9223372036854775807}*/: i128
            ) { }
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

    fun `test declaration equal to myself`() = checkWithExpandValues("""
        fn foo(a/*{?}*/: bool) {
            let t/*{true}*/ = a == a;
            let f/*{false}*/ = a != a;
        }
    """)

    fun `test declaration equal to other with unknown`() = checkWithExpandValues("""
        fn foo(a/*{?}*/: bool, b/*{?}*/: bool) {
            let x/*{?}*/ = a == b;
            let y/*{?}*/ = a != b;
        }
    """)

    fun `test declaration equal to other with constant`() = checkWithExpandValues("""
        fn foo(a/*{?}*/: bool) {
            let b/*{true}*/ = true;
            let x/*{?}*/ = a == b;
            let y/*{?}*/ = a != b;
        }
    """)

    fun `test declaration with identical names`() = checkWithExpandValues("""
        fn foo(a/*{?}*/: bool) {
            let t/*{true}*/ = a == a;
            let f/*{false}*/ = a != a;
        }
    """)

    fun `test if with literal expression`() = checkWithExpandValues("""
       fn main() {
            if <warning descr="Condition is always `true`">true</warning> {
                // do smth
            }

            if <warning descr="Condition is always `false`">false</warning> {
                // do smth
            }
       }
    """)

    fun `test while with literal expression`() = checkWithExpandValues("""
       fn main() {
            while <warning descr="Condition is always `false`">false</warning> {
                // do smth
            }

            while true {
                // do smth
            }
       }
    """)

    fun `test simple boolean expression with or`() = checkWithExpandValues("""
       fn main() {
            let a/*{true}*/ = true;
            let b/*{false}*/ = false;
            if <warning descr="Condition 'a || b' is always 'true'">a || b</warning> {
                let c/*{true}*/ = a;
                let d/*{false}*/ = b;
            } else {
                let c = b;
                let d = a;
            }
       }
    """)

    fun `test simple boolean expression with and`() = checkWithExpandValues("""
       fn main() {
            let a/*{true}*/ = true;
            let b/*{false}*/ = false;
            if <warning descr="Condition 'a && b' is always 'false'">a && b</warning> {
                let c = a;
                let d = b;
            } else {
                let c/*{false}*/ = b;
                let d/*{true}*/ = a;
            }
       }
    """)

    fun `test several constant condition`() = checkWithExpandValues("""
       fn foo(a/*{?}*/: bool) {
            let b/*{false}*/ = false;
            let c/*{true}*/ = true;
            if a || <warning descr="Condition 'c && b' is always 'false'">c && b</warning> || <warning descr="Condition 'b && c' is always 'false'">b && c</warning> {

            }
       }
    """)

    private fun checkWithExpandValues(@Language("Rust") text: String) {
        checkByText(text)
        checkVariables()
    }

    private fun checkVariables() {
        data class Result(val variable: RsPatBinding, val expected: String, val actual: String)

        val errors = myFixture.file.descendantsOfType<RsFunction>().asSequence()
            .flatMap { function ->
                val runner = DataFlowRunner(function)
                val result = runner.analyze()
                if (result != RunnerResult.OK) error("Couldn't analyze `${function.identifier.text}`")
                val state = runner.resultState ?: error("Couldn't find state for `${function.identifier.text}`")
                function.descendantsOfType<RsPatBinding>().asSequence()
                    .filter { it in state }
                    .map {
                        val suffix = myFixture.file.text.substring(it.textRange.endOffset)
                        if (!suffix.startsWith("/*")) error("Couldn't find expected set for `${it.text}`")
                        Result(it, suffix.substring(2).substringBefore("*/"), state.getOrUnknown(it).toString())
                    }
            }
            .filter { it.expected != it.actual }
        if (!errors.none()) error(errors.joinToString(separator = "\n") { "variable `${it.variable.text}` expected `${it.expected}` actual `${it.actual}`" })
    }

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
