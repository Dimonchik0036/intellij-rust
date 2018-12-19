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
            let x/*{200}*/ = 200u8;
            let y/*{!}*/= <warning descr="Expression 'x * 1u8 * 2u8' is overflow">x * 1u8 * 2u8</warning>;
        }
    """)

    fun `test declaration from overflow literal expression`() = checkWithExpandValues("""
        fn main() {
            let x/*{!}*/ = <warning descr="Literal out of range for i8">200i8</warning>;
        }
    """)

    fun `test division by zero stops the analysis`() = checkWithExpandValues("""
        fn main() {
            let x/*{0}*/ = 5 * 0;
            let a = <error descr="Division by zero">10 % x</error>;
            let f = true;
            if f {

            }
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

    fun `test overflow in compare`() = checkWithExpandValues("""
        fn test(input/*{-2147483648..2147483647}*/: i32) {
            if input > 2000000000 && <warning descr="Expression 'input * 10' is overflow">input * 10</warning> > 2000000001 {

            }
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

    fun `test apply condition 1`() = checkWithExpandValues("""
       fn foo(b/*{-128..127}*/: i8) {
            let x/*{42}*/: i8 = 42i8;
            let a/*{37, 47}*/: i8;
            if b > 10i8 && b != x {
                a = x + 5i8;
                let b/*{11..41, 43..127}*/ = b;
            } else {
                a = x - 5i8;
                let b/*{-128..127}*/ = b;
            }
       }
    """)

    fun `test apply condition 2`() = checkWithExpandValues("""
       fn foo(a/*{?}*/: bool) {
            let x/*{true}*/ = true;
            if a != x {
                let c/*{true}*/ = x;
                let d/*{false}*/ = a;
            } else {
                let c/*{true}*/ = x;
                let d/*{true}*/ = a;
            }
       }
    """)

    fun `test apply condition 3`() = checkWithExpandValues("""
       fn test(input/*{-2147483648..2147483647}*/: i32) {
            let x/*{42}*/ = 42;
            let a/*{-2147483648..40, 61..2147483647}*/: i32;
            if input > 50 {
                a = input + 10;
                if <warning descr="Condition 'a < 60' is always 'false'">a < 60</warning> {

                }
            } else {
                a = input - 10;
            }
        }
    """)

    fun `test apply condition 4`() = checkWithExpandValues("""
       fn test(input/*{-2147483648..2147483647}*/: i32) {
            if input >= 50 {
                if input < 100 {
                    let a1/*{60..109}*/ = input + 10;
                    let b1/*{40..89}*/ = input - 10;
                    let c1/*{500..990}*/ = input * 10;
                    let d1/*{0..9}*/ = input % 10;
                } else {
                    let a2/*{110..2147483647}*/ = input + 10;
                    let b2/*{90..2147483637}*/ = input - 10;
                    let c2/*{1000..2147483647}*/ = input * 10;
                    let d2/*{0..9}*/ = input % 10;
                }
            } else {
                    let a3/*{-2147483638..59}*/ = input + 10;
                    let b3/*{-2147483648..39}*/ = input - 10;
                    let c3/*{-2147483648..-10, 0, 10..490}*/ = input * 10;
                    let d3/*{-9..9}*/ = input % 10;
            }
        }
    """)

    fun `test with unary operator`() = checkWithExpandValues("""
       fn test(input/*{-2147483648..2147483647}*/: i32) {
            if !(input == 42 || input > 50) && input >= 0 {
                let a/*{0..41, 43..50}*/ = input;
            } else {
                let a/*{-2147483648..2147483647}*/ = input;
            }
            let a/*{-2147483648..2147483647}*/ = input;
        }
    """)

    fun `test with unary operator always false`() = checkWithExpandValues("""
       fn test(input/*{-2147483648..2147483647}*/: i32) {
            if <warning descr="Condition '!(input != 42 || input != 50)' is always 'false'">!(input != 42 || input != 50)</warning> {
                let a = input;
            } else {
                let a/*{-2147483648..2147483647}*/ = input;
            }
            let a/*{-2147483648..2147483647}*/ = input;
        }
    """)

    fun `test with unary operator always true`() = checkWithExpandValues("""
       fn test(input/*{-2147483648..2147483647}*/: i32) {
            if <warning descr="Condition '!(input == 42 && input == 50)' is always 'true'">!(input == 42 && input == 50)</warning> {
                let a/*{-2147483648..2147483647}*/ = input;
            } else {
                let a = input;
            }
            let a/*{-2147483648..2147483647}*/ = input;
        }
    """)

    fun `test simple match`() = checkWithExpandValues("""
       fn test(input/*{-2147483648..2147483647}*/: i32) {
            let a/*{-10, 1, 50}*/: i32;
            match input {
                1 => { a = 1 }
                5 => { a = -10 }
                _ => { a = 50 }
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
                val state = runner.resultState
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
