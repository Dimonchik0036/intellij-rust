/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.resolve

import org.intellij.lang.annotations.Language
import org.rust.lang.core.psi.ext.RsReferenceElement


class RsMultiResolveTest : RsResolveTestBase() {
    fun `test struct expr`() = doTest("""
        struct S { foo: i32, foo: () }
        fn main() {
            let _ = S { foo: 1 };
                       //^
        }
    """)

    fun `test struct field shorthand`() = doTest("""
        struct S { foo: i32, bar: fn() }
        fn foo() {}
        fn main() {
            let bar = 62;
            let _ = S { bar, foo };
        }                   //^
    """)

    fun `test field expr`() = doTest("""
        struct S { foo: i32, foo: () }
        fn f(s: S) {
            s.foo
             //^
        }
    """)

    fun `test use multi reference`() = doTest("""
        use m::foo;
              //^

        mod m {
            fn foo() {}
            mod foo {}
        }
    """)

    private fun doTest(@Language("Rust") code: String) {
        InlineFile(code)
        val ref = findElementInEditor<RsReferenceElement>().reference
        check(ref.multiResolve().size == 2) {
            "Expected 2 variants, got ${ref.multiResolve()}"
        }
    }
}
