/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.ide.inspections

import com.intellij.codeInspection.ProblemsHolder
import com.intellij.codeInspection.bytecodeAnalysis.asm.ControlFlowGraph
import org.rust.ide.utils.skipParenExprDown
import org.rust.lang.core.cfg.ControlFlowGraph.Companion.buildFor
import org.rust.lang.core.dfa.DataFlowRunner
import org.rust.lang.core.psi.*
import org.rust.lang.core.resolve.ref.deepResolve

class RsConstantConditionInspection : RsLocalInspectionTool() {
    override fun buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean) =
        object : RsVisitor() {
            override fun visitFunction(o: RsFunction) {
                val block = o.block ?: return
                val cfg = buildFor(block)
                println(cfg.createDotDescription())
                cfg.buildLocalIndex().forEach{
                    println("Element $it")
                    val e = it.key
                    if (e is RsPathExpr) {
                        val ref = e.path.reference
                        println("${ref.resolve()?.text}\n" +
                            "${ref.deepResolve()?.text}\n" +
                            "${ref.advancedResolve()}\n" +
                            "${ref.advancedMultiResolve()}")
                        println()
                    }
                }
            }

            override fun visitIfExpr(o: RsIfExpr) {
                val condition = o.condition?.skipParenExprDown() as? RsLitExpr ?: return
                val boolLit = condition.boolLiteral ?: return
                holder.registerProblem(condition, "Condition is always `${boolLit.text}`")
            }
        }

}

private fun registerProblem(holder: ProblemsHolder, expr: RsExpr, value: Boolean) {
    holder.registerProblem(expr, "Condition `${expr.text}` is always `$value`")
}
