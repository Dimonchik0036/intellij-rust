/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.ide.inspections

import com.intellij.codeInspection.ProblemHighlightType
import com.intellij.codeInspection.ProblemsHolder
import org.rust.ide.utils.skipParenExprDown
import org.rust.lang.core.cfg.ControlFlowGraph.Companion.buildFor
import org.rust.lang.core.dfa.DataFlowRunner
import org.rust.lang.core.dfa.DfaMemoryState
import org.rust.lang.core.dfa.RunnerResult
import org.rust.lang.core.psi.*
import org.rust.lang.core.psi.ext.RsElement

class RsConstantConditionInspection : RsLocalInspectionTool() {
    override fun buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean) =
        object : RsVisitor() {
            override fun visitFunction(o: RsFunction) {
//                val block = o.block ?: return
//                val cfg = buildFor(block)
//                println(cfg.createDotDescription())
                val runner = DataFlowRunner(o)
                val result = runner.analyze()
                when (result) {
                    RunnerResult.OK -> createDescription(holder, runner)
                    else -> holder.registerProblem(o, "Couldn't analyze function $result")
                }
            }

            override fun visitIfExpr(o: RsIfExpr) {
                val condition = o.condition?.skipParenExprDown() as? RsLitExpr ?: return
                val boolLit = condition.boolLiteral ?: return
                holder.registerProblem(condition, "Condition is always `${boolLit.text}`")
            }

            override fun visitWhileExpr(o: RsWhileExpr) {
                val condition = o.condition?.skipParenExprDown() as? RsLitExpr ?: return
                if (condition.textMatches("false")) {
                    holder.registerProblem(condition, "Condition is always false")
                }
            }
        }
}

private fun createDescription(holder: ProblemsHolder, runner: DataFlowRunner) {
    val (trueSet, falseSet) = runner.constantConditionalExpression
    trueSet.forEach { registerConstantBoolean(holder, it.anchor, true) }
    falseSet.forEach { registerConstantBoolean(holder, it.anchor, false) }

//    registerUnreachableCode(holder, runner.unvisitedElements)
    //dor debug
    addStates(holder, runner.resultState)
}

private fun addStates(holder: ProblemsHolder, state: DfaMemoryState?) {
    if (state == null) return
    state.variableStates.forEach {
        holder.registerProblem(it.key, "Value is '${it.value}'", ProblemHighlightType.WEAK_WARNING)
    }
}

//private fun registerUnreachableCode(holder: ProblemsHolder, elements: Set<RsElement>) =
//    elements
//        .filter { element -> elements.indexOfFirst { element != it && element in it } == -1 }
//        .forEach { holder.registerProblem(it, "Unreachable code", ProblemHighlightType.WEAK_WARNING) }

private operator fun RsElement.contains(other: RsElement): Boolean = other.textRange in this.textRange

private fun registerConstantBoolean(holder: ProblemsHolder, expr: RsExpr, value: Boolean) {
    holder.registerProblem(expr, "Condition '${expr.text}' is always '$value'")
}
