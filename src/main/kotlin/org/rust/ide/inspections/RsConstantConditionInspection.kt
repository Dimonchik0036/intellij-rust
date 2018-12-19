/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.ide.inspections

import com.intellij.codeInspection.ProblemHighlightType
import com.intellij.codeInspection.ProblemsHolder
import org.rust.ide.utils.skipParenExprDown
import org.rust.lang.core.dfa.*
import org.rust.lang.core.psi.*
import org.rust.lang.core.types.dataFlowAnalysisResult
import org.rust.lang.core.types.ty.TyBool
import org.rust.lang.core.types.ty.TyInteger
import org.rust.lang.core.types.type

class RsConstantConditionInspection : RsLocalInspectionTool() {
    override fun buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean) =
        object : RsVisitor() {
            override fun visitFunction(o: RsFunction) {
                if (o.block == null) return
//                val block = o.block ?: return
//                val cfg = buildFor(block)
//                println(cfg.createDotDescription())
                val (result, runner) = o.dataFlowAnalysisResult
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
                    holder.registerProblem(condition, "Condition is always `false`")
                }
            }
        }
}

private fun createDescription(holder: ProblemsHolder, runner: DataFlowRunner) {
    // TODO: Remove debug functions and move constantConditionalExpression to cache
    val (trueSet, falseSet) = runner.constantConditionalExpression
    trueSet.forEach { registerConstantBoolean(holder, it, true) }
    falseSet.forEach { registerConstantBoolean(holder, it, false) }

    runner.overflowExpressions.forEach { registerOverflow(holder, it) }
    registerError(holder, runner.exception)
//    registerUnreachableCode(holder, runner.unvisitedElements)
    //dor debug
    addStates(holder, runner.resultState)
}

private fun addStates(holder: ProblemsHolder, state: DfaMemoryState?) {
    if (state == null) return
    state.entries.forEach {
        when (it.key.type) {
            is TyBool, is TyInteger -> holder.registerProblem(it.key, "Value is '${it.value}'", ProblemHighlightType.WEAK_WARNING)
        }
    }
}

//private fun registerUnreachableCode(holder: ProblemsHolder, elements: Set<RsElement>) =
//    elements
//        .filter { element -> elements.indexOfFirst { element != it && element in it } == -1 }
//        .forEach { holder.registerProblem(it, "Unreachable code", ProblemHighlightType.WEAK_WARNING) }

private fun registerConstantBoolean(holder: ProblemsHolder, expr: RsExpr, value: Boolean) {
    holder.registerProblem(expr, "Condition '${expr.text}' is always '$value'")
}

private fun registerOverflow(holder: ProblemsHolder, expr: RsExpr) = when (expr) {
    is RsLitExpr -> holder.registerProblem(expr, "Literal out of range for ${expr.type}")
    else -> holder.registerProblem(expr, "Expression '${expr.text}' is overflow")
}

private fun registerError(holder: ProblemsHolder, error: DfaException?) = when (error) {
    is DfaDivisionByZeroException -> holder.registerProblem(error.expr, "Division by zero", ProblemHighlightType.ERROR)
    else -> {
    }
}
