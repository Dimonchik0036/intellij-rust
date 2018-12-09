/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import com.intellij.util.ThreeState
import com.intellij.util.containers.Queue
import org.jetbrains.coverage.gnu.trove.TIntObjectHashMap
import org.rust.ide.utils.skipParenExprDown
import org.rust.ide.utils.skipParenExprUp
import org.rust.lang.core.cfg.CFGNode
import org.rust.lang.core.cfg.CFGNodeData
import org.rust.lang.core.cfg.ControlFlowGraph.Companion.buildFor
import org.rust.lang.core.dfa.value.DfaConstValue
import org.rust.lang.core.dfa.value.DfaUnknownValue
import org.rust.lang.core.dfa.value.DfaValue
import org.rust.lang.core.dfa.value.DfaValueFactory
import org.rust.lang.core.psi.*
import org.rust.lang.core.psi.ext.*
import org.rust.lang.core.types.type

class DataFlowRunner(val function: RsFunction) {
    private val valueFactory: DfaValueFactory = DfaValueFactory()
    private val instructions = hashSetOf<BinOpInstruction>()
    private val states = TIntObjectHashMap<DfaMemoryState>()
    //    var unvisitedElements: Set<RsElement> = emptySet()
//        private set
    //for debug
    var resultState: DfaMemoryState? = null

    init {
        initFunctionParameters()
    }

    val constantConditionalExpression
        get(): DfaResult {
            val trueSet = hashSetOf<BinOpInstruction>()
            val falseSet = hashSetOf<BinOpInstruction>()
            instructions.forEach {
                if (!it.isConst) {
                    val reachable = it.reachable
                    if (reachable.isTrueReachable && !reachable.isFalseReachable) {
                        trueSet += it
                    } else if (reachable.isFalseReachable && !reachable.isTrueReachable) {
                        falseSet += it
                    }
                }
            }
            return DfaResult(trueSet, falseSet)
        }

    private fun createMemoryState(): DfaMemoryState = DfaMemoryState.EMPTY

    fun analyze(): RunnerResult {
        try {
            val visitor = NodeVisitorState(function.block ?: return RunnerResult.NOT_APPLICABLE)
            lineVisit(visitor)
//            unvisitedElements = visitor.unvisitedElements.filter { it !is RsBlock }.toSet()
            return RunnerResult.OK
        } catch (e: Exception) {
            return RunnerResult.NOT_APPLICABLE
        }
    }

    private fun initFunctionParameters() {
        var state = createMemoryState()
        function.valueParameterList?.valueParameterList?.forEach {
            val element = it.pat as? RsPatIdent
            val binPat = element?.patBinding
            if (binPat != null) {
                state = state.plus(binPat, valueFactory.createTypeValue(binPat.type))
            }
        }
        setState(0, state)
    }

    /***
     * @param endIndex 1 is index of [org.rust.lang.core.cfg.CFGNodeData.Exit] node
     */
    private fun lineVisit(visitorState: NodeVisitorState, endIndex: Int = 1) {
        while (true) {
            val node = visitorState.nextNode()
            if (node == null || node.index == endIndex) return
            with(node.data) {
                when {
                    this is CFGNodeData.AST && this.element != null -> visitAstNode(this.element, node, visitorState)
                    this is CFGNodeData.Dummy -> visitDummyNode(node, visitorState)
                    else -> updateNextState(node, visitorState)
                }
            }
        }
    }

    private fun updateNextState(node: CFGNode, visitorState: NodeVisitorState, nextNode: CFGNode? = node.nextNode) {
        if (nextNode == null) return
        visitorState.addNode(nextNode)
        mergeStates(node, nextNode)
    }

    private fun getStateWithCheck(index: Int): DfaMemoryState = states[index] ?: error("Index $index")
    private fun setState(index: Int, state: DfaMemoryState) {
        states.put(index, state)
    }

    private fun mergeStates(nodeFrom: CFGNode, nodeTo: CFGNode) {
        val currentState = states[nodeFrom.index] ?: return
        mergeState(currentState, nodeTo)
    }

    private fun mergeState(state: DfaMemoryState, nodeTo: CFGNode) {
        val nextState = states[nodeTo.index]
        if (nextState != null) {
            setState(nodeTo.index, state.unite(nextState))
        } else {
            setState(nodeTo.index, state)
        }
        resultState = states[nodeTo.index]
    }

    private fun visitAstNode(element: RsElement, node: CFGNode, visitorState: NodeVisitorState) = when (element) {
        is RsLetDecl -> visitLetDeclNode(node, element, visitorState)
        is RsBinaryExpr -> visitBinExpr(node, element, visitorState)
        is RsPathExpr -> tryVisitControlFlow(element, node, visitorState)
        is RsUnaryExpr -> tryVisitControlFlow(element, node, visitorState)
        is RsRetExpr -> {
        }
        else -> updateNextState(node, visitorState)
    }

    private fun visitBinExpr(node: CFGNode, expr: RsBinaryExpr, visitorState: NodeVisitorState) {
        val state = getStateWithCheck(node.index)
        when (expr.operatorType) {
            is AssignmentOp -> visitAssignmentBinOp(expr, state, node, visitorState)
            is BoolOp -> tryVisitControlFlow(expr, node, visitorState)
            else -> updateNextState(node, visitorState)
        }
    }

    private fun tryVisitControlFlow(expr: RsExpr, node: CFGNode, visitorState: NodeVisitorState) {
        val parent = expr.skipParenExprUp().parent
        when (parent) {
            is RsCondition -> visitCondition(expr, parent, node, visitorState)
            is RsMatchExpr -> visitMatchExpr(expr, parent, node, visitorState)
            else -> updateNextState(node, visitorState)
        }
    }

    private fun visitCondition(expr: RsExpr, condition: RsCondition, node: CFGNode, visitorState: NodeVisitorState) {
        val parent = condition.parent
        when (parent) {
            is RsIfExpr -> visitIfExpr(expr, parent, node, visitorState)
            else -> updateNextState(node, visitorState)
        }
    }

    private fun visitIfExpr(expr: RsExpr, ifExpr: RsIfExpr, node: CFGNode, visitorState: NodeVisitorState) {
        val ifNode = visitorState.getNodeFromElement(ifExpr)
        val state = getStateWithCheck(node.index)
        val nodes = node.outgoingNodes
        val trueBranch = nodes.elementAt(1)
        val falseBranch = nodes.elementAt(0)

        val value = tryEvaluateExpr(expr, state)
        val trueState = state.intersect(value.trueState, false)
        val falseState = state.intersect(value.falseState, false)

        when (value.threeState) {
            ThreeState.YES -> {
                visitorState.addNode(trueBranch)
                mergeState(trueState, trueBranch)
                mergeState(falseState, ifNode)
            }
            ThreeState.NO -> {
                visitorState.addNode(falseBranch)
                mergeState(falseState, falseBranch)
                mergeState(trueState, ifNode)
            }
            ThreeState.UNSURE -> {
                visitBranch(trueBranch, trueState, visitorState, ifNode.index + 1)
                visitBranch(falseBranch, falseState, visitorState, ifNode.index + 1)
                updateNextState(ifNode, visitorState)
            }
        }
        val reachable = DfaReachableBranch.fromThreeState(value.threeState)
        instructions += BinOpInstruction(reachable, expr)
    }

    private fun visitMatchExpr(expr: RsExpr, matchExpr: RsMatchExpr, node: CFGNode, visitorState: NodeVisitorState) {
        val matchNode = visitorState.getNodeFromElement(matchExpr)
        val state = getStateWithCheck(node.index)
        val nodes = node.outgoingNodes
        nodes.forEach { visitBranch(it, state, visitorState, matchNode.index) }
        updateNextState(matchNode, visitorState)
    }

    private fun visitBranch(node: CFGNode, state: DfaMemoryState, visitorState: NodeVisitorState, endIndex: Int) {
        setState(node.index, state)
        visitorState.addNode(node)
        lineVisit(visitorState, endIndex)
    }

    private fun tryEvaluateExpr(expr: RsExpr?, state: DfaMemoryState): DfaCondition {
        val expr = expr?.skipParenExprDown() ?: return DfaCondition.UNSURE
        return when (expr) {
            is RsLitExpr -> tryEvaluateLitExpr(expr)
            is RsUnaryExpr -> tryEvaluateUnaryExpr(expr, state)
            is RsBinaryExpr -> tryEvaluateBinExpr(expr, state)
            is RsPathExpr -> tryEvaluatePathExpr(expr, state)
            else -> DfaCondition.UNSURE
        }
    }

    private fun tryEvaluatePathExpr(expr: RsPathExpr, state: DfaMemoryState): DfaCondition {
        val constValue = valueFromPathExpr(expr, state) as? DfaConstValue ?: return DfaCondition.UNSURE
        return DfaCondition(fromBool(constValue.value as? Boolean))
    }

    private fun tryEvaluateLitExpr(expr: RsLitExpr): DfaCondition = DfaCondition(fromBool((expr.kind as? RsLiteralKind.Boolean)?.value))

    private fun tryEvaluateUnaryExpr(expr: RsUnaryExpr, state: DfaMemoryState): DfaCondition {
        if (expr.excl == null) return DfaCondition.UNSURE
        val result = tryEvaluateExpr(expr.expr, state)
        return DfaCondition(result.threeState.not, trueState = result.falseState, falseState = result.trueState)
    }

    private fun tryEvaluateBinExpr(expr: RsBinaryExpr, state: DfaMemoryState): DfaCondition {
        val op = expr.operatorType
        val left = expr.left.skipParenExprDown()
        val right = expr.right?.skipParenExprDown() ?: return DfaCondition.UNSURE
        return when (op) {
            is LogicOp -> tryEvaluateBinExprWithLogicOp(op, left, right, state)
            is BoolOp -> tryEvaluateBinExprWithRange(op, expr, state)
            else -> DfaCondition.UNSURE
        }
    }

    private fun tryEvaluateBinExprWithLogicOp(op: LogicOp, left: RsExpr, right: RsExpr, state: DfaMemoryState): DfaCondition {
        val leftResult = tryEvaluateExpr(left, state)
        return when (op) {
            LogicOp.OR -> if (leftResult.threeState == ThreeState.YES) leftResult else leftResult.or(tryEvaluateExpr(right, state))
            LogicOp.AND -> if (leftResult.threeState == ThreeState.NO) leftResult else leftResult.and(tryEvaluateExpr(right, state))
        }
    }

    private fun tryEvaluateConst(op: BoolOp, leftExpr: RsExpr?, leftValue: DfaValue, rightExpr: RsExpr?, rightValue: DfaValue): DfaCondition? = when {
        leftExpr == null || rightExpr == null || leftValue is DfaUnknownValue && rightValue is DfaUnknownValue -> DfaCondition.UNSURE
        op is EqualityOp && (leftValue is DfaConstValue && rightValue is DfaConstValue
            || leftValue is DfaConstValue && rightValue is DfaUnknownValue
            || leftValue is DfaUnknownValue && rightValue is DfaConstValue) -> {
            val boolValue = ((leftValue as? DfaConstValue)?.value ?: (rightValue as? DfaConstValue)?.value) as? Boolean
            val leftVariable = leftExpr.toVariable()
            val rightVariable = rightExpr.toVariable()
            val value = valueFactory.createBoolValue(boolValue).let { if (op is EqualityOp.EQ) it else it.negated }
            val trueState = createMemoryState()
                .uniteValue(leftVariable, value)
                .uniteValue(rightVariable, value)

            val falseState = createMemoryState()
                .uniteValue(leftVariable, value.negated)
                .uniteValue(rightVariable, value.negated)
            DfaCondition(ThreeState.UNSURE, trueState, falseState)
        }
        else -> null
    }

    private fun tryEvaluateBinExprWithRange(op: BoolOp, expr: RsBinaryExpr, state: DfaMemoryState): DfaCondition {
        val leftValue = valueFromExpr(expr.left, state)
        val rightValue = valueFromExpr(expr.right, state)

        val value = tryEvaluateConst(op, expr.left, leftValue, expr.right, rightValue)
        if (value != null) return value

        if (leftValue.type != rightValue.type) return DfaCondition.UNSURE
        val leftRange = LongRangeSet.fromDfaValue(leftValue) ?: return DfaCondition.UNSURE
        val rightRange = LongRangeSet.fromDfaValue(rightValue) ?: return DfaCondition.UNSURE

        val (leftTrueResult, rightTrueResult) = leftRange.compare(op, rightRange)
        val trueState = createMemoryState()
            .uniteValue(expr.left.toVariable(), valueFactory.createRange(leftTrueResult))
            .uniteValue(expr.right?.toVariable(), valueFactory.createRange(rightTrueResult))

        val (leftFalseResult, rightFalseResult) = leftRange.compare(op.not, rightRange)
        val falseState = createMemoryState()
            .uniteValue(expr.left.toVariable(), valueFactory.createRange(leftFalseResult))
            .uniteValue(expr.right?.toVariable(), valueFactory.createRange(rightFalseResult))

        val resultRange = leftTrueResult.unite(rightTrueResult)
        return when {
            resultRange.isEmpty -> DfaCondition(ThreeState.NO, trueState = trueState, falseState = falseState)
            leftRange in resultRange && rightRange in resultRange -> DfaCondition(ThreeState.YES, trueState = trueState, falseState = falseState)
            else -> DfaCondition(ThreeState.UNSURE, trueState = trueState, falseState = falseState)
        }
    }

    private fun visitLetDeclNode(node: CFGNode, element: RsLetDecl, visitorState: NodeVisitorState) {
        val pat = element.pat
        when (pat) {
            is RsPatIdent -> {
                val state = getStateWithCheck(node.index)
                val bin = pat.patBinding
                val expr = element.expr
                val value = if (expr != null) valueFromExpr(expr, state) else valueFactory.createTypeValue(bin.type)
                setState(node.index, state.plus(bin, value))
            }
            is RsPatTup -> {
                var state = getStateWithCheck(node.index)
                val values = valuesFromTuple(element.expr, state)
                pat.patList.forEachIndexed { index, it ->
                    state = state.plus((it as? RsPatIdent)?.patBinding, values.getOrElse(index) { DfaUnknownValue })
                }
                setState(node.index, state)
            }
        }
        updateNextState(node, visitorState)
    }

    private fun valuesFromTuple(element: RsExpr?, state: DfaMemoryState): List<DfaValue> {
        val tuple = element as? RsTupleExpr ?: return emptyList()
        return tuple.exprList.map { valueFromExpr(it, state) }
    }

    private fun getValueFromElement(element: RsPatBinding, state: DfaMemoryState): DfaValue = state.get(element)

    private fun valueFromExpr(expr: RsExpr?, state: DfaMemoryState): DfaValue {
        val expr = expr?.skipParenExprDown() ?: return DfaUnknownValue
        return when (expr) {
            is RsPathExpr -> valueFromPathExpr(expr, state)
            is RsLitExpr -> valueFactory.createLiteralValue(expr)
            is RsBinaryExpr -> valueFromBinExpr(expr, state)
            is RsUnaryExpr -> valueFromUnaryExpr(expr, state)
            else -> valueFactory.createTypeValue(expr.type)
        }
    }

    private fun valueFromUnaryExpr(expr: RsUnaryExpr, state: DfaMemoryState): DfaValue =
        when {
            expr.excl != null -> valueFromExpr(expr.expr, state).negated
            expr.minus != null -> valueFromExpr(expr.expr, state).minus
            else -> DfaUnknownValue
        }


    private fun valueFromPathExpr(expr: RsPathExpr, state: DfaMemoryState): DfaValue {
        val variable = expr.toVariable() ?: return DfaUnknownValue
        return getValueFromElement(variable, state)
    }

    private fun valueFromBinExpr(expr: RsBinaryExpr, state: DfaMemoryState): DfaValue {
        val op = expr.operatorType
        return when (op) {
            is AssignmentOp -> DfaUnknownValue
            else -> valueFromBinOp(op, expr, state)
        }
    }

    private fun valueFromBinOp(op: BinaryOperator, expr: RsBinaryExpr, state: DfaMemoryState): DfaValue {
        val leftValue = valueFromExpr(expr.left, state)
        val rightValue = valueFromExpr(expr.right, state)
        return valueFromOp(op, leftValue, rightValue)
    }

    private fun valueFromConstValue(op: BinaryOperator, left: DfaValue, right: DfaValue): DfaValue? {
        if (left.type != right.type) return DfaUnknownValue
        if (left is DfaConstValue && right is DfaConstValue) {
            return when (op) {
                EqualityOp.EQ -> valueFactory.createBoolValue(left == right)
                EqualityOp.EXCLEQ -> valueFactory.createBoolValue(left != right)
                else -> DfaUnknownValue
            }
        }
        return null
    }

    private fun valueFromOp(op: BinaryOperator, left: DfaValue, right: DfaValue): DfaValue {
        if (left.type != right.type) return DfaUnknownValue
        val value = valueFromConstValue(op, left, right)
        if (value != null) {
            return value
        }

        val leftRange = LongRangeSet.fromDfaValue(left) ?: return DfaUnknownValue
        val rightRange = LongRangeSet.fromDfaValue(right) ?: return DfaUnknownValue
        return valueFactory.createRange(leftRange.binOpFromToken(op, rightRange))
    }

    private fun visitAssignmentBinOp(expr: RsBinaryExpr, state: DfaMemoryState, node: CFGNode, visitorState: NodeVisitorState) {
        val variable = variable(expr.left, state)
        val value = valueFromExpr(expr.right, state)
        val nextNode = node.nextNode ?: return
        visitorState.addNode(nextNode)
        mergeState(state.plus(variable, value), nextNode)
    }

    private fun variable(expr: RsExpr, state: DfaMemoryState): Variable {
        val variable = expr.toVariable()
        if (variable == null || variable !in state) error("Couldn't find variable '${variable?.text}'")
        return variable
    }

    private fun visitDummyNode(node: CFGNode, visitorState: NodeVisitorState) {
        val nextNode = visitorState.getNode(node.index + 1)
        val expr = nextNode.data.element
        when (expr) {
            is RsLoopExpr -> visitLoop(expr, node, nextNode, visitorState)
            is RsWhileExpr -> visitWhile(expr, node, nextNode, visitorState)
            is RsForExpr -> visitFor(expr, node, nextNode, visitorState)
            else -> updateNextState(node, visitorState)
        }
    }

    private fun visitLoop(expr: RsLoopExpr, dummyNode: CFGNode, loopNode: CFGNode, visitorState: NodeVisitorState) {
        updateNextState(dummyNode, visitorState)
        lineVisit(visitorState, dummyNode.index)
    }

    private fun visitWhile(whileExpr: RsWhileExpr, dummyNode: CFGNode, whileNode: CFGNode, visitorState: NodeVisitorState) {
        updateNextState(dummyNode, visitorState)
        lineVisit(visitorState, whileNode.index)

        val expr = whileExpr.condition?.expr
        if (expr != null) {
            val state = getStateWithCheck(whileNode.index)
            val value = tryEvaluateExpr(expr, state)
            val reachable = DfaReachableBranch.fromThreeState(value.threeState)
            instructions += BinOpInstruction(reachable, expr)
        }
        // TODO: check expr and visit block
        updateNextState(whileNode, visitorState)
    }

    //TODO: check expr return
    private fun visitFor(expr: RsForExpr, dummyNode: CFGNode, forNode: CFGNode, visitorState: NodeVisitorState) {
        updateNextState(dummyNode, visitorState)
        lineVisit(visitorState, forNode.index)
        // TODO: check expr and visit block
        updateNextState(forNode, visitorState)
    }
}

private class NodeVisitorState(block: RsBlock) {
    //    private val visited = hashSetOf<CFGNode>()
    private val cfg = buildFor(block)
    private val table = cfg.buildLocalIndex()
    private val queue = Queue<CFGNode>(2)
//    val unvisitedElements: Set<RsElement> get() = table.entries.filter { it.value.firstOrNull { node -> node in visited } == null }.map { it.key }.toSet()

    init {
        queue.addLast(cfg.entry)
    }

    fun nextNode(): CFGNode? {
        if (queue.isEmpty) return null
        val node = queue.pullFirst()
//        visited += node
        return node
    }

    fun addNode(node: CFGNode): Unit = queue.addLast(node)

    fun getNode(index: Int): CFGNode = cfg.graph.getNode(index)
    fun getNodeFromElement(element: RsElement): CFGNode = table[element]?.firstOrNull()
        ?: error("couldn't find node for '${element.text}'")
}

val CFGNode.outgoingNodes: Sequence<CFGNode> get() = generateSequence(this.firstOutEdge) { it.nextSourceEdge }.map { it.target }
val CFGNode.nextNode: CFGNode? get() = this.outgoingNodes.lastOrNull()
val CFGNode.hasSingleOut: Boolean get() = this.outgoingNodes.singleOrNull() != null
val CFGNode.firstOutNode: CFGNode? get() = this.firstOutEdge?.target
val CFGNode.firstInNode: CFGNode? get() = this.firstInEdge?.source

private fun RsExpr.toVariable(): Variable? {
    val expr = skipParenExprDown()
    return if (expr is RsPathExpr) expr.path.reference.resolve() as? Variable else null
}
