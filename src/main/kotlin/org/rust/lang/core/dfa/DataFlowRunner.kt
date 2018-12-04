/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

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
import java.util.*

class DataFlowRunner(val function: RsFunction) {
    private val valueFactory: DfaValueFactory = DfaValueFactory()
    private val createMemoryState: DfaMemoryState get() = DfaMemoryState(valueFactory)
    private val instructions = hashSetOf<BinOpInstruction>()
    private val states = TIntObjectHashMap<DfaMemoryState>()
    private var myUnvisitedExpr: Sequence<CFGNode> = emptySequence()
    val unvisitedExpr: Sequence<RsElement> get() = myUnvisitedExpr.mapNotNull { it.data.element }
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
                    if (it.isTrueReachable && !it.isFalseReachable) {
                        trueSet += it
                    } else if (it.isFalseReachable && !it.isTrueReachable) {
                        falseSet += it
                    }
                }
            }
            return DfaResult(trueSet, falseSet)
        }

    fun analyze(): RunnerResult {
        try {
            val visitor = NodeVisitorState(function.block ?: return RunnerResult.NOT_APPLICABLE)
            lineVisit(visitor)
            myUnvisitedExpr = visitor.unvisitedNodes
            return RunnerResult.OK
        } catch (e: Exception) {
            return RunnerResult.NOT_APPLICABLE
        }
    }

    private fun initFunctionParameters() {
        val state = createMemoryState
        function.valueParameterList?.valueParameterList?.forEach {
            val element = it.pat as? RsPatIdent
            val binPat = element?.patBinding
            if (binPat != null) {
                state.setVarValue(binPat, valueFactory.createTypeValue(binPat.type))
            }
        }
        states.put(0, state)
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

    private fun updateNextState(node: CFGNode, visitorState: NodeVisitorState, nextNode: CFGNode? = node.firstOutNode) {
        if (nextNode == null) return
        visitorState.addNode(nextNode)
        mergeStates(node, nextNode)
    }

    private fun getStateWithCheck(index: Int): DfaMemoryState = states[index] ?: error("Index $index")

    private fun mergeStates(nodeFrom: CFGNode, nodeTo: CFGNode) {
        val currentState = states[nodeFrom.index] ?: return
        val nextState = states[nodeTo.index]
        if (nextState != null) {
            states.put(nodeTo.index, currentState.merge(nextState))
        } else {
            states.put(nodeTo.index, currentState)
        }
        resultState = states[nodeTo.index]
    }


    private fun visitAstNode(element: RsElement, node: CFGNode, visitorState: NodeVisitorState) = when (element) {
        is RsLetDecl -> visitLetDeclNode(node, element, visitorState)
        is RsBinaryExpr -> visitBinExpr(node, element, visitorState)
        else -> updateNextState(node, visitorState)
    }

    private fun visitBinExpr(node: CFGNode, expr: RsBinaryExpr, visitorState: NodeVisitorState) {
        val state = getStateWithCheck(node.index)
        when (expr.operatorType) {
            is AssignmentOp -> visitAssignmentBinOp(expr, state)
            is BoolOp -> {
                visitBoolBinExpr(expr, node, visitorState)
                return
            }
        }
        updateNextState(node, visitorState)
    }

    private fun visitBoolBinExpr(expr: RsBinaryExpr, node: CFGNode, visitorState: NodeVisitorState) {
        val condition = expr.skipParenExprUp().parent as? RsCondition
        val ifExpr = condition?.parent as? RsIfExpr
        if (ifExpr == null) {
            updateNextState(node, visitorState)
            return
        }
        val ifNode = visitorState.getNodeFromElement(ifExpr) ?: error("couldn't find node for '${ifExpr.text}'")
        val state = getStateWithCheck(node.index)
        val nodes = node.outgoingNodes
        val trueBranch = nodes.elementAt(1)
        val falseBranch = nodes.elementAt(0)

        var isTrueReachable = true
        var isFalseReachable = true

        val value = tryEvaluateExpr(expr, state)
        when (value) {
            ThreeState.YES -> {
                updateNextState(node, visitorState, trueBranch)
                isFalseReachable = false
            }
            ThreeState.NO -> {
                updateNextState(node, visitorState, falseBranch)
                isTrueReachable = false
            }
            ThreeState.UNSURE -> {
                visitBranch(trueBranch, state.copy, visitorState, ifNode.index)
                visitBranch(falseBranch, state, visitorState, ifNode.index)
                updateNextState(ifNode, visitorState)
            }
        }
        instructions += BinOpInstruction(isTrueReachable, isFalseReachable, expr)
    }

    private fun visitBranch(node: CFGNode, state: DfaMemoryState, visitorState: NodeVisitorState, endIndex: Int) {
        states.put(node.index, state)
        visitorState.addNode(node)
        lineVisit(visitorState, endIndex)
    }

    private fun tryEvaluateExpr(expr: RsExpr?, state: DfaMemoryState): ThreeState {
        val expr = expr?.skipParenExprDown() ?: return ThreeState.UNSURE
        return when (expr) {
            is RsLitExpr -> tryEvaluateLitExpr(expr)
            is RsUnaryExpr -> tryEvaluateUnaryExpr(expr, state)
            is RsBinaryExpr -> tryEvaluateBinExpr(expr, state)
            else -> ThreeState.UNSURE
        }
    }

    private fun tryEvaluateLitExpr(expr: RsLitExpr): ThreeState = ThreeState.fromBool((expr.kind as? RsLiteralKind.Boolean)?.value)

    private fun tryEvaluateUnaryExpr(expr: RsUnaryExpr, state: DfaMemoryState): ThreeState {
        if (expr.excl == null) return ThreeState.UNSURE
        return tryEvaluateExpr(expr.expr, state).not
    }

    private fun tryEvaluateBinExpr(expr: RsBinaryExpr, state: DfaMemoryState): ThreeState {
        val op = expr.operatorType
        val left = expr.left.skipParenExprDown()
        val right = expr.right?.skipParenExprDown() ?: return ThreeState.UNSURE
        return when (op) {
            is LogicOp -> tryEvaluateBinExprWithLogicOp(op, left, right, state)
            else -> tryEvaluateBinExprWithRange(expr, state)
        }
    }

    private fun tryEvaluateBinExprWithLogicOp(op: LogicOp, left: RsExpr, right: RsExpr, state: DfaMemoryState): ThreeState {
        val leftResult = tryEvaluateExpr(left, state)
        return when (op) {
            LogicOp.OR -> if (leftResult == ThreeState.YES) leftResult else leftResult.or(tryEvaluateExpr(right, state))
            LogicOp.AND -> if (leftResult == ThreeState.NO) leftResult else leftResult.and(tryEvaluateExpr(right, state))
        }
    }

    private fun tryEvaluateBinExprWithRange(expr: RsBinaryExpr, state: DfaMemoryState): ThreeState {
        val leftValue = valueFromExpr(expr.left, state)
        val rightValue = valueFromExpr(expr.right, state)

        val result = valueFromOp(expr.operatorType, leftValue, rightValue)
        if (result is DfaConstValue) {
            return ThreeState.fromBool(result.value as? Boolean)
        }

        val leftRange = LongRangeSet.fromDfaValue(leftValue) ?: return ThreeState.UNSURE
        val rightRange = LongRangeSet.fromDfaValue(rightValue) ?: return ThreeState.UNSURE
        val resultRange = LongRangeSet.fromDfaValue(result) ?: return ThreeState.UNSURE
        return when {
            resultRange.isEmpty -> ThreeState.NO
            leftRange in resultRange && rightRange in resultRange -> ThreeState.YES
            else -> ThreeState.UNSURE
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
                state.setVarValue(bin, value)
            }
            is RsPatTup -> {
                val state = getStateWithCheck(node.index)
                val values = valuesFromTuple(element.expr, state)
                pat.patList.forEachIndexed { index, it ->
                    state.setVarValue((it as? RsPatIdent)?.patBinding, values.getOrElse(index) { DfaUnknownValue })
                }
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
            else -> DfaUnknownValue
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

    private fun valueFromOp(op: BinaryOperator, left: DfaValue, right: DfaValue): DfaValue {
        if (left.type != right.type) return DfaUnknownValue
        if (left is DfaConstValue && right is DfaConstValue) {
            return when (op) {
                EqualityOp.EQ -> valueFactory.createBoolValue(left == right)
                EqualityOp.EXCLEQ -> valueFactory.createBoolValue(left != right)
                else -> DfaUnknownValue
            }
        }

        val leftRange = LongRangeSet.fromDfaValue(left) ?: return DfaUnknownValue
        val rightRange = LongRangeSet.fromDfaValue(right) ?: return DfaUnknownValue
        return valueFactory.createFactValue(DfaFactType.RANGE, leftRange.binOpFromToken(op, rightRange))
    }

    private fun visitAssignmentBinOp(expr: RsBinaryExpr, state: DfaMemoryState) {
        val variable = variable(expr.left, state)
        val value = valueFromExpr(expr.right, state)
        state.setVarValue(variable, value)
    }

    private fun variable(expr: RsExpr, state: DfaMemoryState): Variable {
        val variable = expr.toVariable()
        if (variable == null || variable !in state) error("Couldn't find variable '${variable?.text}'")
        return variable
    }

    private fun visitDummyNode(node: CFGNode, visitorState: NodeVisitorState) {
        val nextNode = visitorState.getNode(node.index + 1)
        val element = nextNode.data.element
        when (element) {
            is RsLoopExpr -> visitLoop(node, nextNode, visitorState)
            is RsWhileExpr -> visitWhile(node, nextNode, visitorState)
            is RsForExpr -> visitFor(node, nextNode, visitorState)
            else -> updateNextState(node, visitorState)
        }
    }

    private fun visitLoop(dummyNode: CFGNode, loopNode: CFGNode, visitorState: NodeVisitorState) {
        updateNextState(dummyNode, visitorState)
        lineVisit(visitorState, dummyNode.index)
    }

    private fun visitWhile(dummyNode: CFGNode, whileNode: CFGNode, visitorState: NodeVisitorState) {
        updateNextState(dummyNode, visitorState)
        val inNode = whileNode.firstInNode ?: error("couldn't")
        lineVisit(visitorState, inNode.index)
        mergeStates(inNode, whileNode)
        // TODO: check expr and visit block
        updateNextState(whileNode, visitorState)
    }

    //TODO: check expr return
    private fun visitFor(dummyNode: CFGNode, forNode: CFGNode, visitorState: NodeVisitorState) {
        updateNextState(dummyNode, visitorState)
        val inNode = forNode.firstInNode ?: error("couldn't")
        lineVisit(visitorState, inNode.index)
        mergeStates(inNode, forNode)
        // TODO: check expr and visit block
        updateNextState(forNode, visitorState)
    }
}

private class NodeVisitorState(block: RsBlock) {
    private val visited = hashSetOf<CFGNode>()
    private val cfg = buildFor(block)
    private val table = cfg.buildLocalIndex()
    private val queue = Queue<CFGNode>(2)
    val unvisitedNodes: Sequence<CFGNode> get() = cfg.graph.depthFirstTraversal(cfg.entry).filter { it !in visited }

    init {
        queue.addLast(cfg.entry)
    }

    fun nextNode(): CFGNode? {
        val node = queue.pullFirst() ?: return null
        visited += node
        return node
    }

    fun addNode(node: CFGNode): Unit = queue.addLast(node)

    fun getNode(index: Int): CFGNode = cfg.graph.getNode(index)
    fun getNodeFromElement(element: RsElement): CFGNode? = table[element]?.first()
}

val CFGNode.outgoingNodes: Sequence<CFGNode> get() = generateSequence(this.firstOutEdge) { it.nextSourceEdge }.map { it.target }
val CFGNode.hasSingleOut: Boolean get() = this.outgoingNodes.singleOrNull() != null
val CFGNode.firstOutNode: CFGNode? get() = this.firstOutEdge?.target
val CFGNode.firstInNode: CFGNode? get() = this.firstInEdge?.source

private fun createSequence(block: RsBlock): Sequence<CFGNode> {
    val cfg = buildFor(block)
    val visited = hashSetOf<CFGNode>()
    val resultSequence = mutableListOf<CFGNode>()
    val queue = PriorityQueue<CFGNode>(compareBy { it.index })
    queue.add(cfg.entry)

    while (queue.isNotEmpty()) {
        val node = queue.poll()
        if (!visited.add(node)) continue
        resultSequence.add(node)
        val outgoingNodes = cfg.graph.outgoingEdges(node).map { it.target }
        queue.addAll(outgoingNodes)
    }
    return resultSequence.asSequence()
}

private fun RsExpr.toVariable(): RsPatBinding? = if (this is RsPathExpr) this.path.reference.resolve() as? RsPatBinding else null
