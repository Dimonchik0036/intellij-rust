/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

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
    private val instructions = mutableSetOf<BinOpInstruction>()
    private val states = TIntObjectHashMap<DfaMemoryState>()
    private var myUnvisitedExpr: Sequence<CFGNode> = emptySequence()
    val unvisitedExpr: Sequence<RsElement> get() = myUnvisitedExpr.mapNotNull { it.data.element }
    //for debug
    var resultState: DfaMemoryState? = null

    init {
        initFunctionParameters()
    }

    val constantConditionalExpression
        get(): Pair<Set<BinOpInstruction>, Set<BinOpInstruction>> {
            val trueSet = mutableSetOf<BinOpInstruction>()
            val falseSet = mutableSetOf<BinOpInstruction>()
            instructions.forEach {
                if (!it.isConst) {
                    if (it.isTrueReachable && !it.isFalseReachable) {
                        trueSet += it
                    } else if (it.isFalseReachable && !it.isTrueReachable) {
                        falseSet += it
                    }
                }
            }
            return Pair(trueSet, falseSet)
        }

    val analyze: RunnerResult
        get() {
            try {
                val visitor = NodeVisitor(function.block ?: return RunnerResult.NOT_APPLICABLE)
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
    private fun lineVisit(visitor: NodeVisitor, endIndex: Int = 1) {
        while (true) {
            val node = visitor.nextNode()
            if (node == null || node.index == endIndex) return
            with(node.data) {
                when {
                    this is CFGNodeData.AST && this.element != null -> visitAstNode(this.element, node, visitor)
                    this is CFGNodeData.Dummy -> visitDummyNode(node, visitor)
                    else -> updateNextState(node, visitor)
                }
            }
        }
    }

    private fun updateNextState(node: CFGNode, visitor: NodeVisitor, nextNode: CFGNode? = node.firstOutNode) {
        if (nextNode == null) return
        visitor.addNode(nextNode)
        mergeStates(node, nextNode)
    }

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


    private fun visitAstNode(element: RsElement, node: CFGNode, visitor: NodeVisitor) = when (element) {
        is RsLetDecl -> visitLetDeclNode(node, element, visitor)
        is RsBinaryExpr -> visitBinExpr(node, element, visitor)
        else -> updateNextState(node, visitor)
    }

    private fun visitBinExpr(node: CFGNode, expr: RsBinaryExpr, visitor: NodeVisitor) {
        val state = states[node.index] ?: error("Index ${node.index}")
        when (expr.operatorType) {
            is AssignmentOp -> visitAssignmentBinOp(expr, state)
            is BoolOp -> {
                visitBoolBinExpr(expr, node, visitor)
                return
            }
        }
        updateNextState(node, visitor)
    }

    private fun visitBoolBinExpr(expr: RsBinaryExpr, node: CFGNode, visitor: NodeVisitor) {
        val condition = expr.skipParenExprUp().parent as? RsCondition
        val ifExpr = condition?.parent as? RsIfExpr
        if (ifExpr == null) {
            updateNextState(node, visitor)
            return
        }
        val ifNode = visitor.getNodeFromElement(ifExpr) ?: error("couldn't find node for '${ifExpr.text}'")
        val state = states[node.index] ?: error("Index ${node.index}")
        val nodes = node.outgoingNodes
        val trueBranch = nodes.elementAt(1)
        val falseBranch = nodes.elementAt(0)

        var isTrueReachable = true
        var isFalseReachable = true

        val value = tryEvaluateExpr(expr, state)
        when (value) {
            ThreeState.YES -> {
                updateNextState(node, visitor, trueBranch)
                isFalseReachable = false
            }
            ThreeState.NO -> {
                updateNextState(node, visitor, falseBranch)
                isTrueReachable = false
            }
            ThreeState.UNSURE -> {
                visitBranch(trueBranch, state.copy, visitor, ifNode.index)
                visitBranch(falseBranch, state, visitor, ifNode.index)
                updateNextState(ifNode, visitor)
            }
        }
        instructions += BinOpInstruction(isTrueReachable, isFalseReachable, expr)
    }

    private fun visitBranch(node: CFGNode, state: DfaMemoryState, visitor: NodeVisitor, endIndex: Int) {
        states.put(node.index, state)
        visitor.addNode(node)
        lineVisit(visitor, endIndex)
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
        val resultRange = LongRangeSet.fromDfaValue(result) ?: return ThreeState.UNSURE
        return when {
            resultRange.isEmpty -> ThreeState.NO
            resultRange.contains(leftRange) -> ThreeState.YES
            else -> ThreeState.UNSURE
        }
    }


    private fun visitLetDeclNode(node: CFGNode, element: RsLetDecl, visitor: NodeVisitor) {
        val pat = (element.pat as? RsPatIdent)?.patBinding
        if (pat != null) {
            val expr = element.expr
            val state = states[node.index] ?: error("Index ${node.index}")
            val value = if (expr != null) valueFromExpr(expr, state) else valueFactory.createTypeValue(pat.type)
            state.setVarValue(pat, value)
        }

        updateNextState(node, visitor)
    }

    private fun getValueFromElement(element: RsPatBinding, state: DfaMemoryState): DfaValue = state.getValue(element)

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
        val variable = expr.toVariable ?: return DfaUnknownValue
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
        val variable = expr.toVariable
        if (variable == null || !state.contain(variable)) error("Couldn't find variable '${variable?.text}'")
        return variable
    }

    private fun visitDummyNode(node: CFGNode, visitor: NodeVisitor) {
        val nextNode = visitor.getNode(node.index + 1)
        val element = nextNode.data.element
        when (element) {
            is RsLoopExpr -> visitLoop(node, nextNode, visitor)
            is RsWhileExpr -> visitWhile(node, nextNode, visitor)
            is RsForExpr -> visitFor(node, nextNode, visitor)
            else -> updateNextState(node, visitor)
        }
    }

    private fun visitLoop(dummyNode: CFGNode, loopNode: CFGNode, visitor: NodeVisitor) {
        updateNextState(dummyNode, visitor)
        lineVisit(visitor, dummyNode.index)
    }

    private fun visitWhile(dummyNode: CFGNode, whileNode: CFGNode, visitor: NodeVisitor) {
        updateNextState(dummyNode, visitor)
        val inNode = whileNode.firstInNode ?: error("couldn't")
        lineVisit(visitor, inNode.index)
        mergeStates(inNode, whileNode)
        // TODO: check expr and visit block
        updateNextState(whileNode, visitor)
    }

    //TODO: check expr return
    private fun visitFor(dummyNode: CFGNode, forNode: CFGNode, visitor: NodeVisitor) {
        updateNextState(dummyNode, visitor)
        val inNode = forNode.firstInNode ?: error("couldn't")
        lineVisit(visitor, inNode.index)
        mergeStates(inNode, forNode)
        // TODO: check expr and visit block
        updateNextState(forNode, visitor)
    }
}

private class NodeVisitor(block: RsBlock) {
    private val visited = mutableSetOf<CFGNode>()
    private val cfg = buildFor(block)
    private val table = cfg.buildLocalIndex()
    private val queue = PriorityQueue<CFGNode>(compareBy { it.index })
    val unvisitedNodes: Sequence<CFGNode> get() = cfg.graph.depthFirstTraversal(cfg.entry).filter { it !in visited }

    init {
        queue.add(cfg.entry)
    }

    fun nextNode(): CFGNode? {
        val node = queue.poll() ?: return null
        visited.add(node)
        return node
    }

    fun addNode(node: CFGNode): Boolean = queue.add(node)
    fun addAllNodes(nodes: Collection<CFGNode>): Boolean = queue.addAll(nodes)

    fun getNode(index: Int): CFGNode = cfg.graph.getNode(index)
    fun getNodeFromElement(element: RsElement): CFGNode? = table[element]?.first()
}

val CFGNode.outgoingNodes: Sequence<CFGNode> get() = generateSequence(this.firstOutEdge) { it.nextSourceEdge }.map { it.target }
val CFGNode.hasSingleOut: Boolean get() = this.outgoingNodes.singleOrNull() != null
val CFGNode.firstOutNode: CFGNode? get() = this.firstOutEdge?.target
val CFGNode.firstInNode: CFGNode? get() = this.firstInEdge?.source

private fun createSequence(block: RsBlock): Sequence<CFGNode> {
    val cfg = buildFor(block)
    val visited = mutableSetOf<CFGNode>()
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

private val RsExpr.toVariable: RsPatBinding? get() = if (this is RsPathExpr) this.path.reference.resolve() as? RsPatBinding else null
