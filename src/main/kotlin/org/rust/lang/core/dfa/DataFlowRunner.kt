/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import org.jetbrains.coverage.gnu.trove.TObjectIntHashMap
import org.rust.lang.core.psi.RsBlock

abstract class DataFlowRunner {
    /***
     * The index is the CFGNode index
     */
    private val states = TObjectIntHashMap<DfaMemoryState>()

    fun analyzeFunction(
        block: RsBlock
    ): RunnerResult {
        TODO()
    }


//    val nodes: Sequence<CFGNode>
//
//    init {
//        val visited = mutableSetOf<CFGNode>()
//        val resultSequence = mutableListOf<CFGNode>()
//        val queue = PriorityQueue<CFGNode>(compareBy { it.index })
//        queue.add(cfg.entry)
//
//        while (queue.isNotEmpty()) {
//            val node = queue.poll()
//            if (!visited.add(node)) continue
//            resultSequence.add(node)
//            val outgoingNodes = cfg.graph.outgoingEdges(node).map { it.target }
//            queue.addAll(outgoingNodes)
//        }
//        nodes = resultSequence.asSequence()
//    }
}
