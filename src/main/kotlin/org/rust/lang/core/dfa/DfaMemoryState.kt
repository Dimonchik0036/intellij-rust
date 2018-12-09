/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import com.intellij.util.SmartFMap
import org.rust.lang.core.dfa.value.DfaConstValue
import org.rust.lang.core.dfa.value.DfaFactMapValue
import org.rust.lang.core.dfa.value.DfaUnknownValue
import org.rust.lang.core.dfa.value.DfaValue
import org.rust.lang.core.psi.RsPatBinding

typealias Variable = RsPatBinding

class DfaMemoryState private constructor(val variableStates: SmartFMap<Variable, DfaValue>) {
    fun plus(variable: Variable?, value: DfaValue = DfaUnknownValue): DfaMemoryState = if (variable != null) DfaMemoryState(variableStates.plus(variable, value)) else this
    fun plusAll(other: Map<Variable, DfaValue>): DfaMemoryState = DfaMemoryState(variableStates.plusAll(other))
    fun minus(variable: Variable?): DfaMemoryState = if (variable != null) DfaMemoryState(variableStates.minus(variable)) else this
    fun minusAll(other: Collection<Variable>): DfaMemoryState = DfaMemoryState(variableStates.minusAll(other))

    val invert: DfaMemoryState get() = EMPTY.plusAll(variableStates.asSequence().map { it.key to it.value.invert }.toMap())

    operator fun get(variable: Variable): DfaValue = variableStates[variable] ?: DfaUnknownValue

    operator fun contains(variable: Variable): Boolean = variable in variableStates

    val hasEmpty: Boolean get() = variableStates.any { it.value.isEmpty }

    fun unite(other: DfaMemoryState): DfaMemoryState = plusAll(other.variableStates.map { it.key to unite(it.key, it.value) }.toMap())
    fun uniteValue(variable: Variable?, value: DfaValue): DfaMemoryState = if (variable != null) plus(variable, unite(variable, value)) else this

    private fun unite(variable: Variable, value: DfaValue): DfaValue {
        val oldValue = get(variable)
        return if (oldValue !is DfaUnknownValue) oldValue.unite(value) else value
    }

    fun intersect(other: DfaMemoryState, withOverrideConstant: Boolean = true): DfaMemoryState = plusAll(other.variableStates.map { it.key to intersect(it.key, it.value, withOverrideConstant) }.toMap())
    fun intersectValue(variable: Variable?, value: DfaValue, withOverrideConstant: Boolean = true): DfaMemoryState = if (variable != null) plus(variable, intersect(variable, value)) else this

    private fun intersect(variable: Variable, value: DfaValue, withOverrideConstant: Boolean = true): DfaValue {
        val oldValue = get(variable)
        return when {
            value is DfaConstValue && oldValue is DfaUnknownValue -> value
            value is DfaConstValue && oldValue is DfaConstValue -> if (withOverrideConstant && value != oldValue) oldValue.factory.constFactory.dfaNothing else oldValue
            oldValue is DfaFactMapValue -> {
                val range = LongRangeSet.fromDfaValue(value)
                if (range != null) oldValue.withFact(DfaFactType.RANGE, oldValue[DfaFactType.RANGE]?.intersect(range))
                else DfaUnknownValue
            }
            else -> value
        }
    }

    override fun hashCode(): Int = variableStates.hashCode()
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is DfaMemoryState) return false

        return variableStates == other.variableStates
    }

    override fun toString(): String = variableStates.entries.joinToString(prefix = "<vars: ", postfix = ">") { "[${it.key.text}->${it.value}]" }

    companion object {
        val EMPTY = DfaMemoryState(SmartFMap.emptyMap())
    }
}
