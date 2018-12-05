/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import org.rust.lang.core.dfa.value.*
import org.rust.lang.core.psi.RsPatBinding

typealias Variable = RsPatBinding

class DfaMemoryState(val factory: DfaValueFactory, val variableStates: HashMap<Variable, DfaValue> = hashMapOf()) {
    val copy: DfaMemoryState get() = DfaMemoryState(factory, variableStates.clone() as HashMap<Variable, DfaValue>)
    val empty: DfaMemoryState get() = DfaMemoryState(factory)

    fun setVarValue(variable: Variable?, value: DfaValue = DfaUnknownValue) {
        if (variable != null) variableStates[variable] = value
    }

    val invert: DfaMemoryState get() = DfaMemoryState(factory, (variableStates.clone() as HashMap<Variable, DfaValue>).asSequence().map { it.key to it.value.invert }.toMap(hashMapOf()))

    operator fun get(variable: Variable): DfaValue = variableStates[variable] ?: DfaUnknownValue

    operator fun contains(variable: Variable): Boolean = variableStates.containsKey(variable)

    fun unite(other: DfaMemoryState): DfaMemoryState {
        other.variableStates.forEach { variable, value -> unite(variable, value) }
        return this
    }

    fun unite(variable: Variable, value: DfaValue) {
        val oldValue = get(variable)
        if (oldValue !is DfaUnknownValue) setVarValue(variable, oldValue.unite(value))
        else setVarValue(variable, value)
    }

    fun subtract(other: DfaMemoryState): DfaMemoryState {
        val copy = copy
        other.variableStates.forEach { variable, value -> copy.subtract(variable, value) }
        return copy
    }

    fun subtract(variable: Variable, value: DfaValue) {
        val oldValue = get(variable)
        val newValue = when {
            value is DfaConstValue && oldValue is DfaUnknownValue -> value.invert
            value is DfaConstValue && oldValue is DfaConstValue -> if (value == oldValue) DfaUnknownValue else oldValue
            oldValue is DfaFactMapValue -> {
                val range = LongRangeSet.fromDfaValue(value)
                if (range != null) oldValue.withFact(DfaFactType.RANGE, oldValue[DfaFactType.RANGE]?.subtract(range))
                else DfaUnknownValue
            }
            else -> DfaUnknownValue
        }
        setVarValue(variable, newValue)
    }

    override fun hashCode(): Int = variableStates.hashCode()
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is DfaMemoryState) return false

        return variableStates == other.variableStates
    }

    override fun toString(): String = variableStates.entries.joinToString(prefix = "<vars: ", postfix = ">") { "[${it.key.text}->${it.value}]" }
}
