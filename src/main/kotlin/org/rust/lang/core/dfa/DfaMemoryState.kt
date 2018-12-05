/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import org.rust.lang.core.dfa.value.DfaUnknownValue
import org.rust.lang.core.dfa.value.DfaValue
import org.rust.lang.core.dfa.value.DfaValueFactory
import org.rust.lang.core.psi.RsPatBinding

typealias Variable = RsPatBinding

class DfaMemoryState(val factory: DfaValueFactory, val variableStates: HashMap<Variable, DfaValue> = hashMapOf()) {
    val copy: DfaMemoryState get() = DfaMemoryState(factory, variableStates.clone() as HashMap<Variable, DfaValue>)

    fun setVarValue(variable: Variable?, value: DfaValue = DfaUnknownValue) {
        if (variable != null) variableStates[variable] = value
    }

    val invert: DfaMemoryState get() = DfaMemoryState(factory, variableStates.asSequence().map { it.key to it.value.invert }.toMap(hashMapOf()))

    operator fun get(variable: Variable): DfaValue = variableStates[variable] ?: DfaUnknownValue

    operator fun contains(variable: Variable): Boolean = variableStates.containsKey(variable)

    fun merge(other: DfaMemoryState): DfaMemoryState {
        other.variableStates.forEach { variable, state ->
            val oldValue = get(variable)
            if (oldValue !is DfaUnknownValue) setVarValue(variable, oldValue.unite(state))
            else setVarValue(variable, state)
        }
        return this
    }

    override fun hashCode(): Int = variableStates.hashCode()
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is DfaMemoryState) return false

        return variableStates == other.variableStates
    }

    override fun toString(): String = variableStates.entries.joinToString(prefix = "<vars: ", postfix = ">") { "[${it.key.text}->${it.value}]" }
}
