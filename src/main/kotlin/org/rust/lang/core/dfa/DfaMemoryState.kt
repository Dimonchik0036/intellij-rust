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
typealias VariableState = DfaValue

class DfaMemoryState(val factory: DfaValueFactory, val variableStates: HashMap<Variable, VariableState> = hashMapOf()) {
    val copy: DfaMemoryState get() = DfaMemoryState(factory, variableStates.clone() as HashMap<Variable, VariableState>)

    fun setVarValue(variable: Variable?, value: VariableState = DfaUnknownValue) { if (variable != null) variableStates[variable] = value
    }

    fun getValue(variable: Variable): VariableState = variableStates[variable] ?: DfaUnknownValue

    fun contain(variable: Variable): Boolean = variableStates.containsKey(variable)

    fun merge(other: DfaMemoryState): DfaMemoryState {
        other.variableStates.forEach { variable, state -> apply(variable, state) }
        return this
    }

    fun apply(variable: Variable, value: VariableState) {
        val oldValue = getValue(variable)
        setVarValue(variable, oldValue.unite(value))
    }

    override fun hashCode(): Int = variableStates.hashCode()
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is DfaMemoryState) return false

        return variableStates == other.variableStates
    }

    override fun toString(): String = variableStates.entries.joinToString(prefix = "<vars: ", postfix = ">") { "[${it.key.text}->${it.value}]" }
}
