/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

sealed class DfaBranchResult {
    object Ok : DfaBranchResult()
    object IdenticalState : DfaBranchResult()
}

sealed class Interrupt(val state: DfaMemoryState) : DfaBranchResult()

class Return(state: DfaMemoryState) : Interrupt(state)

sealed class WithLabel(val label: String, state: DfaMemoryState) : Interrupt(state)

class Continue(label: String, state: DfaMemoryState) : WithLabel(label, state)
class Break(label: String, state: DfaMemoryState) : WithLabel(label, state)
