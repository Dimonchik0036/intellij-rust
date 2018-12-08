/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import com.intellij.util.ThreeState

data class DfaReachableBranch(val isTrueReachable: Boolean, val isFalseReachable: Boolean) {
    companion object {
        fun fromThreeState(state: ThreeState): DfaReachableBranch = when (state) {
            ThreeState.YES -> DfaReachableBranch(true, false)
            ThreeState.NO -> DfaReachableBranch(false, true)
            ThreeState.UNSURE -> DfaReachableBranch(true, true)
        }
    }
}
