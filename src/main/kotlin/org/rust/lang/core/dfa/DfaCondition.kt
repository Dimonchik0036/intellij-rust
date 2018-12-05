/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import com.intellij.util.ThreeState

data class DfaCondition(val threeState: ThreeState, val trueState: DfaMemoryState = DfaMemoryState(), val falseState: DfaMemoryState = DfaMemoryState()) {
    fun or(other: DfaCondition): DfaCondition {
        val threeState = this.threeState.or(other.threeState)
        val trueState = this.trueState.unite(other.trueState)
        val falseState = this.falseState.intersect(other.falseState)
        return fromStates(threeState, trueState, falseState)
    }

    fun and(other: DfaCondition): DfaCondition {
        val threeState = this.threeState.and(other.threeState)
        val trueState = this.trueState.intersect(other.trueState)
        val falseState = this.falseState.unite(other.falseState)
        return fromStates(threeState, trueState, falseState)
    }

    companion object {
        val UNSURE = DfaCondition(ThreeState.UNSURE)
        fun fromStates(threeState: ThreeState, trueState: DfaMemoryState, falseState: DfaMemoryState): DfaCondition = DfaCondition(when {
            trueState.hasEmpty -> ThreeState.NO
            falseState.hasEmpty -> ThreeState.YES
            else -> threeState
        }, trueState, falseState)
    }
}

fun ThreeState.and(rhs: ThreeState): ThreeState = when {
    this == ThreeState.NO || rhs == ThreeState.NO -> ThreeState.NO
    this == ThreeState.UNSURE || rhs == ThreeState.UNSURE -> ThreeState.UNSURE
    else -> ThreeState.YES
}

fun ThreeState.or(rhs: ThreeState): ThreeState = when {
    this == ThreeState.YES || rhs == ThreeState.YES -> ThreeState.YES
    this == ThreeState.UNSURE || rhs == ThreeState.UNSURE -> ThreeState.UNSURE
    else -> ThreeState.NO
}

val ThreeState.not: ThreeState
    get() = when (this) {
        ThreeState.YES -> ThreeState.NO
        ThreeState.NO -> ThreeState.YES
        else -> ThreeState.UNSURE
    }

fun fromBool(boolean: Boolean?): ThreeState = when (boolean) {
    true -> ThreeState.YES
    false -> ThreeState.NO
    else -> ThreeState.UNSURE
}
