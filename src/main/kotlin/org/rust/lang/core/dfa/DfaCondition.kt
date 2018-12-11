/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import com.intellij.util.ThreeState

data class DfaCondition(val threeState: ThreeState, val trueState: DfaMemoryState = DfaMemoryState.EMPTY, val falseState: DfaMemoryState = DfaMemoryState.EMPTY) {
    val sure: Boolean = threeState.sure

    fun or(other: DfaCondition): DfaCondition {
        var trueState = this.trueState.unite(other.trueState)
        var falseState = this.falseState.intersect(other.falseState)
        val emptyKeys = falseState.emptyKeys

        trueState = trueState.minusAll(emptyKeys)
        falseState = falseState.minusAll(emptyKeys)
        val threeState = if (emptyKeys.isNotEmpty() && falseState.empty) ThreeState.YES else this.threeState.or(other.threeState)
        return DfaCondition(threeState, trueState, falseState)
    }

    fun and(other: DfaCondition): DfaCondition {
        var trueState = this.trueState.intersect(other.trueState)
        var falseState = this.falseState.unite(other.falseState)
        val emptyKeys = trueState.emptyKeys

        trueState = trueState.minusAll(emptyKeys)
        falseState = falseState.minusAll(emptyKeys)
        val threeState = if (emptyKeys.isNotEmpty() && trueState.empty) ThreeState.NO else this.threeState.and(other.threeState)
        return DfaCondition(threeState, trueState, falseState)
    }

    companion object {
        val UNSURE = DfaCondition(ThreeState.UNSURE)
    }
}

val ThreeState.sure: Boolean get() = this != ThreeState.UNSURE

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
