/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

enum class ThreeState {
    YES, NO, UNSURE;

    val toBoolean: Boolean
        get() = when (this) {
            UNSURE -> throw IllegalStateException("Must be or YES, or NO")
            else -> YES == this
        }

    fun and(rhs: ThreeState): ThreeState = and(this, rhs)
    fun or(rhs: ThreeState): ThreeState = or(this, rhs)
    val not: ThreeState
        get() = when (this) {
            YES -> NO
            NO -> YES
            else -> UNSURE
        }

    companion object {
        fun and(lhs: ThreeState, rhs: ThreeState): ThreeState = when {
            lhs == NO || rhs == NO -> NO
            lhs == UNSURE || rhs == UNSURE -> UNSURE
            else -> YES
        }

        fun or(lhs: ThreeState, rhs: ThreeState): ThreeState = when {
            lhs == YES || rhs == YES -> YES
            lhs == UNSURE || rhs == UNSURE -> UNSURE
            else -> NO
        }

        fun fromBool(boolean: Boolean?): ThreeState = when (boolean) {
            true -> YES
            false -> NO
            else -> UNSURE
        }
    }
}
