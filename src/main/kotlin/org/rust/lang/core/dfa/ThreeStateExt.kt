/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import com.intellij.util.ThreeState
import com.intellij.util.ThreeState.*


fun ThreeState.and(rhs: ThreeState): ThreeState = when {
    this == NO || rhs == NO -> NO
    this == UNSURE || rhs == UNSURE -> UNSURE
    else -> YES
}

fun ThreeState.or(rhs: ThreeState): ThreeState = when {
    this == YES || rhs == YES -> YES
    this == UNSURE || rhs == UNSURE -> UNSURE
    else -> NO
}

val ThreeState.not: ThreeState
    get() = when (this) {
        YES -> NO
        NO -> YES
        else -> UNSURE
    }

fun fromBool(boolean: Boolean?): ThreeState = when (boolean) {
    true -> YES
    false -> NO
    else -> UNSURE
}
