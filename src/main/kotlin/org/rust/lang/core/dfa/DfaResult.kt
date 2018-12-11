/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import org.rust.lang.core.psi.RsExpr

data class DfaResult(val trueSet: Set<RsExpr>, val falseSet: Set<RsExpr>)
