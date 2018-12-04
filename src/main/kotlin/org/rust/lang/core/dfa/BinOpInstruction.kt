/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import org.rust.lang.core.psi.RsExpr
import org.rust.lang.core.psi.RsLitExpr

data class BinOpInstruction(val isTrueReachable: Boolean, val isFalseReachable: Boolean, val anchor: RsExpr) {
    val isConst = (anchor as? RsLitExpr)?.boolLiteral != null
}
