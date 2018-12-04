/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa.value

import org.rust.lang.core.psi.RsPatIdent
import org.rust.lang.core.types.ty.Ty
import org.rust.lang.core.types.type

class DfaVariableValue(factory: DfaValueFactory, override val type: Ty) : DfaValue(factory)

class DfaVariableFactory(val factory: DfaValueFactory) {
    fun createVariableValue(variable: RsPatIdent): DfaVariableValue = createVariableValue(variable.patBinding.type)

    fun createVariableValue(type: Ty): DfaVariableValue = DfaVariableValue(factory, type)
}
