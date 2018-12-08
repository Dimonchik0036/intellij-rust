/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa.value

import org.rust.lang.core.dfa.DfaFactType
import org.rust.lang.core.dfa.LongRangeSet
import org.rust.lang.core.psi.RsLitExpr
import org.rust.lang.core.psi.RsLiteralKind
import org.rust.lang.core.psi.kind
import org.rust.lang.core.types.ty.Ty
import org.rust.lang.core.types.ty.TyBool
import org.rust.lang.core.types.ty.TyInteger
import org.rust.lang.core.types.type

class DfaConstValue(factory: DfaValueFactory, val value: Any, override val type: Ty) : DfaValue(factory) {
    override val negated: DfaValue
        get() = when (this) {
            factory.constFactory.dfaTrue -> factory.constFactory.dfaFalse
            factory.constFactory.dfaFalse -> factory.constFactory.dfaTrue
            else -> DfaUnknownValue
        }

    override val invert: DfaValue
        get() = when (type) {
            is TyBool -> negated
            is TyInteger -> factory.createRange(LongRangeSet.fromDfaValue(this)?.invert)
            else -> DfaUnknownValue
        }

    override val minus: DfaValue
        get() = when {
            type is TyInteger && value is Long -> factory.constFactory.createFromValue(-value, type)
            else -> DfaUnknownValue
        }

    override val isEmpty: Boolean get() = this == factory.constFactory.dfaNothing

    override fun toString(): String = if (isEmpty) "{}" else "{$value}"
}

class DfaConstFactory(val factory: DfaValueFactory) {
    private val values = hashMapOf<Any, DfaConstValue>()
    val dfaTrue = DfaConstValue(factory, true, TyBool)
    val dfaFalse = DfaConstValue(factory, false, TyBool)
    val dfaNothing = DfaConstValue(factory, 0, TyBool)

    fun create(expr: RsLitExpr): DfaValue {
        val kind = expr.kind

        return when (kind) {
            is RsLiteralKind.Integer -> {
                val value = kind.offsets.value?.substring(kind.node.text)?.filter { it != '_' }?.toLongOrNull()
                    ?: return DfaUnknownValue
                factory.createFactValue(DfaFactType.RANGE, LongRangeSet.fromConstant(value, expr.type))
//                createFromValue(value, expr.type)
            }
            is RsLiteralKind.Boolean -> createFromValue(kind.value, TyBool)
            else -> DfaUnknownValue
        }
    }

    fun createFromValue(value: Any, type: Ty): DfaConstValue {
        if (value == true) return dfaTrue
        if (value == false) return dfaFalse
        return values.getOrPut(value) { DfaConstValue(factory, value, type) }
    }
}
