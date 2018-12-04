/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import com.google.common.math.LongMath.checkedMultiply
import org.rust.lang.core.dfa.value.DfaConstValue
import org.rust.lang.core.dfa.value.DfaFactMapValue
import org.rust.lang.core.dfa.value.DfaValue
import org.rust.lang.core.psi.ext.*
import org.rust.lang.core.types.ty.Ty
import org.rust.lang.core.types.ty.TyInteger
import java.util.*
import java.util.stream.LongStream
import kotlin.NoSuchElementException
import kotlin.math.max
import kotlin.math.min

sealed class LongRangeSet(val type: TyInteger, val overflow: Boolean) {
    val minPossible = type.minPossible()
    val maxPossible = type.maxPossible()

    /**
     * Subtracts given set from the current
     *
     * @param other set to subtract
     * @return a new set
     */
    abstract fun subtract(other: LongRangeSet): LongRangeSet

    fun without(value: Long): LongRangeSet = subtract(point(value))

    /**
     * @return true if set is empty
     */
    val isEmpty: Boolean get() = this is Empty

    /**
     * @return true if set is empty
     */
    val isUnknown: Boolean get() = this is Unknown

    /**
     * @return true if type is greater than Long
     */
    val isLarge: Boolean get() = isLargeOnTop || isLargeBelow

    val isLargeOnTop: Boolean
        get() = when (type) {
            TyInteger.U64 -> true
            TyInteger.I128 -> true
            TyInteger.U128 -> true
            TyInteger.USize -> true
            else -> false
        }

    val isLargeBelow: Boolean
        get() = when (type) {
            TyInteger.I128 -> true
            else -> false
        }

    /**
     * Intersects current set with other
     *
     * @param other other set to intersect with
     * @return a new set
     */
    abstract fun intersect(other: LongRangeSet): LongRangeSet

    /**
     * Merge current set with other
     *
     * @param other other set to merge with
     * @return a new set
     */
    open fun unite(other: LongRangeSet): LongRangeSet {
        if (other.isUnknown) return other
        if (other.isEmpty || other === this) return this
        return if (other.contains(this)) other
        else {
            val range = allRange
            range.subtract(range.subtract(this).intersect(range.subtract(other)))
        }
    }

    /**
     * @return a minimal value contained in the set
     * @throws NoSuchElementException if set is empty
     */
    abstract val min: Long

    /**
     * @return a maximal value contained in the set
     * @throws NoSuchElementException if set is empty
     */
    abstract val max: Long

    /**
     * Checks if current set and other set have at least one common element
     *
     * @param other other set to check whether intersection exists
     * @return true if this set intersects other set
     */
    abstract fun intersects(other: LongRangeSet): Boolean

    /**
     * Checks whether current set contains given value
     *
     * @param value value to find
     * @return true if current set contains given value
     */
    abstract operator fun contains(value: Long): Boolean

    /**
     * Checks whether current set contains all the values from other set
     *
     * @param other a sub-set candidate
     * @return true if current set contains all the values from other
     */
    abstract operator fun contains(other: LongRangeSet): Boolean

    /**
     * Creates a new set which contains all possible values satisfying given predicate regarding the current set.
     *
     *
     * E.g. if current set is {0..10} and relation is "GT", then result will be {1..RsRange.MAX_VALUE} (values which can be greater than
     * some value from the current set)
     *
     * @param relation relation to be applied to current set
     * @return new set or UNKNOWN if relation is unsupported
     */
    fun fromRelation(relation: BoolOp): LongRangeSet {
        when (relation) {
            EqualityOp.EQ -> return this
            EqualityOp.EXCLEQ -> return allRange.subtract(this)
        }
        return if (isUnknown || isEmpty) unknown()
        else when (relation) {
            ComparisonOp.GT -> {
                val min = min
                if (min == maxPossible)
                    if (isLargeOnTop) unknown() else empty()
                else range(min + 1, maxPossible)
            }
            ComparisonOp.GTEQ -> range(min, maxPossible)
            ComparisonOp.LTEQ -> range(minPossible, max)
            ComparisonOp.LT -> {
                val max = max
                if (max == minPossible)
                    if (isLargeBelow) unknown() else empty()
                else range(minPossible, max - 1)
            }
            else -> unknown()
        }
    }

    /**
     * Performs a supported binary operation from op (defined in [BinaryOperator]).
     *
     * @param op  a op which corresponds to the operation
     * @param right  a right-hand operand
     * @return the resulting LongRangeSet which covers possible results of the operation (probably including some more elements);
     * or UNKNOWN if the supplied op is not supported.
     */
    fun binOpFromToken(op: BinaryOperator, right: LongRangeSet): LongRangeSet = when (op) {
        ArithmeticOp.ADD, ArithmeticAssignmentOp.PLUSEQ -> plus(right)
        ArithmeticOp.SUB, ArithmeticAssignmentOp.MINUSEQ -> minus(right)
        ArithmeticOp.REM, ArithmeticAssignmentOp.REMEQ -> mod(right)
        ArithmeticOp.MUL, ArithmeticAssignmentOp.MULEQ -> times(right)
        EqualityOp.EQ -> intersect(right)
        EqualityOp.EXCLEQ -> subtract(right).unite(right.subtract(this))
        is ComparisonOp -> intersect(right.fromRelation(op))
        else -> unknown()
    }


    /**
     * Returns a range which represents all the possible values after applying [Math.abs] or [Math.abs]
     * to the values from this set
     *
     * @return a new range
     */
    abstract val abs: LongRangeSet

    /**
     * Returns a range which represents all the possible values after applying unary minus
     * to the values from this set
     *
     * @return a new range
     */
    abstract val minus: LongRangeSet


    /**
     * Returns a range which represents all the possible values after performing an addition between any value from this range
     * and any value from other range. The resulting range may contain some more values which cannot be produced by addition.
     * Guaranteed to be commutative.
     *
     * @return a new range
     */
    abstract fun plus(other: LongRangeSet): LongRangeSet

    /**
     * Returns a range which represents all the possible values after performing an addition between any value from this range
     * and any value from other range. The resulting range may contain some more values which cannot be produced by addition.
     *
     * @return a new range
     */
    fun minus(other: LongRangeSet): LongRangeSet {
        return plus(other.minus)
    }

    fun times(other: LongRangeSet): LongRangeSet {
        if (isUnknown || other.isUnknown) return unknown()
        if (other.isEmpty) return empty()
        val left = splitAtZero(asRanges)
        val right = splitAtZero(longArrayOf(other.min, other.max))
        var result = empty()

        for (leftIndex in left.indices step 2) {
            for (rightIndex in right.indices step 2) {
                val part = times(left[leftIndex], left[leftIndex + 1], right[rightIndex], right[rightIndex + 1])
                if (part.isUnknown) return part
                result = result.unite(part)
            }
        }

        return result
    }

    private fun times(lhsMin: Long, lhsMax: Long, rhsMin: Long, rhsMax: Long): LongRangeSet = try {
        val leftLeft = checkedMultiply(lhsMin, rhsMin)
        val leftRight = checkedMultiply(lhsMin, rhsMax)
        val rightLeft = checkedMultiply(lhsMax, rhsMin)
        val rightRight = checkedMultiply(lhsMax, rhsMax)

        val leftValue = min(minOf(leftLeft, leftRight, rightLeft), rightRight)
        val rightValue = max(maxOf(leftLeft, leftRight, rightLeft), rightRight)

        val resultLeftValue = checkOverflow(leftValue)
        val resultRightValue = checkOverflow(rightValue)
        range(resultLeftValue, resultRightValue, (resultLeftValue != leftValue) or (resultRightValue != rightValue))
    } catch (e: ArithmeticException) {
        Unknown.UnknownWithOverflow
    }

    protected fun checkOverflow(value: Long): Long = if (value < minPossible) minPossible else if (value > maxPossible) maxPossible else value

    /**
     * Returns a range which represents all the possible values after applying `x % y` operation for
     * all `x` from this set and for all `y` from the divisor set. The resulting set may contain
     * some more values. Division by zero yields an empty set of possible results.
     *
     * @param divisor divisor set to divide by
     * @return a new range
     */
    abstract fun mod(divisor: LongRangeSet): LongRangeSet

    /**
     * Returns a stream of all values from this range. Be careful: could be huge
     *
     * @return a new stream
     */
    abstract val stream: LongStream

    abstract val asRanges: LongArray

    protected fun fromRanges(ranges: LongArray, bound: Int): LongRangeSet = fromRanges(ranges, bound, type, overflow)

    protected fun point(value: Long): LongRangeSet = point(value, type, overflow)
    protected fun point(value: Long, overflow: Boolean): LongRangeSet = point(value, type, overflow)

    protected fun set(ranges: LongArray): LongRangeSet = set(ranges, type, overflow)
    protected fun set(ranges: LongArray, overflow: Boolean): LongRangeSet = set(ranges, type, overflow)

    protected fun range(from: Long, to: Long): LongRangeSet = range(from, to, type, overflow)
    protected fun range(from: Long, to: Long, overflow: Boolean): LongRangeSet = range(from, to, type, overflow)

    protected val allRange get() = all(type)
    protected fun empty(): LongRangeSet = empty(overflow)
    protected fun unknown(): LongRangeSet = unknown(overflow)

    //    protected fun typeEquals(other: LongRangeSet) = type == other.type
    companion object {
        /**
         * Creates a set containing single value which is equivalent to supplied boxed constant (if its type is supported)
         *
         * @param `value` constant to create a set from
         * @param `type` type of constant
         * @return new LongRangeSet or null if constant type is unsupported
         */
        fun fromConstant(value: Any, type: Ty): LongRangeSet? = if (value is Long && type is TyInteger) point(value, type, false) else null

        fun fromDfaValue(value: DfaValue): LongRangeSet? {
            if (value is DfaFactMapValue) return value[DfaFactType.RANGE]
            if (value is DfaConstValue) return fromConstant(value.value, value.type)
            return null
        }

        fun fromRanges(ranges: LongArray, bound: Int, type: TyInteger, overflow: Boolean): LongRangeSet = when (bound) {
            0 -> empty(overflow)
            2 -> range(ranges[0], ranges[1], type, overflow)
            else -> set(ranges.copyOfRange(0, bound), type, overflow)
        }

        fun fromType(type: Ty?): LongRangeSet? = (type as? TyInteger)?.toRange()

        /**
         * Creates a new set which contains all the numbers between from (inclusive) and to (inclusive)
         *
         * @param from lower bound
         * @param to upper bound (must be greater or equal to `from`)
         * @return a new LongRangeSet
         */
        fun range(from: Long, to: Long, type: TyInteger, overflow: Boolean): LongRangeSet =
            if (from == to) Point(from, type, overflow) else Range(from, to, type, overflow)

        fun point(value: Long, type: TyInteger, overflow: Boolean): LongRangeSet = Point(value, type, overflow)

        fun set(ranges: LongArray, type: TyInteger, overflow: Boolean): LongRangeSet = RangeSet(ranges, type, overflow)

        fun empty(overflow: Boolean): LongRangeSet =
            if (overflow) Empty.EmptyWithOverflow else Empty.EmptyWithoutOverflow

        fun unknown(overflow: Boolean): LongRangeSet =
            if (overflow) Unknown.UnknownWithOverflow else Unknown.UnknownWithoutOverflow

        fun all(type: TyInteger): LongRangeSet = type.toRange()
    }
}

sealed class Empty(overflow: Boolean) : LongRangeSet(TyInteger.DEFAULT, overflow) {
    override fun subtract(other: LongRangeSet): LongRangeSet = this

    override fun intersect(other: LongRangeSet): LongRangeSet = this

    override fun unite(other: LongRangeSet): LongRangeSet = other

    override val min: Long get() = throw NoSuchElementException()

    override val max: Long get() = throw NoSuchElementException()

    override fun intersects(other: LongRangeSet): Boolean = false

    override fun contains(value: Long): Boolean = false

    override fun contains(other: LongRangeSet): Boolean = other.isEmpty

    override val abs: LongRangeSet get() = this

    override val minus: LongRangeSet get() = this

    override fun plus(other: LongRangeSet): LongRangeSet = this

    override fun mod(divisor: LongRangeSet): LongRangeSet = this

    override val stream: LongStream get() = LongStream.empty()

    override val asRanges: LongArray get() = LongArray(0)

    override fun hashCode(): Int = 2154231

    override fun equals(other: Any?): Boolean = other === this

    override fun toString(): String = "{}"

    object EmptyWithOverflow : Empty(true)
    object EmptyWithoutOverflow : Empty(false)
}

sealed class Unknown(overflow: Boolean) : LongRangeSet(TyInteger.DEFAULT, overflow) {
    override fun subtract(other: LongRangeSet): LongRangeSet = this

    override fun intersect(other: LongRangeSet): LongRangeSet = this

    override fun unite(other: LongRangeSet): LongRangeSet = this

    override val min: Long get() = throw NoSuchElementException()

    override val max: Long get() = throw NoSuchElementException()

    override fun intersects(other: LongRangeSet): Boolean = throw UnsupportedOperationException()

    override fun contains(value: Long): Boolean = throw UnsupportedOperationException()

    override fun contains(other: LongRangeSet): Boolean = throw UnsupportedOperationException()

    override val abs: LongRangeSet get() = this

    override val minus: LongRangeSet get() = this

    override fun plus(other: LongRangeSet): LongRangeSet = this

    override fun mod(divisor: LongRangeSet): LongRangeSet = this

    override val stream: LongStream get() = throw UnsupportedOperationException()

    override val asRanges: LongArray get() = throw UnsupportedOperationException()

    override fun hashCode(): Int = 2154230

    override fun equals(other: Any?): Boolean = other === this

    override fun toString(): String = "{?}"

    object UnknownWithOverflow : Unknown(true)
    object UnknownWithoutOverflow : Unknown(false)
}

class Point(val value: Long, type: TyInteger, overflow: Boolean) : LongRangeSet(type, overflow) {
    override fun subtract(other: LongRangeSet): LongRangeSet = when {
        other.isUnknown -> other
        other.contains(this) -> empty()
        else -> this
    }

    override fun intersect(other: LongRangeSet): LongRangeSet = if (other.isUnknown) other else if (other.contains(value)) this else empty()

    override val min: Long get() = value

    override val max: Long get() = value

    override fun intersects(other: LongRangeSet): Boolean = other.contains(value)

    override fun contains(value: Long): Boolean = this.value == value

    override fun contains(other: LongRangeSet): Boolean = other.isEmpty || equals(other)

    override val abs: LongRangeSet get() = if (value >= 0) this else if (value == minPossible) empty(true) else point(-value)

    override val minus: LongRangeSet get() = if (value == minPossible) empty(true) else point(-value)

    override fun plus(other: LongRangeSet): LongRangeSet {
        if (other.isEmpty) return other
        if (other is Point) {
            return point(checkOverflow(value + other.value))
        }
        return other.plus(this)
    }

    override fun mod(divisor: LongRangeSet): LongRangeSet {
        if (divisor.isUnknown) return unknown()
        if (divisor.isEmpty || divisor is Point && divisor.value == 0L) return empty()
        var divisor = divisor
        if (value == 0L) return this
        if (divisor is Point) {
            return point(value % divisor.value)
        }
        if (value != minPossible) {
            val abs = Math.abs(value)
            if (!divisor.intersects(range(-abs, abs))) {
                // like 10 % [15..20] == 10 regardless on exact divisor value
                return this
            }
        }
        var addend = empty()
        if (divisor.contains(minPossible)) {
            divisor = divisor.subtract(point(minPossible))
            addend = point(value)
        }
        val max = Math.max(0, Math.max(Math.abs(divisor.min), Math.abs(divisor.max)) - 1)
        return if (value < 0) {
            range(Math.max(value, -max), 0).unite(addend)
        } else {
            // 10 % [-4..7] is [0..6], but 10 % [-30..30] is [0..10]
            range(0, Math.min(value, max)).unite(addend)
        }
    }

    override val stream: LongStream get() = LongStream.of(value)

    override val asRanges: LongArray get() = longArrayOf(value, value)

    override fun hashCode(): Int = value.hashCode()

    override fun equals(other: Any?): Boolean =
        if (other === this) true
        else other is Point && value == other.value && overflow == other.overflow

    override fun toString(): String = "{$value}"
}

class Range(val from: Long, val to: Long, type: TyInteger, overflow: Boolean) : LongRangeSet(type, overflow) {
    init {
        if (from >= to) throw IllegalArgumentException("$from >= $to")
    }

    override fun subtract(other: LongRangeSet): LongRangeSet {
        if (other.isEmpty) return this
        if (other === this) return empty()
        if (other is Point) {
            val value = other.value
            if (value < this.from || value > this.to) return this
            if (value == this.from) return range(this.from + 1, this.to)
            return if (value == this.to) range(this.from, this.to - 1) else set(
                longArrayOf(
                    this.from,
                    value - 1,
                    value + 1,
                    this.to
                )
            )
        }
        if (other is Range) {
            val from = other.from
            val to = other.to
            if (to < this.from || from > this.to) return this
            if (from <= this.from && to >= this.to) return empty()
            if (from > this.from && to < this.to) {
                return set(longArrayOf(this.from, from - 1, to + 1, this.to))
            }
            if (from <= this.from) {
                return range(to + 1, this.to)
            }
            if (to >= this.to) {
                return range(this.from, from - 1)
            }
            throw InternalError("Impossible: $this:$other")
        }
        val ranges = (other as RangeSet).ranges
        var result: LongRangeSet = this
        var i = 0
        while (i < ranges.size) {
            result = result.subtract(range(ranges[i], ranges[i + 1]))
            if (result.isEmpty) return result
            i += 2
        }
        return result
    }

    override fun intersect(other: LongRangeSet): LongRangeSet {
        if (other === this) return this
        if (other.isEmpty) return other
        if (other is Point) {
            return other.intersect(this)
        }
        if (other is Range) {
            var from = other.from
            var to = other.to
            if (from <= this.from && to >= this.to) return this
            if (from >= this.from && to <= this.to) return other
            if (from < this.from) {
                from = this.from
            }
            if (to > this.to) {
                to = this.to
            }
            return if (from <= to) range(from, to) else empty()
        }
        val ranges = (other as RangeSet).ranges
        val result = LongArray(ranges.size)
        var index = 0
        var i = 0
        while (i < ranges.size) {
            val res = intersect(range(ranges[i], ranges[i + 1])).asRanges
            System.arraycopy(res, 0, result, index, res.size)
            index += res.size
            i += 2
        }
        return fromRanges(result, index)
    }

    override val min: Long get() = from

    override val max: Long get() = to

    override fun intersects(other: LongRangeSet): Boolean =
        if (other.isEmpty) false
        else (other as? RangeSet)?.intersects(this) ?: (this.to >= other.min && this.from <= other.max)

    override fun contains(value: Long): Boolean = value in from..to

    override fun contains(other: LongRangeSet): Boolean = other.isEmpty || other.min >= from && other.max <= to

    override val abs: LongRangeSet
        get() {
            if (from >= 0) return this
            val minValue = minPossible
            var low = from
            var hi = to
            if (low <= minValue) {
                low = minValue + 1
            }
            if (this.to <= 0) {
                hi = -low
                low = -this.to
            } else {
                hi = Math.max(-low, hi)
                low = 0
            }
            return if (this.from <= minValue) set(longArrayOf(minValue, minValue, low, hi)) else range(low, hi)
        }

    override val minus: LongRangeSet
        get() {
            val minValue = minPossible
            return if (this.from <= minValue) if (this.to >= maxPossible) type.toRange() else set(
                longArrayOf(
                    minValue,
                    minValue,
                    -this.to,
                    -(minValue + 1)
                )
            ) else range(-this.to, -this.from)
        }

    override fun plus(other: LongRangeSet): LongRangeSet {
        if (other.isEmpty) return other
        if (equals(type.toRange())) return this
        if (other is Point || other is Range || other is RangeSet && other.ranges.size > 6) {
            return plus(from, to, other.min, other.max)
        }
        val ranges = other.asRanges
        var result = empty()
        var i = 0
        while (i < ranges.size) {
            result = result.unite(plus(this.from, this.to, ranges[i], ranges[i + 1]))
            i += 2
        }
        return result
    }

    fun plus(from1: Long, to1: Long, from2: Long, to2: Long): LongRangeSet {
        val len1 = to1 - from1 // may overflow
        val len2 = to2 - from2 // may overflow
        /* total length more than 2^32*/
        if ((len1 < 0 || len2 < 0) && len1 + len2 + 1 >= 0) return type.toRange()
        val from = from1 + from2
        val to = to1 + to2
        val range = type.toRange()
        // TODO ?
        if (to - from + 1 >= range.max) return range
        return if (to < from) set(longArrayOf(minPossible, to, from, maxPossible)) else range(from, to)
    }

    override fun mod(divisor: LongRangeSet): LongRangeSet {
        if (divisor.isEmpty || divisor == point(0)) return empty()
        if (divisor is Point && divisor.value == Long.MIN_VALUE) {
            return if (contains(Long.MIN_VALUE)) this.subtract(divisor).unite(point(0)) else this
        }
        if (divisor.contains(Long.MIN_VALUE)) {
            return possibleMod()
        }
        val min = divisor.min
        val max = divisor.max
        val maxDivisor = Math.max(Math.abs(min), Math.abs(max))
        val minDivisor = if (min > 0) min else if (max < 0) Math.abs(max) else 0
        return if (!intersects(range(Long.MIN_VALUE, -minDivisor)) && !intersects(
                range(
                    minDivisor,
                    Long.MAX_VALUE
                )
            )
        ) {
            this
        } else possibleMod().intersect(range(-maxDivisor + 1, maxDivisor - 1))
    }

    private fun possibleMod(): LongRangeSet {
        if (contains(0)) return this
        return if (min > 0) range(0, max) else range(min, 0)
    }

    override val stream: LongStream get() = LongStream.rangeClosed(from, to)

    override val asRanges: LongArray get() = longArrayOf(from, to)

    override fun hashCode(): Int = from.hashCode() * 1337 + to.hashCode()

    override fun equals(other: Any?): Boolean =
        if (other === this) true else other is Range && overflow == other.overflow && from == other.from && to == other.to

    override fun toString(): String = "{${toString(from, to)}}"

    private object RangeValuesHolder {
        val I8 = rangeFromType(TyInteger.I8)
        val U8 = rangeFromType(TyInteger.U8)
        val I16 = rangeFromType(TyInteger.I16)
        val U16 = rangeFromType(TyInteger.U16)
        val I32 = rangeFromType(TyInteger.I32)
        val U32 = rangeFromType(TyInteger.U32)
        val I64 = rangeFromType(TyInteger.I64)
        val U64 = rangeFromType(TyInteger.U64)
        val I128 = rangeFromType(TyInteger.I128)
        val U128 = rangeFromType(TyInteger.U128)
        val ISize = rangeFromType(TyInteger.ISize)
        val USize = rangeFromType(TyInteger.USize)
    }

    companion object {
        val I8 get() = RangeValuesHolder.I8
        val U8 get() = RangeValuesHolder.U8
        val I16 get() = RangeValuesHolder.I16
        val U16 get() = RangeValuesHolder.U16
        val I32 get() = RangeValuesHolder.I32
        val U32 get() = RangeValuesHolder.U32
        val I64 get() = RangeValuesHolder.I64
        val U64 get() = RangeValuesHolder.U64
        val I128 get() = RangeValuesHolder.I128
        val U128 get() = RangeValuesHolder.U128
        val ISize get() = RangeValuesHolder.ISize
        val USize get() = RangeValuesHolder.USize
    }
}

private fun toString(from: Long, to: Long): String =
    if (from == to) from.toString() else "$from${if (to - from == 1L) ", " else ".."}$to"

fun TyInteger.minPossible(): Long =
    when (this) {
        is TyInteger.I8 -> Byte.MIN_VALUE.toLong()
        is TyInteger.U8 -> 0L
        is TyInteger.I16 -> Short.MIN_VALUE.toLong()
        is TyInteger.U16 -> 0L
        is TyInteger.I32 -> Int.MIN_VALUE.toLong()
        is TyInteger.U32 -> 0L
        is TyInteger.I64 -> Long.MIN_VALUE
        is TyInteger.U64 -> 0L
        is TyInteger.I128 -> Long.MIN_VALUE
        is TyInteger.U128 -> 0L
        is TyInteger.ISize -> TyInteger.I64.minPossible()
        is TyInteger.USize -> TyInteger.U64.minPossible()
    }

fun TyInteger.maxPossible(): Long =
    when (this) {
        is TyInteger.I8 -> Byte.MAX_VALUE.toLong()
        is TyInteger.U8 -> 255L
        is TyInteger.I16 -> Short.MAX_VALUE.toLong()
        is TyInteger.U16 -> 65_535L
        is TyInteger.I32 -> Int.MAX_VALUE.toLong()
        is TyInteger.U32 -> 4_294_967_295L
        is TyInteger.I64 -> Long.MAX_VALUE
        is TyInteger.U64 -> Long.MAX_VALUE
        is TyInteger.I128 -> Long.MAX_VALUE
        is TyInteger.U128 -> Long.MAX_VALUE
        is TyInteger.ISize -> TyInteger.I64.maxPossible()
        is TyInteger.USize -> TyInteger.U64.maxPossible()
    }

private fun rangeFromType(type: TyInteger): Range = with(type) { Range(minPossible(), maxPossible(), this, false) }

class RangeSet(val ranges: LongArray, type: TyInteger, overflow: Boolean) : LongRangeSet(type, overflow) {
    init {
        if (ranges.size < 4 || ranges.size % 2 != 0) {
            // 0 ranges = Empty; 1 range = Range
            throw IllegalArgumentException("Bad length: ${ranges.size} $ranges")
        }
        var i = 0
        while (i < ranges.size) {
            if (ranges[i + 1] < ranges[i]) throw IllegalArgumentException("Bad sub-range #${i / 2} $ranges")
            if (i > 0 && (ranges[i - 1] == Long.MAX_VALUE || 1 + ranges[i - 1] > ranges[i])) throw IllegalArgumentException(
                "Bad sub-ranges #${(i / 2 - 1)} and #${i / 2} $ranges"
            )
            i += 2
        }
    }

    override fun subtract(other: LongRangeSet): LongRangeSet {
        if (other.isEmpty) return this
        if (other === this) return empty()
        val result = LongArray(ranges.size + other.asRanges.size)
        var index = 0
        var i = 0
        while (i < ranges.size) {
            val res = range(ranges[i], ranges[i + 1]).subtract(other)
            val ranges = res.asRanges
            System.arraycopy(ranges, 0, result, index, ranges.size)
            index += ranges.size
            i += 2
        }
        return fromRanges(result, index)
    }

    override fun intersect(other: LongRangeSet): LongRangeSet {
        if (other === this) return this
        if (other.isEmpty) return other
        return if (other is Point || other is Range) other.intersect(this) else subtract(allRange.subtract(other))
    }

    override val min: Long
        get() = ranges.first()

    override val max: Long
        get() = ranges.last()

    override fun intersects(other: LongRangeSet): Boolean {
        if (other.isEmpty) return false
        if (other is Point) {
            return contains(other.value)
        }
        val otherRanges = other.asRanges
        var a = 0
        var b = 0
        while (true) {
            val aFrom = ranges[a]
            val aTo = ranges[a + 1]
            val bFrom = otherRanges[b]
            val bTo = otherRanges[b + 1]
            if (aFrom <= bTo && bFrom <= aTo) return true
            if (aFrom > bTo) {
                b += 2
                if (b >= otherRanges.size) return false
            } else {
                a += 2
                if (a >= ranges.size) return false
            }
        }
    }

    override fun contains(value: Long): Boolean {
        var i = 0
        while (i < ranges.size) {
            if (value >= ranges[i] && value <= ranges[i + 1]) {
                return true
            }
            i += 2
        }
        return false
    }

    override fun contains(other: LongRangeSet): Boolean {
        if (other.isEmpty || other === this) return true
        if (other is Point) {
            return contains(other.value)
        }
        var result = other
        var i = 0
        while (i < ranges.size) {
            result = result.subtract(range(ranges[i], ranges[i + 1]))
            if (result.isEmpty) return true
            i += 2
        }
        return false
    }

    override val abs: LongRangeSet
        get() {
            var result = allRange
            var i = 0
            while (i < ranges.size) {
                result = result.subtract(range(ranges[i], ranges[i + 1]).abs)
                i += 2
            }
            return allRange.subtract(result)
        }

    override val minus: LongRangeSet
        get() {
            var result = allRange
            var i = 0
            while (i < ranges.size) {
                result = result.subtract(range(ranges[i], ranges[i + 1]).minus)
                i += 2
            }
            return allRange.subtract(result)
        }

    override fun plus(other: LongRangeSet): LongRangeSet {
        if (ranges.size > 6) {
            return range(min, max).plus(other)
        }
        var result = empty()
        var i = 0
        while (i < ranges.size) {
            result = result.unite(range(ranges[i], ranges[i + 1]).plus(other))
            i += 2
        }
        return result
    }

    override fun mod(divisor: LongRangeSet): LongRangeSet {
        if (divisor.isEmpty) return empty()
        var result = empty()
        var i = 0
        while (i < ranges.size) {
            result = result.unite(range(ranges[i], ranges[i + 1]).mod(divisor))
            i += 2
        }
        return result
    }

    override val stream: LongStream
        get() = LongStream.range(0, (ranges.size / 2).toLong()).mapToObj {
            LongStream.rangeClosed(
                ranges[(it * 2).toInt()],
                ranges[(it * 2 + 1).toInt()]
            )
        }.reduce(LongStream::concat).orElseGet(LongStream::empty)

    override val asRanges: LongArray get() = ranges

    override fun hashCode(): Int = Arrays.hashCode(ranges)

    override fun equals(other: Any?): Boolean =
        if (other === this) true
        else other is RangeSet && overflow == other.overflow && ranges.contentEquals(other.ranges)

    override fun toString(): String {
        val sb = StringBuilder("{")
        var i = 0
        while (i < ranges.size) {
            if (i > 0) sb.append(", ")
            sb.append(toString(ranges[i], ranges[i + 1]))
            i += 2
        }
        sb.append("}")
        return sb.toString()
    }
}

private fun TyInteger.toRange(): Range = when (this) {
    TyInteger.I8 -> Range.I8
    TyInteger.U8 -> Range.U8
    TyInteger.I16 -> Range.I16
    TyInteger.U16 -> Range.U16
    TyInteger.I32 -> Range.I32
    TyInteger.U32 -> Range.U32
    TyInteger.I64 -> Range.I64
    TyInteger.U64 -> Range.U64
    TyInteger.I128 -> Range.I128
    TyInteger.U128 -> Range.U128
    TyInteger.ISize -> Range.ISize
    TyInteger.USize -> Range.USize
}

private fun TyInteger.size(): Long = when (this) {
    TyInteger.I8 -> 8
    TyInteger.U8 -> 8
    TyInteger.I16 -> 16
    TyInteger.U16 -> 16
    TyInteger.I32 -> 32
    TyInteger.U32 -> 32
    TyInteger.I64 -> 64
    TyInteger.U64 -> 64
    TyInteger.I128 -> 128
    TyInteger.U128 -> 128
    TyInteger.ISize -> 64
    TyInteger.USize -> 64
}

private fun LongRangeSet.checkOverflow(value: Long): Long {
    var value = value
    if (value > maxPossible) {
        value -= maxPossible
        value -= 1
        value += minPossible
    } else if (value > minPossible) {
        value -= minPossible
        value += 1
        value += maxPossible
    }
    return value
}

private fun splitAtZero(ranges: LongArray): LongArray {
    var i = 0
    while (i < ranges.size) {
        if (ranges[i] < 0 && ranges[i + 1] >= 0) {
            val result = LongArray(ranges.size + 2)
            System.arraycopy(ranges, 0, result, 0, i + 1)
            result[i + 1] = -1
            System.arraycopy(ranges, i + 1, result, i + 3, ranges.size - i - 1)
            return result
        }
        i += 2
    }
    return ranges
}
