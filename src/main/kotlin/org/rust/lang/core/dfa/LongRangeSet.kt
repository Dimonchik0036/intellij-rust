/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import org.rust.lang.core.psi.ext.ArithmeticOp
import org.rust.lang.core.psi.ext.BoolOp
import org.rust.lang.core.psi.ext.ComparisonOp
import org.rust.lang.core.psi.ext.EqualityOp
import org.rust.lang.core.types.ty.Ty
import org.rust.lang.core.types.ty.TyInteger
import java.util.*
import java.util.stream.LongStream

sealed class LongRangeSet(val type: TyInteger) {
    val minPossible by lazy { type.toRange().from }
    val maxPossible by lazy { type.toRange().to }
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
    fun isEmpty(): Boolean = this == empty()

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
    open fun union(other: LongRangeSet): LongRangeSet {
        if (other.isEmpty() || other === this) return this
        return if (other.contains(this)) other
        else {
            val range = type.toRange()
            range.subtract(range.subtract(this).intersect(range.subtract(other)))
        }
    }

    /**
     * @return a minimal value contained in the set
     * @throws NoSuchElementException if set is empty
     */
    abstract fun min(): Long

    /**
     * @return a maximal value contained in the set
     * @throws NoSuchElementException if set is empty
     */
    abstract fun max(): Long

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
     * @return new set or null if relation is unsupported
     */
    fun fromRelation(relation: BoolOp?): LongRangeSet? {
        if (isEmpty() || relation == null) return null
        when (relation) {
            EqualityOp.EQ -> return this
            EqualityOp.EXCLEQ -> {
                val min = min()
                return if (min == max()) all().without(min) else all()
            }
            ComparisonOp.GT -> {
                val min = min()
                return if (min == maxPossible) empty() else range(min + 1, maxPossible)
            }
            ComparisonOp.GTEQ -> return range(min(), maxPossible)
            ComparisonOp.LTEQ -> return range(minPossible, max())
            ComparisonOp.LT -> {
                val max = max()
                return if (max == minPossible) empty() else range(minPossible, max - 1)
            }
            else -> return null
        }
    }

    /**
     * Performs a supported binary operation from op (defined in [ArithmeticOp]).
     *
     * @param op  a op which corresponds to the operation
     * @param right  a right-hand operand
     * @return the resulting LongRangeSet which covers possible results of the operation (probably including some more elements);
     * or null if the supplied op is not supported.
     */
    fun binOpFromToken(op: ArithmeticOp?, right: LongRangeSet): LongRangeSet? = when (op) {
        ArithmeticOp.ADD -> plus(right)
        ArithmeticOp.SUB -> minus(right)
        ArithmeticOp.REM -> mod(right)
        ArithmeticOp.DIV -> div(right)
        else -> null
    }


    /**
     * Returns a range which represents all the possible values after applying [Math.abs] or [Math.abs]
     * to the values from this set
     *
     * @return a new range
     */
    abstract fun abs(): LongRangeSet

    /**
     * Returns a range which represents all the possible values after applying unary minus
     * to the values from this set
     *
     * @return a new range
     */
    abstract fun negate(): LongRangeSet


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
        return plus(other.negate())
    }

    /**
     * Returns a range which represents all the possible values after applying `x / y` operation for
     * all `x` from this set and for all `y` from the divisor set. The resulting set may contain
     * some more values. Division by zero yields an empty set of possible results.
     *
     * @param divisor divisor set to divide by
     * treatment of `MIN_VALUE/-1` division; other division results do not depend on the resulting type.
     * @return a new range
     */
    fun div(divisor: LongRangeSet): LongRangeSet {
        if (divisor.isEmpty() || divisor == point(0)) return empty()
        val left = splitAtZero(asRanges())
        val right = splitAtZero(longArrayOf(divisor.min(), divisor.max()))
        var result = empty()
        var i = 0
        while (i < left.size) {
            var j = 0
            while (j < right.size) {
                result = result.union(divide(left[i], left[i + 1], right[j], right[j + 1]))
                j += 2
            }
            i += 2
        }
        return result
    }

    fun divide(dividendMin: Long, dividendMax: Long, divisorMin: Long, divisorMax: Long): LongRangeSet {
        var divisorMin = divisorMin
        if (divisorMin == 0L) {
            if (divisorMax == 0L) return empty()
            divisorMin = 1
        }
        if (dividendMin >= 0) {
            return if (divisorMin > 0) range(dividendMin / divisorMax, dividendMax / divisorMin)
            else range(dividendMax / divisorMax, dividendMin / divisorMin)
        }
        if (divisorMin > 0) {
            return range(dividendMin / divisorMin, dividendMax / divisorMax)
        }
        return if (dividendMin == minPossible && divisorMax == -1L) {
            // MIN_VALUE/-1 = MIN_VALUE
            point(minPossible)
                .union(
                    if (divisorMin == -1L) empty() else range(
                        dividendMin / divisorMin,
                        dividendMin / (divisorMax - 1)
                    )
                )
                .union(
                    if (dividendMax == minPossible) empty() else range(
                        dividendMax / divisorMin,
                        (dividendMin + 1) / divisorMax
                    )
                )
        } else range(dividendMax / divisorMin, dividendMin / divisorMax)
    }

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
    abstract fun stream(): LongStream

    abstract fun asRanges(): LongArray
}

/**
 * Creates a set containing single value which is equivalent to supplied boxed constant (if its type is supported)
 *
 * @param `value` constant to create a set from
 * @param `type` type of constant
 * @return new LongRangeSet or null if constant type is unsupported
 */
fun fromConstant(value: Long, type: TyInteger = TyInteger.DEFAULT): LongRangeSet = point(value, type)

class Empty(type: TyInteger) : LongRangeSet(type) {
    override fun subtract(other: LongRangeSet): LongRangeSet = this

    override fun intersect(other: LongRangeSet): LongRangeSet = this

    override fun union(other: LongRangeSet): LongRangeSet = other

    override fun min(): Long = throw NoSuchElementException()

    override fun max(): Long = throw NoSuchElementException()

    override fun intersects(other: LongRangeSet): Boolean = false

    override fun contains(value: Long): Boolean = false

    override fun contains(other: LongRangeSet): Boolean = other.isEmpty()

    override fun abs(): LongRangeSet = this

    override fun negate(): LongRangeSet = this

    override fun plus(other: LongRangeSet): LongRangeSet = this

    override fun mod(divisor: LongRangeSet): LongRangeSet = empty()

    override fun stream(): LongStream = LongStream.empty()

    override fun asRanges(): LongArray = LongArray(0)

    override fun hashCode(): Int = 2154231

    override fun equals(other: Any?): Boolean = other === this

    override fun toString(): String {
        return "{}"
    }
}

object RsEmptyRange {
    val i8 = Empty(TyInteger.I8)
    val u8 = Empty(TyInteger.U8)
    val i16 = Empty(TyInteger.I16)
    val u16 = Empty(TyInteger.U16)
    val i32 = Empty(TyInteger.I32)
    val u32 = Empty(TyInteger.U32)
    val i64 = Empty(TyInteger.I64)
    //    val u64
    //    val i128
    //    val u128
    val iSize = Empty(TyInteger.ISize)
    //    val uSize
}

class Point(val value: Long, type: TyInteger) : LongRangeSet(type) {
    override fun subtract(other: LongRangeSet): LongRangeSet = if (other.contains(value)) empty() else this

    override fun intersect(other: LongRangeSet): LongRangeSet = if (other.contains(value)) this else empty()

    override fun min(): Long = value

    override fun max(): Long = value

    override fun intersects(other: LongRangeSet): Boolean = other.contains(value)

    override fun contains(value: Long): Boolean = this.value == value

    override fun contains(other: LongRangeSet): Boolean = other.isEmpty() || equals(other)

    override fun abs(): LongRangeSet = if (value >= 0 || value == minPossible) this else point(-value)

    override fun negate(): LongRangeSet = if (value == minPossible) this else point(-value)

    override fun plus(other: LongRangeSet): LongRangeSet {
        if (other.isEmpty()) return other
        if (other is Point) {
            return point(checkOverflow(value + other.value))
        }
        return other.plus(this)
    }

    override fun mod(divisor: LongRangeSet): LongRangeSet {
        var divisor = divisor
        if (divisor.isEmpty() || divisor == point(0)) return empty()
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
        val max = Math.max(0, Math.max(Math.abs(divisor.min()), Math.abs(divisor.max())) - 1)
        return if (value < 0) {
            range(Math.max(value, -max), 0).union(addend)
        } else {
            // 10 % [-4..7] is [0..6], but 10 % [-30..30] is [0..10]
            range(0, Math.min(value, max)).union(addend)
        }
    }

    override fun stream(): LongStream = LongStream.of(value)

    override fun asRanges(): LongArray = longArrayOf(value, value)

    override fun hashCode(): Int = value.hashCode()

    override fun equals(other: Any?): Boolean = if (other === this) true else other is Point && value == other.value

    override fun toString(): String = "{$value}"
}

class Range(val from: Long, val to: Long, type: TyInteger) : LongRangeSet(type) {
    init {
        if (from >= to) throw IllegalArgumentException("$from >= $to")
    }

    override fun subtract(other: LongRangeSet): LongRangeSet {
        if (other.isEmpty()) return this
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
            if (result.isEmpty()) return result
            i += 2
        }
        return result
    }

    override fun intersect(other: LongRangeSet): LongRangeSet {
        if (other === this) return this
        if (other.isEmpty()) return other
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
            val res = intersect(range(ranges[i], ranges[i + 1])).asRanges()
            System.arraycopy(res, 0, result, index, res.size)
            index += res.size
            i += 2
        }
        return fromRanges(result, index)
    }

    override fun min(): Long = from

    override fun max(): Long = to

    override fun intersects(other: LongRangeSet): Boolean =
        if (other.isEmpty()) false else (other as? RangeSet)?.intersects(this)
            ?: (this.to >= other.min() && this.from <= other.max())

    override fun contains(value: Long): Boolean = value in from..to

    override fun contains(other: LongRangeSet): Boolean = other.isEmpty() || other.min() >= from && other.max() <= to

    override fun abs(): LongRangeSet {
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

    override fun negate(): LongRangeSet {
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
        if (other.isEmpty()) return other
        if (equals(type.toRange())) return this
        if (other is Point || other is Range || other is RangeSet && other.ranges.size > 6) {
            return plus(from, to, other.min(), other.max())
        }
        val ranges = other.asRanges()
        var result = empty()
        var i = 0
        while (i < ranges.size) {
            result = result.union(plus(this.from, this.to, ranges[i], ranges[i + 1]))
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
        if (to - from + 1 >= range.max()) return range
        return if (to < from) set(longArrayOf(minPossible, to, from, maxPossible)) else range(from, to)
    }

    override fun mod(divisor: LongRangeSet): LongRangeSet {
        if (divisor.isEmpty() || divisor == point(0)) return empty()
        if (divisor is Point && divisor.value == RsRanges.MIN_VALUE) {
            return if (contains(RsRanges.MIN_VALUE)) this.subtract(divisor).union(point(0)) else this
        }
        if (divisor.contains(RsRanges.MIN_VALUE)) {
            return possibleMod()
        }
        val min = divisor.min()
        val max = divisor.max()
        val maxDivisor = Math.max(Math.abs(min), Math.abs(max))
        val minDivisor = if (min > 0) min else if (max < 0) Math.abs(max) else 0
        return if (!intersects(range(RsRanges.MIN_VALUE, -minDivisor)) && !intersects(range(minDivisor, RsRanges.MAX_VALUE))) {
            this
        } else possibleMod().intersect(range(-maxDivisor + 1, maxDivisor - 1))
    }

    private fun possibleMod(): LongRangeSet {
        if (contains(0)) return this
        return if (min() > 0) range(0, max()) else range(min(), 0)
    }

    override fun stream(): LongStream = LongStream.rangeClosed(from, to)

    override fun asRanges(): LongArray = longArrayOf(from, to)

    override fun hashCode(): Int = from.hashCode() * 1337 + to.hashCode()

    override fun equals(other: Any?): Boolean =
        if (other === this) true else other is Range && from == other.from && to == other.to

    override fun toString(): String = "{${toString(from, to)}}"
}

private fun toString(from: Long, to: Long): String =
    if (from == to) from.toString() else "$from${if (to - from == 1L) ", " else ".."}$to"

object RsRanges {
    const val MAX_VALUE: Long = Long.MAX_VALUE
    const val MIN_VALUE: Long = Long.MIN_VALUE

    val i8 = Range(Byte.MIN_VALUE.toLong(), Byte.MAX_VALUE.toLong(), TyInteger.I8)
    val u8 = Range(0, 255, TyInteger.U8)
    val i16 = Range(Short.MIN_VALUE.toLong(), Short.MAX_VALUE.toLong(), TyInteger.I16)
    val u16 = Range(0, 65_535, TyInteger.U16)
    val i32 = Range(Int.MIN_VALUE.toLong(), Int.MAX_VALUE.toLong(), TyInteger.I32)
    val u32 = Range(0, 4_294_967_295, TyInteger.U32)
    val i64 = Range(MIN_VALUE, MAX_VALUE, TyInteger.I64)
    //    val u64
    //    val i128
    //    val u128
    val iSize = Range(MIN_VALUE, MAX_VALUE, TyInteger.ISize)
    //    val uSize
}

class RangeSet(val ranges: LongArray, type: TyInteger) : LongRangeSet(type) {
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
        if (other.isEmpty()) return this
        if (other === this) return empty()
        val result = LongArray(ranges.size + other.asRanges().size)
        var index = 0
        var i = 0
        while (i < ranges.size) {
            val res = range(ranges[i], ranges[i + 1]).subtract(other)
            val ranges = res.asRanges()
            System.arraycopy(ranges, 0, result, index, ranges.size)
            index += ranges.size
            i += 2
        }
        return fromRanges(result, index)
    }

    override fun intersect(other: LongRangeSet): LongRangeSet {
        if (other === this) return this
        if (other.isEmpty()) return other
        return if (other is Point || other is Range) other.intersect(this) else subtract(all().subtract(other))
    }

    override fun min(): Long = ranges.first()

    override fun max(): Long = ranges.last()

    override fun intersects(other: LongRangeSet): Boolean {
        if (other.isEmpty()) return false
        if (other is Point) {
            return contains(other.value)
        }
        val otherRanges = other.asRanges()
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
        if (other.isEmpty() || other === this) return true
        if (other is Point) {
            return contains(other.value)
        }
        var result = other
        var i = 0
        while (i < ranges.size) {
            result = result.subtract(range(ranges[i], ranges[i + 1]))
            if (result.isEmpty()) return true
            i += 2
        }
        return false
    }

    override fun abs(): LongRangeSet {
        var result = all()
        var i = 0
        while (i < ranges.size) {
            result = result.subtract(range(ranges[i], ranges[i + 1]).abs())
            i += 2
        }
        return all().subtract(result)
    }

    override fun negate(): LongRangeSet {
        var result = all()
        var i = 0
        while (i < ranges.size) {
            result = result.subtract(range(ranges[i], ranges[i + 1]).negate())
            i += 2
        }
        return all().subtract(result)
    }

    override fun plus(other: LongRangeSet): LongRangeSet {
        if (ranges.size > 6) {
            return range(min(), max()).plus(other)
        }
        var result = empty()
        var i = 0
        while (i < ranges.size) {
            result = result.union(range(ranges[i], ranges[i + 1]).plus(other))
            i += 2
        }
        return result
    }

    override fun mod(divisor: LongRangeSet): LongRangeSet {
        if (divisor.isEmpty()) return empty()
        var result = empty()
        var i = 0
        while (i < ranges.size) {
            result = result.union(range(ranges[i], ranges[i + 1]).mod(divisor))
            i += 2
        }
        return result
    }

    override fun stream(): LongStream = LongStream.range(0, (ranges.size / 2).toLong()).mapToObj {
        LongStream.rangeClosed(
            ranges[(it * 2).toInt()],
            ranges[(it * 2 + 1).toInt()]
        )
    }.reduce(LongStream::concat).orElseGet(LongStream::empty)

    override fun asRanges(): LongArray = ranges

    override fun hashCode(): Int = Arrays.hashCode(ranges)

    override fun equals(other: Any?): Boolean =
        if (other === this) true else other is RangeSet && ranges.contentEquals(other.ranges)

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

fun fromRanges(ranges: LongArray, bound: Int, type: TyInteger): LongRangeSet = when (bound) {
    0 -> empty(type)
    2 -> range(ranges[0], ranges[1], type)
    else -> RangeSet(ranges.copyOfRange(0, bound), type)
}

fun fromType(type: Ty?): LongRangeSet? = (type as? TyInteger)?.toRange()

private fun LongRangeSet.fromRanges(ranges: LongArray, bound: Int): LongRangeSet = fromRanges(ranges, bound, type)

private fun TyInteger.toRange(): Range = when (this) {
    TyInteger.I8 -> RsRanges.i8
    TyInteger.U8 -> RsRanges.u8
    TyInteger.I16 -> RsRanges.i16
    TyInteger.U16 -> RsRanges.u16
    TyInteger.I32 -> RsRanges.i32
    TyInteger.U32 -> RsRanges.u32
    TyInteger.I64 -> RsRanges.i64
    //    TyInteger.U64 -> RsRanges.u64
    //    TyInteger.I128 -> RsRanges.i128
    //    TyInteger.U128 -> RsRanges.u128
    TyInteger.ISize -> RsRanges.iSize
    //    TyInteger.USize -> RsRanges.uSize
    else -> RsRanges.i64
}

private fun TyInteger.size(): Long = when (this) {
    TyInteger.I8 -> 8
    TyInteger.U8 -> 8
    TyInteger.I16 -> 16
    TyInteger.U16 -> 16
    TyInteger.I32 -> 32
    TyInteger.U32 -> 32
    TyInteger.I64 -> 64
//    TyInteger.U64 -> 64
//    TyInteger.I128 -> 128
//    TyInteger.U128 -> 128
    TyInteger.ISize -> 64
//    TyInteger.USize -> 64
    else -> 64
}

fun point(value: Long, type: TyInteger = TyInteger.DEFAULT): LongRangeSet = Point(value, type)
private fun LongRangeSet.point(value: Long): LongRangeSet = point(value, type)

fun set(ranges: LongArray, type: TyInteger = TyInteger.DEFAULT): LongRangeSet = RangeSet(ranges, type)
private fun LongRangeSet.set(ranges: LongArray): LongRangeSet = RangeSet(ranges, type)

fun all(): LongRangeSet = RsRanges.i64

fun empty(type: TyInteger = TyInteger.DEFAULT): LongRangeSet = when (type) {
    TyInteger.I8 -> RsEmptyRange.i8
    TyInteger.U8 -> RsEmptyRange.i8
    TyInteger.I16 -> RsEmptyRange.i16
    TyInteger.U16 -> RsEmptyRange.u16
    TyInteger.I32 -> RsEmptyRange.i32
    TyInteger.U32 -> RsEmptyRange.u32
    TyInteger.I64 -> RsEmptyRange.i64
    //    TyInteger.U64 -> RsEmptyRange.u64
    //    TyInteger.I128 -> RsEmptyRange.i128
    //    TyInteger.U128 -> RsEmptyRange.u128
    TyInteger.ISize -> RsEmptyRange.iSize
    //    TyInteger.USize -> RsEmptyRange.uSize
    else -> RsEmptyRange.i64
}

private fun LongRangeSet.empty(): LongRangeSet = empty(type)

/**
 * Creates a new set which contains all the numbers between from (inclusive) and to (inclusive)
 *
 * @param from lower bound
 * @param to upper bound (must be greater or equal to `from`)
 * @return a new LongRangeSet
 */
fun range(from: Long, to: Long, type: TyInteger = TyInteger.DEFAULT): LongRangeSet =
    if (from == to) Point(from, type) else Range(from, to, type)

private fun LongRangeSet.range(from: Long, to: Long): LongRangeSet = range(from, to, type)

private fun LongRangeSet.checkOverflow(value: Long) : Long {
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
