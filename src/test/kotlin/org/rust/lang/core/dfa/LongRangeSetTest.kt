/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import org.rust.RsTestBase
import org.rust.lang.core.dfa.LongRangeSet.Companion.all
import org.rust.lang.core.dfa.LongRangeSet.Companion.empty
import org.rust.lang.core.dfa.LongRangeSet.Companion.fromConstant
import org.rust.lang.core.dfa.LongRangeSet.Companion.point
import org.rust.lang.core.dfa.LongRangeSet.Companion.range
import org.rust.lang.core.dfa.LongRangeSet.Companion.unknown
import org.rust.lang.core.psi.ext.ComparisonOp
import org.rust.lang.core.psi.ext.EqualityOp
import org.rust.lang.core.types.ty.Ty
import org.rust.lang.core.types.ty.TyFloat
import org.rust.lang.core.types.ty.TyInteger
import org.rust.lang.core.types.ty.TyStr
import java.util.*
import kotlin.collections.set
import kotlin.streams.asSequence
import kotlin.test.assertFails

class LongRangeSetTest : RsTestBase() {
    fun `test long range set from string`() {
        checkSet("{}", setFromString(""))
        checkSet("{!}", setFromString("!"))
        checkSet("{?}", setFromString("?"))
        checkSet("{42}", setFromString("42"))
        checkSet("{42}", setFromString("42, 42"))
        checkSet("{42}", setFromString("42..42"))
        checkSet("{0..42}", setFromString("0..42"))
        checkSet("{-10..42}", setFromString("-10..42"))
        checkSet("{-10..-5}", setFromString("-10..-5"))
        checkSet("{-10..-1, 1..10}", setFromString("-10..-1, 1..10"))
        checkSet("{-10..-1, 1..10, 42}", setFromString("-10..-1, 1..10, 42"))
        assertFails { setFromString("a") }
        assertFails { setFromString(",1,42") }
    }

    fun `test to string`() {
        checkSet("{}", setFromString(""))
        checkSet("{!}", empty(true))
        TyInteger.VALUES.forEach {
            checkSet("{10}", point(10, it))
            checkSet("{10}", range(10, 10, it))
            checkSet("{10, 11}", range(10, 11, it))
            checkSet("{10..100}", range(10, 100, it))
            checkSet("{0..10, 12..15}", setFromString("0..10, 12..15", it))
        }
    }

    fun `test from type`() {
        assertNull(LongRangeSet.fromType(TyFloat.DEFAULT))
        assertNull(LongRangeSet.fromType(TyStr))
        checkSet("{-128..127}", fromType(TyInteger.I8))
        checkSet("{0..255}", fromType(TyInteger.U8))
        checkSet("{-32768..32767}", fromType(TyInteger.I16))
        checkSet("{0..65535}", fromType(TyInteger.U16))
        checkSet("{-2147483648..2147483647}", fromType(TyInteger.I32))
        checkSet("{0..4294967295}", fromType(TyInteger.U32))
        checkSet("{-9223372036854775808..9223372036854775807}", fromType(TyInteger.I64))
        checkSet("{0..9223372036854775807}", fromType(TyInteger.U64))
        checkSet("{-9223372036854775808..9223372036854775807}", fromType(TyInteger.I128))
        checkSet("{0..9223372036854775807}", fromType(TyInteger.U128))
        checkSet("{-9223372036854775808..9223372036854775807}", fromType(TyInteger.ISize))
        checkSet("{0..9223372036854775807}", fromType(TyInteger.USize))
    }

    fun `test equals empty`() {
        val empty = empty()
        checkMethodWithBooleanResult(
            listOf(empty()) to { it -> empty == it && it == empty },
            listOf(
                unknown(),
                point(42),
                range(-5, 50),
                setFromString("-100..0, 2..50")
            ) to { it -> empty != it && it != empty }
        )
    }

    fun `test equals unknown`() {
        val unknown = unknown()
        checkMethodWithBooleanResult(
            listOf(unknown()) to { it -> unknown == it && it == unknown },
            listOf(
                empty(),
                point(42),
                range(-5, 50),
                setFromString("-100..0, 2..50")
            ) to { it -> unknown != it && it != unknown }
        )
    }

    fun `test equals point`() {
        val point = point(42)
        checkMethodWithBooleanResult(
            listOf(
                point(42),
                range(42, 42)
            ) to { it -> point == it && it == point },
            listOf(
                empty(),
                unknown(),
                point(43),
                range(-5, 50),
                setFromString("-100..0, 2..50")
            ) to { it -> point != it && it != point }
        )
    }

    fun `test equals range`() {
        val range = range(-5, 15)
        checkMethodWithBooleanResult(
            listOf(
                range(-5, 15)
            ) to { it -> range == it && it == range },
            listOf(
                empty(),
                unknown(),
                point(3),
                range(-5, 50),
                range(-6, 15),
                range(-5, 16),
                setFromString("-100..0, 3, 5..50")
            ) to { it -> range != it && it != range }
        )
    }

    fun `test equals set`() {
        val set = setFromString("-5..1, 10, 30..44")
        checkMethodWithBooleanResult(
            listOf(
                setFromString("-5..1, 10, 30..44")
            ) to { it -> set == it && it == set },
            listOf(
                empty(),
                unknown(),
                point(10),
                point(100),
                range(-5, 1),
                range(9, 11),
                setFromString("-5..1, 30..44")
            ) to { it -> set != it && it != set }
        )
    }

    fun `test diff`() {
        assertEquals(empty(), empty().subtract(point(10)))
        assertEquals(point(10), point(10).subtract(empty()))
        assertEquals(point(10), point(10).subtract(point(11)))
        assertEquals(empty(), point(10).subtract(point(10)))
        assertEquals(point(10), point(10).subtract(range(15, 20)))
        assertEquals(point(10), point(10).subtract(range(-10, -5)))
        assertTrue(point(10).subtract(range(10, 20)).isEmpty)
        assertTrue(point(10).subtract(range(-10, 20)).isEmpty)
        assertTrue(point(10).subtract(range(-10, 10)).isEmpty)

        checkSet("{0..20}", range(0, 20).subtract(range(30, Long.MAX_VALUE)))
        checkSet("{0..19}", range(0, 20).subtract(range(20, Long.MAX_VALUE)))
        checkSet("{0..18}", range(0, 20).subtract(range(19, Long.MAX_VALUE)))
        checkSet("{0}", range(0, 20).subtract(range(1, Long.MAX_VALUE)))
        assertTrue(range(0, 20).subtract(range(0, Long.MAX_VALUE)).isEmpty)

        checkSet(
            "{${Long.MIN_VALUE}}",
            all().subtract(range(Long.MIN_VALUE + 1, Long.MAX_VALUE))
        )
        checkSet(
            "{${Long.MAX_VALUE}}",
            all().subtract(range(Long.MIN_VALUE, Long.MAX_VALUE - 1))
        )
        assertTrue(all().subtract(range(Long.MIN_VALUE, Long.MAX_VALUE)).isEmpty)
        assertTrue(all().subtract(all()).isEmpty)
    }

    fun `test sets`() {
        checkSet("{0..9, 11..20}", range(0, 20).without(10))
        checkSet("{0, 20}", range(0, 20).subtract(range(1, 19)))
        checkSet("{0, 1, 19, 20}", range(0, 20).subtract(range(2, 18)))

        checkSet("{0..9, 12..20}", range(0, 20).without(10).without(11))
        checkSet("{0..9, 12..14, 16..20}", range(0, 20).without(10).without(11).without(15))
        checkSet("{0, 4..20}", range(0, 20).without(3).without(2).without(1))
        checkSet("{4..20}", range(0, 20).without(3).without(2).without(1).without(0))

        checkSet("{0..2, 5..15, 19, 20}", range(0, 20).subtract(range(3, 18).subtract(range(5, 15))))

        val first = fromType(TyInteger.U8).without(45)
        val second = fromType(TyInteger.U8).without(32).without(40).without(44).without(45).without(46).without(58).without(59).without(61)
        checkSet("{0..44, 46..255}", first)
        checkSet("{0..31, 33..39, 41..43, 47..57, 60, 62..255}", second)
        checkSet("{32, 40, 44, 46, 58, 59, 61}", first.subtract(second))
    }

    fun `test hash`() {
        val map = hashMapOf<LongRangeSet, String>()
        map[empty()] = "empty"
        map[empty(overflow = true)] = "empty_o"
        map[unknown()] = "unknown"
        map[point(10)] = "10"
        map[range(10, 10)] = "10-10"
        map[range(10, 11)] = "10-11"
        map[range(10, 12)] = "10-12"
        map[setFromString("0..5, 8..10")] = "0-5,8-10"
        map[fromType(TyInteger.I64).subtract(fromType(TyInteger.U16))] = "I64NotU16"

        assertEquals("empty", map[empty()])
        assertEquals("empty_o", map[empty(overflow = true)])
        assertEquals("unknown", map[unknown()])
        assertEquals("10-10", map[point(10)])
        assertEquals("10-10", map[range(10, 10)])
        assertEquals("10-11", map[range(10, 11)])
        assertEquals("10-12", map[range(10, 12)])
        assertEquals("0-5,8-10", map[setFromString("0..5, 8..10")])
        assertNull(map[range(11, 11)])
        assertEquals("I64NotU16", map[fromType(TyInteger.I64).subtract(fromType(TyInteger.U16))])
    }


    fun `test intersects`() {
        assertFalse(empty().intersects(fromType(TyInteger.I64)))
        assertTrue(point(Long.MIN_VALUE).intersects(fromType(TyInteger.I64)))
        assertFalse(point(10).intersects(point(11)))
        assertTrue(point(10).intersects(point(10)))

        assertTrue(range(10, 100).intersects(point(10)))
        assertTrue(range(10, 100).intersects(point(100)))
        assertFalse(range(10, 100).intersects(point(101)))
        assertFalse(range(10, 100).intersects(point(9)))

        val range1020 = range(10, 20)
        assertTrue(range1020.intersects(range1020))
        assertTrue(range1020.intersects(range(10, 30)))
        assertTrue(range1020.intersects(range(20, 30)))
        assertTrue(range1020.intersects(range(0, 30)))
        assertTrue(range1020.intersects(range(0, 10)))
        assertTrue(range1020.intersects(range(0, 20)))

        assertFalse(range1020.intersects(range(0, 9)))
        assertFalse(range1020.intersects(range(21, 30)))

        val rangeSet = range1020.subtract(range(12, 13)).subtract(range(17, 18))
        assertFalse(rangeSet.intersects(point(12)))
        assertFalse(point(12).intersects(rangeSet))
        assertFalse(rangeSet.intersects(empty()))
        assertFalse(rangeSet.intersects(range(12, 13)))
        assertFalse(range(12, 13).intersects(rangeSet))
        assertFalse(rangeSet.intersects(range(0, 9)))
        assertFalse(rangeSet.intersects(range(21, 30)))
        assertTrue(rangeSet.intersects(rangeSet))
        assertTrue(rangeSet.intersects(range1020))
        assertTrue(rangeSet.intersects(point(11)))

        val rangeSet2 = range1020.subtract(rangeSet)
        checkSet("{12, 13, 17, 18}", rangeSet2)
        assertFalse(rangeSet.intersects(rangeSet2))
    }

    fun `test intersect`() {
        checkSet("{0..100}", range(0, 100).intersect(range(0, 100)))
        checkSet("{100}", range(0, 100).intersect(range(100, 200)))
        assertTrue(range(0, 100).intersect(range(101, 200)).isEmpty)
        assertTrue(point(100).intersect(point(200)).isEmpty)
        assertFalse(point(100).intersect(range(99, 101)).isEmpty)

        val rangeSet = range(-1000, 1000).subtract(range(100, 500)).subtract(range(-500, -100))
        checkSet("{-1000..-501, -99..99, 501..1000}", rangeSet)
        assertEquals(point(99), rangeSet.intersect(point(99)))
        assertTrue(rangeSet.intersect(point(100)).isEmpty)
    }

    fun `test intersect subtract randomized`() {
        val r = Random(1)
        val data = r.ints(1000, 0, 1000).mapToObj { range(it.toLong(), (it + r.nextInt(it % 20 * 100 + 1)).toLong()) }.asSequence().toMutableList()
        for (i in 0..2000) {
            val idx = r.nextInt(data.size)
            val left = data[idx]
            val right = data[r.nextInt(data.size)]
            val lDiff = left.subtract(right)
            val rDiff = right.subtract(left)
            val intersection = left.intersect(right)
            val message = "$left & $right = $intersection"
            assertEquals(message, intersection, right.intersect(left))
            if (!intersection.isEmpty) {
                assertTrue(message, intersection.min >= Math.max(left.min, right.min))
                assertTrue(message, intersection.max <= Math.min(left.max, right.max))
            }
            assertEquals(message, intersection, right.subtract(fromType(TyInteger.I64).subtract(left)))
            assertEquals(message, intersection, left.subtract(fromType(TyInteger.I64).subtract(right)))
            intersection.stream.limit(1000).forEach { e ->
                assertTrue(left.contains(e))
                assertTrue(right.contains(e))
            }
            lDiff.stream.limit(1000).forEach { e ->
                assertTrue(left.contains(e))
                assertFalse(right.contains(e))
            }
            rDiff.stream.limit(1000).forEach { e ->
                assertFalse(left.contains(e))
                assertTrue(right.contains(e))
            }
            when (r.nextInt(3)) {
                0 -> data[idx] = lDiff
                1 -> data[idx] = rDiff
                2 -> data[idx] = intersection
            }
        }
    }

    fun `test from constant`() {
        checkSet("{0}", fromConstant(0, TyInteger.I8))
        checkSet("{-20}", fromConstant(-20, TyInteger.I8))
        checkSet("{!}", fromConstant(-20, TyInteger.U8))
        checkSet("{1}", fromConstant(1, TyInteger.I8))
        checkSet("{42}", fromConstant(42, TyInteger.I8))
        checkSet("{?}", fromConstant(null, TyInteger.I128))
        checkSet("{42}", fromConstant(42, TyInteger.I128))
        checkSet("{!}", fromConstant(-10, TyInteger.U128))
    }

    fun `test from relation`() {
        assertEquals(range(101, Long.MAX_VALUE), range(100, 200).fromRelation(ComparisonOp.GT))
        assertEquals(range(100, Long.MAX_VALUE), range(100, 200).fromRelation(ComparisonOp.GTEQ))
        assertEquals(range(Long.MIN_VALUE, 199), range(100, 200).fromRelation(ComparisonOp.LT))
        assertEquals(range(Long.MIN_VALUE, 200), range(100, 200).fromRelation(ComparisonOp.LTEQ))
        assertEquals(range(100, 200), range(100, 200).fromRelation(EqualityOp.EQ))
        checkSet("{0..99, 150, 201..255}", range(100, 200, TyInteger.U8).without(150).fromRelation(EqualityOp.EXCLEQ))
        checkSet("{${Long.MIN_VALUE}..99, 101..${Long.MAX_VALUE}}", point(100).fromRelation(EqualityOp.EXCLEQ))
    }

    fun testAbs() {
        assertTrue(empty().abs.isEmpty)
        assertEquals(point(Long.MAX_VALUE), point(Long.MIN_VALUE + 1).abs)
        assertEquals(empty(overflow = true), point(Long.MIN_VALUE).abs)
        assertEquals(empty(overflow = true), point(Int.MIN_VALUE.toLong(), TyInteger.I32).abs)
        assertEquals(range(100, 200), range(100, 200).abs)
        assertEquals(range(0, 200), range(-1, 200).abs)
        assertEquals(range(0, 200), range(-200, 200).abs)
        assertEquals(range(0, 201), range(-201, 200).abs)
        assertEquals(range(0, Long.MAX_VALUE), all().abs)
        assertEquals(range(100, Int.MAX_VALUE.toLong(), TyInteger.I32), range(Int.MIN_VALUE.toLong(), -100, TyInteger.I32).abs)
        assertEquals(range(100, Int.MAX_VALUE + 1L), range(Int.MIN_VALUE.toLong(), -100).abs)

        val set = range(-900, 1000).subtract(range(-800, -600)).subtract(range(-300, 100)).subtract(range(500, 700))
        checkSet("{-900..-801, -599..-301, 101..499, 701..1000}", set)
        checkSet("{101..599, 701..1000}", set.abs)
    }

    fun `test unaryMinus`() {
        assertTrue(empty().unaryMinus().isEmpty)
        assertEquals(point(Long.MAX_VALUE), -point(Long.MIN_VALUE + 1))
        assertEquals(empty(overflow = true), -point(Int.MIN_VALUE.toLong(), TyInteger.I32))
        assertEquals(point(Int.MAX_VALUE + 1L), -point(Int.MIN_VALUE.toLong()))
        assertEquals(range(-200, -100), -range(100, 200))
        assertEquals(range(-200, 1), -range(-1, 200))
        assertEquals(range(-200, 200), -range(-200, 200))
        assertEquals(range(-200, 201), -range(-201, 200))
        assertEquals(all(), -all())
        assertEquals(range(100, Int.MAX_VALUE.toLong(), TyInteger.I32), -range(Int.MIN_VALUE.toLong(), -100, TyInteger.I32))
        assertEquals(point(Long.MAX_VALUE), -range(Long.MIN_VALUE, Long.MIN_VALUE + 1))
        assertEquals(range(100, Int.MAX_VALUE + 1L), -range(Int.MIN_VALUE.toLong(), -100))
        val set = range(-900, 1000).subtract(range(-800, -600)).subtract(range(-300, 100)).subtract(range(500, 700))
        checkSet("{-900..-801, -599..-301, 101..499, 701..1000}", set)
        checkSet("{-1000..-701, -499..-101, 301..599, 801..900}", -set)
    }

    fun `test contains point`() {
        val point = point(42)
        checkMethodWithBooleanResult(
            listOf(
                point(42),
                range(0, 100),
                range(0, 42),
                range(42, 100),
                setFromString("0, 11, 42"),
                setFromString("0..42, 55..10000"),
                setFromString("42..44, 55..10000"),
                unknown()
            ) to { it -> point in it },
            listOf(
                empty(),
                point(666),
                range(0, 41),
                setFromString("0, 11, 44"),
                setFromString("0..41, 43..10000")
            ) to { it -> point !in it }
        )
    }

    fun `test contains range`() {
        val range = range(-5, 15)
        checkMethodWithBooleanResult(
            listOf(
                range(-5, 15),
                range(-100, 100500),
                setFromString("-70..55, 99"),
                unknown()
            ) to { it -> range in it },
            listOf(
                point(10),
                range(-4, 15),
                setFromString("-4..15, 20"),
                point(5),
                empty()
            ) to { it -> range !in it }
        )
    }

    fun `test contains set`() {
        val set = setFromString("-5..10, 20..50, 70..120")
        checkMethodWithBooleanResult(
            listOf(
                range(-5, 120),
                setFromString("-5..10, 20..50, 70..120, 200"),
                setFromString("-50..50, 66..200"),
                unknown()
            ) to { it -> set in it },
            listOf(
                point(42),
                range(0, 100),
                empty()
            ) to { it -> set !in it }
        )
    }

    fun `test contains empty`() {
        val empty = empty()
        checkMethodWithBooleanResult(
            listOf(
                point(42),
                range(0, 100),
                setFromString("0, 11, 42"),
                empty(),
                unknown()
            ) to { it -> empty in it }
        )
    }

    fun `test contains unknown`() {
        val unknown = unknown()
        checkMethodWithBooleanResult(
            listOf(
                point(42),
                range(0, 100),
                setFromString("0, 11, 42"),
                unknown()) to { it -> unknown in it },
            listOf(empty()) to { it -> unknown !in it }
        )
    }

    private fun checkSet(expected: String, actual: LongRangeSet?) = assertEquals(expected, actual.toString())

    private fun checkPredicate(collection: Collection<LongRangeSet>, predicate: (LongRangeSet) -> Boolean): Unit = collection.filterNot(predicate).let {
        if (it.isNotEmpty()) error("False predicate in ${it.joinToString(", ")}")
    }

    private inline fun <reified T> checkType(collection: Collection<LongRangeSet>) {
        if (!collection.any { set -> set is T }) error("Couldn't find ${T::class}")
    }

    private fun checkHasAllTypes(collection: Collection<LongRangeSet>) {
        checkType<Empty>(collection)
        checkType<Unknown>(collection)
        checkType<Point>(collection)
        checkType<Range>(collection)
        checkType<RangeSet>(collection)
    }

    private fun checkMethodWithBooleanResult(
        vararg pairs: Pair<Collection<LongRangeSet>, (LongRangeSet) -> Boolean>) {
        checkHasAllTypes(pairs.flatMap { it.first })
        pairs.forEach {
            checkPredicate(it.first, it.second)
        }
    }
//
//    fun `test mod`() {
//        assertEquals(empty(), empty().mod(all()))
//        assertEquals(empty(TyInteger.I64), all().mod(empty(TyInteger.I64)))
//        assertEquals(empty(TyInteger.I64), point(1, TyInteger.I64).mod(empty(TyInteger.I64)))
//        assertEquals(
//            empty(TyInteger.I64),
//            point(1, TyInteger.I64).union(point(3, TyInteger.I64)).mod(empty(TyInteger.I64))
//        )
//
//        assertEquals(point(10, TyInteger.I64), point(110, TyInteger.I64).mod(point(100, TyInteger.I64)))
//        checkMod(range(10, 20), range(30, 40), "{10..20}")
//        checkMod(range(-10, 10), range(20, 30), "{-10..10}")
//        checkMod(point(0), range(-100, -50).union(range(20, 80)), "{0}")
//        checkMod(point(30), range(10, 40), "{0..30}")
//        checkMod(point(-30), range(-10, 40), "{-30..0}")
//        checkMod(point(RsRanges.MIN_VALUE, TyInteger.I64), range(-10, 40, TyInteger.I64), "{-39..0}")
//        checkMod(range(-10, 40, TyInteger.I64), point(RsRanges.MIN_VALUE, TyInteger.I64), "{-10..40}")
//        checkMod(range(-30, -20), point(23), "{-22..0}")
//        checkMod(point(10), range(30, 40), "{10}")
//        checkMod(
//            range(-10, 40, TyInteger.I64),
//            point(RsRanges.MIN_VALUE, TyInteger.I64).union(point(70, TyInteger.I64)),
//            "{-10..40}"
//        )
//        checkMod(
//            range(-10, 40, TyInteger.I64),
//            point(RsRanges.MIN_VALUE, TyInteger.I64).union(point(0, TyInteger.I64)),
//            "{-10..40}"
//        )
//        checkMod(
//            point(10, TyInteger.I64),
//            point(RsRanges.MIN_VALUE, TyInteger.I64).union(point(0, TyInteger.I64)),
//            "{0, 10}"
//        )
//        checkMod(range(0, 10).union(range(30, 50)), range(-20, -10).union(range(15, 25)), "{0..24}")
//        checkMod(point(10), point(0), "{}")
//        checkMod(range(0, 10), point(0), "{}")
//        checkMod(
//            range(RsRanges.MIN_VALUE, RsRanges.MIN_VALUE + 3, TyInteger.I64),
//            point(RsRanges.MIN_VALUE, TyInteger.I64),
//            "{-9223372036854775807..-9223372036854775805, 0}"
//        )
//        checkMod(
//            range(RsRanges.MAX_VALUE - 3, RsRanges.MAX_VALUE, TyInteger.I64),
//            point(RsRanges.MAX_VALUE, TyInteger.I64),
//            "{0..${Long.MAX_VALUE - 1}}"
//        )
//    }
//
//    fun `test div`() {
//        assertEquals(empty(TyInteger.I64), empty(TyInteger.I64).div(all()))
//        assertEquals(empty(TyInteger.I64), all().div(empty(TyInteger.I64)))
//        assertEquals(empty(TyInteger.I64), point(1, TyInteger.I64).div(empty(TyInteger.I64)))
//        assertEquals(
//            empty(TyInteger.I64),
//            point(1, TyInteger.I64).div(point(3, TyInteger.I64)).div(empty(TyInteger.I64))
//        )
//        assertEquals(all(), all().div(all()))
//        assertEquals(empty(TyInteger.I64), all().div(point(0, TyInteger.I64)))
//        assertEquals(all(), all().div(point(1)))
//        assertEquals(all(), all().div(point(-1)))
//        assertEquals(point(11), point(110).div(point(10)))
//
//        checkDiv(range(1, 20), range(1, 5), "{0..20}", true)
//        checkDiv(range(1, 20), range(-5, -1), "{-20..0}", true)
//        checkDiv(range(-20, -1), range(1, 5), "{-20..0}", true)
//        checkDiv(range(-20, -1), range(-5, -1), "{0..20}", true)
//        checkDiv(range(-10, 10), range(2, 4), "{-5..5}", true)
//        checkDiv(range(100, 120), range(-2, 2), "{-120..-50, 50..120}", true)
//        checkDiv(
//            range(Int.MIN_VALUE.toLong(), (Int.MIN_VALUE + 20).toLong(), TyInteger.I64),
//            range(-2, 2, TyInteger.I64),
//            "{${Int.MIN_VALUE}..-1073741814, 1073741814..2147483648}"
//        )
//        checkDiv(
//            range(Int.MIN_VALUE.toLong(), (Int.MIN_VALUE + 20).toLong()),
//            range(-2, 2),
//            "{${Int.MIN_VALUE}..-1073741814, 1073741814..${Int.MAX_VALUE}}",
//            true
//        )
//        checkDiv(
//            range(Int.MIN_VALUE.toLong(), (Int.MIN_VALUE + 20).toLong(), TyInteger.I64),
//            range(-2, -1, TyInteger.I64),
//            "{1073741814..2147483648}"
//        )
//        checkDiv(
//            range(Int.MIN_VALUE.toLong(), (Int.MIN_VALUE + 20).toLong()),
//            range(-2, -1),
//            "{${Int.MIN_VALUE}, 1073741814..${Int.MAX_VALUE}}",
//            true
//        )
//    }
//
//    fun testAdd() {
//        checkAdd(empty(), empty(), "{}")
//        checkAdd(empty(), point(0), "{}")
//        checkAdd(empty(), range(0, 10), "{}")
//        checkAdd(empty(), range(0, 10).union(range(15, 20)), "{}")
//
//        checkAdd(point(5), point(10), "{15}")
//        checkAdd(point(Int.MAX_VALUE.toLong()), point(Int.MAX_VALUE.toLong()), "{-2}", true)
//        checkAdd(
//            point(Int.MAX_VALUE.toLong(), TyInteger.I64),
//            point(Int.MAX_VALUE.toLong(), TyInteger.I64),
//            "{${0xFFFF_FFFEL}}"
//        );
//        checkAdd(range(0, 10), point(10), "{10..20}")
//        checkAdd(
//            range((Int.MAX_VALUE - 10).toLong(), Int.MAX_VALUE.toLong()),
//            point(1),
//            "{2147483638..2147483648}"
//        )
//        checkAdd(
//            range((Int.MAX_VALUE - 10).toLong(), Int.MAX_VALUE.toLong()),
//            point(1),
//            "{${Int.MIN_VALUE}, 2147483638..${Int.MAX_VALUE}}",
//            true
//        )
//        checkAdd(
//            range((Int.MAX_VALUE - 10).toLong(), Int.MAX_VALUE.toLong()),
//            point(10),
//            "{${Int.MIN_VALUE}..-2147483639, ${Int.MAX_VALUE}}"
//        )
//        checkAdd(
//            range((Int.MAX_VALUE - 10).toLong(), Int.MAX_VALUE.toLong()),
//            point(11),
//            "{${Int.MIN_VALUE}..-2147483638}"
//        )
//
//        checkAdd(range(0, 10), range(20, 30), "{20..40}")
//        checkAdd(
//            range((Int.MAX_VALUE - 10).toLong(), Int.MAX_VALUE.toLong()),
//            range(0, 10),
//            "{2147483637..2147483657}"
//        )
//        checkAdd(
//            range((Int.MAX_VALUE - 10).toLong(), Int.MAX_VALUE.toLong()),
//            range(0, 10),
//            "{${Int.MIN_VALUE}..-2147483639, 2147483637..${Int.MAX_VALUE}}"
//        )
//
//        checkAdd(range(10, 20).union(range(40, 50)), range(0, 3).union(range(5, 7)), "{10..27, 40..57}")
//
//        val intDomain = range(Int.MIN_VALUE.toLong(), Int.MAX_VALUE.toLong())
//        assertEquals(intDomain, intDomain.plus(point(20)))
//        assertEquals(intDomain.without(20), intDomain.without(0).plus(point(20)))
//        assertEquals(all().without(20), all().without(0).plus(point(20, TyInteger.I64)))
//        assertEquals(intDomain, range(20, 30).union(range(40, 50)).plus(intDomain))
//        assertEquals(intDomain, range(Int.MIN_VALUE.toLong(), 2).plus(range(-2, Int.MAX_VALUE.toLong())))
//        assertEquals(
//            all(),
//            range(RsRanges.MIN_VALUE, 2, TyInteger.I64).plus(range(-2, RsRanges.MAX_VALUE, TyInteger.I64))
//        )
//    }
//
//    private fun checkAdd(addend1: LongRangeSet, addend2: LongRangeSet, expected: String, overflow: Boolean = false) {
//        val result = addend1.plus(addend2)
//        assertEquals(result, addend2.plus(addend1)) // commutative
//        checkBinOp(
//            addend1,
//            addend2,
//            result,
//            LongPredicate { true },
//            LongBinaryOperator { a, b -> if (overflow) (a + b).toInt().toLong() else a + b },
//            expected,
//            "+"
//        )
//    }
//
//    private fun checkMod(dividendRange: LongRangeSet, divisorRange: LongRangeSet, expected: String) {
//        val result = dividendRange.mod(divisorRange)
//        checkBinOp(
//            dividendRange,
//            divisorRange,
//            result,
//            LongPredicate { it != 0L },
//            LongBinaryOperator { a, b -> a % b },
//            expected,
//            "%"
//        )
//    }
//
//    private fun checkDiv(
//        dividendRange: LongRangeSet,
//        divisorRange: LongRangeSet,
//        expected: String,
//        overflow: Boolean = false
//    ) {
//        val result = dividendRange.div(divisorRange)
//        checkBinOp(
//            dividendRange,
//            divisorRange,
//            result,
//            LongPredicate { it != 0L },
//            LongBinaryOperator { a, b -> if (overflow) (a.toInt() / b.toInt()).toLong() else a / b },
//            expected,
//            "/"
//        )
//    }
//
//    private fun checkBinOp(
//        op1: LongRangeSet,
//        op2: LongRangeSet,
//        result: LongRangeSet,
//        filter: LongPredicate,
//        operator: LongBinaryOperator,
//        expected: String,
//        sign: String
//    ) {
//        assertEquals(expected, result.toString())
//        val errors = op1.stream
//            .mapToObj<Stream<String>> { a ->
//                op2.stream
//                    .filter(filter)
//                    .filter { b -> !result.contains(operator.applyAsLong(a, b)) }
//                    .mapToObj { b -> a.toString() + " " + sign + " " + b + " = " + operator.applyAsLong(a, b) }
//            }.flatMap { it }
//            .collect(Collectors.joining("\n"))
//        if (!errors.isEmpty()) {
//            fail("Expected range $expected is not satisfied:\n$errors")
//        }
//    }
}

val numberRegex: Regex = Regex("-?[\\d]+")
private fun setFromString(set: String, type: TyInteger = TyInteger.I64): LongRangeSet = when (set) {
    "" -> empty()
    "!" -> empty(true)
    "?" -> unknown()
    else -> {
        set.splitToSequence(',')
            .map { string ->
                val numbers = numberRegex.findAll(string).map { it.value.toLong() }.toList()
                when (numbers.size) {
                    1 -> point(numbers[0], type)
                    2 -> range(numbers[0], numbers[1], type)
                    else -> error("Invalid set '$set'")
                }
            }
            .reduce(LongRangeSet::unite)
    }
}

private fun fromType(type: Ty?): LongRangeSet = LongRangeSet.fromType(type) ?: error("Bad type '$type'")
