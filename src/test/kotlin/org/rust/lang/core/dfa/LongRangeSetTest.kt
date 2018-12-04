/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

import com.google.common.collect.Range.closed
import com.google.common.math.LongMath
import com.google.common.math.LongMath.checkedAdd
import org.rust.RsTestBase
import org.rust.lang.core.psi.ext.ComparisonOp
import org.rust.lang.core.psi.ext.EqualityOp
import org.rust.lang.core.types.ty.TyFloat
import org.rust.lang.core.types.ty.TyInteger
import org.rust.lang.core.types.ty.TyStr
import java.math.RoundingMode
import java.util.*
import java.util.function.LongBinaryOperator
import java.util.function.LongPredicate
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.math.abs
import kotlin.streams.asSequence
import kotlin.test.assertNotEquals

class LongRangeSetTest : RsTestBase() {
    fun `test other`() {
        val a: LongRangeSet = Empty.EmptyWithoutOverflow
        val b: LongRangeSet = Empty.EmptyWithoutOverflow
        val c: LongRangeSet = Empty.EmptyWithOverflow
        val d = hashMapOf(a to 1, c to 2)
        d[a] = 3
        d[b] = 4
        d[c] = 0
        println(d)
        println(Empty.EmptyWithoutOverflow.hashCode())
        println(a == b)
        println(a == c)
        println(Empty.EmptyWithOverflow.hashCode())
        println(Empty.EmptyWithoutOverflow.hashCode())
        println(Unknown.UnknownWithOverflow.hashCode())
        println(Unknown.UnknownWithoutOverflow.hashCode())
    }
//    fun `test to string`() {
//        assertEquals("{}", empty().toString())
//        assertEquals("{10}", point(10).toString())
//        assertEquals("{10}", range(10, 10).toString())
//        assertEquals("{10, 11}", range(10, 11).toString())
//        assertEquals("{10..100}", range(10, 100).toString())
////        assertEquals("{0..10, 12..15}", set(longArrayOf(0, 10, 12, 15)).toString())
//    }
//
//    fun `test from type`() {
//        assertNull(fromType(TyFloat.DEFAULT))
//        assertNull(fromType(TyStr))
//        assertEquals("{-128..127}", fromType(TyInteger.I8).toString())
//        assertEquals("{0..255}", fromType(TyInteger.U8).toString())
//        assertEquals("{-32768..32767}", fromType(TyInteger.I16).toString())
//        assertEquals("{0..65535}", fromType(TyInteger.U16).toString())
//        assertEquals("{-2147483648..2147483647}", fromType(TyInteger.I32).toString())
//        assertEquals("{0..4294967295}", fromType(TyInteger.U32).toString())
//        assertEquals("{-9223372036854775808..9223372036854775807}", fromType(TyInteger.I64).toString())
//        assertEquals("{-9223372036854775808..9223372036854775807}", fromType(TyInteger.ISize).toString())
//    }
//
//    fun `test equals`() {
//        assertEquals(empty(), empty())
//        assertEquals(point(10), point(10))
//        assertNotEquals(point(10), point(11))
//        assertEquals(point(10), range(10, 10))
//        assertNotEquals(point(10), range(10, 11))
//        assertEquals(range(10, 11), range(10, 11))
//        assertNotEquals(range(10, 11), range(10, 12))
////        assertEquals(set(longArrayOf(0, 10, 12, 15)), set(longArrayOf(0, 10, 12, 15)))
////        assertNotEquals(set(longArrayOf(0, 10, 12, 15)), set(longArrayOf(0, 9, 12, 15)))
//    }
//
//    fun `test diff`() {
//        assertEquals(empty(), empty().subtract(point(10)))
//        assertEquals(point(10), point(10).subtract(empty()))
//        assertEquals(point(10), point(10).subtract(point(11)))
//        assertEquals(empty(), point(10).subtract(point(10)))
//        assertEquals(point(10), point(10).subtract(range(15, 20)))
//        assertEquals(point(10), point(10).subtract(range(-10, -5)))
//        assertTrue(point(10).subtract(range(10, 20)).isEmpty)
//        assertTrue(point(10).subtract(range(-10, 20)).isEmpty)
//        assertTrue(point(10).subtract(range(-10, 10)).isEmpty)
//
//        assertEquals("{0..20}", range(0, 20).subtract(range(30, RsRanges.MAX_VALUE)).toString())
//        assertEquals("{0..19}", range(0, 20).subtract(range(20, RsRanges.MAX_VALUE)).toString())
//        assertEquals("{0..18}", range(0, 20).subtract(range(19, RsRanges.MAX_VALUE)).toString())
//        assertEquals("{0}", range(0, 20).subtract(range(1, RsRanges.MAX_VALUE)).toString())
//        assertTrue(range(0, 20).subtract(range(0, RsRanges.MAX_VALUE)).isEmpty)
//
//        assertEquals(
//            "{${RsRanges.MIN_VALUE}}",
//            all().subtract(range(RsRanges.MIN_VALUE + 1, RsRanges.MAX_VALUE)).toString()
//        )
//        assertEquals(
//            "{${RsRanges.MAX_VALUE}}",
//            all().subtract(range(RsRanges.MIN_VALUE, RsRanges.MAX_VALUE - 1)).toString()
//        )
//        assertTrue(all().subtract(range(RsRanges.MIN_VALUE, RsRanges.MAX_VALUE)).isEmpty)
//        assertTrue(all().subtract(all()).isEmpty)
//    }
//
//    fun `test sets`() {
//        assertEquals("{0..9, 11..20}", range(0, 20).without(10).toString())
//        assertEquals("{0, 20}", range(0, 20).subtract(range(1, 19)).toString())
//        assertEquals("{0, 1, 19, 20}", range(0, 20).subtract(range(2, 18)).toString())
//
//        assertEquals("{0..9, 12..20}", range(0, 20).without(10).without(11).toString())
//        assertEquals("{0..9, 12..14, 16..20}", range(0, 20).without(10).without(11).without(15).toString())
//        assertEquals("{0, 4..20}", range(0, 20).without(3).without(2).without(1).toString())
//        assertEquals("{4..20}", range(0, 20).without(3).without(2).without(1).without(0).toString())
//
//        assertEquals(
//            "{0..2, 5..15, 19, 20}", range(0, 20).subtract(range(3, 18).subtract(range(5, 15))).toString()
//        )
//
//        val first = fromType(TyInteger.U16)!!.without(45)
//        val second = fromType(TyInteger.U16)!!.without(32).without(40).without(44).without(45).without(46).without(58)
//            .without(59).without(61)
//        assertEquals("{0..44, 46..65535}", first.toString())
//        assertEquals("{0..31, 33..39, 41..43, 47..57, 60, 62..65535}", second.toString())
//        assertEquals("{32, 40, 44, 46, 58, 59, 61}", first.subtract(second).toString())
//    }
//
//    fun `test hash`() {
//        val map = HashMap<LongRangeSet, String>()
//        map[empty()] = "empty"
//        map[point(10)] = "10"
//        map[range(10, 10)] = "10-10"
//        map[range(10, 11)] = "10-11"
//        map[range(10, 12)] = "10-12"
//        val longNotChar = fromType(TyInteger.I64)!!.subtract(fromType(TyInteger.U16)!!)
//        map[longNotChar] = "I64NotU16"
//
//        assertEquals("empty", map[empty()])
//        assertEquals("10-10", map[point(10)])
//        assertEquals("10-11", map[range(10, 11)])
//        assertEquals("10-12", map[range(10, 12)])
//        assertNull(map[range(11, 11)])
//        assertEquals("I64NotU16", map[fromType(TyInteger.I64)!!.subtract(fromType(TyInteger.U16)!!)])
//    }
//
//    fun `test intersects`() {
//        assertFalse(empty().intersects(fromType(TyInteger.I64)!!))
//        assertTrue(point(RsRanges.MIN_VALUE).intersects(fromType(TyInteger.I64)!!))
//        assertFalse(point(10).intersects(point(11)))
//        assertTrue(point(10).intersects(point(10)))
//
//        assertTrue(range(10, 100).intersects(point(10)))
//        assertTrue(range(10, 100).intersects(point(100)))
//        assertFalse(range(10, 100).intersects(point(101)))
//        assertFalse(range(10, 100).intersects(point(9)))
//
//        val range1020 = range(10, 20)
//        assertTrue(range1020.intersects(range1020))
//        assertTrue(range1020.intersects(range(10, 30)))
//        assertTrue(range1020.intersects(range(20, 30)))
//        assertTrue(range1020.intersects(range(0, 30)))
//        assertTrue(range1020.intersects(range(0, 10)))
//        assertTrue(range1020.intersects(range(0, 20)))
//
//        assertFalse(range1020.intersects(range(0, 9)))
//        assertFalse(range1020.intersects(range(21, 30)))
//
//        val rangeSet = range1020.subtract(range(12, 13)).subtract(range(17, 18))
//        assertFalse(rangeSet.intersects(point(12)))
//        assertFalse(point(12).intersects(rangeSet))
//        assertFalse(rangeSet.intersects(empty()))
//        assertFalse(rangeSet.intersects(range(12, 13)))
//        assertFalse(range(12, 13).intersects(rangeSet))
//        assertFalse(rangeSet.intersects(range(0, 9)))
//        assertFalse(rangeSet.intersects(range(21, 30)))
//        assertTrue(rangeSet.intersects(rangeSet))
//        assertTrue(rangeSet.intersects(range1020))
//        assertTrue(rangeSet.intersects(point(11)))
//
//        val rangeSet2 = range1020.subtract(rangeSet)
//        assertEquals("{12, 13, 17, 18}", rangeSet2.toString())
//        assertFalse(rangeSet.intersects(rangeSet2))
//    }
//
//    fun `test intersect`() {
//        assertEquals("{0..100}", range(0, 100).intersect(range(0, 100)).toString())
//        assertEquals("{100}", range(0, 100).intersect(range(100, 200)).toString())
//        assertTrue(range(0, 100).intersect(range(101, 200)).isEmpty)
//        assertTrue(point(100).intersect(point(200)).isEmpty)
//        assertFalse(point(100).intersect(range(99, 101)).isEmpty)
//
//        val rangeSet = range(-1000, 1000).subtract(range(100, 500)).subtract(range(-500, -100))
//        assertEquals("{-1000..-501, -99..99, 501..1000}", rangeSet.toString())
//        assertEquals(point(99), rangeSet.intersect(point(99)))
//        assertTrue(rangeSet.intersect(point(100)).isEmpty)
//    }
//
//    fun `test intersect subtract randomized`() {
//        val r = Random(1)
//        val data = r.ints(1000, 0, 1000).mapToObj { range(it.toLong(), (it + r.nextInt(it % 20 * 100 + 1)).toLong()) }
//            .asSequence().toMutableList()
//        for (i in 0..1999) {
//            val idx = r.nextInt(data.size)
//            val left = data[idx]
//            val right = data[r.nextInt(data.size)]
//            val lDiff = left.subtract(right)
//            val rDiff = right.subtract(left)
//            val intersection = left.intersect(right)
//            val message = "$left & $right = $intersection"
//            assertEquals(message, intersection, right.intersect(left))
//            if (!intersection.isEmpty) {
//                assertTrue(message, intersection.min >= Math.max(left.min, right.min))
//                assertTrue(message, intersection.max <= Math.min(left.max, right.max))
//            }
//            assertEquals(message, intersection, right.subtract(fromType(TyInteger.I64)!!.subtract(left)))
//            assertEquals(message, intersection, left.subtract(fromType(TyInteger.I64)!!.subtract(right)))
//            intersection.stream.limit(1000).forEach { e ->
//                assertTrue(left.contains(e))
//                assertTrue(right.contains(e))
//            }
//            lDiff.stream.limit(1000).forEach { e ->
//                assertTrue(left.contains(e))
//                assertFalse(right.contains(e))
//            }
//            rDiff.stream.limit(1000).forEach { e ->
//                assertFalse(left.contains(e))
//                assertTrue(right.contains(e))
//            }
//            when (r.nextInt(3)) {
//                0 -> data[idx] = lDiff
//                1 -> data[idx] = rDiff
//                2 -> data[idx] = intersection
//            }
//        }
//    }
//
//    fun `test from constant`() {
//        assertEquals("{0}", fromConstant(0).toString())
//        assertEquals("{-20}", fromConstant(-20).toString())
//        assertEquals("{1}", fromConstant(1).toString())
//        assertEquals("{42}", fromConstant(42).toString())
//    }
//
//    fun `test from relation`() {
//        assertEquals(range(101, RsRanges.MAX_VALUE), range(100, 200, TyInteger.I64).fromRelation(ComparisonOp.GT))
//        assertEquals(range(100, RsRanges.MAX_VALUE), range(100, 200, TyInteger.I64).fromRelation(ComparisonOp.GTEQ))
//        assertEquals(range(RsRanges.MIN_VALUE, 199), range(100, 200, TyInteger.I64).fromRelation(ComparisonOp.LT))
//        assertEquals(range(RsRanges.MIN_VALUE, 200), range(100, 200, TyInteger.I64).fromRelation(ComparisonOp.LTEQ))
//        assertEquals(range(100, 200), range(100, 200).fromRelation(EqualityOp.EQ))
//        assertEquals(fromType(TyInteger.I64), range(100, 200, TyInteger.I64).fromRelation(EqualityOp.EXCLEQ))
//        assertEquals(
//            "{${RsRanges.MIN_VALUE}..99, 101..${RsRanges.MAX_VALUE}}",
//            point(100, TyInteger.I64).fromRelation(EqualityOp.EXCLEQ).toString()
//        )
//    }
//
//    fun testAbs() {
//        assertTrue(empty().abs.isEmpty)
//        assertEquals(point(RsRanges.MAX_VALUE), point(RsRanges.MIN_VALUE + 1).abs)
//        assertEquals(point(RsRanges.MIN_VALUE), point(RsRanges.MIN_VALUE).abs)
//        assertEquals(point(Integer.MIN_VALUE.toLong()), point(Integer.MIN_VALUE.toLong()).abs)
//        assertEquals(
//            point(Integer.MAX_VALUE + 1L, TyInteger.I64),
//            point(Integer.MIN_VALUE.toLong(), TyInteger.I64).abs
//        )
//        assertEquals(range(100, 200), range(100, 200).abs)
//        assertEquals(range(0, 200), range(-1, 200).abs)
//        assertEquals(range(0, 200), range(-200, 200).abs)
//        assertEquals(range(0, 201), range(-201, 200).abs)
//        assertEquals(
//            range(0, RsRanges.MAX_VALUE, TyInteger.I64).union(point(RsRanges.MIN_VALUE, TyInteger.I64)),
//            all().abs
//        )
//        assertEquals(
//            range(100, Integer.MAX_VALUE.toLong()).union(point(Integer.MIN_VALUE.toLong())),
//            range(Integer.MIN_VALUE.toLong(), -100).abs
//        )
//        assertEquals(
//            range(100, Integer.MAX_VALUE + 1L, TyInteger.I64),
//            range(Integer.MIN_VALUE.toLong(), -100, TyInteger.I64).abs
//        )
//        val set = range(-900, 1000).subtract(range(-800, -600)).subtract(range(-300, 100)).subtract(range(500, 700))
//        assertEquals("{-900..-801, -599..-301, 101..499, 701..1000}", set.toString())
//        assertEquals("{101..599, 701..1000}", set.abs.toString())
//    }
//
//    fun `test minus`() {
//        assertTrue(empty().minus.isEmpty)
//        assertEquals(point(RsRanges.MAX_VALUE), point(RsRanges.MIN_VALUE + 1).minus)
//        assertEquals(point(RsRanges.MIN_VALUE), point(RsRanges.MIN_VALUE).minus)
//        assertEquals(point(Integer.MIN_VALUE.toLong()), point(Integer.MIN_VALUE.toLong()).minus)
//        assertEquals(
//            point(Integer.MAX_VALUE + 1L, TyInteger.I64),
//            point(Integer.MIN_VALUE.toLong(), TyInteger.I64).minus
//        )
//        assertEquals(range(-200, -100), range(100, 200).minus)
//        assertEquals(range(-200, 1), range(-1, 200).minus)
//        assertEquals(range(-200, 200), range(-200, 200).minus)
//        assertEquals(range(-200, 201), range(-201, 200).minus)
//        assertEquals(all(), all().minus)
//        assertEquals(
//            range(100, Integer.MAX_VALUE.toLong()).union(point(Integer.MIN_VALUE.toLong())),
//            range(Integer.MIN_VALUE.toLong(), -100).minus
//        )
//        assertEquals(
//            point(RsRanges.MAX_VALUE, TyInteger.I64).union(point(RsRanges.MIN_VALUE, TyInteger.I64)),
//            range(RsRanges.MIN_VALUE, RsRanges.MIN_VALUE + 1, TyInteger.I64).minus
//        )
//        assertEquals(
//            range(100, Integer.MAX_VALUE + 1L, TyInteger.I64),
//            range(Integer.MIN_VALUE.toLong(), -100, TyInteger.I64).minus
//        )
//        val set = range(-900, 1000).subtract(range(-800, -600)).subtract(range(-300, 100)).subtract(range(500, 700))
//        assertEquals("{-900..-801, -599..-301, 101..499, 701..1000}", set.toString())
//        assertEquals("{-1000..-701, -499..-101, 301..599, 801..900}", set.minus.toString())
//    }
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
//            range(Integer.MIN_VALUE.toLong(), (Integer.MIN_VALUE + 20).toLong(), TyInteger.I64),
//            range(-2, 2, TyInteger.I64),
//            "{${Integer.MIN_VALUE}..-1073741814, 1073741814..2147483648}"
//        )
//        checkDiv(
//            range(Integer.MIN_VALUE.toLong(), (Integer.MIN_VALUE + 20).toLong()),
//            range(-2, 2),
//            "{${Integer.MIN_VALUE}..-1073741814, 1073741814..${Integer.MAX_VALUE}}",
//            true
//        )
//        checkDiv(
//            range(Integer.MIN_VALUE.toLong(), (Integer.MIN_VALUE + 20).toLong(), TyInteger.I64),
//            range(-2, -1, TyInteger.I64),
//            "{1073741814..2147483648}"
//        )
//        checkDiv(
//            range(Integer.MIN_VALUE.toLong(), (Integer.MIN_VALUE + 20).toLong()),
//            range(-2, -1),
//            "{${Integer.MIN_VALUE}, 1073741814..${Integer.MAX_VALUE}}",
//            true
//        )
//    }
//
//    fun `test contains`() {
//        assertTrue(range(0, 10).contains(5))
//        assertTrue(range(0, 10).union(range(13, 20)).contains(point(5)))
//        assertTrue(range(0, 10).union(range(13, 20)).contains(empty()))
//        assertFalse(range(0, 10).union(range(13, 20)).contains(point(12)))
//        assertFalse(range(0, 10).union(range(13, 20)).contains(range(9, 15)))
//        assertTrue(range(0, 10).union(range(13, 20)).contains(range(2, 8).union(range(15, 17))))
//    }
//
//    fun testAdd() {
//        checkAdd(empty(), empty(), "{}")
//        checkAdd(empty(), point(0), "{}")
//        checkAdd(empty(), range(0, 10), "{}")
//        checkAdd(empty(), range(0, 10).union(range(15, 20)), "{}")
//
//        checkAdd(point(5), point(10), "{15}")
//        checkAdd(point(Integer.MAX_VALUE.toLong()), point(Integer.MAX_VALUE.toLong()), "{-2}", true)
//        checkAdd(
//            point(Integer.MAX_VALUE.toLong(), TyInteger.I64),
//            point(Integer.MAX_VALUE.toLong(), TyInteger.I64),
//            "{${0xFFFF_FFFEL}}"
//        );
//        checkAdd(range(0, 10), point(10), "{10..20}")
//        checkAdd(
//            range((Integer.MAX_VALUE - 10).toLong(), Integer.MAX_VALUE.toLong()),
//            point(1),
//            "{2147483638..2147483648}"
//        )
//        checkAdd(
//            range((Integer.MAX_VALUE - 10).toLong(), Integer.MAX_VALUE.toLong()),
//            point(1),
//            "{${Integer.MIN_VALUE}, 2147483638..${Integer.MAX_VALUE}}",
//            true
//        )
//        checkAdd(
//            range((Integer.MAX_VALUE - 10).toLong(), Integer.MAX_VALUE.toLong()),
//            point(10),
//            "{${Integer.MIN_VALUE}..-2147483639, ${Integer.MAX_VALUE}}"
//        )
//        checkAdd(
//            range((Integer.MAX_VALUE - 10).toLong(), Integer.MAX_VALUE.toLong()),
//            point(11),
//            "{${Integer.MIN_VALUE}..-2147483638}"
//        )
//
//        checkAdd(range(0, 10), range(20, 30), "{20..40}")
//        checkAdd(
//            range((Integer.MAX_VALUE - 10).toLong(), Integer.MAX_VALUE.toLong()),
//            range(0, 10),
//            "{2147483637..2147483657}"
//        )
//        checkAdd(
//            range((Integer.MAX_VALUE - 10).toLong(), Integer.MAX_VALUE.toLong()),
//            range(0, 10),
//            "{${Integer.MIN_VALUE}..-2147483639, 2147483637..${Integer.MAX_VALUE}}"
//        )
//
//        checkAdd(range(10, 20).union(range(40, 50)), range(0, 3).union(range(5, 7)), "{10..27, 40..57}")
//
//        val intDomain = range(Integer.MIN_VALUE.toLong(), Integer.MAX_VALUE.toLong())
//        assertEquals(intDomain, intDomain.plus(point(20)))
//        assertEquals(intDomain.without(20), intDomain.without(0).plus(point(20)))
//        assertEquals(all().without(20), all().without(0).plus(point(20, TyInteger.I64)))
//        assertEquals(intDomain, range(20, 30).union(range(40, 50)).plus(intDomain))
//        assertEquals(intDomain, range(Integer.MIN_VALUE.toLong(), 2).plus(range(-2, Integer.MAX_VALUE.toLong())))
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
