package org.renaissance.scala.stdlib

import org.renaissance.Benchmark
import org.renaissance.Benchmark._
import org.renaissance.BenchmarkContext
import org.renaissance.BenchmarkResult
import org.renaissance.BenchmarkResult.Assert
import org.renaissance.BenchmarkResult.Validators
import org.renaissance.License


import scala.collection._
import scala.util.Random

trait PiUtilities{
  class Transformator(var q: BigInt, var r: BigInt, var s: BigInt, var t: BigInt, var k : BigInt = 0 ){

    def extract(j: BigInt): BigInt = {
      val numerator =  q * j + r
      val denumerator = s * j + t

      return numerator / denumerator
    }

    def next() : Transformator = {
      k = k + 1
      q = k
      r = 4 * k + 2
      s = 0
      t = 2 * k + 1

      return this
    }

    def qrst(_q: BigInt, _r: BigInt, _s: BigInt, _t: BigInt): Transformator = {
        q = _q
        r = _r
        s = _s
        t = _t
        k = 0
        return this
    }

    def compose(tran: Transformator): Transformator = {
        return new Transformator(
            q * tran.q,
            q * tran.r + r * tran.t,
            s * tran.q + t * tran.s,
            s * tran.r + t * tran.t);
    }

  }
  
  class PiDigitGenerator(
    var z: Transformator = new Transformator(1, 0, 0, 1),
    var x: Transformator = new Transformator(0, 0, 0, 0),
    var inverse: Transformator = new Transformator(0, 0, 0, 0)){

    def next(): BigInt = {
        val y = digit()
        if (isSafe(y)){
            z = produce(y)
            return y
        }
        else{
            z = consume(x.next())
            return next()
        }
    }

    def digit(): BigInt = {
        return z.extract(3)
    }

    def isSafe(digit: BigInt): Boolean = {
        return digit == z.extract(4)
    }

    def produce(i : BigInt): Transformator = {
        return (inverse.qrst(10, -10 * i, 0, 1)).compose(z)
    }

    def consume(tran : Transformator): Transformator = {
        return z.compose(tran)
    }
  }
}

@Name("scala-pi")
@Group("scala")
@Group("scala-stdlib")
@Summary("Computes digits of pi.")
@Licenses(Array(License.MIT))
@Repetitions(20)
final class ScalaPi extends Benchmark with PiUtilities {
    private var CorrectPiDigits = new Array[Int](1001)
    private val PiDigits: Int = 1000
    override def setUpBeforeAll(c: BenchmarkContext): Unit = {
        val source = io.Source.fromFile("./PiDigits.in")
        var idx: Int = 0
        for (char <- source) {
            if (idx <= 1000){
                CorrectPiDigits(idx) = char.toInt - 48 //-48 to get num from ascii
            }
            idx = idx + 1
        }

        source.close
    }

    override def run(c: BenchmarkContext): BenchmarkResult = {
        val generator: PiDigitGenerator = new PiDigitGenerator()
        var dumpster: BigInt = 0

        var i = 0
        for (i <- 0 to PiDigits){
            dumpster = generator.next()
            Assert.assertEquals(CorrectPiDigits(i), dumpster.intValue, "pi digit")
        }

        Validators.simple("correct", 0, 0);
    }
}
