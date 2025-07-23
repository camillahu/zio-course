package com.rockthejvm.part5testing

import zio._
import zio.test._
import com.rockthejvm.utils._

object PropertyBasedTesting extends ZIOSpecDefault {

  //is used for so-called "proofs" - validate statements with varying degrees of generality
  // testing generalized properties, you generate the arguments by random with Gen.
  // check runs a 100 times by default, and it needs to be true every time to pass.
  def spec = test("propoerty-based-testing basics") {
    check(Gen.int, Gen.int, Gen.int) { (x,y,z) =>
      assertTrue(((x + y) + z) == (x + (y + z)))
    }
  }

  //Generators have two type args: Gen[R, A]: R = "env", A = value


  //Generator examples:
  val intGenerator = Gen.int
  val charGenerator = Gen.char //alphaChar, AlphaNumericChar etc.
  val stringGenerator = Gen.string
  val cappedLengthStringGenerator = Gen.stringN(10)(Gen.alphaNumericChar)
  val constGenerator = Gen.const("Scala") //will always be "Scala"
  val valuesGenerator = Gen.elements(1,3,5,7,9) //will select one from these numbers
  val valuesIterableGenerator = Gen.fromIterable(1 to 1000)
  val uniformDoublesGenerator = Gen.uniform //select doubles between 0 and 1

  //produces collections
  val listGenerator = Gen.listOf(Gen.string) // unbounded list of strings
  val finiteSetGenerator = Gen.setOfN(10)(Gen.int) // sets of 10 ints

  //option, either
  val optionGenerator = Gen.option(Gen.int) // produce Option[Int]
  val eitherGenerator = Gen.either(Gen.string, Gen.int) //produce Either[String,Int]

  //combinators
  val zippedGenerators = Gen.int.zip(Gen.string) // produces (Int, String)
  val filteredGenerator = intGenerator.filter(_ % 3 == 0)
  val mappedGenerator = intGenerator.map(n => (1 to n).map(_ => 'a').mkString)
  val flatMappedGenerator = filteredGenerator.flatMap(l => Gen.stringN(l)(Gen.alphaNumericChar))

  //for comprehensions
  val uuidGenerator = for {
    part1 <- Gen.stringN(8)(Gen.alphaNumericChar)
    part2 <- Gen.stringN(4)(Gen.alphaNumericChar)
    part3 <- Gen.stringN(4)(Gen.alphaNumericChar)
    part4 <- Gen.stringN(12)(Gen.alphaNumericChar)
  } yield s"$part1-$part2-$part3-$part4"

  //general
  val randomGenerator = Gen.fromRandom(random => random.nextUUID)
  val effectGenerator = Gen.fromZIO(ZIO.succeed(42))
  val generalGenerator = Gen.unfoldGen(0)(i => Gen.const(i + 1).zip(Gen.stringN(i)(Gen.alphaNumericChar)))
    //the generator above will:
    //produce lists of strings, with the property that every string will have increasing length
}

object GenerationPlayground extends ZIOAppDefault {

  def run = {
    val generalGenerator = Gen.unfoldGen(0)(i => Gen.const(i + 1).zip(Gen.stringN(i)(Gen.alphaNumericChar)))
    val generatedListZIO = generalGenerator.runCollectN(100)

    val generatedListZIO_v2 = generatedListZIO.provideLayer(Sized.default)
    //this way we create a Layer with Sized, eliminating the req of R for Gen.
    generatedListZIO_v2.debugThread
  }
}
