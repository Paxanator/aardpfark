package com.ibm.aardpfark.pfa.functions

import com.ibm.aardpfark.pfa.dsl._
import com.ibm.aardpfark.pfa.document.PFABuilder
import org.scalactic.Tolerance._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.JsonMethods.{parse =>parseJSON}

class MathLibrarySuite extends FunctionLibrarySuite {

  private def roundToTwoDigits(input: AnyRef): Double =
    (math floor (input.asInstanceOf[Double] * 100.0)) / 100

  test("Math abs") {
    val action = m.abs(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("-0.5")) == 0.5)
  }

  test("Math pi") {
    val action = m.pi()

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(roundToTwoDigits(engine.action(engine.jsonInput("-0.0"))) == 3.14)
  }

  test("Math e") {
    val action = m.e()

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(roundToTwoDigits(engine.action(engine.jsonInput("-0.0"))) == 2.71)
  }

  test("Math acos") {
    val action = m.acos(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("1.0")) == 0.0)
  }

  test("Math asin") {
    val action = m.asin(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("0.0")) == 0.0)
  }

  test("Math atan") {
    val action = m.atan(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("0.0")) == 0.0)
  }

  test("Math ceil") {
    val action = m.ceil(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("3.4")) == 4.0)
  }



  test("Math copysign") {
    val action = m.copysign(inputExpr,inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("-3.4")).asInstanceOf[Double] < 0)
  }

  test("Math cos") {
    val action = m.cos(core.div(m.pi(),inputExpr))

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    val result = engine.jsonOutput(engine.action(engine.jsonInput("2.0"))).toDouble
    assert(result === 0.0 +- 1e-6)
  }

  test("Math cosh") {
    val action = m.cosh(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    val result = engine.jsonOutput(engine.action(engine.jsonInput("0.0"))).toDouble
    assert(result == 1.0)
  }

  test("Math exp") {
    val action = m.exp(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    val result = engine.jsonOutput(engine.action(engine.jsonInput("2"))).toDouble
    assert(result === 7.3890560989 +- 1e-6)
  }

  test("Math expm1") {
    val action = m.expm1(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    val result = engine.jsonOutput(engine.action(engine.jsonInput("2"))).toDouble
    assert(result === 6.3890560989 +- 1e-6)
  }

  test("Math floor") {
    val action = m.floor(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("3.4")) == 3.0)
  }

  test("Math hypot") {
    val action = m.hypot(inputExpr,inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(roundToTwoDigits(engine.action(engine.jsonInput("1"))) == 1.41)
  }

  test("Math ln") {
    val action = m.ln(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    val result = engine.jsonOutput(engine.action(engine.jsonInput("2"))).toDouble
    assert(result === 0.6931471806 +- 1e-6)
  }

  test("Math log10") {
    val action = m.log10(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    val result = engine.jsonOutput(engine.action(engine.jsonInput("100"))).toDouble
    assert(result == 2.0)
  }

  test("Math log") {
    val action = m.log(inputExpr,inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Int]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    val result = engine.jsonOutput(engine.action(engine.jsonInput("100"))).toDouble
    assert(result == 1.0)
  }


  test("Math ln1p") {
    val action = m.ln1p(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    val result = engine.action(engine.jsonInput("2.0"))
    assert(roundToTwoDigits(result) == 1.09)
  }

  test("Math round") {
    val action = m.round(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("3.4")) == 3.0)
  }

  test("Math rint") {
    val action = m.rint(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("3.5")) == 4.0)
  }

  test("Math signum") {
    val action = m.signum(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("-3.5")) == -1)
  }

  test("Math sin") {
    val action = m.sin(core.div(m.pi(),inputExpr))

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    val result = engine.jsonOutput(engine.action(engine.jsonInput("2.0"))).toDouble
    assert(result === 1.0 +- 1e-6)
  }

  test("Math sinh") {
    val action = m.sinh(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("0.0")) == 0.0)
  }

  test("Math sqrt") {
    val action = m.sqrt(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("4.0")) == 2.0)
  }

  test("Math tan") {
    val action = m.tan(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("0.0")) == 0.0)
  }

  test("Math tanh") {
    val action = m.tanh(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("0.0")) == 0.0)
  }

  test("m.special.nChooseK") {
    val action = m.special.nChooseK(inputExpr,3)

    val pfaDoc = new PFABuilder()
      .withInput[Int]
      .withOutput[Int]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("10")) == 120)
  }

  test("m.special.lnBeta") {
    val action = m.special.lnBeta(inputExpr,inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("1.0")) == 0.0)
  }

  test("m.special.erf") {
    val action = m.special.erf(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("1.0")) == 0.0)
  }

  test("m.special.erfc") {
    val action = m.special.erfc(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("1.0")) == 0.0)
  }

  test("m.special.lnGamma") {
    val action = m.special.lnGamma(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("1.0")) == 0.0)
  }


  test("m.link logit") {
    val action = m.link.logit(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    val result = engine.action(engine.jsonInput("1"))
    assert(result.toString.toDouble === 0.73105 +- tol)
  }

  test("m.link softmax") {
    val action = m.link.softmax(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput(doubleArraySchema)
      .withOutput(doubleArraySchema)
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    val pfaResult = engine.action(engine.jsonInput("[1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0]"))

    val parsedResult = parseJSON(pfaResult.toString).extract[List[Double]]
    val expectedResult = List(0.024, 0.064, 0.175, 0.475, 0.024, 0.064, 0.175)

    parsedResult.zip(expectedResult).foreach { case (actual, expected) =>
        assert(actual === expected +- tol)
    }
  }

}
