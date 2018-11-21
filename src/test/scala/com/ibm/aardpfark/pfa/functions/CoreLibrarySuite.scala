package com.ibm.aardpfark.pfa.functions

import com.ibm.aardpfark.pfa.document.PFABuilder
import com.ibm.aardpfark.pfa.dsl._
import org.apache.avro.SchemaBuilder

class CoreLibrarySuite extends FunctionLibrarySuite {

  test("Core plus") {
    val action = core.plus(inputExpr, 1.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("0.5")) == 1.5)
  }

  test("Core minus") {
    val action = core.minus(inputExpr, 0.5)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("2.0")) == 1.5)
  }

  test("Core mult") {
    val action = core.mult(inputExpr, 2.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("0.5")) == 1.0)
  }

  test("Core div") {
    val action = core.div(inputExpr, 2.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("4.0")) == 2.0)
  }

  test("Core divfloor") {
    val action = core.divfloor(inputExpr, 3)

    val pfaDoc = new PFABuilder()
      .withInput[Int]
      .withOutput[Int]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("10")) == 3)
  }

  test("Core addinv") {
    val action = core.addinv(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("-1.0")) == 1.0)
  }

  test("Core mod") {
    val action = core.mod(inputExpr,3)

    val pfaDoc = new PFABuilder()
      .withInput[Int]
      .withOutput[Int]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("-10")) == 2)
  }

  test("Core modmod") {
    val action = core.modmod(inputExpr,3)

    val pfaDoc = new PFABuilder()
      .withInput[Int]
      .withOutput[Int]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("-10")) == -1)
  }

  test("Core and") {
    val action = core.and(inputExpr, inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Boolean]
      .withOutput[Boolean]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("true")) == true)
  }

  test("Core xor") {
    val action = core.xor(inputExpr, inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Boolean]
      .withOutput[Boolean]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("true")) == false)
  }

  test("Core nullableAnd") {
    val action = core.nullableAnd(inputExpr, inputExpr)

    val nullBool = SchemaBuilder.nullable().booleanType()

    val pfaDoc = new PFABuilder()
      .withInput(nullBool) //TODO: Upgrade to Option[Boolean] with avro4s 2.0+
      .withOutput(nullBool)
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("null")) == null)
  }

  test("Core nullableOr") {
    val action = core.nullableOr(inputExpr, inputExpr)

    val nullBool = SchemaBuilder.nullable().booleanType()

    val pfaDoc = new PFABuilder()
      .withInput(nullBool)
      .withOutput(nullBool)
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("null")) == null)
  }

  test("Core nullableNot") {
    val action = core.nullableNot(inputExpr)

    val nullBool = SchemaBuilder.nullable().booleanType()

    val pfaDoc = new PFABuilder()
      .withInput(nullBool)
      .withOutput(nullBool)
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("null")) == null)
  }


  test("Core eq") {
    val action = core.eq(inputExpr, 1.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Boolean]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("1.0")) == true)
  }

  test("Core cmp") {
    val action = core.cmp(inputExpr, 1.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Int]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("2.0")) == 1)
  }

  test("Core lt") {
    val action = core.lt(inputExpr, 2.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Boolean]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("1.0")) == true)
  }

  test("Core lte") {
    val action = core.lte(inputExpr, 2.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Boolean]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("1.0")) == true)
  }

  test("Core gt") {
    val action = core.gt(inputExpr, 1.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Boolean]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("2.0")) == true)
  }

  test("Core gte") {
    val action = core.gte(inputExpr, 1.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Boolean]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("2.0")) == true)
  }

  test("Core net") {
    val action = core.net(inputExpr, 1.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Boolean]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("2.0")) == true)
  }

  test("Core max") {
    val action = core.max(inputExpr, 1.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("2.0")) == 2.0)
  }

  test("Core min") {
    val action = core.min(inputExpr, 1.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("2.0")) == 1.0)
  }

  test("Core pow") {
    val action = core.pow(inputExpr, 3.0)

    val pfaDoc = new PFABuilder()
      .withInput[Double]
      .withOutput[Double]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("2.0")) == 8.0)
  }

  test("Core not") {
    val action = core.not(inputExpr)

    val pfaDoc = new PFABuilder()
      .withInput[Boolean]
      .withOutput[Boolean]
      .withAction(action)
      .pfa

    val engine = getPFAEngine(pfaDoc.toJSON())
    assert(engine.action(engine.jsonInput("false")) == true)
  }

}
