package com.ibm.aardpfark.spark.ml.classification

import com.ibm.aardpfark.spark.ml.classification.PFAOneVsRestModel
import com.ibm.aardpfark.pfa.ClassifierResult
import org.apache.spark.ml.classification.{LogisticRegression, OneVsRest}

class OneVsRestSuite extends SparkClassifierPFASuiteBase[ClassifierResult] {

  val inputPath = "data/sample_multiclass_classification_data.txt"
  val data = spark.read.format("libsvm").load(inputPath)
  val lr = new LogisticRegression()
    .setElasticNetParam(0.0)

  val ovr = new OneVsRest().setClassifier(lr)

  override val sparkTransformer = ovr.fit(data)
  val testThing = new PFAOneVsRestModel(sparkTransformer)
  println(testThing.pfa.toJSON(true))
  val result = sparkTransformer.transform(data)
  override val input = withColumnAsArray(result, ovr.getFeaturesCol).toJSON.collect()
  override val expectedOutput = result.select(ovr.getPredictionCol)
    .toJSON.collect()

  //TODO Test with raw prediction and probability
}


