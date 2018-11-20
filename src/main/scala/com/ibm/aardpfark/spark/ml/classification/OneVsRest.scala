package com.ibm.aardpfark.spark.ml.classification

import com.ibm.aardpfark.pfa.document.{NamedCell, PFABuilder, PFADocument}
import com.ibm.aardpfark.pfa.expression.PFAExpression
import com.ibm.aardpfark.pfa.types.WithSchema
import com.ibm.aardpfark.spark.ml.{PFAModel, PFAModelSet}
import org.apache.avro.SchemaBuilder
import org.apache.spark.ml.classification._

class PFAOneVsRestModel(val sparkTransformer: OneVsRestModel) extends PFAModelSet {
  import com.ibm.aardpfark.pfa.dsl._

  private val predictionCol = sparkTransformer.getPredictionCol
  private val models = sparkTransformer.models

  // TODO: Figure out how to do this implicilty with Cell Case Class
  private def convertOne(model: ClassificationModel[_, _]): PFAModel[_] = model.getClass.getSimpleName match {
    case "LogisticRegressionModel" => new PFALogisticRegressionModel(model.asInstanceOf[LogisticRegressionModel])
    case "DecisionTreeClassificationModel" => new PFADecisionTreeClassificationModel(model.asInstanceOf[DecisionTreeClassificationModel])
    case "GBTClassificationModel" => new PFAGBTClassificationModel(model.asInstanceOf[GBTClassificationModel])
    case "LinearSVCModel" => new PFALinearSVCModel(model.asInstanceOf[LinearSVCModel])
    case "MLPClassifier" => new PFAMultilayerPerceptronClassificationModel(model.asInstanceOf[MultilayerPerceptronClassificationModel])
    case "NaiveBayesModel" => new PFANaiveBayesModel(model.asInstanceOf[NaiveBayesModel])
    case "RandomForestClassificationModel" => new PFARandomForestClassificationModel(model.asInstanceOf[RandomForestClassificationModel])
    case unknownModel: String => throw new RuntimeException(s"Couldn't translate model type ${unknownModel}")
  }

  private val pfaModels: Array[PFAModel[_]] = models.map(convertOne)

  override protected def inputSchema = {
    SchemaBuilder.record(withUid(inputBaseName)).fields()
      .name(sparkTransformer.getFeaturesCol).`type`().array().items().doubleType().noDefault()
      .endRecord()
  }

  override def outputSchema = SchemaBuilder.record(withUid(outputBaseName)).fields()
    .name(predictionCol).`type`.doubleType().noDefault()
    .endRecord()

  override def modelCells: Seq[NamedCell[WithSchema]] = pfaModels.map(_.getModelCell.asInstanceOf[NamedCell[WithSchema]])

  private val modelArray = Let("modelArray",NewArray[Double](Seq()))
  private val scoreModels = ExprSeq(pfaModels.zipWithIndex.map{case (model,index) => Set(modelArray.ref,a.append(modelArray.ref, model.getAction))})
  private val pred = Let("pred", a.argmax(modelArray.ref))

  override def action: PFAExpression = {
    Action(
      modelArray,
      scoreModels,
      NewRecord(outputSchema, Map(
        predictionCol -> pred.ref)
      )
    )
  }

  override def pfa: PFADocument = {
    PFABuilder()
      .withName(sparkTransformer.uid)
      .withMetadata(getMetadata)
      .withInput(inputSchema)
      .withOutput(outputSchema)
      .withCells(modelCells)
      .withAction(action)
      .pfa
  }
}
