package foreverse.afmsynthesis.solver

import foreverse.afmsynthesis.afm.VariationPoint
import foreverse.afmsynthesis.algorithm.AFMSynthesisProblem
import foreverse.afmsynthesis.algorithm.BinaryImplicationGraph
import foreverse.afmsynthesis.input.ProductList
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.afm.Attribute

class SATProductListSolver (
	productList : ProductList 
) extends AFMSynthesisProblem(Nil,Nil) {
  
  features = productList.variationPoints.map(_ match {
    case vp : Feature => Some(vp)
    case _ => None
  }).flatten
  
  attributes = productList.variationPoints.map(_ match {
    case vp : Attribute => Some(vp)
    case _ => None
  }).flatten
  
  
  override def removeDeadFeatures() {
    
  }
  
  override def computeBinaryImplicationGraph() : BinaryImplicationGraph = {
    val big = new BinaryImplicationGraph
    
    big
  }
  
}