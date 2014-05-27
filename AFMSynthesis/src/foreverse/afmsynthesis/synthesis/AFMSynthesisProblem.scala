package foreverse.afmsynthesis.synthesis

import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.Feature

abstract class AFMSynthesisProblem(
    initFeatures : List[Feature],
    initAttributes : List[Attribute]
){

  protected var _features : List[Feature] = initFeatures
  def features = _features
  
  protected var _attributes : List[Attribute] = initAttributes
  def attributes = _attributes
  
  def removeDeadFeatures()
  
//  def computeBinaryImplicationGraph()
//  
//  def computeMutexGraph()
//  
//  def computeMutexGroups()
//  
//  def computeOrGroups()
//  
//  def computeXOrGroups()
//  
//  def computePossibleFeaturesForAttributes() //: Map[Attribute,List[Feature]]
// 
//  def computeReadableConstraints()
  
}