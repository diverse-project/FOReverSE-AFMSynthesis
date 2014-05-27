package foreverse.afmsynthesis.synthesis

import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.Feature

abstract class AFMSynthesisProblem(
    initFeatures : List[Feature],
    initAttributes : List[Attribute]
){

  var features : List[Feature] = initFeatures
  var attributes : List[Attribute] = initAttributes
  
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