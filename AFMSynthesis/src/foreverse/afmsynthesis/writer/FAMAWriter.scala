package foreverse.afmsynthesis.writer

import java.io.File

import foreverse.afmsynthesis.afm.AttributedFeatureModel

trait FAMAWriter {

  def write(afm : AttributedFeatureModel, file : File)
  
}
