package synthesis

class AFMSynthesizer {

	def synthesize(features : List[String], products : List[Map[String, String]]) {
		
		val domains = (for (feature <- features) yield {
		  val domain = products.map(p => p.get(feature).get)
		  (feature, domain.distinct)
		}).toMap
		
		println(features)
		println(products)
		domains.foreach(d => println(d._1 + " : " + d._2.mkString("{",", ","}") + " (" + d._2.forall(v => v.matches("\\d+")) + ")"))
		
	}
}