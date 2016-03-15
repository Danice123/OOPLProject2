class Query(val items : Iterable[String])

class WeightedQuery(override val items: Iterable[String]) extends Query(items) with Weighted[String]{

	def weighting(s : String) : Double = { s.length / (s.length + 5.0) }
	val weightingFn : String => Double = weighting	
}