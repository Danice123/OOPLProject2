
class SearchResults(val query: Query, val pages: IndexedPages) {

	def calcIdf(word: String): Double = {
		var numOccurences: Double = 0.0
		for (p<-pages; if (p.containsWord(word))) numOccurences += 1.0
		if (numOccurences == 0.0) return 0.0
		
		return scala.math.log(pages.items.length /numOccurences)			
	}
	
	def calcTF(word: String, page: Page): Double = page.numContains(word)/page.terms.length.toDouble
	
	def sumWordWeights(page: Page): Double = {
		var totalWeight: Double = 0.0
		query.items.foreach( i => totalWeight += calcIdf(i) * calcTF(i, page) )

		return totalWeight
	}

	def results(): Iterable[(Double, String)] = { 
		val result = for (p<-pages) yield (sumWordWeights(p), p.url)
		return result.toList.sortWith( _._1 > _._1 )
	}
	
	def printTop(n: Int): Unit = {
		results.take(n).foreach(x => println( x._1 + ", " + x._2))
	}	
}

class SearchResultsQWeighted(override val query : WeightedQuery, override val pages : IndexedPages) extends SearchResults(query, pages) {
	override def sumWordWeights(page: Page): Double = {
		var totalWeight: Double = 0.0
		query.items.foreach( i => totalWeight += calcIdf(i) * calcTF(i, page) * query.weighting(i))

		return totalWeight
	}
}

class SearchResultsWeighted(override val query : Query, override val pages : IndexedPages, val over : Iterable[(Double, String)]) extends SearchResults(query, pages) {
	override def results() : Iterable[(Double, String)] = over.toList.sortWith( _._1 > _._1 )
}