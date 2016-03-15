//Given a Query and a set of IndexedPages, your program should assign a score to each Page, with higher
//scores indicating a stronger match between the Query and the Page.
class SearchResults(val query: Query, val pages: IndexedPages) {
    //You get to pick which scoring method you use. TF-IDF is one good option you can research.
	
	//The string in each tuple is the url of the web page the double is the score for the page
	//sorted in decreasing order
	
	
	//uses td-idf --- if you want to look at the site I used http://www.tfidf.com/
	def results(): Iterable[(Double, String)] = { 
		
		def calcIdf(word: String): Double = {
			var numOccurences: Double = 0.0
			
			for (p<-pages; if (p.containsWord(word))) numOccurences += 1.0
						
			if (numOccurences == 0.0) return 0.0
					
			return scala.math.log(pages.items.length /numOccurences)			
		}
		
		def calcTF(word: String, page: Page): Double = {
			//println(word + " in " + page.url)
			//println(page.numContains(word))
			return page.numContains(word)/page.terms.length.toDouble
		}
		
		def sumWordWeights(page: Page): Double = {
			var totalWeight: Double = 0.0
			query.items.foreach( i => totalWeight += calcIdf(i) * calcTF(i, page) )

			return totalWeight
		}
		
	
		val result = for (p<-pages) yield (sumWordWeights(p), p.url)
		
		return result.toList.sortWith( _._1 > _._1 )
		
	}
	
	//SearchResults should also have a printTop(n: Int): Unit method that prints the top n results to the
	//console, one on each line, first the score, then the URL [2 pts].
	def printTop(n: Int): Unit = {
		results.take(n).foreach(x => println( x._1 + ", " + x._2))
	}	
}