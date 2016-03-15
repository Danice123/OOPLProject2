
class Page(val url : String) {
	val src = SearchEngine.fetch(url)
	val terms = SearchEngine.getTerms(src, (s : String) => s.length > 1)
	
	def numContains(word: String): Int = terms.count( _.toUpperCase == word.toUpperCase )

	
	def containsWord(word: String): Boolean = {
		for (i<-terms) {
			if (i.toUpperCase == word.toUpperCase) return true
		}
		
		return false
	}
}

class IndexedPages(val pages : List[Page]) extends Iterable[Page] {
    val items: scala.collection.mutable.Seq[Page] with scala.collection.generic.Growable[Page] = scala.collection.mutable.Buffer[Page]()
	
	//populate items
	for (i<-pages) { items += i }
	
	override def iterator = pages.iterator
	
	def numContaining(word : String): Double = {
		var total = 0.0
		for (i<-pages) {
			if (i.containsWord(word)) total += 1.0
		}
		total
	}
	
	//def search(q: Query): SearchResults = {
    //   val beforeWeights: SearchResults = //TODO: call superclass method [1 pts]
    //   val oldScores = beforeWeights.results.unzip._1
    //   
    //   val unnormScores = oldScores.zip(weights).map { (x) => (x._1 * x._2) }
    //   
    //   //Multiplying by weights may change the scale of the scores
    //   //Thus, newScores is unnormScores by the total of the unnormScores
    //   // (This is called "normalizing" the scores)
    //   val total = unnormScores.foldLeft(0.0) {_+_}
    //   val newScores = unnormScores.map { _ / total }
    //   
    //   // TODO: create and return adjusted SearchResults from newScores [4 pts]
    //   }
}

//Sometimes it makes sense to have a fixed, immutable set of indexed pages. In other situations, we might
//want to add to the pages as we crawl more of the web. If our default IndexedPages interface does not
//allow us to add new pages (which is a good idea in general), then we can expose that additional
//functionality by mixing in traits.
trait Augmentable[A] {
 val items: scala.collection.mutable.Seq[A] with scala.collection.generic.Growable[A]

//Complete the add method. It should return false if the item is already in the collection (in which case
//nothing is added) and true if it was a new item (in which case it is added to items) [2 pts].
 def add(newItem: A): Boolean = {
	if (items.contains(newItem)) return false
	else items += newItem
	
	return true
 }
}
