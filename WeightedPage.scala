
trait Weighted[A] {
	val items: Iterable[A]
	val weightingFn: A => Double
 
	def weights: Iterable[Double] = {
		items.map(weightingFn(_))
	}
 
	def totalWeight: Double = {
		weights.fold[Double](0)((v : Double, i : Double) => v + i)
	}
 
	def sumIf(p: A => Boolean): Double = {
		items.filter(p).map(weightingFn(_)).fold[Double](0)((v : Double, i : Double) => v + i)
	}
}

class WeightedPages(override val items : scala.collection.mutable.Seq[Page] with scala.collection.generic.Growable[Page]) extends IndexedPages(items) with Weighted[Page] {
	def weighting(page : Page) : Double = { 1.0 / page.url.length }
	val weightingFn : Page => Double = weighting
	
	override def numContaining(word: String): Double = {
		sumIf( (p: Page) => p.containsWord(word) )
	}
	
	override def search(query: Query): SearchResults = {
		val beforeWeights: SearchResults = super.search(query)
		val oldScores = beforeWeights.results.unzip._1
		val unnormScores = oldScores.zip(weights).map { (x) => (x._1 * x._2) }
       
		//Multiplying by weights may change the scale of the scores
		//Thus, newScores is unnormScores by the total of the unnormScores
		// (This is called "normalizing" the scores)
		val total = unnormScores.foldLeft(0.0) {_+_}
		val newScores = unnormScores.map { _ / total }
		// TODO: create and return adjusted SearchResults from newScores [4 pts]
		return new SearchResultsWeighted(query, this, newScores.zip(beforeWeights.results.unzip._2))
  }
}
