
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

<<<<<<< HEAD
class WeightedPages(override val items : List[Page]) extends IndexedPages(items) with Weighted[Page] {
=======
class WeightedIndexedPages(val w_pages : List[Page]) extends IndexedPages(w_pages) with Weighted[Page] {
>>>>>>> 420c4f6354f42ab6b314ebab7fe261bfd0f59bac
	def weighting(page : Page) : Double = { 1.0 / page.url.length }
	val weightingFn : Page => Double = weighting
	
	override def numContaining(word: String): Double = {
		sumIf( (p: Page) => p.containsWord(word) )
	}
}
