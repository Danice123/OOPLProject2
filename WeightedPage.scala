
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

class WeightedPages(val items : List[Page]) extends IndexedPages(items) with Weighted[Page] {
	def weighting(page : Page) : Double = { 1.0 / page.url.length }
	val weightingFn : Page => Double = weighting
	
	
}