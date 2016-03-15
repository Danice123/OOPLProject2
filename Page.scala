
class Page(val url : String) {

}

class IndexedPages(val items : List[Page]) extends Iterable[Page] {
	override def iterator = items.iterator
	
	def numContaining(word : String) : Double = {
		0
	}
}