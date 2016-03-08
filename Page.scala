
class Page(val url : String) {

}

class IndexedPages(var pages : List[Page]) extends Iterable[Page] {
	override def iterator = pages.iterator
	
	def numContaining(word : String) : Double = {
		0
	}
}