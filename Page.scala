class Page(val url : String) {

	def filterFunction(s: String): Boolean = {
		val result = s match {
	
			//decently common html
			case "col" => false
			case "br" => false
			case "colgroup" => false
			case "dd" => false
			case "em" => false
			case "hr" => false
			case "ol" => false
			case "rp" => false
			case "td" => false
			case "textarea" => false
			case "tfoot" => false
			case "tbody" => false
			case "sup" => false
			case "ul" => false
			case "wbr" => false
			
			//articles and conjunctions
			case "the"     => false
			case "and"     => false
			case "or"      => false
			case "either"  => false
			case "neither" => false
			case "who"     => false
			case "what"    => false
			case "where"   => false
			case "why"     => false
			case "when"    => false
			
			//string is too short
			case _ => true
		}
		if (s.size <= 1) return false
		return result
	}
	
	val terms = SearchEngine.getTerms(SearchEngine.fetch(url), filterFunction)
	val html = SearchEngine.fetch(url)
	val links = SearchEngine.getLinks(html, url)
	
	def numContains(word: String): Int = terms.count( _.toUpperCase == word.toUpperCase )
	def containsWord(word: String): Boolean = terms.exists { _.toUpperCase == word.toUpperCase }
	
	override def hashCode = url.hashCode
	override def equals(other: Any) = other match {
		case that : Page => this.url == that.url
		case _ => false
	}
	
	def fracMatching(term : String) : Double = {
		(for (s <- terms if term.compareToIgnoreCase(s) == 0) yield s).size / terms.size.toDouble
	}
	
	def fracMatching(list : List[String]) : Double =  {
		list.size match {
			case 0 => 0
			case 1 => fracMatching(list.last)
			case _ => fracMatching(list.last) + fracMatching(list.take(list.size - 1))
		}
	}
}

class IndexedPages(val items : scala.collection.mutable.Seq[Page] with scala.collection.generic.Growable[Page]) extends Iterable[Page] {
	
	override def iterator = items.iterator
	
	def numContaining(word : String): Double = {
		(for (i <- items if i.containsWord(word)) yield i).size
	}
	
	def search(q : Query) : SearchResults = q match {
		case weighted : WeightedQuery => new SearchResultsQWeighted(weighted, this)
		case query : Query => new SearchResults(query, this)
	}
}

trait Augmentable[A] {
	val items: scala.collection.mutable.Seq[A] with scala.collection.generic.Growable[A]

	def add(newItem: A): Boolean = {
		if (items.contains(newItem)) return false
		items += newItem
		return true
	}
}
