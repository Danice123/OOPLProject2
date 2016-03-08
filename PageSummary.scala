
class PageSummary(val url : String, val terms : List[String]) {
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