import org.apache.http._
import org.apache.http.client.entity._
import org.apache.http.client.methods._
import org.apache.http.impl.client._
import org.apache.http.client.utils._
import org.apache.http.message._
import org.apache.http.params._
import java.net.URL
import org.apache.http.client.HttpResponseException



object SearchEngine extends App {
	
	//val p = new Page("http://www.randyconnolly.com/tests/process.php")
	//val p2 = new Page("http://www.tfidf.com/")
	//
	//val y = new IndexedPages(List(p,p2))
	//val q: Query = new Query(List("Frequency", "no"))
	//
	//val test = new SearchResults(q, y)
	//
	//val list = test.results()
	//test.printTop(2)
	
	def getLinks( html : String , baseURL : String) : List[String] = {
		// See http://www.mkyong.com/regular-expressions/how-to-extract-html-links-with-regular-expression/ for explanation of regex
		val aTagRegex = """(?i)<a([^>]+)>(.+?)</a>""".r
		val urlRegex = """\s*(?i)href\s*=\s*(\"([^"]*\")|'[^']*'|([^'">\s]+))""".r
		
		val opts = for ( a <- aTagRegex findAllMatchIn html ) yield urlRegex.findFirstMatchIn(a.toString)
		
		val hrefs = opts collect { case Some(x) => x group 1 }
		
		// remove leading and trailing quotes, if any
		val cleaned = hrefs map { _.stripPrefix("\"").stripPrefix("\'").stripSuffix("\"").stripPrefix("\'") } filter { ! _.startsWith("javascript") }
		
		// Use Java's URL class to parse the URL
		//   and get the full URL string (including implied context)
		val contextURL = new java.net.URL(baseURL)
		
			def getURL(x: String) = {
				var result = ""
				try {
					result = new java.net.URL(contextURL, x).toString()
				}
				catch {
					case e: java.net.MalformedURLException => Unit
				}
				result
			}
				
		(cleaned map { getURL(_) } ).filter(_.length > 0).toList        
	}
	
	def fetch(url : String) : String = {
		try { new DefaultHttpClient().execute(new HttpGet(url), new BasicResponseHandler()) } catch {
			case hre : HttpResponseException => {
				print("There has been an error")
				""
			}
		}
	}
	
	def getTerms(html : String, sort : String => Boolean) : List[String] = {
		for (s <- html.split("[^a-zA-Z0-9]").toList if s != "" && sort(s)) yield s
	}
	
	//mode should be "read" or "augment": for "read", no mixins are needed; for "augment", Augmentable
	//should be mixed in to the returned object [4 pts].
	//If weight is true, then the returned object should be a WeightedIndexedPages [1 pts]. If weight is false, a
	//plain IndexedPages is returned [1 pts].
	
	def crawlAndIndex(startUrl: String, maxPages: Int, mode: String = "read", weight: Boolean = true): IndexedPages = {
		
	
		return new IndexedPages(Nil) with Augmentable[Page]
	}

	//Old version
	//def crawlAndIndex(url : String, maxPages : Int) : List[PageSummary] = {
	//	crawlAndIndex(url, maxPages, List[PageSummary]())
	//}
	
	//def crawlAndIndex(url : String,  maxPages : Int, list : List[PageSummary]) : List[PageSummary] = {
	//	val html = fetch(url)
	//	val links = getLinks(html, url)
	//	var newList = new PageSummary(url, getTerms(html, (s : String) => s.length > 1)) :: list
	//	for (link <- links if !(for (p <- newList) yield p.url).contains(link) && newList.size < maxPages) {
	//		newList = crawlAndIndex(link, maxPages, newList)
	//	}
	//	newList
	//}
}