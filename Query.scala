class Query(ls: Iterable[String]) {	
	val items = ls
}

class WeightedQuery(ls: Iterable[String]) extends Query(ls) with Weighted[String]{
	
	//the longer the term the greater the weight
    def weighting(s : String) : Double = { s.length / (s.length + 5.0) }
	val weightingFn : String => Double = weighting
	
	//You can pick a default weighting scheme for WeightedQuery, 
    //but it has to be different than uniform. 
    //That is, terms should have unequal weights [2 pts]. 
	
}