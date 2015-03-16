matchedTerms <-
  function(data, ents){
    
    matches = matchEntitiesToDTM(data, ents)
    matchedTerms = getTerms(data)[matches]
    
    return(matchedTerms)
    
  }