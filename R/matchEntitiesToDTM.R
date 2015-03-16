matchEntitiesToDTM <-
  function(data, ents){
    require("NLP")
    terms = getTerms(data) # passed a dtm
    
    ents = as.matrix(ents) 
    ents = ents[-which(unlist(ents)=="")]
    ents = paste(ents, collapse=" ")
    ents = tolower(as.String(ents))
    ents = unique(unlist(strsplit(ents, split=" ")))
    
    matches = lapply(ents, grep, terms)
    matches = unique(unlist(matches))
    
    return(matches)
    
  }