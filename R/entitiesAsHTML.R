entitiesAsHTML <-
  function(data){
    
    ent = getEntities(data)
    
    ents = lapply(ent, function(x) unlist(lapply(unlist(x), paste, collapse="    ")))
    ents = lapply(lapply(ent, unlist), paste, collapse="    ")
    
    return(matrixAsHTML(as.matrix(ents)))
    
  }