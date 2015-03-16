getEntityList <-
  function(data){
    
    ents = unlist(getEntities(data))
    
    return(unique(ents))
    
  }