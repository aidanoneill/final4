topicModelEnts.terms <-
  function(data, ents, k){
    require("topicmodels")
    
    lda.ents = LDA(x = as.matrix(data)[,matchEntitiesToDTM(data, ents)], 
                   k = k, method = "VEM")
    
    return(terms(lda.ents, 10))
    
  }