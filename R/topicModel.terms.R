topicModel.terms <-
  function(data, k){
    require("topicmodels")
    
    lda = LDA(x = data, k = k, method = "VEM")
    return(terms(lda, 10)) # which documents belong to which topic
    
  }