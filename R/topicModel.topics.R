topicModel.topics <-
  function(data, k){
    require("topicmodels")
    
    lda = LDA(x = data, k = k, method = "VEM")
    return(topics(lda)) # which documents belong to which topic

  }