getTerms <-
  function(data){
    require("tm")
    
    return(data$dimnames$Terms)
    
  }
