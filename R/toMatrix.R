toMatrix <-
  function(data){
    require("tm")
    
    docs = lapply(data, function(x) (x[1])) # Extracts document contents
    vec = unlist(docs) # Converts from nested lists to a single vector
    mat = matrix(data = vec, nrow=length(data), byrow=T)
    
    return(mat)
    
  }
