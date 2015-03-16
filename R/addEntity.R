addEntity <-
  function(data, term){
    
    data[length(data)+1] = term
    
    return(data)
    
  }