toTermDocMatrix <-
function(data, tokenize){
  require("tm")
  require("RWeka")
  
  if(tokenize == TRUE)
    return(TermDocumentMatrix(data, control = list(tokenize = NGramTokenizer)))
  return(TermDocumentMatrix(data))
  
}
