docTermMatrixAsMatrix <- function(data, tokenize, min, max){
  require("tm")
  require("RWeka")
  
  if(tokenize == TRUE){
    
    new_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = min, max = max))
    dtm = DocumentTermMatrix(data, control = list(tokenize = new_tokenizer))
  }else{
    dtm = DocumentTermMatrix(data)
  }
  
  return(as.matrix(dtm))
  
}