toDocTermMatrix <-
function(data, tokenize, min, max){
  require("tm")
  require("RWeka")
  
  if(tokenize == TRUE){
    #options(mc.cores=1)
    new_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = min, max = max))
    return(DocumentTermMatrix(data, control = list(tokenize = new_tokenizer)))
  }
  return(DocumentTermMatrix(data))
  
}
