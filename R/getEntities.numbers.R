getEntities.numbers <- function(data){
  require("NLP")
  findEntities.string <- function(s){
    g = strsplit(gsub("[[:alpha:]]", "", s), " ")
    l = lapply(g[[1]], function(x) ifelse(nchar(gsub("[[:punct:]]", "", x))==0, TRUE, FALSE))
    return(g[[1]][which(l ==FALSE)])
  }
  entities = list()
  for(i in 1:length(data)){
    docAsString = apply(as.matrix(data[[i]]$content), 1, FUN=as.String) # Converts the text content a corpus element to a string
    entities[[i]] = apply(as.matrix(docAsString), 1, findEntities.string) # Passes each converted string element through the string punctuation removal function and reassigns to the passed corpus
  }
  return(entities)
}
