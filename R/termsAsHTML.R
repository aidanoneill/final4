termsAsHTML <- function(data){
  terms = getTerms(data)
  terms = lapply(terms, function(x)  paste("<option value='", x, "'>", x, "</option>", sep=""))
  return(paste(paste(terms, callapse=""), collapse=" "))
}