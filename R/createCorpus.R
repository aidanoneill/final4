createCorpus <-
function(data){
  require("tm")
  
  data.frameSource <- DataframeSource(data)
  
  data.corpus = VCorpus(data.frameSource, readerControl = list(language="en"))
  
  return(data.corpus)
  
}
