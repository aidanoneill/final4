createCorpus.csv <-
function(file, header){
  require("tm")
  
  data <- read.csv(file, header = header, stringsAsFactors=FALSE)
  data.frameSource <- DataframeSource(data)
  data.corpus = VCorpus(data.frameSource, readerControl = list(language="en"))
  return(data.corpus)
  
}
