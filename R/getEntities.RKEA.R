getEntities.RKEA <- 
  function(data, trainingCorpus, trainingKeywords){
    require("RKEA")
    require("RWeka")
    require("rJava")
    
    tmpdir <- tempfile()
    dir.create(tmpdir)
    model <- file.path(tmpdir, "RKEAModel")
    
    train.corpus.preproc = preprocess(data = createCorpus(trainingCorpus), 
                                      removewhitespace = TRUE, removepunctuation = TRUE, removestopwords = TRUE, 
                                      tolowercase = FALSE, stemming = FALSE)
    
    createModel(corpus = train.corpus.preproc, keywords = trainingKeywords, model = model)
    
    return(extractKeywords(data, model))
  
}