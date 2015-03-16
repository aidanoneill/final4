plotDendogram <-
  function(data){
    
    dissimilarityMatrix = dist(as.matrix(data))
    clusters = hclust(dissimilarityMatrix, method="average")
    return(plot(clusters, xlab="Documents", main="Document Clusters Dendogram"))
    
  }

