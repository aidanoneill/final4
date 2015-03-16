plotDendogram.entities <-
  function(data, ents, n){
    
    mat = as.matrix(data)[,matchEntitiesToDTM(data = data, ents = ents)]
    sums = apply(mat, 2, sum)
    term.index = tail(sort(sums, index.return=TRUE)$ix, n)
    mat = mat[, term.index]
    
    dissimilarity = dist(t(mat))
    clusters = hclust(dissimilarity)
    return(plot(clusters, xlab="Entities", main="Entity Clusters Dendogram"))
    
  }