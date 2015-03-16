\name{plotDendogram.entities}
\alias{plotDendogram.entities}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a dendogram of clustered entities
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotDendogram.entities(data, ents, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{ents}{
%%     ~~Describe \code{data} here~~
}
  \item{n}{
%%     ~~Describe \code{data} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
 function(data, ents, n){
    
    mat = as.matrix(data)[,matchEntitiesToDTM(data = data, ents = ents)]
    sums = apply(mat, 2, sum)
    term.index = tail(sort(sums, index.return=TRUE)$ix, n)
    mat = mat[, term.index]
    
    dissimilarity = dist(t(mat))
    clusters = hclust(dissimilarity)
    return(plot(clusters, xlab="Entities", main="Entity Clusters Dendogram"))
    
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
