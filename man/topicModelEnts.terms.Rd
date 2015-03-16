\name{topicModelEnts.terms}
\alias{topicModelEnts.terms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify entities most associated with each topic
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
topicModelEnts.terms(data, ents, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{ents}{
%%     ~~Describe \code{data} here~~
}
  \item{k}{
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
function(data, ents, k){
    require("topicmodels")
    
    lda.ents = LDA(x = as.matrix(data)[,matchEntitiesToDTM(data, ents)], 
                   k = k, method = "VEM")
    
    return(terms(lda.ents, 10))
    
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
