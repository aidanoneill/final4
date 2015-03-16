\name{matchEntitiesToDTM}
\alias{matchEntitiesToDTM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finds the index in the DTM of matches between DTM terms and entities
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
matchEntitiesToDTM(data, ents)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{ents}{
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
  function(data, ents){
    require("NLP")
    terms = getTerms(data) # passed a dtm
    
    ents = as.matrix(ents) 
    ents = ents[-which(unlist(ents)=="")]
    ents = paste(ents, collapse=" ")
    ents = tolower(as.String(ents))
    ents = unique(unlist(strsplit(ents, split=" ")))
    
    matches = lapply(ents, grep, terms)
    matches = unique(unlist(matches))
    
    return(matches)
    
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
