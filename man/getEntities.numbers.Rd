\name{getEntities.numbers}
\alias{getEntities.numbers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a corpus as a html table
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getEntities.numbers(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
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
function(data){
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
