\name{docTermMatrixAsMatrix}
\alias{docTermMatrixAsMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return the terms held by a term document matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
docTermMatrixAsMatrix(data, tokenize, min, max)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{tokenize}{
%%     ~~Describe \code{data} here~~
}
  \item{min}{
%%     ~~Describe \code{data} here~~
}
  \item{max}{
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
docTermMatrixAsMatrix <- function(data, tokenize, min, max){
  require("tm")
  require("RWeka")
  
  if(tokenize == TRUE){
    
    new_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = min, max = max))
    dtm = DocumentTermMatrix(data, control = list(tokenize = new_tokenizer))
  }else{
    dtm = DocumentTermMatrix(data)
  }
  
  return(as.matrix(dtm))
  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
