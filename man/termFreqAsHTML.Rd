\name{termFreqAsHTML}
\alias{termFreqAsHTML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Present term frequencies between max and min as html code
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
termFreqAsHTML(data, min, max)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
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
termFreqAsHTML <- function(data, min, max){
  term_totals = apply(as.matrix(data), 2, sum)
  terms = term_totals[which(term_totals>=min & term_totals<=max)]
  terms = terms[order(terms, decreasing=TRUE)]
  return(matrixAsHTML(as.matrix(terms)))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
