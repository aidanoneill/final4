\name{getEntities.RKEA}
\alias{getEntities.RKEA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getEntities.RKEA
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getEntities.RKEA(data, trainingCorpus, trainingKeywords)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{trainingCorpus}{
%%     ~~Describe \code{data} here~~
}
  \item{trainingKeywords}{
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
function(data, trainingCorpus, trainingKeywords){
  require("RKEA")
  
  tmpdir <- tempfile()
  dir.create(tmpdir)
  model <- file.path(tmpdir, "RKEAModel")
  
  train.corpus.preproc = preprocess(data = createCorpus(trainingCorpus), 
                                    removewhitespace = TRUE, removepunctuation = FALSE, removestopwords = FALSE, 
                                    tolowercase = FALSE, stemming = FALSE)
  
  createModel(corpus = train.corpus.preproc, keywords = trainingKeywords, model = model)
  
  return(extractKeywords(data, model))
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
