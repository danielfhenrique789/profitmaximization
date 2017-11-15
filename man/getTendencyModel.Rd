\name{getTendencyModel}
\alias{getTendencyModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get dataframe of tendencies.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getTendencyModel(en, CAs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{en}{
%%     ~~Describe \code{en} here~~
}
  \item{CAs}{
%%     ~~Describe \code{CAs} here~~
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
function (en, CAs) 
{
    tickets <- as.character(read.csv("objTickets.csv")[[1]])
    newObj <- new.env()
    for (ticket in tickets) {
        lengthDs <- nrow(en[[ticket]])
        newObj[[ticket]] <- en[[ticket]]
        newObj[[ticket]]$CA <- -1
        if (!is.null(lengthDs)) 
            if ((lengthDs - 12) > 0) 
                newObj[[ticket]][7:(lengthDs - 6), ]$CA <- CAs[[ticket]]$CA[1:(lengthDs - 
                  12)]
        newObj[[ticket]]$Tendency <- ifelse((newObj[[ticket]]$MA.Fast > 
            newObj[[ticket]]$MA.Slow) & (newObj[[ticket]]$CA > 
            0), "up", "down")
    }
    return(newObj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
