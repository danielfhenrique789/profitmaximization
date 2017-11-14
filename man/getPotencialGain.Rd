\name{getPotencialGain}
\alias{getPotencialGain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPotencialGain(obj, from, to)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{from}{
%%     ~~Describe \code{from} here~~
}
  \item{to}{
%%     ~~Describe \code{to} here~~
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
function (obj, from, to) 
{
    tickets <- as.character(read.csv("objTickets.csv")[[1]])
    newObj <- new.env()
    for (ticket in tickets) {
        print(ticket)
        ds <- obj[[ticket]]
        lfrom <- from
        year <- c()
        gain <- c()
        if (class(ds) == "data.frame") {
            while (as.numeric(lfrom) <= as.numeric(to)) {
                gain <- c(gain, getPotencialYearGain(ds, lfrom))
                year <- c(year, lfrom)
                lfrom <- as.character(as.numeric(lfrom) + 1)
            }
            newObj[[ticket]] <- data.frame(year, gain)
        }
    }
    return(newObj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
