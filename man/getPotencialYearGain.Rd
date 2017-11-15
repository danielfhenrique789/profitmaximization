\name{getPotencialYearGain}
\alias{getPotencialYearGain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get potential profit gain in a year.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPotencialYearGain(ds, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ds}{
%%     ~~Describe \code{ds} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
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
function (ds, year) 
{
    from <- paste(year, "-01-01", sep = "")
    to <- paste(year, "-12-31", sep = "")
    lucro <- 0
    ds <- ds[as.character(ds$Date) >= from & as.character(ds$Date) <= 
        to, ]
    if (nrow(ds) > 0) {
        first <- head(ds, 1)
        tendency <- first$Tendency
        buy <- ifelse(tendency == "up", first$Ticket, 0)
        sell <- 0
        for (idx in 1:nrow(ds)) {
            day <- ds[idx, ]
            if (day$Tendency != tendency) {
                tendency <- day$Tendency
                if (day$Tendency == "up") {
                  buy <- day$Ticket
                }
                else {
                  if (buy != 0) {
                    sell <- day$Ticket
                    lucro <- lucro + ((sell/buy) - 1) * 100
                    buy <- 0
                  }
                }
            }
        }
        if (buy != 0) {
            sell <- day$Ticket
            lucro <- lucro + ((sell/buy) - 1) * 100
        }
    }
    return(lucro)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
