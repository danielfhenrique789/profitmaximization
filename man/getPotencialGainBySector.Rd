\name{getPotencialGainBySector}
\alias{getPotencialGainBySector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get potential profit gain by sector.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPotencialGainBySector(obj, regr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{regr}{
%%     ~~Describe \code{regr} here~~
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
function (obj, regr) 
{
    sectors <- getSectors(regr)
    newObj <- new.env()
    df <- data.frame()
    for (sector in sectors) {
        tickets <- getTicketsBySetor(sector, regr)
        tickets <- tickets[!is.na(tickets)]
        if (!is.na(tickets[1])) {
            year <- obj[[as.character(tickets[1])]]$year
            gain <- (1:nrow(obj[[as.character(tickets[1])]])) * 
                0
            for (ticket in tickets) {
                ds <- obj[[ticket]]
                if (!is.null(ds)) {
                  for (idx in 1:nrow(ds)) {
                    gain <- gain + ds$gain
                  }
                }
            }
        }
        df <- rbind(df, data.frame(year, (gain/NROW(tickets)), 
            sector))
        newObj[[sector]] <- df
    }
    names(df) <- c("year", "gain", "sector")
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
