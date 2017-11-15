\name{getCATickets}
\alias{getCATickets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get Tickets angular coefficients.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getCATickets(tickets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tickets}{
%%     ~~Describe \code{tickets} here~~
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
function (tickets) 
{
    tryCatch({
        wTickets <- as.data.frame(tickets[1:7])
        wlm <- c(summary(lm(wTickets[, 1] ~ index(wTickets[, 
            1])))$coefficients[2])
        if ((NROW(tickets) - 6) > 1) {
            return(c(wlm, getCATickets(tickets[-1])))
        }
        else {
            return(wlm)
        }
    }, error = function(e) {
        print(paste("Error:", e))
        return(c(-1))
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
