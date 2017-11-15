\name{analiseTendencia}
\alias{analiseTendencia}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Tendency Analise
}
\description{
  Verify if some ticket are in a up case or down case.
}
\usage{
analiseTendencia(obj, vFast, vSlow)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{vFast}{
%%     ~~Describe \code{vFast} here~~
}
  \item{vSlow}{
%%     ~~Describe \code{vSlow} here~~
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
function (obj, vFast, vSlow) 
{
    tickets <- as.character(read.csv("objTickets.csv")[[1]])
    newObj <- new.env()
    for (ticket in tickets) {
        tryCatch({
            dataRange <- obj[[ticket]][!is.na(obj[[ticket]]$Adjusted), 
                ]
            if (nrow(dataRange) > vSlow) {
                ma <- SMA(dataRange$Adjusted, n = vSlow)
                mar <- SMA(dataRange$Adjusted, n = vFast)
                newObj[[ticket]] <- data.frame(dataRange[vSlow:nrow(dataRange), 
                  ]$Date, dataRange[vSlow:nrow(dataRange), ]$Adjusted, 
                  ma[vSlow:nrow(dataRange)], mar[vSlow:nrow(dataRange)])
                names(newObj[[ticket]]) <- c("Date", "Ticket", 
                  "MA.Slow", "MA.Fast")
                newObj[[ticket]]$Tendency <- ifelse((newObj[[ticket]]$MA.Slow < 
                  newObj[[ticket]]$MA.Fast), "up", "down")
            }
        }, error = function(e) {
            print("Error:")
            print(e)
        })
    }
    return(newObj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
