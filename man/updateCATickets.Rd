\name{updateCATickets}
\alias{updateCATickets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Update ticket's angular coefficients.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
updateCATickets(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
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
function (obj) 
{
    tickets <- as.character(read.csv("objTickets.csv")[[1]])
    newObj <- new.env()
    count <- NROW(tickets)
    for (ticket in tickets) {
        tryCatch({
            print(ticket)
            dataRange <- obj[[ticket]][!is.na(obj[[ticket]]$Ticket), 
                ]
            newObj[[ticket]]$Date <- dataRange$Date
            newObj[[ticket]]$CA <- sapply(1:nrow(dataRange), 
                function(x) {
                  ve <- as.data.frame(dataRange[x:(x + 7), ])
                  return(summary(lm(ve$Ticket ~ index(ve$Ticket)))$coefficients[2])
                })
            print(count)
            count <- count - 1
            write.csv(newObj[[ticket]], file = paste("Analises/Tendencias/CAs/", 
                ticket, ".csv", sep = ""), row.names = FALSE)
        }, error = function(e) {
            print(paste("Error:", e))
        })
    }
    return(newObj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
