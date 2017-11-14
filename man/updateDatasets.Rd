\name{updateDatasets}
\alias{updateDatasets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
updateDatasets()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    tryCatch({
        tickets <- as.character(read.csv("tempTickets.csv")[[1]])
        print(NROW(tickets))
        for (tticket in tickets) {
            tickets <- tickets[tickets != as.character(tticket)]
            ticket <- paste(tticket, ".SA", sep = "")
            if (file.exists(paste("./Datasets/", ticket, ".csv", 
                sep = ""))) {
            }
            dataset <- as.data.frame(getSymbols(Symbols = ticket, 
                auto.assign = F, symbol.lookup = TRUE))
            setDT(dataset, keep.rownames = TRUE)[]
            dataset <- formateTicketDf(dataset)
            dataset$Amplitude <- dataset$High - dataset$Low
            write.csv(dataset, file = paste("Datasets/", ticket, 
                ".csv", sep = ""), row.names = FALSE)
        }
    }, error = function(e) {
        print("Error:")
        print(e)
        write.csv(tickets, file = "tempTickets.csv", row.names = FALSE)
        updateDatasets()
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
