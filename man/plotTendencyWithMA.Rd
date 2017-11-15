\name{plotTendencyWithMA}
\alias{plotTendencyWithMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot tendency with moving avarage.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotTendencyWithMA(ds)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ds}{
%%     ~~Describe \code{ds} here~~
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
function (ds) 
{
    ggplot() + geom_line(data = ds, aes(x = index(Date), y = Ticket, 
        colour = "Tickets")) + geom_line(data = ds, aes(x = index(Date), 
        y = MA.Slow, colour = "M\303\251dia lenta")) + geom_line(data = ds, 
        aes(x = index(Date), y = MA.Fast, colour = "M\303\251dia r\303\241pida")) + 
        ylab("Cota\303\247\303\243o")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
