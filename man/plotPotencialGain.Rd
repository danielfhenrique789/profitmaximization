\name{plotPotencialGain}
\alias{plotPotencialGain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotPotencialGain(ds, regr, shiny)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ds}{
%%     ~~Describe \code{ds} here~~
}
  \item{regr}{
%%     ~~Describe \code{regr} here~~
}
  \item{shiny}{
%%     ~~Describe \code{shiny} here~~
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
function (ds, regr, shiny) 
{
    if (!shiny) {
        ggplot() + geom_line(data = ds[!is.na(ds$sector), ], 
            aes(x = year, y = gain, group = sector, color = sector)) + 
            scale_y_log10() + ylab("Cota\303\247\303\243o")
    }
    else {
        ui <- fluidPage(plotlyOutput("distPlot"))
        server <- function(input, output) {
            output$distPlot <- renderPlotly({
                ggplot() + geom_line(data = ds[!is.na(ds$sector), 
                  ], aes(x = year, y = gain, group = sector, 
                  color = sector)) + scale_y_log10() + ylab("Cota\303\247\303\243o")
            })
        }
        shinyApp(ui = ui, server = server)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
