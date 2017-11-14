\name{updateVarCATicket}
\alias{updateVarCATicket}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
updateVarCATicket(pDf, dfName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pDf}{
%%     ~~Describe \code{pDf} here~~
}
  \item{dfName}{
%%     ~~Describe \code{dfName} here~~
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
function (pDf, dfName) 
{
    objDf <- pDf[!is.na(pDf$Adjusted), ]
    fieldsName <- c("Date", "Ticket", "Predict", "CA", "b")
    if (file.exists(paste("./Analises/VarCA/", dfName, ".csv", 
        sep = ""))) {
        print(nrow(df) + 1)
        if (!is.null(objDf[nrow(df) + 1, ])) {
            to <- nrow(df) + 1
            from <- ifelse(to - 7 <= 0, 1, to - 7)
            objDfLm <- objDf[from:to, ]
            lmdf <- lm(objDfLm$Adjusted ~ index(objDfLm$Date))
            ca <- ifelse(is.na(summary(lmdf)$coefficients[2]), 
                0, summary(lmdf)$coefficients[2])
            b <- summary(lmdf)$coefficients[1]
            pred <- ca * (to + 1) + b
            dfnew <- data.frame(objDf[to, ]$Date, objDf[to, ]$Adjusted, 
                pred, ca, b)
            names(dfnew) <- fieldsName
            df <- rbind(df, dfnew)
            write.csv(df, file = paste("./Analises/VarCA/", dfName, 
                ".csv", sep = ""), row.names = FALSE)
            updateVarCATicket(pDf, dfName)
        }
    }
    else {
        df <- data.frame(objDf[1, ]$Date, objDf[1, ]$Adjusted, 
            0, 0, 0)
        names(df) <- fieldsName
        write.csv(df, file = paste("Analises/VarCA/", dfName, 
            ".csv", sep = ""), row.names = FALSE)
        updateVarCATicket(pDf, dfName)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
