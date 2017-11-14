\name{updateAnalises}
\alias{updateAnalises}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
updateAnalises(obj)
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
    tickets <- paste(getTickets(), ".SA", sep = "")
    fund <- getFundamentusData()
    ticketValue <- c()
    name <- c()
    setor <- c()
    subsetor <- c()
    percW <- c()
    percM <- c()
    percT <- c()
    maxM <- c()
    maxS <- c()
    maxY <- c()
    mediaAmpl <- c()
    concavity <- c()
    corWeek <- c()
    corMonth <- c()
    corTri <- c()
    volD <- c()
    caD <- c()
    vD <- c()
    volW <- c()
    caW <- c()
    caW2 <- c()
    bW <- c()
    caM <- c()
    caT <- c()
    sigW <- c()
    sigM <- c()
    sigT <- c()
    for (ticket in tickets) {
        tryCatch({
            name <- c(name, ifelse((fund[fund$papel == ticket, 
                ]$empresa != ""), as.character(fund[fund$papel == 
                ticket, ]$empresa), NA))
            setor <- c(setor, ifelse((fund[fund$papel == ticket, 
                ]$setor != ""), as.character(fund[fund$papel == 
                ticket, ]$setor), NA))
            subsetor <- c(subsetor, ifelse((fund[fund$papel == 
                ticket, ]$subsetor != ""), as.character(fund[fund$papel == 
                ticket, ]$subsetor), NA))
            if (!is.null(obj[[ticket]])) {
                lastTicket <- getLastTicket(obj[[ticket]])
                ticketValue <- c(ticketValue, lastTicket[1, ]$Adjusted)
                mediaAmpl <- c(mediaAmpl, ifelse(is.numeric(lastTicket[1, 
                  ]$Amplitude), abs(lastTicket[1, ]$Amplitude), 
                  NULL))
                D3 <- getLast3Days(obj[[ticket]])
                D3 <- D3[!is.na(D3$Adjusted), ]
                week <- getLastWeek(obj[[ticket]])
                week <- week[!is.na(week$Open) | !is.na(week$Close) | 
                  !is.na(week$High) | !is.na(week$Low) | !is.na(week$Volume) | 
                  !is.na(week$Adjusted) | !is.na(week$Amplitude), 
                  ]
                week2 <- getLastLastWeek(obj[[ticket]])
                week2 <- week2[!is.na(week2$Open) | !is.na(week2$Close) | 
                  !is.na(week2$High) | !is.na(week2$Low) | !is.na(week2$Volume) | 
                  !is.na(week2$Adjusted) | !is.na(week2$Amplitude), 
                  ]
                month <- getLastMonth(obj[[ticket]])
                month <- week[!is.na(week$Open) | !is.na(week$Close) | 
                  !is.na(week$High) | !is.na(week$Low) | !is.na(week$Volume) | 
                  !is.na(week$Adjusted) | !is.na(week$Amplitude), 
                  ]
                trimester <- getLastTrimester(obj[[ticket]])
                trimester <- trimester[!is.na(trimester$Open) | 
                  !is.na(trimester$Close) | !is.na(trimester$High) | 
                  !is.na(trimester$Low) | !is.na(trimester$Volume) | 
                  !is.na(trimester$Adjusted) | !is.na(trimester$Amplitude), 
                  ]
                semester <- getLastSemester(obj[[ticket]])
                semester <- semester[!is.na(semester$Open) | 
                  !is.na(semester$Close) | !is.na(semester$High) | 
                  !is.na(semester$Low) | !is.na(semester$Volume) | 
                  !is.na(semester$Adjusted) | !is.na(semester$Amplitude), 
                  ]
                year <- getLastYear(obj[[ticket]])
                year <- year[!is.na(year$Open) | !is.na(year$Close) | 
                  !is.na(year$High) | !is.na(year$Low) | !is.na(year$Volume) | 
                  !is.na(year$Adjusted) | !is.na(year$Amplitude), 
                  ]
                percW <- c(percW, (week[nrow(week), ]$Close * 
                  100)/(week[1, ]$Open) - 100)
                percM <- c(percM, (month[nrow(month), ]$Close * 
                  100)/(month[1, ]$Open) - 100)
                percT <- c(percT, (trimester[nrow(trimester), 
                  ]$Close * 100)/(trimester[1, ]$Open) - 100)
                maxM <- c(maxM, max(month[!is.na(month$Adjusted), 
                  ]$Adjusted))
                maxS <- c(maxS, max(semester[!is.na(semester$Adjusted), 
                  ]$Adjusted))
                maxY <- c(maxY, max(year[!is.na(year$Adjusted), 
                  ]$Adjusted))
                lmD3 <- lm(D3$Adjusted ~ index(D3$Date))
                lmWeek <- lm(week$Adjusted ~ index(week$Date))
                lmWeek2 <- lm(week2$Adjusted ~ index(week2$Date))
                lmMonth <- lm(month$Adjusted ~ index(month$Date))
                lmTri <- lm(trimester$Adjusted ~ index(trimester$Date))
                corWeek <- c(corWeek, cor(week$Adjusted, index(week)))
                corMonth <- c(corMonth, cor(month$Adjusted, index(month)))
                corTri <- c(corTri, cor(trimester$Adjusted, index(trimester)))
                volW <- c(volW, mean(week$Volume))
                caD <- c(caD, summary(lmD3)$coefficients[2])
                vD <- c(vD, getSpeed(obj[[ticket]]))
                caW <- c(caW, summary(lmWeek)$coefficients[2])
                caW2 <- c(caW2, summary(lmWeek2)$coefficients[2])
                bW <- c(bW, summary(lmWeek)$coefficients[1])
                caM <- c(caM, summary(lmMonth)$coefficients[2])
                caT <- c(caT, summary(lmTri)$coefficients[2])
                sigW <- c(sigW, summary(lmWeek)$sigma)
                sigM <- c(sigM, summary(lmMonth)$sigma)
                sigT <- c(sigT, summary(lmTri)$sigma)
                concavity <- ifelse(!is.na(caW) & !is.na(caW2), 
                  caW - caW2, NA)
            }
            else {
                tickets <- tickets[!tickets \%in\% ticket]
            }
        }, error = function(e) {
            print("Error:")
            print(e)
        })
    }
    df <- data.frame(tickets[1:379], name[1:379], ticketValue[1:379], 
        setor[1:379], subsetor[1:379], maxM[1:379], maxS[1:379], 
        maxY[1:379], percW[1:379], percM[1:379], percT[1:379], 
        mediaAmpl[1:379], concavity[1:379], volW[1:379], caD[1:379], 
        vD[1:379], corWeek[1:379], caW[1:379], bW[1:379], sigW[1:379], 
        corMonth[1:379], caM[1:379], sigM[1:379], corTri[1:379], 
        caT[1:379], sigT[1:379])
    names(df) <- c("Ticket", "Name", "Value", "Sector", "Subsector", 
        "MaxM", "MaxS", "MaxY", "PercW", "PercM", "PercT", "MediaAmpl", 
        "Concavity", "VolW", "CaD", "VD", "CorrW", "CaW", "BW", 
        "SigW", "CorrM", "CaM", "BM", "SigM", "CorrT", "CaT", 
        "BT", "SigT")
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
