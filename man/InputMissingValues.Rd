\name{InputMissingValues}
\alias{InputMissingValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
InputMissingValues
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
InputMissingValues(extendedData, replaceMode = "crossmean", log = FALSE, language = "en", functionName = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{extendedData}{
%%     ~~Describe \code{extendedData} here~~
}
  \item{replaceMode}{
%%     ~~Describe \code{replaceMode} here~~
}
  \item{log}{
%%     ~~Describe \code{log} here~~
}
  \item{language}{
%%     ~~Describe \code{language} here~~
}
  \item{functionName}{
%%     ~~Describe \code{functionName} here~~
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
function (extendedData, replaceMode = "crossmean", log = FALSE, 
    language = "en", functionName = NULL) 
{
    nbReplacements = 0
    for (i in 1:dim(extendedData)[2]) {
        if (is.numeric(extendedData[, i])) {
            indices = which(is.na(extendedData[, i]))
            nbReplacements = nbReplacements + length(indices)
            if (replaceMode == "zeros") {
                extendedData[, i] = replace(extendedData[, i], 
                  is.na(extendedData[, i]), 0)
            }
            if (replaceMode == "colmean") {
                extendedData[, i] = replace(extendedData[, i], 
                  is.na(extendedData[, i]), mean(extendedData[-indices, 
                    i]))
            }
            if (replaceMode == "crossmean") {
                for (indice in indices) {
                  productCode = extendedData[indice, "ProductCode"]
                  subjectCode = extendedData[indice, "SubjectCode"]
                  grandMean = mean(extendedData[, i], na.rm = TRUE)
                  productMean = mean(extendedData[extendedData$ProductCode == 
                    productCode, i], na.rm = TRUE)
                  subjectMean = mean(extendedData[extendedData$SubjectCode == 
                    subjectCode, i], na.rm = TRUE)
                  if (is.na(subjectMean)) {
                    subjectMean = grandMean
                  }
                  if (is.na(productMean)) {
                    productMean = grandMean
                  }
                  extendedData[indice, i] = productMean + subjectMean - 
                    grandMean
                }
            }
        }
    }
    if (log == TRUE && nbReplacements > 0) {
        logTxt = paste("<p>", GetLabel("MissingDataReplaced", 
            language), sep = "")
        if (!is.null(functionName)) {
            logTxt = paste(logTxt, " in ", functionName, sep = "")
        }
        logTxt = paste(logTxt, ": ", nbReplacements, " (", GetLabel("Method", 
            language), "=", replaceMode, ")", "</p>", sep = "")
        LogEntry(logTxt)
    }
    return(extendedData)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
