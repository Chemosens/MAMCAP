\name{ReshapeData}
\alias{ReshapeData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ReshapeData
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReshapeData(input, replaceNA = "none", language = "en")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{replaceNA}{
%%     ~~Describe \code{replaceNA} here~~
}
  \item{language}{
%%     ~~Describe \code{language} here~~
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
function (input, replaceNA = "none", language = "en") 
{
    data = reshape(input, idvar = c("ProductCode", "SubjectCode", 
        "Replicate", "Session"), timevar = "AttributeCode", direction = "wide")
    colNames = colnames(data)
    colNames[-c(1:4)] = substr(colNames[-c(1:4)], 7, 100)
    colnames(data) = colNames
    data[, 1] = as.factor(data[, 1])
    data[, 2] = as.factor(data[, 2])
    data[, 3] = as.factor(data[, 3])
    data[, 4] = as.factor(data[, 4])
    listAttributes = colnames(data)[-c(1:4)]
    listProducts = as.character(unique(data$ProductCode))
    listSubjects = as.character(unique(data$SubjectCode))
    listReplicates = as.character(unique(data$Replicate))
    nbAttributes = length(listAttributes)
    nbProducts = length(listProducts)
    nbSubjects = length(listSubjects)
    nbReplicates = length(listReplicates)
    isNotNAData = data
    isNotNAData[, -c(1:4)] = !is.na(isNotNAData[, -c(1:4)])
    occurrenceDataList = apply(isNotNAData[, -c(1:4)], 2, FUN = "aggregate", 
        list(isNotNAData[, "ProductCode"], isNotNAData[, "SubjectCode"]), 
        "sum")
    occurrenceData = occurrenceDataList[[listAttributes[1]]]
    for (i in 2:nbAttributes) {
        occurrenceData = cbind(occurrenceData, occurrenceDataList[[listAttributes[i]]][, 
            3])
    }
    colnames(occurrenceData) = c("ProductCode", "SubjectCode", 
        listAttributes)
    if (sum(occurrenceData[, -c(1, 2)] == 0) == 0) {
        fullData = TRUE
    }
    else {
        fullData = FALSE
    }
    if (sum(occurrenceData[, -c(1, 2)] != occurrenceData[1, 3]) == 
        0) {
        balancedData = TRUE
    }
    else {
        balancedData = FALSE
    }
    if (replaceNA != "none") {
        data = InputMissingValues(data, replaceNA, TRUE, language)
    }
    result = list(data = data, listAttributes = listAttributes, 
        listProducts = listProducts, listSubjects = listSubjects, 
        listReplicates = listReplicates, nbAttributes = nbAttributes, 
        nbProducts = nbProducts, nbSubjects = nbSubjects, nbReplicates = nbReplicates, 
        fullData = fullData, balancedData = balancedData)
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
