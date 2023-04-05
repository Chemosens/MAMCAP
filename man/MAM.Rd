\name{MAM}
\alias{MAM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MAM
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MAM(profileObject, language = "en", output = "MAM.html")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{profileObject}{
%%     ~~Describe \code{profileObject} here~~
}
  \item{language}{
%%     ~~Describe \code{language} here~~
}
  \item{output}{
%%     ~~Describe \code{output} here~~
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
function (profileObject, language = "en", output = "MAM.html") 
{
    nbProducts = length(profileObject$Products)
    nbReplicates = length(profileObject$Replicates)
    nbSubjects = length(profileObject$Subjects)
    nbAttributes = length(profileObject$Attributes)
    N = nrow(profileObject$CanonicalData)
    if (nbProducts < 3) {
        stop(GetLabel("AtLeastThreeProducts", language))
    }
    if (nbReplicates < 2) {
        stop(GetLabel("AtLeastTwoReplicates", language))
    }
    SubjectCode = profileObject$ExtendedData[, "SubjectCode"]
    ProductCode = profileObject$ExtendedData[, "ProductCode"]
    Replicate = profileObject$ExtendedData[, "Replicate"]
    input = cbind(SubjectCode, ProductCode, Replicate, profileObject$ExtendedData[, 
        -c(1:4)])
    resultat = scalesensperf(input, pvalue = FALSE)
    subjectPerformancesTables = resultat[[1]]
    AnovaTables = resultat[[2]]
    txt = "<html>"
    for (i in 1:nbAttributes) {
        subjectPerformancesTables[, , profileObject$Attributes[i]]
        AnovaTable = AnovaTables[, , profileObject$Attributes[i]] = AnovaTables[, 
            , profileObject$Attributes[i]][, c("DF", "SS", "MS", 
            "F", "Pval")]
        colnames(AnovaTable) = c("DF", "SS", "MS", "F", "Pval")
        txt = paste(txt, "<h3>", profileObject$Attributes[i], 
            "</h3>", MatrixToHtml(AnovaTable), sep = "")
    }
    txt = paste(txt, "</html>")
    write(txt, paste(output, ".html", sep = ""))
    return(resultat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
