\name{MatrixToHtml}
\alias{MatrixToHtml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MatrixToHtml
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MatrixToHtml(matrix, decimals = 3, flip = FALSE, style = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{
%%     ~~Describe \code{matrix} here~~
}
  \item{decimals}{
%%     ~~Describe \code{decimals} here~~
}
  \item{flip}{
%%     ~~Describe \code{flip} here~~
}
  \item{style}{
%%     ~~Describe \code{style} here~~
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
function (matrix, decimals = 3, flip = FALSE, style = NULL) 
{
    matrix = as.data.frame(matrix)
    addTD = function(x) {
        x = paste("<td align='center'>", x, "</td>", sep = "")
    }
    applyStyle = function(rowStyle) {
        x = rowStyle[1]
        y = rowStyle[2]
        textStyle = "<td align='center' style='"
        if (rowStyle[3] != "") {
            textStyle = paste(textStyle, " background-color:", 
                rowStyle[3], ";", sep = "")
        }
        if (rowStyle[4] != "") {
            textStyle = paste(textStyle, " color:", rowStyle[4], 
                ";", sep = "")
        }
        if (rowStyle[5] != "") {
            textStyle = paste(textStyle, " font-weight:", rowStyle[5], 
                sep = "")
        }
        textStyle = paste(textStyle, "'>", sep = "")
        tmp = substring(dataframe[x, y], 20)
        newText = paste(textStyle, tmp, sep = "")
        dataframe[x, y] <<- newText
    }
    numericCols = sapply(matrix, is.numeric)
    matrix[, numericCols] = round(matrix[, numericCols], decimals)
    if (flip == TRUE) {
        matrix = t(matrix)
    }
    dataframe = as.data.frame(matrix)
    dataframe = apply(dataframe, 2, as.character)
    if (is.null(dim(dataframe))) {
        dataframe = sapply(dataframe, addTD)
    }
    else {
        dataframe = apply(dataframe, 2, addTD)
    }
    rownames(dataframe) = rownames(matrix)
    if (!is.null(style)) {
        apply(style, 1, applyStyle)
    }
    firstLine = paste("<th>", colnames(matrix), "</th>", sep = "")
    dataframe = rbind(firstLine, dataframe)
    firstCol = c("", rownames(matrix))
    firstCol = paste("<td><strong>", firstCol, "</strong></td>", 
        sep = "")
    firstCol[1] = "<th></th>"
    dataframe = cbind(firstCol, dataframe)
    dataframe = cbind("<tr>", dataframe)
    dataframe = cbind(dataframe, "</tr>")
    vector = as.vector(t(dataframe))
    string = paste(vector, collapse = "")
    string = paste("<table>", string, "</table><br/>", sep = "", 
        collapse = "")
    return(string)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
