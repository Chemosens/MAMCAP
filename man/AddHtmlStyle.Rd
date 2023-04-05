\name{AddHtmlStyle}
\alias{AddHtmlStyle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AddHtmlStyle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AddHtmlStyle(style, x, y, backgroundColor = "", color = "", fontWeight = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{style}{
%%     ~~Describe \code{style} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{backgroundColor}{
%%     ~~Describe \code{backgroundColor} here~~
}
  \item{color}{
%%     ~~Describe \code{color} here~~
}
  \item{fontWeight}{
%%     ~~Describe \code{fontWeight} here~~
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
function (style, x, y, backgroundColor = "", color = "", fontWeight = "") 
{
    if (is.null(style)) {
        style = as.data.frame(cbind(x, y, backgroundColor, color, 
            fontWeight))
        colnames(style) = c("X", "Y", "backgroundColor", "color", 
            "fontWeight")
    }
    else {
        if (nrow(subset(style, X == x & Y == y)) > 0) {
            style[, X == x & Y == y]$backgroundColor = backgroundColor
            style[, X == x & Y == y]$color = color
            style[, X == x & Y == y]$fontWeight = fontWeight
        }
        else {
            newStyle = cbind(x, y, backgroundColor, color, fontWeight)
            colnames(newStyle) = c("X", "Y", "backgroundColor", 
                "color", "fontWeight")
            style = rbind(style, newStyle)
        }
    }
    return(style)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
