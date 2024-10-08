\name{CAPTable}
\alias{CAPTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CAPTable
}
\description{
This function build an html file (CAPTable.html) containing a panel and panelist performance table (CAP or MAM-CAP table) in current working directory.
}
\usage{
CAPTable(profileObject, model = "TwoWayANOVA", panelLimit = 0.05, indivLimit = 0.05, language = "fr", output = "CAPTable", correction = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{profileObject}{
profile Object resulted from \link{ReadProfileData}
}
  \item{model}{ character. "TwoWayANOVA" or "MAM": model of performance tests. 

"TwoWayANOVA" refers to the first Schlich CAP table available on the SensoBase (www.sensobase.fr) and does not take the scaling into account.

"MAM" refers to the MAM-CAP table, using the performances tests of Mixed Assessor Model (2013) 
}
  \item{limitPanel}{
limit of the statistical tests concerning panel performances
}
  \item{limitIndiv}{
limit of the statistical tests concerning individual performances. 
When the chosen model is CAP, limitIndiv is only the limit of discrimination tests (Kendall agreement test limit = 0.2 and repeatability test= 0.01)
}
  \item{language}{character. "fr" for french and "en" for english. Language of 
}
  \item{output}{
the name of the html file is created in the current working directory 
}
  \item{correction}{
logical. If TRUE, limitIndiv is divided by the number of panelists
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
www.sensobase.fr

Brockhoff P. , Schlich P.; Skovgaard I, Taking individual scaling differences into account by analyzing profile data with the Mixed Assessor Model. (submitted in Food Quality and Preferences in July 2013)

Peltier C., Brockhoff, P., Visalli, M., Schlich, P. The MAM-CAP table: a new tool to monitor panel performances. (accepted in Food Quality and Preferences in July 2013)

}
\author{
Caroline Peltier, Per Brockhoff, Michel Visalli, Pascal Schlich
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
wine=ReadProfileData("http://www.timesens.com/docs/wines.txt",sep="\t")
CAPTable(wine,model="TwoWayANOVA")

or

data(wine)
CAPTable(wine)
CAPTable(wine,model="TwoWayANOVA")
}

