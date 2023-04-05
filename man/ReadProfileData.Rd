\name{ReadProfileData}
\alias{ReadProfileData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ReadProfileData
}
\description{
Transforms a file into "Profile Object". Profile Object is taken as input parameter in most of TimeSens functions. Usual data manipulations are done in ReadProfileData. It avoids redundancy in the TimeSens functions.
}
\usage{
ReadProfileData(file, allData = NULL, header = TRUE, sep = ";", defaultGraphicalFormat = "pdf", outputsCsv = NULL, replaceNA = "crossmean", language = "en")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
the name of the file which the data are to be read from. Each row of the table appears as one line of the file. If it does not contain an absolute path, the file name is relative to the current working directory, getwd(). Tilde-expansion is performed where supported. As from R 2.10.0 this can be a compressed file (see file).

It has to be a dataset with columns: "ProductCode","SubjectCode","Replicate","AttributeCode","Score"

file can also be a complete URL. (For the supported URL schemes, see the ‘URLs’ section of the help for url.)
}
  \item{allData}{
A data.frame to transform into a ProfileObject. 
}

  \item{sep}{ the field separator character. Values on each line of the file are separated by this character. If sep = "" (the default for read.table) the separator is ‘white space’, that is one or more spaces, tabs, newlines or carriage returns.

}
  \item{defaultGraphicalFormat}{"jpeg","png","wmf" or "pdf".
Graphical Analyses applied to the Profile Object will be recorded in this format. 
}
  \item{outputsCsv}{
NULL or "ExtendedTable". If "ExtendedTable, write the result ExtendedData into ExtendedTable.csv
}
  \item{replaceNA}{
"none", "crossmean", "zeros" or "colmean". Indicates how to deal with NA's.\
If "none", nothing is done. If "zeros", NA are replaced by zeros. If "colmeans", NA are replaced by the mean of attribute. \
If "crossmean", NAs are replaced by product mean + subject mean - general mean
}
  \item{language}{
"fr" for "french", "en for english. Language will be used in analysis output and will be contained in the Profile Object.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

\item{CanonicalData }{data representation in its initial form. Column names are ProductCode, SubjectCode, Replicate, Session, AttributeCode and Score }
\item{ExtendedData}{data representation with attributes in columns. Column names are ProductCode,SubjectCode,Replicate, Session and attributes names}
\item{Attributes}{vector containing all modalities of AttributeCode}
\item{Products}{vector containing all modalities of ProductCode}
\item{Subjects}{vector containing all modalities of SubjectCode}
\item{Replicates}{vector containing all modalities of Replicate}
\item{Language}{language to use in future analyses applied to the Profile Object}
\item{BalancedData}{logical, TRUE if the dataset is balanced (all subject have scored all attributes and all products the same number of times)}
\item{FullData}{logical, TRUE if the dataset is full (all subject have scored all attributes at least one time per product)}
\item{DefaultGraphicalFormat}{same as DefaultGraphicalFormat input}
\item{ProductColors}{colors associated with products (for graphical outputs)}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Caroline Peltier, Michel Visalli
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

ReadProfileData(file="http://www.timesens.com/docs/wines.txt",sep="\t")

or

data(wine)
write.table(wine$CanonicalData,"wine.txt",sep="\t")
ReadProfileData(file="wine.txt",sep="\t")
}