\name{MAMCAP-package}
\alias{MAMCAP-package}
\alias{MAMCAP}
\docType{package}
\title{
MAMCAP-package
}
\description{

This package allows to use the Mixed Assessor Model for the panel and panelist performances in profile sensory studies (MAM-CAP table)
}
\details{
\tabular{ll}{
Package: \tab MAMCAP\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-07-12\cr
License: \tab GPL (>= 2)\cr
}
This package allows to plot performances table. CAPTable and ReadProfileData are the most important functions.

\link{ReadProfileData} allows to get a "Profile Object" required in CAPTable.

\link{CAPTable} produces CAP table (Schlich, www.sensobase.fr) or MAM-CAP Table (Peltier et al., 2013).

Thus, to get a CAPTable, there are three steps: 
\itemize{
\item To have data whose colnames are: ProductCode, SubjectCode,AttributeCode, Replicate, Score in an extern file
\item To read this file with myProfileObject=ReadProfileData("data.txt")
\item To run CAPTable : CAPTable(myProfileObject)
}
}
\author{
Caroline Peltier, Per Brockhoff, Michel Visalli, Pascal Schlich

Maintainer: Caroline Peltier <caroline.peltier@dijon.inra.fr>

}
\references{
Peltier, C., Brockhoff, P., Visalli, M. Schlich, P., The MAM-CAP table a new tool for monitoring panel performances. (accepted in Food Quality and Preferences in July 2013)

Brockhoff P. , Schlich P.; Skovgaard I, Taking individual scaling differences into account by analyzing profile data with the Mixed Assessor Model. (submitted in Food Quality and Preferences in July 2013)
}
\examples{
data(wine)
CAPTable(wine)
}
