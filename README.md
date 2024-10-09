# MAMCAP
## Description
This package aims to produce a MAM-CAP table as presented in Peltier et al. 2012.
## Installation of MAMCAP R package
```R
## install devtools
if (!requireNamespace("devtools", quietly = TRUE)) {    install.packages("devtools") }
## Install MAMCAP
library(devtools)
install_github("https://github.com/ChemoSens/MAMCAP")
```
## Usage
### Loading profile data as profile object
In this example, we suppose that the data are in a file 'yourdata.txt', containing the following columns: "ProductCode","SubjectCode","Replicate","AttributeCode","Score"
```R
profileObject=ReadProfileData(file='yourdata.txt',sep="\t")
```
If any question, please type
```R
?ReadProfileData
```
### Using CAPTable function for obtaining CAPTable
This command returned the CAP Table (without taking scaling into account)
```R
CAPTable(profileObject)
```
This command returned the MAMCAP Table (taking scaling into account)
```R
CAPTable(profileObject,model="MAM")
```

The CAP Table is produced in your working directory. If you don't know where is your working directory, it can be returned by the command
```R
getwd()
```
If any question, please type
```R
?CAPTable
```
## A free example
An example of use is available on wine data
```R
data(wine)
CAPTable(wine)
```

## References
C. Peltier, P.B. Brockhoff, M. Visalli, P. Schlich, The MAM-CAP table: A new tool for monitoring panel performances,Food Quality and Preference, Volume 32, Part A, 2014,Pages 24-27, ISSN 0950-3293, https://doi.org/10.1016/j.foodqual.2013.07.004.

