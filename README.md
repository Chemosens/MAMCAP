# MAMCAP
## Description
This package aims to produce a MAM-CAP table as presented in Peltier et al. 2012.
## Installation of MAMCAP R package
```R
## install devtools
if (!requireNamespace("devtools", quietly = TRUE)) {    install.packages("devtools") }
## Install BETR
library(devtools)
install_github("https://github.com/ChemoSens/MAMCAP")
```
## Usage
### Loading profile data as profile object
In this example, we suppose that the data are in a file yourdata.txt, containing these columns: 
```R
profileObject=ReadProfileData(yourdata.txt,sep="\t")
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
