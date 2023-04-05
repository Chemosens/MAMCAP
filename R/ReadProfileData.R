ReadProfileData <-
function (file, allData = NULL, sep = ";", defaultGraphicalFormat = "pdf", 
    outputsCsv = NULL, replaceNA = "crossmean", language = "en") 
{
    res = list()
    if (is.null(allData)) {
        allData = read.csv(file, header = TRUE, sep = sep)
    }
    if (!"Type" %in% colnames(allData)) {
        allData[, "Type"] = "Profile"
    }
    if (!"Session" %in% colnames(allData)) {
        allData[, "Session"] = "Unknown"
    }
    canonicalData = subset(allData, Type == "Profile", select = c(ProductCode, 
        SubjectCode, Replicate, Session, AttributeCode, Score))
    canonicalData = subset(canonicalData, AttributeCode != "")
    canonicalData = subset(canonicalData, ProductCode != "")
    canonicalData = subset(canonicalData, SubjectCode != "")
    canonicalData$ProductCode = factor(canonicalData$ProductCode)
    canonicalData$AttributeCode = factor(canonicalData$AttributeCode)
    canonicalData$SubjectCode = factor(canonicalData$SubjectCode)
    reshapedObject = ReshapeData(input = canonicalData, replaceNA = replaceNA)
    res[["CanonicalData"]] = canonicalData
    res[["ExtendedData"]] = reshapedObject$data
    res[["Attributes"]] = reshapedObject$listAttributes
    res[["Products"]] = reshapedObject$listProducts
    res[["Subjects"]] = reshapedObject$listSubjects
    res[["Replicates"]] = reshapedObject$listReplicates
    res[["Language"]] = language
    res[["BalancedData"]] = reshapedObject$balancedData
    res[["FullData"]] = reshapedObject$fullData
    res[["DefaultGraphicalFormat"]] = defaultGraphicalFormat
    productColors = rainbow(length(reshapedObject$listProducts))
    names(productColors) = reshapedObject$listProducts
    res[["ProductColors"]] = productColors
    if (!is.null(outputsCsv)) {
        if ("ExtendedTable" %in% outputsCsv) {
            write.table(reshapedObject$data, "ExtendedTable.csv", 
                sep = ",", row.names = FALSE)
        }
    }
    return(res)
}
