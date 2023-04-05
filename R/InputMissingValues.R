InputMissingValues <-
function (extendedData, replaceMode = "crossmean", log = FALSE, 
    language = "en", functionName = NULL) 
{
    nbReplacements = 0
    for (i in 1:dim(extendedData)[2]) {
        if (is.numeric(extendedData[, i])) {
            indices = which(is.na(extendedData[, i]))
            nbReplacements = nbReplacements + length(indices)
            if (replaceMode == "zeros") {
                extendedData[, i] = replace(extendedData[, i], 
                  is.na(extendedData[, i]), 0)
            }
            if (replaceMode == "colmean") {
                extendedData[, i] = replace(extendedData[, i], 
                  is.na(extendedData[, i]), mean(extendedData[-indices, 
                    i]))
            }
            if (replaceMode == "crossmean") {
                for (indice in indices) {
                  productCode = extendedData[indice, "ProductCode"]
                  subjectCode = extendedData[indice, "SubjectCode"]
                  grandMean = mean(extendedData[, i], na.rm = TRUE)
                  productMean = mean(extendedData[extendedData$ProductCode == 
                    productCode, i], na.rm = TRUE)
                  subjectMean = mean(extendedData[extendedData$SubjectCode == 
                    subjectCode, i], na.rm = TRUE)
                  if (is.na(subjectMean)) {
                    subjectMean = grandMean
                  }
                  if (is.na(productMean)) {
                    productMean = grandMean
                  }
                  extendedData[indice, i] = productMean + subjectMean - 
                    grandMean
                }
            }
        }
    }
    if (log == TRUE && nbReplacements > 0) {
        logTxt = paste("<p>", GetLabel("MissingDataReplaced", 
            language), sep = "")
        if (!is.null(functionName)) {
            logTxt = paste(logTxt, " in ", functionName, sep = "")
        }
        logTxt = paste(logTxt, ": ", nbReplacements, " (", GetLabel("Method", 
            language), "=", replaceMode, ")", "</p>", sep = "")
        LogEntry(logTxt)
    }
    return(extendedData)
}
