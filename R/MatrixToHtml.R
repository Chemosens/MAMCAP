MatrixToHtml <-
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
