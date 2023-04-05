CompleteData <-
function (input) 
{
    listProducts = as.character(unique(input$ProductCode))
    listSubjects = as.character(unique(input$SubjectCode))
    listReplicates = as.character(unique(input$Replicate))
    cols = merge(listReplicates, listProducts, by = NULL)
    cols = merge(cols, listSubjects, by = NULL)
    colnames(cols) = c("Replicate", "ProductCode", "SubjectCode")
    input = merge(input, cols, by = c("Replicate", "ProductCode", 
        "SubjectCode"), all.y = TRUE)
    return(input)
}
