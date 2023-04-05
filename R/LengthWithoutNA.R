LengthWithoutNA <-
function (vector) 
{
    length = sum(!is.na(vector))
    return(length)
}
