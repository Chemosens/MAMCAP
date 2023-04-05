decimal <-
function (nb, digits) 
{
    nb = as.numeric(nb)
    if (!is.na(nb)) {
        if (nb != "") {
            numberOfZeros = ""
            for (i in 0:(digits - 1)) {
                if (nb - round(nb, i) == 0) {
                  numberOfZeros = paste(numberOfZeros, "0", sep = "")
                }
            }
            if (nb - round(nb, digits) == 0) {
                if (nb - round(nb, 0) == 0) {
                  nb.inter = as.character(nb)
                  nb.inter.2 = paste(nb.inter, ".", numberOfZeros, 
                    sep = "")
                  res = noquote(nb.inter.2)
                }
                else {
                  nb.inter = as.character(nb)
                  nb.inter.2 = paste(nb.inter, numberOfZeros, 
                    sep = "")
                  res = noquote(nb.inter.2)
                }
            }
            else {
                res1 = round(nb, digits = digits)
                res = decimal(res1, digits)
            }
        }
        else (res = "")
    }
    else {
        res = "NA"
    }
    return((res))
}
