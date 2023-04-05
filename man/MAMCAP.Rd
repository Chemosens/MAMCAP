\name{MAMCAP}
\alias{MAMCAP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MAMCAP
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MAMCAP(object, seuilPanel = 0.05, seuilIndiv = 0.05, language = "fr", output = "tabCAP", correction = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{seuilPanel}{
%%     ~~Describe \code{seuilPanel} here~~
}
  \item{seuilIndiv}{
%%     ~~Describe \code{seuilIndiv} here~~
}
  \item{language}{
%%     ~~Describe \code{language} here~~
}
  \item{output}{
%%     ~~Describe \code{output} here~~
}
  \item{correction}{
%%     ~~Describe \code{correction} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, seuilPanel = 0.05, seuilIndiv = 0.05, language = "fr", 
    output = "tabCAP", correction = TRUE) 
{
    seuilIndivrim.1 = 0.01
    seuilIndivrim.2 = 0.05
    seuilIndivrim.3 = 0.1
    texteCellule = function(discrim, seuilIndivrim.1 = 0.01, 
        seuilIndivrim.2 = 0.05, seuilIndivrim.3 = 0.1, scaling, 
        p.scaling, seuilIndiv, p.rep, text.rep) {
        V = sort(c(seuilIndivrim.1, seuilIndivrim.2, seuilIndivrim.3))
        seuilIndivrim.1 = V[1]
        seuilIndivrim.2 = V[2]
        seuilIndivrim.3 = V[3]
        text.cellule = ""
        if (!is.na(discrim)) {
            if (p.scaling < seuilIndiv) {
                if (scaling > 1) {
                  text.cellule = paste(text.cellule, "&lt;", 
                    sep = "")
                }
                if (scaling <= 1 & scaling > 0) {
                  text.cellule = paste(text.cellule, "&gt;", 
                    sep = "")
                }
                if (scaling <= 0) {
                  text.cellule = paste(text.cellule, "?", sep = "")
                }
            }
            else {
                text.cellule = paste(text.cellule, "|", sep = "")
            }
            if (seuilIndivrim.1 < discrim & discrim < seuilIndivrim.2) {
                text.cellule = paste(text.cellule, "--", sep = "")
            }
            if (seuilIndivrim.1 > discrim) {
                text.cellule = paste(text.cellule, "---", sep = "")
            }
            if (seuilIndivrim.3 < discrim) {
                text.cellule = paste(text.cellule, " ", sep = "")
            }
            if (seuilIndivrim.2 < discrim & discrim < seuilIndivrim.3) {
                text.cellule = paste(text.cellule, "-", sep = "")
            }
            if (p.rep < seuilIndiv) {
                text.cellule = paste(text.cellule, text.rep, 
                  sep = "")
            }
            if (p.scaling < seuilIndiv) {
                if (scaling > 1) {
                  text.cellule = paste(text.cellule, "&gt;", 
                    sep = "")
                }
                if (scaling <= 1 & scaling > 0) {
                  text.cellule = paste(text.cellule, "&lt;", 
                    sep = "")
                }
                if (scaling <= 0) {
                  text.cellule = paste(text.cellule, "?", sep = "")
                }
            }
            else {
                text.cellule = paste(text.cellule, "|", sep = "")
            }
        }
        if (is.na(discrim)) {
            text.cellule = "NA"
        }
        return(text.cellule)
    }
    data = object$CanonicalData
    dataRes = object$ExtendedData
    firstvar = 5
    lastvar = dim(dataRes)[2]
    attribut = GetLabel("Attributes", language)
    titre = paste(GetLabel("TitreCAP1", language), seuilPanel, 
        GetLabel("TitreCAP2", language), seuilIndiv, ")")
    text.rep = GetLabel("LR", language)
    att = object$Attributes
    nb.att = length(att)
    rep = object$Replicates
    nb.rep = length(rep)
    mean.vec = rep(0, nb.att)
    fprod.vec = rep(0, nb.att)
    finter.vec = rep(0, nb.att)
    fdisag.vec = rep(0, nb.att)
    frepet.vec = rep(0, nb.att)
    pprod.vec = rep(0, nb.att)
    pinter.vec = rep(0, nb.att)
    pdisag.vec = rep(0, nb.att)
    perror.vec = rep(0, nb.att)
    ferror.vec = rep(0, nb.att)
    panel.repet = rep(0, nb.att)
    rmse.vec = rep(0, nb.att)
    suj = object$Subjects
    prod = object$Products
    nb.prod = length(prod)
    nb.suj = length(suj)
    if (correction) {
        seuilIndiv = seuilIndiv/nb.suj
    }
    mat.f = matrix(0, nb.att, nb.suj)
    mat.f3 = matrix(NA, nb.att, nb.suj)
    mat.repet = matrix(1, nb.att, nb.suj)
    mat.repet2 = matrix(1, nb.att, nb.suj)
    mat.repet2 = matrix(1, nb.att, nb.suj)
    mat.SS.suj = matrix(1, nb.att, nb.suj)
    mat.repet.f = matrix(0, nb.att, nb.suj)
    mat.disag.f = matrix(0, nb.att, nb.suj)
    mat.level = matrix(0, nb.att, nb.suj)
    mat.disag = matrix(0, nb.att, nb.suj)
    mat.scal = matrix("", nb.att, nb.suj)
    mat.scal.coef = matrix(0, nb.att, nb.suj)
    mat.p = matrix(0, nb.att, nb.suj)
    mat.sign = matrix(FALSE, nb.att, nb.suj)
    mat.rank = matrix(0, nb.att, nb.suj)
    colnames(mat.rank) = suj
    rownames(mat.rank) = att
    vec.rank.f = rep(0, nb.suj)
    if (nb.prod < 3) {
        stop(GetLabel("WarningThreeProd", language))
    }
    if (nb.rep < 2) {
        stop(GetLabel("WarningTwoRep", language))
    }
    if (sum(is.na(dataRes)) > 0) {
        stop(GetLabel("WarningBalanced", language))
    }
    dataRes2 = dataRes[, c(2, 1, 3, 5:dim(dataRes)[2])]
    resultat = scalesensperf(dataRes2)
    TabPerf = resultat[[1]]
    DIM1 = dim(TabPerf)
    ListATT = dimnames(TabPerf)[[3]]
    for (i in 1:DIM1[3]) {
        TabPerf1 = TabPerf[, , i]
        if (language == "en") {
            TabPerf1 = cbind(dimnames(TabPerf1)[[1]], TabPerf1)
        }
        else {
            TabPerf1 = cbind(c("Niveau", " ", "Produit", " ", 
                "Echelle", " ", "Correlation", " ", "Desaccord", 
                " ", "Repetabilite", " "), TabPerf1)
        }
        TabPerf1[c(1, 3, 5, 7, 9, 11), 2:(nb.suj + 2)] = as.numeric(noquote(TabPerf1[c(1, 
            3, 5, 7, 9, 11), 2:(nb.suj + 2)]))
        for (j in 1:nb.suj) {
            mat.repet.f[i, j] = as.numeric(noquote(TabPerf1[11, 
                1 + j]))
            mat.f[i, j] = as.numeric(noquote(TabPerf1[3, 1 + 
                j]))
            if (mat.repet.f[i, j] == 0) {
                mat.f3[i, j] = NA
            }
            else {
                mat.f3[i, j] = mat.f[i, j] * mat.f[i, j]/(mat.repet.f[i, 
                  j] * mat.repet.f[i, j])
            }
            mat.p[i, j] = as.numeric(noquote(TabPerf1[4, 1 + 
                j]))
            mat.disag.f[i, j] = as.numeric(noquote(TabPerf1[9, 
                1 + j]))
            mat.disag[i, j] = as.numeric(noquote(TabPerf1[10, 
                1 + j]))
            mat.repet[i, j] = mat.repet.f[i, j] * mat.repet.f[i, 
                j] * nb.prod * (nb.rep - 1)
            panel.repet[i] = TabPerf1[12, 1 + nb.suj + 1]
            mat.repet2[i, j] = as.numeric(TabPerf1[12, 1 + j])
            mat.level[i, j] = as.numeric(noquote(TabPerf1[1, 
                1 + j]))
            mat.scal[i, j] = TabPerf1[6, 1 + j]
            mat.scal.coef[i, j] = TabPerf1[5, 1 + j]
        }
    }
    ANOV = resultat[[2]]
    for (i in 1:nb.att) {
        data.i = data[data[, "AttributeCode"] == ListATT[i], 
            ]
        mean.vec[i] = mean(data.i[!is.na(data.i[, "Score"]), 
            "Score"])
        res.anova = anova(lm(data.i[, "Score"] ~ data.i[, "ProductCode"] * 
            data.i[, "SubjectCode"]))
        tab = ANOV[, , i]
        fprod.vec[i] = tab[2, 4]
        finter.vec[i] = tab[3, 4]
        pprod.vec[i] = tab[2, 5]
        pinter.vec[i] = tab[3, 5]
        fdisag.vec[i] = tab[4, 4]
        pdisag.vec[i] = tab[4, 5]
        ferror.vec[i] = sqrt(as.numeric(tab[5, 2]))
    }
    mat.f3[is.na(mat.f3)] = 0
    mat.f2 = max(mat.f3[!is.na(mat.f3)]) - mat.f3
    mat.rank = apply(mat.f2, 1, rank)
    colnames(mat.rank) = att
    rownames(mat.rank) = suj
    for (j in 1:nb.suj) {
        vec.rank.f[j] = mean(mat.rank[j, ])
    }
    names(vec.rank.f) = suj
    rownames(mat.p) = att
    colnames(mat.p) = suj
    colnames(mat.disag) = suj
    rownames(mat.disag) = att
    colnames(mat.repet) = suj
    rownames(mat.repet) = att
    colnames(mat.f) = suj
    rownames(mat.f) = att
    ind = sort(vec.rank.f, index.return = TRUE)$ix
    names(finter.vec) = att
    names(fprod.vec) = att
    indice = (1:nb.suj)[ind]
    suj = suj[ind]
    vec.rank.f = vec.rank.f[ind]
    ind2 = sort(fprod.vec, index.return = TRUE, decreasing = TRUE)$ix
    att = att[ind2]
    fprod.vec = fprod.vec[ind2]
    finter.vec = finter.vec[ind2]
    fdisag.vec = fdisag.vec[ind2]
    ferror.vec = ferror.vec[ind2]
    pprod.vec = pprod.vec[ind2]
    pinter.vec = pinter.vec[ind2]
    pdisag.vec = pdisag.vec[ind2]
    perror.vec = perror.vec[ind2]
    mat.f = mat.f[ind2, ind]
    mat.p = mat.p[ind2, ind]
    mat.sign = mat.sign[ind2, ind]
    mat.repet = mat.repet[ind2, ind]
    mat.repet2 = mat.repet2[ind2, ind]
    mat.disag = mat.disag[ind2, ind]
    mat.repet.f = mat.repet.f[ind2, ind]
    mat.level = mat.level[ind2, ind]
    mat.scal = mat.scal[ind2, ind]
    mat.scal.coef = mat.scal.coef[ind2, ind]
    panel.repet = panel.repet[ind2]
    mean.vec = mean.vec[ind2]
    style = NULL
    indices = which(pprod.vec <= seuilPanel, arr.ind = TRUE)
    if (length(indices) > 0) {
        for (i in 1:length(indices)) {
            style = AddHtmlStyle(style, att[indices[i]], GetLabel("FProd", 
                language), backgroundColor = "#00FF00")
        }
    }
    indices = which(pprod.vec > seuilPanel, arr.ind = TRUE)
    if (length(indices) > 0) {
        for (i in 1:length(indices)) {
            style = AddHtmlStyle(style, att[indices[i]], GetLabel("FProd", 
                language), backgroundColor = "red")
        }
    }
    indices = which(pinter.vec <= seuilPanel, arr.ind = TRUE)
    if (length(indices) > 0) {
        for (i in 1:length(indices)) {
            style = AddHtmlStyle(style, att[indices[i]], GetLabel("FScal", 
                language), backgroundColor = "red")
        }
    }
    indices = which(pinter.vec > seuilPanel, arr.ind = TRUE)
    if (length(indices) > 0) {
        for (i in 1:length(indices)) {
            style = AddHtmlStyle(style, att[indices[i]], GetLabel("FScal", 
                language), backgroundColor = "#00FF00")
        }
    }
    indices = which(pdisag.vec <= seuilPanel, arr.ind = TRUE)
    if (length(indices) > 0) {
        for (i in 1:length(indices)) {
            style = AddHtmlStyle(style, att[indices[i]], GetLabel("FDisag", 
                language), backgroundColor = "red")
        }
    }
    indices = which(pdisag.vec > seuilPanel, arr.ind = TRUE)
    if (length(indices) > 0) {
        for (i in 1:length(indices)) {
            style = AddHtmlStyle(style, att[indices[i]], GetLabel("FDisag", 
                language), backgroundColor = "#00FF00")
        }
    }
    indices = which(mat.p <= seuilIndiv & mat.disag > seuilIndiv, 
        arr.ind = TRUE)
    if (length(indices) > 0) {
        for (i in 1:nrow(indices)) {
            style = AddHtmlStyle(style, att[indices[i, 1]], suj[indices[i, 
                2]], backgroundColor = "#00FF00")
        }
    }
    indices = which(mat.p <= seuilIndiv & mat.disag <= seuilIndiv, 
        arr.ind = TRUE)
    if (length(indices) > 0) {
        for (i in 1:nrow(indices)) {
            style = AddHtmlStyle(style, att[indices[i, 1]], suj[indices[i, 
                2]], backgroundColor = "red")
        }
    }
    indices = which(mat.p > seuilIndiv, arr.ind = TRUE)
    if (length(indices) > 0) {
        for (i in 1:nrow(indices)) {
            style = AddHtmlStyle(style, att[indices[i, 1]], suj[indices[i, 
                2]], backgroundColor = "yellow")
        }
    }
    tableauTxt = matrix("", nb.att, nb.suj)
    for (j in 1:nb.suj) {
        for (i in 1:nb.att) {
            tableauTxt[i, j] = texteCellule(discrim = mat.p[i, 
                j], seuilIndivrim.1, seuilIndivrim.2, seuilIndivrim.3, 
                mat.scal.coef[i, j], mat.scal[i, j], seuilIndiv, 
                mat.repet2[i, j], text.rep = text.rep)
        }
    }
    tableauRes = cbind(round(mean.vec, digits = 2), fprod.vec, 
        finter.vec, fdisag.vec, round(ferror.vec, digits = 2), 
        tableauTxt)
    tableauRes = rbind(tableauRes, c("-", "-", "-", "-", "-", 
        as.character(round(vec.rank.f, digits = 2))))
    colnames(tableauRes) = c(GetLabel("Mean", language), GetLabel("FProd", 
        language), GetLabel("FScal", language), GetLabel("FDisag", 
        language), GetLabel("RMSE", language), suj)
    rownames(tableauRes) = c(att, GetLabel("RankF", language))
    txt = "<html>"
    txt = paste(txt, "<h3>", titre, "</h3>", MatrixToHtml(tableauRes, 
        style = style), sep = "")
    txt = paste(txt, "<table border='1' cellspacing='1'><tr><td colspan='3'><b>", 
        GetLabel("PanelPerf", language), "</b></td><td><b>", 
        GetLabel("PanelistPerf", language), "</b></td></tr>")
    txt = paste(txt, "<tr><td><b>", GetLabel("FProd", language), 
        "</b></td><td><b>", GetLabel("FScal", language), "</b></td><td><b>", 
        GetLabel("FDisag", language), "</b></td><td>", GetLabel("LegendDiscrim", 
            language), "</td></tr>")
    txt = paste(txt, "<tr><td style='text-align:center;background-color:#FF0000'>p> ", 
        seuilPanel, "</td><td style='text-align:center;background-color:#FF0000'>p<", 
        seuilPanel, "</td><td style='text-align:center;background-color:#FF0000'> p<", 
        seuilPanel, "</td><td rowspan=2>", GetLabel("LegendScaling", 
            language), "</td></tr>")
    txt = paste(txt, "<tr><td style='text-align:center;background-color:#00FF00'>p< ", 
        seuilPanel, "</td><td style='text-align:center;background-color:#00FF00'>p>", 
        seuilPanel, "</td><td style='text-align:center;background-color:#00FF00'> p>", 
        seuilPanel, "</td></tr>")
    txt = paste(txt, "<tr><td></td><td></td><td></td><td>", GetLabel("LegendDisag", 
        language), "</td></tr>")
    txt = paste(txt, "<tr><td></td><td></td><td></td><td>", GetLabel("LegendRepet", 
        language), "</td></tr>")
    txt = paste(txt, "</table><br>", GetLabel("PhraseNA", language), 
        "</html>")
    write(txt, file = paste(output, ".html", sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
