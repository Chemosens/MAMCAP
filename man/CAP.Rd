\name{CAP}
\alias{CAP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CAP
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CAP(object, seuilPanel = 0.05, seuilIndivRepeatability = 0.01, seuilIndivDiscrimination = 0.1, seuilIndivAgreement = 0.2, language = "fr", output = "tabCAP", correction = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{seuilPanel}{
%%     ~~Describe \code{seuilPanel} here~~
}
  \item{seuilIndivRepeatability}{
%%     ~~Describe \code{seuilIndivRepeatability} here~~
}
  \item{seuilIndivDiscrimination}{
%%     ~~Describe \code{seuilIndivDiscrimination} here~~
}
  \item{seuilIndivAgreement}{
%%     ~~Describe \code{seuilIndivAgreement} here~~
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
function (object, seuilPanel = 0.05, seuilIndivRepeatability = 0.01, 
    seuilIndivDiscrimination = 0.1, seuilIndivAgreement = 0.2, 
    language = "fr", output = "tabCAP", correction = TRUE) 
{
    contrasts.sas.2 = function(hedo, prod, suj, data, num.prod) {
        data[, prod] = as.factor(as.character(data[, prod]))
        data[, suj] = as.factor(as.character(data[, suj]))
        nb.suj = length(levels(data[, suj]))
        nb.prod = length(levels(data[, prod]))
        mat.contraste = matrix(1/(nb.prod - 1), nb.prod, nb.prod - 
            1)
        mat.contraste[num.prod, ] = -1
        if (nb.suj > 1) {
            A.classique = anova(lm(data[, hedo] ~ data[, prod] + 
                data[, suj]))
            deg.lib.res = A.classique["Residuals", "Df"]
            if (A.classique["Residuals", "Df"] != 0) {
                MSres = A.classique["Residuals", "Mean Sq"]
                contrasts(data[, prod]) = mat.contraste
                A = anova(lm(data[, hedo] ~ data[, prod] + data[, 
                  suj]))
                deg.lib.prod = A[1, "Df"]
                MSprod = A[1, 3]
                if (MSres != 0) {
                  F = MSprod/MSres
                  PF = pf(F, df1 = deg.lib.prod, df2 = deg.lib.res, 
                    lower.tail = FALSE)
                }
                if (MSres == 0) {
                  PF = 1
                  print("Attention le mean square nul")
                }
            }
            else {
                PF = 1
                print("Attention le mean square residus n'est pas defini")
            }
        }
        else {
            PF = 1
            print("Attention,un seul sujet")
        }
        return(PF)
    }
    data = object$CanonicalData
    dataRes = object$ExtendedData
    tab2 = data
    if (language == "fr") {
        attribut = "Attribut"
        fprod = "Fprod"
        finter = "Finter"
        mean = "Moyenne"
        rmse = "RMSE"
        titre = paste("Table CAP(seuil.rep=", seuilIndivRepeatability, 
            ",seuil.disc=", seuilIndivDiscrimination, ",seuil.accord=", 
            seuilIndivAgreement, ")")
        rangk = "rang de F"
        discriminant = "Panéliste discriminant ?"
        accord = "Panéliste en acccord ?"
        repetable = "Panéliste répétable ?"
        diagnostic = "diagnostic"
        oui = "oui"
        non = "non"
        non.teste = "non testé"
        phrase.na = "La présence d'éventuels NA montre un manque de variabilité empêchant de faire les calculs. La présence d'éventuels ? montre qu'il n'y a pas de répétitions. La présence de 0 montre que le sujet a donné exatement les memes notes à tous les produits"
    }
    if (language == "en") {
        attribut = "Attribute"
        fprod = "Fprod"
        finter = "Finter"
        mean = "Mean"
        rmse = "RMSE"
        titre = paste("CAP Table(rep.limit=", seuilIndivRepeatability, 
            ",disc.limit=", seuilIndivDiscrimination, ",agree.limit=", 
            seuilIndivAgreement, ")")
        rangk = "F Rank"
        discriminant = "differential panellist ?"
        accord = "Panellist in acccord ?"
        repetable = "Repeatable panellist ?"
        diagnostic = "diagnosis"
        oui = "yes"
        non = "no"
        non.teste = "no test"
        phrase.na = "Possible NA's shows a problem in calculation (no variability). Possible ? shows there is just one repetition. Possible 0 shows that the subject scores the products all the same "
    }
    att = levels(data[, "AttributeCode"])
    nb.att = length(att)
    mean.vec = rep(0, nb.att)
    fprod.vec = rep(0, nb.att)
    finter.vec = rep(0, nb.att)
    pprod.vec = rep(0, nb.att)
    pinter.vec = rep(0, nb.att)
    rmse.vec = rep(0, nb.att)
    suj = levels(as.factor(as.character(data[, "SubjectCode"])))
    prod = levels(as.factor(data[, "ProductCode"]))
    nb.prod = length(prod)
    repetitions = levels(as.factor(data[, "Replicate"]))
    nb.rep = length(repetitions)
    nb.suj = length(suj)
    mat.f = matrix(0, nb.att, nb.suj)
    mat.repet = matrix(0, nb.att, nb.suj)
    mat.disag = matrix(0, nb.att, nb.suj)
    mat.disag.2 = matrix(0, nb.att, nb.suj)
    mat.p = matrix(0, nb.att, nb.suj)
    mat.sign = matrix(FALSE, nb.att, nb.suj)
    mat.rank = matrix(0, nb.att, nb.suj)
    colnames(mat.rank) = suj
    rownames(mat.rank) = att
    vec.rank.f = rep(0, nb.suj)
    for (i in 1:nb.att) {
        data.i = data[data[, "AttributeCode"] == att[i], ]
        mean.vec[i] = mean(data.i[!is.na(data.i[, "Score"]), 
            "Score"])
        res.anova = anova(lm(data.i[, "Score"] ~ data.i[, "ProductCode"] * 
            data.i[, "SubjectCode"]))
        rmse.vec[i] = sqrt(res.anova[4, 3])
        if (!is.na(rmse.vec[i]) & rmse.vec[i] != 0) {
            fprod.vec[i] = (res.anova[1, 2]/res.anova[1, 1])/(res.anova[3, 
                2]/res.anova[3, 1])
            finter.vec[i] = res.anova[3, 4]
            pprod.vec[i] = pf(fprod.vec[i], res.anova[1, 1], 
                res.anova[3, 1], lower.tail = FALSE)
            pinter.vec[i] = res.anova[3, 5]
        }
        else {
            fprod.vec[i] = NA
            finter.vec[i] = NA
            pprod.vec[i] = NA
            pinter.vec[i] = NA
        }
        dataEvent = data.i
        for (j in 1:nb.suj) {
            repetable1 = sqldf("select ProductCode,SubjectCode,sqrt(variance(Score)) as var from dataEvent group by ProductCode,SubjectCode")
            mat.repet[i, j] = contrasts.sas.2("var", "SubjectCode", 
                "ProductCode", repetable1, j)
            data.ij = data.i[data.i[, "SubjectCode"] == suj[j], 
                ]
            res.anova2 = anova(lm(data.ij[, "Score"] ~ data.ij[, 
                "ProductCode"]))
            if (res.anova2[2, 2] != 0) {
                mat.f[i, j] = res.anova2[1, 4]
                mat.p[i, j] = res.anova2[1, 5]
            }
            else {
                mat.f[i, j] = NA
                mat.p[i, j] = NA
            }
        }
    }
    mat.f2 = max(mat.f[!is.na(mat.f)]) - mat.f
    mat.rank = apply(mat.f2, 1, rank)
    colnames(mat.rank) = att
    rownames(mat.rank) = suj
    for (j in 1:nb.suj) {
        vec.rank.f[j] = mean(mat.rank[j, ])
    }
    names(vec.rank.f) = suj
    rownames(mat.p) = att
    colnames(mat.p) = suj
    mat.sign = (mat.p < seuilIndivDiscrimination)
    colnames(mat.sign) = suj
    rownames(mat.sign) = att
    for (i in 1:nb.att) {
        for (j in 1:nb.suj) {
            datasuj = data[data[, "SubjectCode"] == suj[j] & 
                data[, "AttributeCode"] == att[i], ]
            vect.suj = as.data.frame(datasuj)
            resu = sqldf("select ProductCode,SubjectCode,avg(Score) as moy from datasuj group by ProductCode,SubjectCode")
            prod.1 = resu[, "ProductCode"]
            if (length(suj[mat.sign[i, ]]) > 1) {
                if (nb.rep > 1) {
                  vect.moy = data[data[, "SubjectCode"] != suj[j] & 
                    data[, "AttributeCode"] == att[i] & dans.vect(data[, 
                    "SubjectCode"], suj[mat.sign[i, ]]), ]
                }
                else {
                  vect.moy = data[data[, "SubjectCode"] != suj[j] & 
                    data[, "AttributeCode"] == att[i], ]
                }
                vect.res = rep(0, nb.prod)
                for (k in 1:nb.prod) {
                  vect.res[k] = mean(vect.moy[vect.moy[, "ProductCode"] == 
                    prod.1[k], "Score"])
                }
                names(vect.res) = prod.1
                test.cor = cor.test(resu[, "moy"], vect.res, 
                  method = "pearson", alternative = "greater")
                mat.disag[i, j] = test.cor$p.value
                mat.disag.2[i, j] = test.cor$estimate
            }
            else {
                mat.disag[i, j] = 0
                mat.disag.2[i, j] = 1
            }
        }
    }
    colnames(mat.disag) = suj
    rownames(mat.disag) = att
    colnames(mat.disag.2) = suj
    rownames(mat.disag.2) = att
    colnames(mat.repet) = suj
    rownames(mat.repet) = att
    colnames(mat.f) = suj
    rownames(mat.f) = att
    ind = sort(vec.rank.f, index.return = TRUE)$ix
    indice = (1:nb.suj)[ind]
    suj = suj[ind]
    vec.rank.f = vec.rank.f[ind]
    if (nb.rep > 1) {
        ind2 = sort(fprod.vec, index.return = TRUE, decreasing = TRUE)$ix
    }
    else {
        ind2 = 1:nb.att
    }
    att = att[ind2]
    fprod.vec = fprod.vec[ind2]
    finter.vec = finter.vec[ind2]
    pprod.vec = pprod.vec[ind2]
    pinter.vec = pinter.vec[ind2]
    mat.f = mat.f[ind2, ind]
    mat.p = mat.p[ind2, ind]
    mat.sign = mat.sign[ind2, ind]
    mat.repet = mat.repet[ind2, ind]
    mat.disag = mat.disag[ind2, ind]
    mat.disag.2 = mat.disag.2[ind2, ind]
    mean.vec = mean.vec[ind2]
    rmse.vec = rmse.vec[ind2]
    txt = "<html>"
    txt = paste(txt, "<h3 style='text-align:center'>", titre, 
        "</h3>")
    txt = paste(txt, "<table border=2 cellspacing=1 width=100\%>")
    txt = paste(txt, "<tr><td style='text-align:center;background-color:#EEEEEE'><b>", 
        attribut, "</b></td><td style='text-align:center;background-color:#EEEEEE'><b>", 
        mean, "</b></td><td style='text-align:center;background-color:#EEEEEE'><b>", 
        fprod, "</b></td>")
    txt = paste(txt, "<td style='text-align:center;background-color:#EEEEEE'><b>", 
        finter, "</b></td>")
    txt = paste(txt, "<td style='text-align:center;background-color:#EEEEEE'><b>", 
        rmse, "</b></td>")
    for (j in 1:nb.suj) {
        txt = paste(txt, "<td style='text-align:center;background-color:#EEEEEE'><b>", 
            suj[j], "</td>")
    }
    txt = paste(txt, "</tr>")
    for (i in 1:nb.att) {
        txt = paste(txt, "<tr>")
        txt = paste(txt, "<td style='text-align:center;background-color:#EEEEEE'>", 
            att[i], "</td>")
        txt = paste(txt, "<td  style='text-align:center'>", decimal(mean.vec[i], 
            digits = 2), "</td>")
        pval1 = pprod.vec[i]
        if (!is.na(pval1)) {
            if (pval1 < seuilPanel) {
                rep1 = fprod.vec[i]
                col1 = noquote("#AAFFAA")
            }
            if (pval1 < seuilPanel & pval1 > seuilPanel) {
                rep1 = fprod.vec[i]
                col1 = noquote("#FFFFAA")
            }
            if (pval1 > seuilPanel) {
                rep1 = fprod.vec[i]
                col1 = noquote("#FFAAAA")
            }
        }
        if (is.na(pval1)) {
            rep1 = "NA"
            col1 = noquote("#DDDDDD")
        }
        txt = paste(txt, "<td style='text-align:center;background-color:", 
            col1, "'>", decimal(rep1, digits = 2), "</td>")
        pval2 = pinter.vec[i]
        if (!is.na(pval2)) {
            if (pval2 > seuilPanel) {
                rep2 = finter.vec[i]
                col2 = noquote("#AAFFAA")
            }
            if (pval2 < seuilPanel & pval2 > seuilPanel) {
                rep2 = finter.vec[i]
                col2 = noquote("#FFFFAA")
            }
            if (pval2 < seuilPanel) {
                rep2 = finter.vec[i]
                col2 = noquote("#FFAAAA")
            }
        }
        if (is.na(pval2)) {
            rep2 = "NA"
            col2 = noquote("#DDDDDD")
        }
        txt = paste(txt, "<td style='text-align:center;background-color:", 
            col2, "'>", decimal(rep2, digits = 2), "</td>")
        txt = paste(txt, "<td  style='text-align:center'>", decimal(rmse.vec[i], 
            digits = 2), "</td>")
        for (j in 1:nb.suj) {
            pval6 = mat.p[i, j]
            if (!is.na(pval6)) {
                if (pval6 < seuilIndivDiscrimination) {
                  pea2 = mat.disag[i, j]
                  if (!is.na(pea2)) {
                    if (pea2 < seuilIndivAgreement) {
                      pval6ter = mat.repet[i, j]
                      if (!is.na(pval6ter)) {
                        if (pval6ter > seuilIndivRepeatability) {
                          rep6 = "+"
                          col6 = noquote("#AAFFAA")
                        }
                        else {
                          rep6 = "+!"
                          col6 = noquote("#AADDAA")
                        }
                      }
                      else {
                        rep6 = "+na"
                        col6 = noquote("#AADDAA")
                      }
                    }
                    else {
                      pval6ter = mat.repet[i, j]
                      if (!is.na(pval6ter)) {
                        if (pval6ter > seuilIndivRepeatability) {
                          rep6 = "-"
                          col6 = noquote("#FFAAAA")
                        }
                        else {
                          rep6 = "-!"
                          col6 = noquote("#DDAAAA")
                        }
                      }
                      else {
                        rep6 = "-na"
                        col6 = noquote("#FFAAAA")
                      }
                    }
                  }
                  if (is.na(pea2)) {
                    rep6 = "NA"
                    col6 = noquote("#FFFFFF")
                  }
                }
                else {
                  pval6ter = mat.repet[i, j]
                  if (!is.na(pval6ter)) {
                    if (pval6ter > seuilIndivRepeatability) {
                      rep6 = "="
                      col6 = noquote("#FFFFAA")
                    }
                    else {
                      rep6 = "=!"
                      col6 = noquote("#DDDDAA")
                    }
                  }
                  else {
                    rep6 = "=na"
                    col6 = noquote("#DDDDAA")
                  }
                }
            }
            if (is.na(pval6)) {
                vect.suj = data[data[, "SubjectCode"] == suj[j] & 
                  data[, "AttributeCode"] == att[i], ]
                vect.moy = data[data[, "SubjectCode"] != suj[j] & 
                  data[, "AttributeCode"] == att[i], ]
                if (var(vect.suj[, "Score"]) == 0) {
                  rep6 = "0"
                  col6 = "#DDDDDD"
                }
                pea2 = mat.disag[i, j]
                if (!is.na(pea2)) {
                  if (pea2 < seuilIndivAgreement) {
                    rep6 = "+?"
                    col6 = "#DDFFDD"
                  }
                  if (pea2 > seuilIndivAgreement) {
                    rep6 = "-?"
                    col6 = "#FFDDDD"
                  }
                }
                else {
                  rep6 = "na?"
                  col6 = "#FFDDDD"
                }
            }
            vect.suj = data[data[, "SubjectCode"] == suj[j] & 
                data[, "AttributeCode"] == att[i], ]
            if (var(vect.suj[, "Score"]) == 0) {
                rep6 = "0"
                col6 = "#FFFFAA"
            }
            txt = paste(txt, "<td style='text-align:center;background-color:", 
                col6, "'>", rep6, "</td>")
        }
        txt = paste(txt, "</tr>")
    }
    txt = paste(txt, "<tr>")
    txt = paste(txt, "<td style='text-align:center;background-color:#EEEEEE'>", 
        rangk, "</td>")
    txt = paste(txt, "<td  style='text-align:center'>-</td>")
    txt = paste(txt, "<td  style='text-align:center'>-</td>")
    txt = paste(txt, "<td  style='text-align:center'>-</td>")
    txt = paste(txt, "<td style='text-align:center'>-</td>")
    for (i in 1:nb.suj) {
        txt = paste(txt, "<td style='text-align:center'>", round(vec.rank.f[i], 
            digits = 2), "</td>")
    }
    txt = paste(txt, "</tr>")
    txt = paste(txt, "</table>")
    txt = paste(txt, "<p>")
    txt = paste(txt, "<table border=2 cellspacing=1>")
    txt = paste(txt, "<tr>")
    txt = paste(txt, "<td > F PROD </td>")
    txt = paste(txt, "<td> F INTER </td>")
    txt = paste(txt, "</tr>")
    txt = paste(txt, "<tr><td style='text-align:center;background-color:#AAFFAA'>p<=0.05</td><td style='text-align:center;background-color:#FFAAAA'>p<=0.01</td></tr>")
    txt = paste(txt, "<tr><td style='text-align:center;background-color:#FFAAAA'>p>0.05</td><td style='text-align:center;background-color:#AAFFAA'>p>0.1</td></tr>")
    txt = paste(txt, "</table>")
    txt = paste(txt, "</p>")
    txt = paste(txt, "<p>")
    txt = paste(txt, "<table border=2 cellspacing=1>")
    txt = paste(txt, "<tr>")
    txt = paste(txt, "<td>", discriminant, " </td>")
    txt = paste(txt, "<td> ", accord, "</td>")
    txt = paste(txt, "<td> ", repetable, "</td>")
    txt = paste(txt, "<td> ", diagnostic, "</td>")
    txt = paste(txt, "</tr>")
    txt = paste(txt, "<tr><td rowspan=4>", oui, "</td><td rowspan=2>", 
        oui, "</td><td>", oui, "</td><td style='text-align:center;background-color:#AAFFAA'>+</td></tr>")
    txt = paste(txt, "<tr><td>", non, "</td><td style='text-align:center;background-color:#AADDAA'>!+</td></tr>")
    txt = paste(txt, "<tr><td rowspan=2>", non, "</td><td>", 
        oui, "</td><td style='text-align:center;background-color:#FFAAAA'>-</td></tr>")
    txt = paste(txt, "<tr><td>", non, "</td><td style='text-align:center;background-color:#DDAAAA'>!-</td></tr>")
    txt = paste(txt, "<tr><td rowspan=2>", non, "</td><td rowspan=2>", 
        non.teste, "</td><td>", oui, "</td><td style='text-align:center;background-color:#FFFFAA'>=</td></tr>")
    txt = paste(txt, "<tr><td>", non, "</td><td style='text-align:center;background-color:#DDDDAA'>=!</td></tr>")
    txt = paste(txt, "</table>")
    txt = paste(txt, phrase.na)
    txt = paste(txt, "</p>")
    txt = paste(txt, "</html>")
    write(txt, file = paste(output, ".html", sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
