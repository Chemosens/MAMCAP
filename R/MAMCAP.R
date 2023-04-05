MAMCAP=function (object, panelLimit = 0.05, indivLimit = 0.05, language = "fr",
          output = "tabCAP", correction = TRUE)
{
  indivLimit1 = 0.01
  indivLimit2 = 0.05
  indivLimit3 = 0.1
  texteCellule = function(discrim, indivLimit1 = 0.01, indivLimit2 = 0.05,
                          indivLimit3 = 0.1, scaling, p.scaling, indivLimit, p.rep,
                          text.rep) {
    V = sort(c(indivLimit1, indivLimit2, indivLimit3))
    indivLimit1 = V[1]
    indivLimit2 = V[2]
    indivLimit3 = V[3]
    cell = ""
    if (!is.na(discrim)) {
      if (p.scaling < indivLimit) {
        if (scaling > 1) {
          cell = paste(cell, "&lt;", sep = "")
        }
        if (scaling <= 1 & scaling > 0) {
          cell = paste(cell, "&gt;", sep = "")
        }
        if (scaling <= 0) {
          cell = paste(cell, "?", sep = "")
        }
      }
      else {
        cell = paste(cell, "|", sep = "")
      }
      if (indivLimit1 < discrim & discrim < indivLimit2) {
        cell = paste(cell, "--", sep = "")
      }
      if (indivLimit1 > discrim) {
        cell = paste(cell, "---", sep = "")
      }
      if (indivLimit3 < discrim) {
        cell = paste(cell, " ", sep = "")
      }
      if (indivLimit2 < discrim & discrim < indivLimit3) {
        cell = paste(cell, "-", sep = "")
      }
      if (p.rep < indivLimit) {
        cell = paste(cell, text.rep, sep = "")
      }
      if (p.scaling < indivLimit) {
        if (scaling > 1) {
          cell = paste(cell, "&gt;", sep = "")
        }
        if (scaling <= 1 & scaling > 0) {
          cell = paste(cell, "&lt;", sep = "")
        }
        if (scaling <= 0) {
          cell = paste(cell, "?", sep = "")
        }
      }
      else {
        cell = paste(cell, "|", sep = "")
      }
    }
    if (is.na(discrim)) {
      cell = "NA"
    }
    return(cell)
  }
  data = object$CanonicalData
  dataRes = object$ExtendedData
  firstvar = 5
  lastvar = dim(dataRes)[2]
  attribut = GetLabel("Attributes", language)
  titre = paste(GetLabel("TitreCAP1", language), panelLimit,
                GetLabel("TitreCAP2", language), indivLimit, ")", sep = "")
  text.rep = GetLabel("LR", language)
  att = object$Attributes
  attributeNumber = length(att)
  rep = object$Replicates
  replicateNumber = length(rep)
  panelAverage = rep(0, attributeNumber)
  panelProductF = rep(0, attributeNumber)
  panelScalingF = rep(0, attributeNumber)
  panelDisagreementF = rep(0, attributeNumber)
  frepet.vec = rep(0, attributeNumber)
  panelScalingPvalue = rep(0, attributeNumber)
  panelProductPvalue = rep(0, attributeNumber)
  panelDisagreementPvalue = rep(0, attributeNumber)
  panelRepeatability = rep(0, attributeNumber)
  panel.repet = rep(0, attributeNumber)
  suj = object$Subjects
  prod = object$Products
  productNumber = length(prod)
  subjectNumber = length(suj)
  if (correction) {
    indivLimit = indivLimit/subjectNumber
  }
  individualProductF = matrix(0, attributeNumber, subjectNumber)
  individualProductF3 = matrix(NA, attributeNumber, subjectNumber)
  individualRepeatability = matrix(1, attributeNumber, subjectNumber)
  individualRepeatability2 = matrix(1, attributeNumber, subjectNumber)
  individualRepeatability2 = matrix(1, attributeNumber, subjectNumber)
  mat.SS.suj = matrix(1, attributeNumber, subjectNumber)
  individualRepeatability.f = matrix(0, attributeNumber, subjectNumber)
  individualDisagreementF.f = matrix(0, attributeNumber, subjectNumber)
  mat.level = matrix(0, attributeNumber, subjectNumber)
  individualDisagreementF = matrix(0, attributeNumber, subjectNumber)
  mat.scal = matrix("", attributeNumber, subjectNumber)
  mat.scal.coef = matrix(0, attributeNumber, subjectNumber)
  individualProductPvalue = matrix(0, attributeNumber, subjectNumber)
  significanceMatrix = matrix(FALSE, attributeNumber, subjectNumber)
  rankMatrix = matrix(0, attributeNumber, subjectNumber)
  colnames(rankMatrix) = suj
  rownames(rankMatrix) = att
  FRank = rep(0, subjectNumber)
  if (productNumber < 3) {
    stop(GetLabel("WarningThreeProd", language))
  }
  if (replicateNumber < 2) {
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
    TabPerf1[c(1, 3, 5, 7, 9, 11), 2:(subjectNumber + 2)] = as.numeric(noquote(TabPerf1[c(1,
                                                                                          3, 5, 7, 9, 11), 2:(subjectNumber + 2)]))
    for (j in 1:subjectNumber) {
      individualRepeatability.f[i, j] = as.numeric(noquote(TabPerf1[11,
                                                                    1 + j]))
      individualProductF[i, j] = as.numeric(noquote(TabPerf1[3,
                                                             1 + j]))
      if (individualRepeatability.f[i, j] == 0) {
        individualProductF3[i, j] = NA
      }
      else {
        individualProductF3[i, j] = individualProductF[i,
                                                       j] * individualProductF[i, j]/(individualRepeatability.f[i,
                                                                                                                j] * individualRepeatability.f[i, j])
      }
      individualProductPvalue[i, j] = as.numeric(noquote(TabPerf1[4,
                                                                  1 + j]))
      individualDisagreementF.f[i, j] = as.numeric(noquote(TabPerf1[9,
                                                                    1 + j]))
      individualDisagreementF[i, j] = as.numeric(noquote(TabPerf1[10,
                                                                  1 + j]))
      individualRepeatability[i, j] = individualRepeatability.f[i,
                                                                j] * individualRepeatability.f[i, j] * productNumber *
        (replicateNumber - 1)
      panel.repet[i] = TabPerf1[12, 1 + subjectNumber +
                                  1]
      individualRepeatability2[i, j] = as.numeric(TabPerf1[12,
                                                           1 + j])
      mat.level[i, j] = as.numeric(noquote(TabPerf1[1,
                                                    1 + j]))
      mat.scal[i, j] = TabPerf1[6, 1 + j]
      mat.scal.coef[i, j] = TabPerf1[5, 1 + j]
    }
  }
  ANOV = resultat[[2]]
  for (i in 1:attributeNumber) {
    dataI = data[data[, "AttributeCode"] == ListATT[i], ]
    panelAverage[i] = mean(dataI[!is.na(dataI[, "Score"]),
                                 "Score"])
    anovaResults = anova(lm(dataI[, "Score"] ~ dataI[, "ProductCode"] *
                              dataI[, "SubjectCode"]))
    tab = ANOV[, , i]
    panelProductF[i] = tab[2, 4]
    panelScalingF[i] = tab[3, 4]
    panelProductPvalue[i] = tab[2, 5]
    panelScalingPvalue[i] = tab[3, 5]
    panelDisagreementF[i] = tab[4, 4]
    panelDisagreementPvalue[i] = tab[4, 5]
    panelRepeatability[i] = sqrt(as.numeric(tab[5, 2]))
  }
  individualProductF3[is.na(individualProductF3)] = 0
  individualProductF2 = max(individualProductF3[!is.na(individualProductF3)]) -
    individualProductF3
  rankMatrix = apply(individualProductF2, 1, rank)
  colnames(rankMatrix) = att
  rownames(rankMatrix) = suj
  for (j in 1:subjectNumber) {
    FRank[j] = mean(rankMatrix[j, ])
  }
  names(FRank) = suj
  rownames(individualProductPvalue) = att
  colnames(individualProductPvalue) = suj
  colnames(individualDisagreementF) = suj
  rownames(individualDisagreementF) = att
  colnames(individualRepeatability) = suj
  rownames(individualRepeatability) = att
  colnames(individualProductF) = suj
  rownames(individualProductF) = att
  ind = sort(FRank, index.return = TRUE)$ix
  names(panelScalingF) = att
  names(panelProductF) = att
  indice = (1:subjectNumber)[ind]
  suj = suj[ind]
  FRank = FRank[ind]
  ind2 = sort(panelProductF, index.return = TRUE, decreasing = TRUE)$ix
  att = att[ind2]
  panelProductF = panelProductF[ind2]
  panelScalingF = panelScalingF[ind2]
  panelDisagreementF = panelDisagreementF[ind2]
  panelRepeatability = panelRepeatability[ind2]
  panelScalingPvalue = panelScalingPvalue[ind2]
  panelProductPvalue = panelProductPvalue[ind2]
  panelDisagreementPvalue = panelDisagreementPvalue[ind2]
  individualProductF = individualProductF[ind2, ind]
  individualProductPvalue = individualProductPvalue[ind2, ind]
  significanceMatrix = significanceMatrix[ind2, ind]
  individualRepeatability = individualRepeatability[ind2, ind]
  individualRepeatability2 = individualRepeatability2[ind2,
                                                      ind]
  individualDisagreementF = individualDisagreementF[ind2, ind]
  individualRepeatability.f = individualRepeatability.f[ind2,
                                                        ind]
  mat.level = mat.level[ind2, ind]
  mat.scal = mat.scal[ind2, ind]
  mat.scal.coef = mat.scal.coef[ind2, ind]
  panel.repet = panel.repet[ind2]
  panelAverage = panelAverage[ind2]
  style = NULL
  indices = which(panelProductPvalue <= panelLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:length(indices)) {
      style = AddHtmlStyle(style, att[indices[i]], GetLabel("FProd",
                                                            language), backgroundColor = "#00FF00")
    }
  }
  indices = which(panelProductPvalue > panelLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:length(indices)) {
      style = AddHtmlStyle(style, att[indices[i]], GetLabel("FProd",
                                                            language), backgroundColor = "red")
    }
  }
  indices = which(panelScalingPvalue <= panelLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:length(indices)) {
      style = AddHtmlStyle(style, att[indices[i]], GetLabel("FScal",
                                                            language), backgroundColor = "red")
    }
  }
  indices = which(panelScalingPvalue > panelLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:length(indices)) {
      style = AddHtmlStyle(style, att[indices[i]], GetLabel("FScal",
                                                            language), backgroundColor = "#00FF00")
    }
  }
  indices = which(panelDisagreementPvalue <= panelLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:length(indices)) {
      style = AddHtmlStyle(style, att[indices[i]], GetLabel("FDisag",
                                                            language), backgroundColor = "red")
    }
  }
  indices = which(panelDisagreementPvalue > panelLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:length(indices)) {
      style = AddHtmlStyle(style, att[indices[i]], GetLabel("FDisag",
                                                            language), backgroundColor = "#00FF00")
    }
  }
  indices = which(individualProductPvalue <= indivLimit & individualDisagreementF >
                    indivLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:nrow(indices)) {
      style = AddHtmlStyle(style, att[indices[i, 1]], suj[indices[i,
                                                                  2]], backgroundColor = "#00FF00")
    }
  }
  indices = which(individualProductPvalue <= indivLimit & individualDisagreementF <=
                    indivLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:nrow(indices)) {
      style = AddHtmlStyle(style, att[indices[i, 1]], suj[indices[i,
                                                                  2]], backgroundColor = "red")
    }
  }
  indices = which(individualProductPvalue > indivLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:nrow(indices)) {
      style = AddHtmlStyle(style, att[indices[i, 1]], suj[indices[i,
                                                                  2]], backgroundColor = "yellow")
    }
  }
  tableauTxt = matrix("", attributeNumber, subjectNumber)
  for (j in 1:subjectNumber) {
    for (i in 1:attributeNumber) {
      tableauTxt[i, j] = texteCellule(discrim = individualProductPvalue[i,
                                                                        j], indivLimit1, indivLimit2, indivLimit3, mat.scal.coef[i,
                                                                                                                                 j], mat.scal[i, j], indivLimit, individualRepeatability2[i,
                                                                                                                                                                                          j], text.rep = text.rep)
    }
  }
  tableauRes = cbind(round(panelAverage, digits = 2), panelProductF,
                     panelScalingF, panelDisagreementF, round(panelRepeatability,
                                                              digits = 2), tableauTxt)
  tableauRes = rbind(tableauRes, c("-", "-", "-", "-", "-",
                                   as.character(round(FRank, digits = 2))))
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
              panelLimit, "</td><td style='text-align:center;background-color:#FF0000'>p<",
              panelLimit, "</td><td style='text-align:center;background-color:#FF0000'> p<",
              panelLimit, "</td><td rowspan=2>", GetLabel("LegendScaling",
                                                          language), "</td></tr>")
  txt = paste(txt, "<tr><td style='text-align:center;background-color:#00FF00'>p< ",
              panelLimit, "</td><td style='text-align:center;background-color:#00FF00'>p>",
              panelLimit, "</td><td style='text-align:center;background-color:#00FF00'> p>",
              panelLimit, "</td></tr>")
  txt = paste(txt, "<tr><td></td><td></td><td></td><td>", GetLabel("LegendDisag",
                                                                   language), "</td></tr>")
  txt = paste(txt, "<tr><td></td><td></td><td></td><td>", GetLabel("LegendRepet",
                                                                   language), "</td></tr>")
  txt = paste(txt, "</table><br>", GetLabel("PhraseNA", language),
              "</html>")
  write(txt, file = paste(output, ".html", sep = ""))
}