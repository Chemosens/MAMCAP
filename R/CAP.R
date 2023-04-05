CAP=function (object, panelLimit = 0.05, indivRepeatabilityLimit = 0.01,
          indivDiscriminationLimit = 0.1, indivAgreementLimit = 0.2,
          language = "fr", output = "tabCAP", correction = TRUE)
{
  contrastSas = function(hedo, prod, suj, data, num.prod) {
    data[, prod] = as.factor(as.character(data[, prod]))
    data[, suj] = as.factor(as.character(data[, suj]))
    subjectNumber = length(levels(data[, suj]))
    productNumber = length(levels(data[, prod]))
    contrastMatrix = matrix(1/(productNumber - 1), productNumber,
                            productNumber - 1)
    contrastMatrix[num.prod, ] = -1
    if (subjectNumber > 1) {
      classicalAnova = anova(lm(data[, hedo] ~ data[, prod] +
                                  data[, suj]))
      ResidualDegreesOfFreedom = classicalAnova["Residuals",
                                                "Df"]
      if (classicalAnova["Residuals", "Df"] != 0) {
        MSres = classicalAnova["Residuals", "Mean Sq"]
        contrasts(data[, prod]) = contrastMatrix
        A = anova(lm(data[, hedo] ~ data[, prod] + data[,
                                                        suj]))
        ProductDegreesOfFreedom = A[1, "Df"]
        MSprod = A[1, 3]
        if (MSres != 0) {
          F = MSprod/MSres
          PF = pf(F, df1 = ProductDegreesOfFreedom, df2 = ResidualDegreesOfFreedom,
                  lower.tail = FALSE)
        }
        if (MSres == 0) {
          PF = 1
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
  vectorialIn = function(valeurs, vecteur) {
    res = rep(FALSE, length(valeurs))
    for (i in 1:length(valeurs)) {
      res[i] = valeurs[i] %in% vecteur
    }
    return(res)
  }
  data = object$CanonicalData
  dataRes = object$ExtendedData
  tab2 = data
  titre = paste(GetLabel("TitleCAP", language), panelLimit,
                GetLabel("TitreCAP2", language), indivDiscriminationLimit,
                ")", sep = "")
  att = levels(data[, "AttributeCode"])
  attributeNumber = length(att)
  panelAverage = rep(0, attributeNumber)
  panelProductF = rep(0, attributeNumber)
  panelProductPvalue = rep(0, attributeNumber)
  panelInteractionF = rep(0, attributeNumber)
  panelInteractionPvalue = rep(0, attributeNumber)
  panelInteractionPvalue = rep(0, attributeNumber)
  panelRepeatability = rep(0, attributeNumber)
  suj = levels(as.factor(as.character(data[, "SubjectCode"])))
  prod = levels(as.factor(data[, "ProductCode"]))
  productNumber = length(prod)
  repetitions = levels(as.factor(data[, "Replicate"]))
  replicateNumber = length(repetitions)
  subjectNumber = length(suj)
  individualProductF = matrix(0, attributeNumber, subjectNumber)
  individualRepeatability = matrix(0, attributeNumber, subjectNumber)
  individualDisagreementPvalue = matrix(0, attributeNumber,
                                        subjectNumber)
  individualDisagreementF = matrix(0, attributeNumber, subjectNumber)
  individualProductPvalue = matrix(0, attributeNumber, subjectNumber)
  significanceMatrix = matrix(FALSE, attributeNumber, subjectNumber)
  rankMatrix = matrix(0, attributeNumber, subjectNumber)
  colnames(rankMatrix) = suj
  rownames(rankMatrix) = att
  FRank = rep(0, subjectNumber)
  if (correction) {
    indivDiscriminationLimit = indivDiscriminationLimit/subjectNumber
  }
  for (i in 1:attributeNumber) {
    dataI = data[data[, "AttributeCode"] == att[i], ]
    panelAverage[i] = mean(dataI[!is.na(dataI[, "Score"]),
                                 "Score"])
    anovaResults = anova(lm(dataI[, "Score"] ~ dataI[, "ProductCode"] *
                              dataI[, "SubjectCode"]))
    panelRepeatability[i] = sqrt(anovaResults[4, 3])
    if (!is.na(panelRepeatability[i]) & panelRepeatability[i] !=
        0) {
      panelProductF[i] = (anovaResults[1, 2]/anovaResults[1,
                                                          1])/(anovaResults[3, 2]/anovaResults[3, 1])
      panelInteractionF[i] = anovaResults[3, 4]
      panelProductPvalue[i] = pf(panelProductF[i], anovaResults[1,
                                                                1], anovaResults[3, 1], lower.tail = FALSE)
      panelInteractionPvalue[i] = anovaResults[3, 5]
    }
    else {
      panelProductF[i] = NA
      panelInteractionF[i] = NA
      panelProductPvalue[i] = NA
      panelInteractionPvalue[i] = NA
    }
    dataEvent = dataI
    for (j in 1:subjectNumber) {
      repetable1 = aggregate(dataEvent[, "Score"], list(dataEvent[,
                                                                  "ProductCode"], dataEvent[, "SubjectCode"]),
                             sd)
      colnames(repetable1) = c("ProductCode", "SubjectCode",
                               "var")
      individualRepeatability[i, j] = contrastSas("var",
                                                  "SubjectCode", "ProductCode", repetable1, j)
      dataIj = dataI[dataI[, "SubjectCode"] == suj[j],
      ]
      anovaResults2 = anova(lm(dataIj[, "Score"] ~ dataIj[,
                                                          "ProductCode"]))
      if (anovaResults2[2, 2] != 0) {
        individualProductF[i, j] = anovaResults2[1, 4]
        individualProductPvalue[i, j] = anovaResults2[1,
                                                      5]
      }
      else {
        individualProductF[i, j] = NA
        individualProductPvalue[i, j] = NA
      }
    }
  }
  individualProductF2 = max(individualProductF[!is.na(individualProductF)]) -
    individualProductF
  rankMatrix = apply(individualProductF2, 1, rank)
  colnames(rankMatrix) = att
  rownames(rankMatrix) = suj
  for (j in 1:subjectNumber) {
    FRank[j] = mean(rankMatrix[j, ])
  }
  names(FRank) = suj
  rownames(individualProductPvalue) = att
  colnames(individualProductPvalue) = suj
  significanceMatrix = (individualProductPvalue < indivDiscriminationLimit)
  colnames(significanceMatrix) = suj
  rownames(significanceMatrix) = att
  for (i in 1:attributeNumber) {
    for (j in 1:subjectNumber) {
      subjectData = data[data[, "SubjectCode"] == suj[j] &
                           data[, "AttributeCode"] == att[i], ]
      resu = aggregate(subjectData[, "Score"], list(subjectData[,
                                                                "ProductCode"], subjectData[, "SubjectCode"]),
                       mean)
      colnames(resu) = c("ProductCode", "SubjectCode",
                         "moy")
      prod1 = resu[, "ProductCode"]
      if (length(suj[significanceMatrix[i, ]]) > 1) {
        if (replicateNumber > 1) {
          meanVec = data[data[, "SubjectCode"] != suj[j] &
                           data[, "AttributeCode"] == att[i] & vectorialIn(data[,
                                                                                "SubjectCode"], suj[significanceMatrix[i,
                                                                                ]]), ]
        }
        else {
          meanVec = data[data[, "SubjectCode"] != suj[j] &
                           data[, "AttributeCode"] == att[i], ]
        }
        resultVec = rep(0, productNumber)
        for (k in 1:productNumber) {
          resultVec[k] = mean(meanVec[meanVec[, "ProductCode"] ==
                                        prod1[k], "Score"])
        }
        names(resultVec) = prod1
        test.cor = cor.test(resu[, "moy"], resultVec,
                            method = "pearson", alternative = "greater")
        individualDisagreementPvalue[i, j] = test.cor$p.value
        individualDisagreementF[i, j] = test.cor$estimate
      }
      else {
        individualDisagreementPvalue[i, j] = 0
        individualDisagreementF[i, j] = 1
      }
    }
  }
  colnames(individualDisagreementPvalue) = suj
  rownames(individualDisagreementPvalue) = att
  colnames(individualDisagreementF) = suj
  rownames(individualDisagreementF) = att
  colnames(individualRepeatability) = suj
  rownames(individualRepeatability) = att
  colnames(individualProductF) = suj
  rownames(individualProductF) = att
  ind = sort(FRank, index.return = TRUE)$ix
  indice = (1:subjectNumber)[ind]
  suj = suj[ind]
  FRank = FRank[ind]
  if (replicateNumber > 1) {
    ind2 = sort(panelProductF, index.return = TRUE, decreasing = TRUE)$ix
  }
  else {
    ind2 = 1:attributeNumber
  }
  att = att[ind2]
  panelProductF = panelProductF[ind2]
  panelProductPvalue = panelProductPvalue[ind2]
  panelInteractionF = panelInteractionF[ind2]
  panelInteractionPvalue = panelInteractionPvalue[ind2]
  panelAverage = panelAverage[ind2]
  panelRepeatability = panelRepeatability[ind2]
  individualProductF = individualProductF[ind2, ind]
  individualProductPvalue = individualProductPvalue[ind2, ind]
  significanceMatrix = significanceMatrix[ind2, ind]
  individualRepeatability = individualRepeatability[ind2, ind]
  individualDisagreementPvalue = individualDisagreementPvalue[ind2,
                                                              ind]
  individualDisagreementF = individualDisagreementF[ind2, ind]
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
  indices = which(panelInteractionPvalue <= panelLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:length(indices)) {
      style = AddHtmlStyle(style, att[indices[i]], GetLabel("FDisag",
                                                            language), backgroundColor = "red")
    }
  }
  indices = which(panelInteractionPvalue > panelLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:length(indices)) {
      style = AddHtmlStyle(style, att[indices[i]], GetLabel("FDisag",
                                                            language), backgroundColor = "#00FF00")
    }
  }
  indices = which(individualProductPvalue <= indivDiscriminationLimit &
                    individualDisagreementPvalue <= indivAgreementLimit,
                  arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:nrow(indices)) {
      style = AddHtmlStyle(style, att[indices[i, 1]], suj[indices[i,
                                                                  2]], backgroundColor = "#00FF00")
    }
  }
  indices = which(individualProductPvalue <= indivDiscriminationLimit &
                    individualDisagreementPvalue > indivAgreementLimit, arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:nrow(indices)) {
      style = AddHtmlStyle(style, att[indices[i, 1]], suj[indices[i,
                                                                  2]], backgroundColor = "red")
    }
  }
  indices = which(individualProductPvalue > indivDiscriminationLimit,
                  arr.ind = TRUE)
  if (length(indices) > 0) {
    for (i in 1:nrow(indices)) {
      style = AddHtmlStyle(style, att[indices[i, 1]], suj[indices[i,
                                                                  2]], backgroundColor = "yellow")
    }
  }
  tableauTxt = matrix("", attributeNumber, subjectNumber)
  texteCellule = function(indivDiscrimination, indivDiscriminationLimit,
                          indivAgreement, indivAgreementLimit, indivRepeatability,
                          indivRepeatabilityLimit) {
    cell = "?"
    if (indivDiscrimination < indivDiscriminationLimit &
        indivAgreement < indivAgreementLimit) {
      cell = "+"
    }
    if (indivDiscrimination < indivDiscriminationLimit &
        indivAgreement >= indivAgreementLimit) {
      cell = "-"
    }
    if (indivDiscrimination >= indivDiscriminationLimit) {
      cell = "="
    }
    if (indivRepeatability < indivRepeatabilityLimit) {
      cell = paste(cell, GetLabel("LR", language))
    }
    return(cell)
  }
  for (j in 1:subjectNumber) {
    for (i in 1:attributeNumber) {
      tableauTxt[i, j] = texteCellule(individualProductPvalue[i,
                                                              j], indivDiscriminationLimit, individualDisagreementPvalue[i,
                                                                                                                         j], indivAgreementLimit, individualRepeatability[i,
                                                                                                                                                                          j], indivRepeatabilityLimit)
    }
  }
  tableauRes = cbind(round(panelAverage, digits = 2), round(panelProductF,
                                                            digits = 2), round(panelInteractionF, digits = 2), round(panelRepeatability,
                                                                                                                     digits = 2), tableauTxt)
  tableauRes = rbind(tableauRes, c("-", "-", "-", "-", as.character(round(FRank,
                                                                          digits = 2))))
  colnames(tableauRes) = c(GetLabel("Mean", language), GetLabel("FProd",
                                                                language), GetLabel("FDisag", language), GetLabel("RMSE",
                                                                                                                  language), suj)
  rownames(tableauRes) = c(att, GetLabel("RankF", language))
  txt = "<html>"
  txt = paste(txt, "<h3>", titre, "</h3>", MatrixToHtml(tableauRes,
                                                        style = style), sep = "")
  txt = "<html>"
  txt = paste(txt, "<h3>", titre, "</h3>", MatrixToHtml(tableauRes,
                                                        style = style), sep = "")
  txt = paste(txt, "<table border='1' cellspacing='1'><tr><td colspan='2'><b>",
              GetLabel("PanelPerf", language), "</b></td><td><b>",
              GetLabel("PanelistPerf", language), "</b></td></tr>")
  txt = paste(txt, "<tr><td><b>", GetLabel("FProd", language),
              "</b></td><td><b>", GetLabel("FDisag", language), "</b></td><td rowspan=3>",
              GetLabel("LegendCAP", language), "</td></tr>")
  txt = paste(txt, "<tr><td style='text-align:center;background-color:#FF0000'>p> ",
              panelLimit, "</td><td style='text-align:center;background-color:#FF0000'>p<",
              panelLimit, "</td></tr>")
  txt = paste(txt, "<tr><td style='text-align:center;background-color:#00FF00'>p< ",
              panelLimit, "</td><td style='text-align:center;background-color:#00FF00'>p>",
              panelLimit, "</td></tr>")
  txt = paste(txt, "<tr><td></td><td></td><td>", GetLabel("LegendRepet",
                                                          language), "</td></tr>")
  txt = paste(txt, "</table><br>", GetLabel("PhraseNA", language),
              "</html>")
  write(txt, file = paste(output, ".html", sep = ""))
}