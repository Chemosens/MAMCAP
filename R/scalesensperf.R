scalesensperf <-
function (frame, pvalue = TRUE) 
{
    library(doBy)
    attnames = labels(frame)[[2]][-1:-3]
    natt = length(attnames)
    ass = factor(as.character(frame[, 1]))
    assnames = levels(ass)
    nass = length(assnames)
    prod = factor(frame[, 2])
    prodnames = levels(prod)
    nprod = length(prodnames)
    rep = factor(frame[, 3])
    repnames = levels(rep)
    nrep = length(repnames)
    nrow = dim(frame)[1]
    ncol = dim(frame)[2]
    attnames1 = attnames2 = attnames
    asslevels = levels(unique(ass))
    frame[, 1] = ass
    frame[, 2] = prod
    frame[, 3] = rep
    names(frame)[1] = "ass"
    names(frame)[2] = "prod"
    names(frame)[3] = "rep"
    if (natt > 1) 
        attnames1[seq(2, natt, 2)] = ""
    attnames2[seq(1, natt, 2)] = ""
    prodFs = prodPs = SEs = rep(0, natt)
    prodco = matrix(rep(0, natt * nprod), nrow = natt)
    const = rep(1, nrow)
    indivTable = array(dim = c(7, nass, natt))
    perfTable = array(dim = c(14, nass, natt))
    IPTable = array(dim = c(6, nass + 1, natt))
    PIPTable = array(dim = c(6, nass, natt))
    FinalTable = array(dim = c(12, nass + 1, natt))
    AOVtable = array(dim = c(5, 5, natt))
    prodmeans = array(dim = c(nprod, natt))
    assmeans = array(dim = c(nass, natt))
    proddiffs = array(dim = c(nprod * (nprod - 1)/2, natt))
    prodeffects = array(dim = c(natt, nprod))
    mins = apply(frame[, 4:ncol], 2, min)
    maxs = apply(frame[, 4:ncol], 2, max)
    ranges = maxs - mins
    matriceDisag = matrix(NA, nprod * nass, natt)
    colnames(matriceDisag) = colnames(frame[, 4:ncol])
    matriceProd = matrix(NA, nprod, natt)
    colnames(matriceProd) = colnames(frame[, 4:ncol])
    matriceScaling = matrix(NA, nprod * nass, natt)
    colnames(matriceScaling) = colnames(frame[, 4:ncol])
    matriceError = matrix(NA, nprod * nass, natt)
    colnames(matriceError) = colnames(frame[, 4:ncol])
    for (p in 4:ncol) {
        X = frame[, p]
        mu = mean(X)
        frame$X = X
        xam = orderBy(~ass, data = data.frame(summaryBy(X ~ ass, 
            data = frame)))
        xpm = orderBy(~prod, data = data.frame(summaryBy(X ~ 
            prod, data = frame)))
        prodmeans[, p - 3] = xpm$X.mean
        assmeans[, p - 3] = xam$X.mean
        xapm = orderBy(~ass + prod, data = data.frame(summaryBy(X ~ 
            ass + prod, data = frame)))
        xapm$int = xapm$X.mean - rep(xam$X.mean, rep(nprod, nass)) - 
            rep(xpm$X.mean, nass) + mu
        xapm$beta = NA
        ssa = nprod * nrep * (xam$X.mean - mu)^2
        ssp = rep(0, nass)
        sse = rep(0, nass)
        sssca = rep(0, nass)
        ssdis = rep(0, nass)
        beta = rep(0, nass)
        xapm$xs = rep(xpm$X.mean, nass)
        sses = X - ave(X, factor(ass:prod))
        for (i in 1:nass) {
            ssp[i] = anova(lm(X ~ prod, data = subset(frame, 
                ass == asslevels[i])))[1, 2]
            res = lm(int ~ xs, data = subset(xapm, ass == asslevels[i]))
            aovres = anova(res)[, 2]
            sssca[i] = nrep * aovres[1]
            ssdis[i] = nrep * aovres[2]
            ssp[i] = ssp[i] - sssca[i] - ssdis[i]
            beta[i] = res$coef[2]
            sse[i] = sum(sses[ass == asslevels[i]]^2)
            xapm[xapm[, "ass"] == asslevels[i], "beta"] = beta[i]
        }
        xapm[, "disag"] = xapm[, "int"] - xapm[, "beta"] * xapm[, 
            "xs"]
        matriceScaling[, p - 3] = xapm[, "beta"] * xapm[, "xs"]
        matriceDisag[, p - 3] = xapm[, "disag"]
        matriceProd[, p - 3] = xpm[, "X.mean"]
        indivTable[1, , p - 3] = ssa
        indivTable[2, , p - 3] = ssp
        indivTable[3, , p - 3] = sssca
        indivTable[4, , p - 3] = ssdis
        indivTable[5, , p - 3] = sse
        indivTable[6, , p - 3] = beta + 1
        indivTable[7, , p - 3] = scale(assmeans[, p - 3], scale = F)
        k = 0
        for (i in 1:(nprod - 1)) for (j in (i + 1):nprod) {
            k = k + 1
            proddiffs[k, p - 3] = (prodmeans[i, p - 3] - prodmeans[j, 
                p - 3])
        }
        for (i in 1:nprod) prodeffects[p - 3, i] = prodmeans[i, 
            p - 3] - mean(prodmeans[-i, p - 3])
    }
    aovs = apply(indivTable, c(1, 3), sum)[1:5, ]
    msa = aovs[1, ]/(nass - 1)
    msc = aovs[3, ]/(nass - 1)
    msd = aovs[4, ]/((nass - 1) * (nprod - 2))
    msp = aovs[2, ]/(nprod - 1)
    mse = aovs[5, ]/(nass * nprod * (nrep - 1))
    msint = (aovs[3, ] + aovs[4, ])/((nprod - 1) * (nass - 1))
    for (p in 4:ncol) {
        perfTable[1, , p - 3] = nass * indivTable[1, , p - 3]/(msint[p - 
            3] * (nass - 1))
        perfTable[2, , p - 3] = 1 - pf(perfTable[1, , p - 3], 
            1, (nass - 1) * (nprod - 1))
        perfTable[3, , p - 3] = scale(assmeans[, p - 3], scale = F)
        perfTable[4, , p - 3] = ((indivTable[2, , p - 3] + indivTable[3, 
            , p - 3] + indivTable[4, , p - 3])/(nprod - 1))/(indivTable[5, 
            , p - 3]/(nprod * (nrep - 1)))
        perfTable[5, , p - 3] = 1 - pf(perfTable[4, , p - 3], 
            nprod - 1, nprod * (nrep - 1))
        perfTable[6, , p - 3] = indivTable[3, , p - 3]/(indivTable[4, 
            , p - 3]/(nprod - 2))
        perfTable[7, , p - 3] = 1 - pf(perfTable[6, , p - 3], 
            1, (nprod - 2))
        perfTable[8, , p - 3] = (indivTable[6, , p - 3]^2) * 
            (aovs[2, p - 3]/(nass))/(indivTable[4, , p - 3]/(nprod - 
            2))
        perfTable[9, , p - 3] = 1 - pf(perfTable[8, , p - 3], 
            1, (nprod - 2))
        perfTable[10, , p - 3] = indivTable[6, , p - 3] * sqrt((aovs[2, 
            p - 3]/nass)/(indivTable[2, , p - 3] + indivTable[3, 
            , p - 3] + indivTable[4, , p - 3]))
        perfTable[11, , p - 3] = (indivTable[4, , p - 3]/(nprod - 
            2))/(indivTable[5, , p - 3]/(nprod * (nrep - 1)))
        perfTable[12, , p - 3] = 1 - pf(perfTable[11, , p - 3], 
            nprod - 2, nprod * (nrep - 1))
        for (i in 1:nass) perfTable[13, i, p - 3] = indivTable[5, 
            i, p - 3]/mean(indivTable[5, -i, p - 3])
        perfTable[14, , p - 3] = 1 - pf(perfTable[13, , p - 3], 
            nprod * (nrep - 1), (nass - 1) * nprod * (nrep - 
                1))
        IPTable[1, 1:nass, p - 3] = scale(assmeans[, p - 3], 
            scale = F)
        IPTable[2, 1:nass, p - 3] = sqrt(((indivTable[2, , p - 
            3] + indivTable[3, , p - 3] + indivTable[4, , p - 
            3])/(nrep * (nprod - 1))))
        IPTable[3, 1:nass, p - 3] = indivTable[6, , p - 3]
        IPTable[4, 1:nass, p - 3] = indivTable[6, , p - 3] * 
            sqrt((aovs[2, p - 3]/nass)/(indivTable[2, , p - 3] + 
                indivTable[3, , p - 3] + indivTable[4, , p - 
                3]))
        IPTable[5, 1:nass, p - 3] = sqrt(indivTable[4, , p - 
            3]/(nprod - 2))
        IPTable[6, 1:nass, p - 3] = sqrt(indivTable[5, , p - 
            3]/(nprod * (nrep - 1)))
        IPTable[, nass + 1, p - 3] = apply(IPTable[, 1:nass, 
            p - 3], 1, mean)
        PIPTable[1, , p - 3] = perfTable[2, , p - 3]
        PIPTable[2, , p - 3] = perfTable[5, , p - 3]
        PIPTable[3, , p - 3] = perfTable[7, , p - 3]
        PIPTable[4, , p - 3] = perfTable[9, , p - 3]
        PIPTable[5, , p - 3] = perfTable[12, , p - 3]
        PIPTable[6, , p - 3] = perfTable[14, , p - 3]
        IPtabT = matrix(as.character(round(IPTable[, , p - 3], 
            2)), nrow = 6)
        PIPtabT = IPtabT
        for (i in 1:6) {
            if (pvalue == FALSE) {
                PIPtabT[i, 1:nass] = " "
                PIPtabT[i, 1:nass][PIPTable[i, , p - 3] <= 0.1] = "o"
                PIPtabT[i, 1:nass][PIPTable[i, , p - 3] <= 0.05] = "*"
                PIPtabT[i, 1:nass][PIPTable[i, , p - 3] <= 0.01] = "**"
                PIPtabT[i, 1:nass][PIPTable[i, , p - 3] <= 0.001] = "***"
            }
            else {
                PIPtabT[i, 1:nass] = round(PIPTable[i, , p - 
                  3], digits = 3)
            }
        }
        FinalTable[, , p - 3] = matrix(c(IPtabT[1, ], PIPtabT[1, 
            ], IPtabT[2, ], PIPtabT[2, ], IPtabT[3, ], PIPtabT[3, 
            ], IPtabT[4, ], PIPtabT[4, ], IPtabT[5, ], PIPtabT[5, 
            ], IPtabT[6, ], PIPtabT[6, ]), ncol = nass + 1, byrow = T)
        for (i in c(2, 4, 6, 8, 10, 12)) FinalTable[i, nass + 
            1, p - 3] = ""
    }
    Fmatr = matrix(rep(0, 4 * natt), nrow = 4)
    Pmatr = matrix(rep(0, 4 * natt), nrow = 4)
    Fmatr[1, ] = msa/msint
    Fmatr[2, ] = msp/msd
    Fmatr[3, ] = msc/msd
    Fmatr[4, ] = msd/mse
    DFnums = c(nass - 1, nprod - 1, nass - 1, (nprod - 2) * (nass - 
        1))
    DFdens = c((nprod - 1) * (nass - 1), (nprod - 2) * (nass - 
        1), (nprod - 2) * (nass - 1), nass * nprod * (nrep - 
        1))
    for (i in 1:4) Pmatr[i, ] = 1 - pf(Fmatr[i, ], DFnums[i], 
        DFdens[i])
    ns = c(nass, nprod, nrep, natt)
    sigma = sqrt(mse)
    sigmascal = nass * (msc - msd)/aovs[2, ]
    sigmascal[sigmascal < 0] = 0
    sigmascal = sqrt(sigmascal)
    sigmadis = (msd - mse)/nrep
    sigmadis[sigmadis < 0] = 0
    sigmadis = sqrt(sigmadis)
    sigmas = cbind(sigmascal, sigmadis, sigma)
    dfs = c(nass - 1, nprod - 1, nass - 1, (nprod - 2) * (nass - 
        1), nass * nprod * (nrep - 1))
    AOVtable[, 1, ] = round(aovs, 2)
    AOVtable[, 2, ] = round(aovs/dfs, 2)
    AOVtable[, 3, ] = dfs
    AOVtable[1:4, 4, ] = round(Fmatr, 2)
    if (pvalue) {
        AOVtable[1:4, 5, ] = round(Pmatr, 4)
    }
    else {
        AOVtable[1:4, 5, ] = " "
        AOVtable[1:4, 5, ][Pmatr <= 0.1] = "o"
        AOVtable[1:4, 5, ][Pmatr <= 0.05] = "*"
        AOVtable[1:4, 5, ][Pmatr <= 0.01] = "**"
        AOVtable[1:4, 5, ][Pmatr <= 0.001] = "***"
    }
    dimnames(FinalTable)[[1]] = c("Level", "", "Product", " ", 
        "Scaling", "  ", "Correlation", "   ", "Disagreement", 
        "    ", "Repeatability", "      ")
    dimnames(FinalTable)[[2]] = c(levels(as.factor(as.character(asslevels))), 
        "AVE")
    dimnames(FinalTable)[[3]] = attnames
    dimnames(AOVtable)[[1]] = c("Assessor", "Product", "Scaling", 
        "Disagreement", "Error")
    dimnames(AOVtable)[[2]] = c("SS", "MS", "DF", "F", "Pval")
    dimnames(AOVtable)[[3]] = attnames
    diffpvals = 1 - pt(abs(proddiffs)/matrix(rep(sqrt(2 * msd/(nrep * 
        nass)), nprod * (nprod - 1)/2), ncol = natt, byrow = T), 
        (nprod - 2) * (nass - 1))
    diffpvals = 2 * diffpvals
    effectspvals = 1 - pt(abs(prodeffects)/matrix(rep(sqrt(msd * 
        (1/(nrep * nass) + 1/(nrep * nass * (nprod - 1)))), rep(nprod, 
        natt)), ncol = nprod, byrow = T), (nprod - 2) * (nass - 
        1))
    effectspvals = 2 * effectspvals
    PtabT = matrix(as.character(round(prodeffects, 2)), ncol = nprod)
    PPtabT = effectspvals
    for (i in 1:natt) {
        if (pvalue == FALSE) {
            PPtabT[i, ] = " "
            PPtabT[i, ][effectspvals[i, ] <= 0.1] = "o"
            PPtabT[i, ][effectspvals[i, ] <= 0.05] = "*"
            PPtabT[i, ][effectspvals[i, ] <= 0.01] = "**"
            PPtabT[i, ][effectspvals[i, ] <= 0.001] = "***"
        }
        else {
            PPtabT[i, ] = round(effectspvals[i, ], digits = 3)
        }
    }
    ProdTable = matrix(rep(" ", 2 * nprod * natt), ncol = nprod)
    rownames(ProdTable) = c(rbind(attnames, rep(" ", natt)))
    colnames(ProdTable) = prodnames
    i1 = (1:natt) * 2 - 1
    i2 = (1:natt) * 2
    ProdTable[i1, ] = PtabT
    ProdTable[i2, ] = PPtabT
    i1 = (1:nprod) * 2 - 1
    i2 = (1:nprod) * 2
    DIFtable = array(dim = c(2 * (nprod - 1), nprod - 1, natt))
    k = 0
    for (i in 1:(nprod - 1)) for (j in (i + 1):nprod) {
        k = k + 1
        DIFtable[2 * i - 1, j - 1, ] = round(proddiffs[k, ], 
            2)
        DIFtable[2 * i, j - 1, ] = round(diffpvals[k, ], 4)
    }
    dimnames(DIFtable)[[1]] = c(rbind(prodnames[-nprod], rep(" ", 
        nprod - 1)))
    dimnames(DIFtable)[[2]] = prodnames[-1]
    dimnames(DIFtable)[[3]] = attnames
    MSDisag = nrep * t(matriceDisag) %*% matriceDisag/((nass - 
        1) * (nprod - 2))
    meanRepet = mean(AOVtable[5, 2, ])
    PanelScal = t(matriceScaling) %*% matriceScaling
    PanelDisag = t(matriceDisag) %*% matriceDisag
    PanelProd = t(matriceProd) %*% matriceProd
    IndivProd = NULL
    IndivDisag = NULL
    IndivScal = NULL
    IndivRep = NULL
    return(list(FinalTable, AOVtable, DIFtable, ProdTable, MSDisag, 
        PanelDisag, PanelScal, PanelProd, IndivProd, IndivDisag, 
        IndivScal, IndivRep, meanRepet))
}
