#'CAPTable
#'
#'This function build an html file (CAPTable.html) containing a panel and panelist performance table (CAP or MAM-CAP table) in current working directory.
#'@param profileObject resulted from \link{profileReadData}
#'@param model character. "TwoWayANOVA" or "MAM": model of performance tests. "TwoWayANOVA" refers to the first Schlich CAP table available on the SensoBase (www.sensobase.fr) and does not take the scaling into account."MAM" refers to the MAM-CAP table, using the performances tests of Mixed Assessor Model (2013)
#'@param panelLimit limit of the statistical tests concerning panel performances
#'@param indivLimit limit of the statistical tests concerning individual performances. When the chosen model is CAP, limitIndiv is only the limit of discrimination tests (Kendall agreement test limit = 0.2 and repeatability test= 0.01)
#' @param output the name of the html file is created in the current working directory
#' @param correction logical. If TRUE, limitIndiv is divided by the number of panelists
#' @param correlationTest if model=='CAP', test for the agreement between panelists 'kendall' by default, but can also be "spearman" or "pearson". See cor.test
#' @param indivRepLimCap if model =='CAP', limit for significance in repeatability tests. 0.01 by default
#' @param indivAgLimCap if model =='CAP', limit for significance in disagreement tests. 0.2 by default
#' @param repInIndModel if model =='CAP', repInIndModel indicates whether the replicate should be included in the individual model (using the model 'TwoWayAdditiveBySubject'). Default to FALSE (using the model 'OneWayBySubject')
#' @references
#'Brockhoff P. , Schlich P.; Skovgaard I, Taking individual scaling differences into account by analyzing profile data with the Mixed Assessor Model. (submitted in Food Quality and Preferences in July 2013)
#' Peltier C., Brockhoff, P., Visalli, M., Schlich, P. The MAM-CAP table: a new tool to monitor panel performances. (accepted in Food Quality and Preferences in July 2013)
#' @examples
#' data(wine)
#' CAPTable(wine)
#' CAPTable(wine,model="CAP")
#'@export
#'@import doBy
CAPTable=function (profileObject, model = "MAM", panelLimit = 0.05, indivLimit = 0.05,
          language = "fr", output = "CAPTable", correction = FALSE)
{
  if (model == "TwoWayANOVA") {
    CAP(object = profileObject, panelLimit = panelLimit,
        indivRepeatabilityLimit = 0.01, indivAgreementLimit = 0.2,
        indivDiscriminationLimit = indivLimit, language = language,
        output = output, correction = correction)
  }
  if (model == "MAM") {
    MAMCAP(object = profileObject, panelLimit = panelLimit,
           indivLimit = indivLimit, language = language, output = output,
           correction = correction)
  }
  print(paste(" Your CAPTable has been built in your working directory as ",
              output, ".html", sep = ""))
}