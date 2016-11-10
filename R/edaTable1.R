#' Function to generate pdf table to check balance before and after matching in an observational study

#' @param baselinevars A vector of baseline variables
#' @param expvars A variables defining exposure group
#' @param matched a matched dataset
#' @param unmatched an unmatched dataset (original dataset)
#' @export
#' @examples  data("Telemedicine.matchedsample");data("Telemedicine") # loading matched and unmatched datasets respectively
#'
#'vars <- c("Age", "Gender", "Race1", "ESRD", "BMI", "TransplantType", "Retransplant", "Hypertension", "Dialysis", "DGF", "Induction", "PSrisk",  "Surgeon","ps","linps") # vector of baseline variables
#' # calling funtion
#' edaTable(baselinevars = vars,expvars = "Telehealth",matched = Telemedicine.matchedsample,unmatched = Telemedicine)



edaTable = function(baselinevars,expvars,matched,unmatched){
results = list()
formula= reformulate(termlabels=baselinevars, response =expvars)

res1 <- compareGroups::compareGroups(formula, data = unmatched, ref = 1)
res2 <- compareGroups::compareGroups(formula, data = matched, ref = 1)


table01=compareGroups::createTable(res1,show.p.mul = TRUE,show.p.overall = TRUE)
table02=compareGroups::createTable(res2, show.p.mul =TRUE,show.p.overall = TRUE)
xptable01=cbind("Before matching"= table01,"After matching"=table02)
compareGroups::export2pdf(xptable01,'table1.pdf',size="small")
Biobase::openPDF("table1.pdf")

#results$CombTable = xptable01
#return(results)

}

