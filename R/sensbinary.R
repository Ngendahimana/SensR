


#' Sensitivity Analysis for A Simple Comparison for binary outcomes


#' @param data A matched sample
#' @param exp A variables defining exposure group
#' @param outcome The outcome variable
#' @param Gamma Bias to be assessed
#' @param Gammainterval interval between two hidden bias to be assessed
#' @param alpha Significance level
#' @keywords Sensitivity
#' @references Section 4.4.8. of Rosenbaum PR (2002) Observational Studies, 2nd Edition.
#' @export
#' @examples  data("Telemedicine.matchedsample") # Reading in the data produced by a matching method
#'
#' sensBinary(data=Telemedicine.matchedsample,StartGamma=1,EndGamma=1.9,Gammainterval=0.01,outcome ="Readmission", exp="Telehealth")



sensBinary = function(data,StartGamma,EndGamma,Gammainterval,outcome,exp,alpha = 0.05){

  results = list()

  data1c =subset(data,select =c("matches",exp,outcome))
  names(data1c)[c(2:3)] = c("exp","outcome")

    gamma = seq(StartGamma,EndGamma, by=Gammainterval)
    pplus = 1/(1+gamma)
    pminus = gamma/(1+gamma)


    c=table(data1c$outcome,data1c$exp)[2,1]
    n =c+table(data1c$outcome,data1c$exp)[1,2]

    table2=data.frame(cbind(gamma,pplus,pminus))

    plower = NULL

    for(i in 1:length(gamma)){

    table2$pval_lowerbound[i] = 2*(pbinom(c,n,table2[i,2],lower.tail = FALSE))
    }

    for(i in 1:length(gamma)){

      table2$pval_upperbound[i] = 2*(pbinom(c,n,table2[i,3],lower.tail = FALSE))
    }

    ylim=table2$pval_upperbound[length(gamma)]
    unbiasedP=round(table2$pval_upperbound[1],3)


    table2$min = abs(alpha-table2$pval_upperbound)
    vrt = table2[table2$min==min(table2$min),]$gamma
    hrz = round(table2[table2$min==min(table2$min),]$pval_upperbound,3)

   plot(table2$pval_upperbound~table2$gamma,type="l",xlab="Gamma",ylab="p-val upper bound",main="Sensitivity analysis plot")

   mtext( paste0("   p = ",unbiasedP),side = 3)
   mtext(expression(Gamma == 1),side = 3,adj = 0)
   text(vrt, hrz, paste("(",hrz,",",vrt,")"),pos = 4,cex = .8)
   segments(x0 = 0, y0 = hrz, x1 = vrt, y1 = hrz, col = "pink", lty = "dashed", lwd = 3)
   segments(x0 = vrt, y0 = 0, x1 = vrt, y1 = hrz, col = "pink", lty = "dashed", lwd = 3)

   segments(x0=0,y0=hrz,x1=vrt,y1=hrz,col = "pink",lty = "dashed",lwd = 3)
   segments(x0=vrt,y0=0,x1=vrt,y1=hrz,col = "pink",lty = "dashed",lwd = 3)


   k=rbind(vrt,hrz)
   rownames(k)=c("Gamma .......","p_val upperbound")
   results$BiasMeasures=round(k,3)
   return(results)
}






