#' Sensitivity Analysis for A Simple Comparison for continious data
#'
#' This function allows you to assess how sensitive your results are to unmeasured variable.

#' @param data A matched sample
#' @param exp A variables defining exposure group
#' @param outcome The outcome variable
#' @param failtime Time to event
#' @param Gamma Bias to be assessed
#' @param Gammainterval interval between two hidden bias to be assessed
#' @param alpha Significance level
#' @keywords Sensitivity
#' @references Section 4.4.8. of Rosenbaum PR (2002) Observational Studies, 2nd Edition.
#' @export
#' @examples  data("Telemedicine.matchedsample") # Reading in the data produced by a matching method
#'
#' sensCont(data=Telemedicine.matchedsample,exp="Telehealth.n",outcome = "GFRoneyr",Gamma=1.5,Gammainterval = 0.06)



sensCont = function(data,exp,outcome,Gamma,Gammainterval){

  results = list()
  data1c =subset(data,select =c("matches",exp,outcome))
  names(data1c)[c(2:3)] = c("exp","outcome")
  data2c= subset(data1c,exp == 0)
  data3c = subset(data1c,exp == 1)
  data4c = subset(data2c,select =c("matches","exp","outcome"))
  data5c = subset(data3c,select =c("matches","exp","outcome"))
  data6c= dplyr::full_join(data4c,data5c,by="matches")

  names(data6c)[c(3,5)] = c("noexpOutcome","expOutcome")

  pairs = nrow(data6c)
  results$pairs = nrow(data6c)
  data6c$absdiff =  abs(data6c$expOutcome-data6c$noexpOutcome)

  for(i in 1:nrow(data6c)){
    if ( data6c[i,5]>data6c[i,3]) {
      data6c$sign[i] = 1

    } else if (data6c[i,5] <data6c[i,3]) {
      data6c$sign[i] = -1
    } else
      data6c$sign[i] = 0

  }

  data7c=data6c[with(data6c, order(absdiff)), ]

  for(i in 1:nrow(data7c)){
    data7c$Rank[i]=i
  }

  g <- data7c$absdiff
  l <- split(data7c,data7c$absdiff)
  l <- lapply(l, transform, mRank = mean(Rank))
  data7c <- unsplit(l, g)

  data8c = subset(data7c, sign== -1 )
  teststat=sum(data8c$mRank)

  results$Test.statistic = sum(data8c$mRank)



  gamVal=seq(1,Gamma,by=Gammainterval)
  pplus = 1/(1+gamVal)
  pminus = gamVal/(1+gamVal)

  table1=data.frame(cbind(gamVal,pplus,pminus))
  table1$expTplus=pairs*table1$pplus
  table1$expTminus = table1$pminus*(pairs*(pairs+1)/2)
  table1$sd_expT = sqrt(table1$pplus*(1-table1$pplus)*(pairs*(pairs+1)*(2*pairs+1)/6))


  for(i in 1:length(gamVal)){
    table1$pupper[i]=round(min(1,2*pnorm((teststat-table1[i,5])/table1[i,6],lower.tail = FALSE) ),4)
  }

  for(i in 1:length(gamVal)){
    table1$plower[i]=round(min(1,2*pnorm((teststat-table1[i,4])/table1[i,6],lower.tail = FALSE) ),4)
  }

  #results$sensitivitytable = table1

  table1$min = abs(0.05-table1$pupper)

  vrt = table1[table1$min==min(table1$min),]$gamVal
  hrz = table1[table1$min==min(table1$min),]$pupper
  ylim=table1$pupper[length(gamVal)]
  unbiasedP=table1$pupper[1]



  plot(table1$pupper~table1$gamVal,type="l",xlab="Gamma",ylab="p-val upper bound",main="Sensitivity analysis plot")
  segments(x0=0,y0=hrz,x1=vrt,y1=hrz,col = "pink",lty = "dashed",lwd = 3)
  segments(x0=vrt,y0=0,x1=vrt,y1=hrz,col = "pink",lty = "dashed",lwd = 3)

  mtext( paste0("   p = ",unbiasedP),side = 3)
  mtext(expression(Gamma == 1),side = 3,adj = 0)
  text(vrt, hrz, paste("(",hrz,",",vrt,")"),pos = 4,cex = .8)


  results$upperbound_pval =table1[table1$min==min(table1$min),]$pupper
  results$Gamma = table1[table1$min==min(table1$min),]$gamVal

  return(results)

}








