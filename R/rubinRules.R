
rubinRules = function(data,Treatment,matchscore="ps",covlist){

  results=list()

  # Rubin 1
  data1a = subset(data, select = c(Treatment,matchscore))
  names(data1a)[c(1:2)] = c("Treatment", "matchscore")
  data1a$Treatment=as.factor(data1a$Treatment)

  results$RUBIN1<- with(data1a, abs(100*(mean(matchscore[Treatment=="1"])-mean(matchscore[Treatment=="0"]))/sd(matchscore)))

  # Rubin 2

  results$RUBIN2 <- with(data1a, var(matchscore[Treatment=="1"])/var(matchscore[Treatment=="0"]))


  # Rubin 3
    data1d = subset(data, select = c(Treatment,matchscore))
    names(data1d)=c("Treatment","matchscore")
    data1f=as.data.frame(cbind(data,data1d))

    data1f$Treatment=as.factor(data1f$Treatment)
    covlist1 = data1f[covlist]
    covlist2 <- as.matrix(covlist1)
    res <- NA
    for(i in 1:ncol(covlist2)) {
      cov <- as.numeric(covlist2[,i])
      num <- var(resid(lm(cov ~ data1f$matchscore))[data1f$Treatment=="1"])
      den <- var(resid(lm(cov ~ data1f$matchscore))[data1f$Treatment=="0"])
      res[i] <- round(num/den, 3)
    }
    names(res) <- names(covlist1)
    #print(res)

    results$RUBIN3=res

    d <- sort(res)
    low <- min(min(d), 0.45)
    high <- max(max(d), 2.05)

    dotchart(d, pch=15, col="black", main="Rubin's Rules plot", xlab="Residual Variance Ratio", xlim=c(low, high))
    abline(v=1, lty=1)
    abline(v=0.8, lty=2, lwd=2, col="blue")
    abline(v=1.25, lty=2, lwd=2, col="blue")
    abline(v=0.5, lty=2, lwd=2, col="red")
    abline(v=2, lty=2, lwd=2, col="red")

    mtext(paste("Rubin Two :",round(results$RUBIN2,2)),side = 3)
    mtext(paste("Rubin One :",round(results$RUBIN1,2)),side = 3,adj=0)

    return(results)

}

#Example
covlist1=c("covA", "covB", "covC", "covD", "covE", "covF.Middle", "covF.High", "Asqr","BC", "BD")

rubinRules(data=toy.matchedsample,Treatment="treated",covlist=covlist1)








data= toy.matchedsample
Treatment="treated"
matchscore="ps"

covlist=c("covA", "covB", "covC", "covD", "covE", "covF.Middle", "covF.High", "Asqr","BC", "BD")


