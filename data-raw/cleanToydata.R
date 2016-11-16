library(Matching)

toy =  read.csv("data-raw/toy.csv")

## Re-expressing Binary Variables
toy$treated.f <- factor(toy$treated, levels=c(1,0),labels=c("Treated","Control"))
toy$covB.f <- factor(toy$covB, levels=c(1,0), labels=c("Has B", "No B"))
toy$out2.f <- factor(toy$out2.event, levels=c("Yes","No"), labels=c("Event Occurred", "No Event"))
toy$out2 <- as.numeric(toy$out2.event)-1 # subtracting 1 at the end changes the default 1/2 code to 0/1

## Re-expressing the Multi-Categorical Variable
toy$covF.Low <- as.numeric(toy$covF=="1-Low")
toy$covF.Middle <- as.numeric(toy$covF=="2-Middle")
toy$covF.High <- as.numeric(toy$covF=="3-High")


toy$Asqr <- toy$covA^2
toy$BC <- toy$covB*toy$covC
toy$BD <- toy$covB*toy$covD

# generating propensity score model
psmodel <- glm(treated ~ covA + covB + covC + covD + covE + covF + Asqr + BC + BD, family=binomial(), data=toy)
toy$ps <- psmodel$fitted
toy$linps <- psmodel$linear.predictors


#Matching
X <- toy$linps ## matching on the linear propensity score
Tr <- as.logical(toy$treated)
match1 <- Match(Tr=Tr, X=X, M = 1, replace=FALSE, ties=FALSE)

matches <- factor(rep(match1$index.treated, 2))
toy.matchedsample <- cbind(matches, toy[c(match1$index.control, match1$index.treated),])






