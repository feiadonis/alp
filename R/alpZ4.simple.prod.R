## tt1.sel4 = c("bbandsi.mom.W10.cad.ema10.Beta_Idx",
#               "dx.raw.W10.BetaInd_Idx","dx.W10.BetaInd_Idx","adx.W10.BetaInd_Idx","dx.mom.W10.BetaInd_Idx",
#               "adx.mom.W10.BetaInd_Idx","oscillator.W10.BetaInd_Idx","osci.mom.W10.BetaInd_Idx",
#               "bbandsi.W10.BetaInd_Idx","bbandsi.rev.W10.BetaInd_Idx","bbandsi.mom2.W10.BetaInd_Idx")

library(icsUtil)
library(abind)
library(plyr)
library(fitter)

ema.weights <- function(HL=10, W=HL*2, norm=FALSE){w=0.5^((1:W)/HL); if(norm)w=w/sum(w);w}

winsorize <- function(x, p=0.05, demean = FALSE){
  q = quantile(x,c(p,1-p),na.rm=TRUE);x[x<q[1]]=q[1];x[x>q[2]]=q[2];
  if(demean) x=x-mean(x,na.rm=TRUE)
  X
}


DV.to.indicators <- function(DV,W){
  data <- as.data.table(DV)
  data$D <- as.Date(rownames(DV),"%Y%m%d")
  dailyData <- as.xts(data[,.(Volume=Vol,High=High,Low=Low,Close=Cls),by="D"])
  
  adx = tryCatch(
    ADX(dailyData[,c("High","Low","Close")],n=W),error=function(e){
      out=dailyData[,c("Close","Close","Close","Close")]*NA
      colnames(out) = c("DIp","DIn","DX","ADX")
      out
    })
  
  adxi = as.matrix(data.frame(
    dx.raw = (adx[,"DIp"] - adx[,"DIn"])/100
    , dx = (adx[,"DIp"] - adx[,"DIn"])/100 * adx[,"DX"]/100
    , adx = (adx[,"DIp"] - adx[,"DIn"])/100 * adx[,"ADX"]/100
    , dx.mom = (adx[,"DIp"] - adx[,"DIn"])/100 * as.integer(adx[,"DX"] > 30)
    , adx.mom = (adx[,"DIp"] - adx[,"DIn"])/100*as.integer(adx[,"ADX"] > 30)
  ))
  colnames(adxi) = c("dx.raw","dx","adx","dx.mom","adx.mom")
  
  
}