# simCorrclast function genera 

simCorrclast <- function(n,r){
  require(MASS)
  muestra <- mvrnorm(n/2, Sigma = matrix(c(1,r,r,1),2),mu = rep(0,2))
  out.cor <- cor(muestra[,1],muestra[,2])
  tstat <- out.cor*sqrt(n-2)/sqrt(1-out.cor^2)
  pvalue <- ifelse(tstat<=0, pt(tstat,n-2),1-pt(tstat,n-2))
  while (pvalue<0.25 && pvalue>.01){
    muestra <- rbind(muestra,mvrnorm(1, Sigma = matrix(c(1,r,r,1),2),mu = rep(0,2)))
    out.cor <- cor(muestra[,1],muestra[,2])
    tstat <- out.cor*sqrt(n-2)/sqrt(1-out.cor^2)
    pvalue <- ifelse(tstat<=0, pt(tstat,n-2),1-pt(tstat,n-2))
    if (dim(muestra)[1]==n*1.5)
#       decision <- ifelse(pvalue<.05,1,0)
      break
  }
  decision <- ifelse(pvalue<.01,1,0)
  return(c(dim(muestra)[1],out.cor,tstat,pvalue,decision))
}