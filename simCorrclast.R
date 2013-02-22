# simCorrclast function genera 

simCorrclast <- function(n,r){
  require(MASS)
  muestra <- mvrnorm(n/2, Sigma = matrix(c(1,r,r,1),2),mu = rep(0,2))
  out.cor <- cor(muestra[,1],muestra[,2])
  fishZ <- fisherZ(out.cor) 
  z <- fishZ/(1/sqrt(n-3)) 
  pvalue <- 1-pnorm(z)
  while (pvalue<0.25 & pvalue>.01){
    muestra <- rbind(muestra,mvrnorm(1, Sigma = matrix(c(1,r,r,1),2),mu = rep(0,2)))
    out.cor <- cor(muestra[,1],muestra[,2])
    fishZ <- fisherZ(out.cor) 
    z <- fishZ/(1/sqrt(n-3)) 
    pvalue <- 1-pnorm(z)
    if (dim(muestra)[1]==n*1.5)
      decision <- ifelse(pvalue<.05,1,0)
      break
  }
  return(c(dim(muestra)[1],out.cor,fishZ,z,pvalue))
}