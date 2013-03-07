# simCorr function. Esta función toma dos muestras de tamaño n de la distribución normal multivariante con correlaciones marcadas por r en la matriz de correlaciones Sigma. Las medias de las distribuciones poblacionales se fijan a cero. 

simCorr <- function(n,r){
  require(MASS)
  muestra <- mvrnorm(n, Sigma = matrix(c(1,r,r,1),2),mu = rep(0,2)) # Toma dos vectores de tamaño n, matriz de correlaciones Sigma
  out.cor <- cor(muestra[,1],muestra[,2]) # Calcula la correlación entre los dos vectores
  tstat <- out.cor*sqrt(n-2)/sqrt(1-out.cor^2)
  pvalue <- 1-pt(tstat,n-2)
  decision <- ifelse(pvalue<=.05,1,0) # Decisión == 1, rechaza hipótesis. 
  return(c(out.cor,tstat,pvalue,decision))
}