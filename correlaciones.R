#====================================#
#   SIMULACIONES CORRELACIONES       #
#   Simulaciones empleando la        #
#     regla CLAST para el            #
#     análisis de correlaciones      #
#====================================#

# CREACIÓN PREVIA DEL DATA FRAME

# Condiciones de simulación
num.sim <- 10^4 # número de simulaciones
nfsr <- c(16, 20, 24, 30, 40) # tamaño muestral inicial de referencia
corr.teo <- c(0,.20,.60,.80)


# Data Frame ----
datos.corrs <- data.frame(corr_teo = rep(corr.teo,each=num.sim*length(nfsr)))
datos.corrs$nfsr <- rep(nfsr,each=num.sim)
# set.seed(1234)

simCorr <- function(n,r){
  muestra <- mvrnorm(n, Sigma = matrix(c(1,r,r,1),2),mu = rep(0,2))
  out.cor <- cor(muestra[,1],muestra[,2])
  return(out.cor)
}




r.corr <- aggregate.data.frame(datos.corrs, by = list(datos.corrs$corr_teo,datos.corrs$nfsr),FUN=mean)

corr.plot <- ggplot(r.corr,aes(x=corr_teo,y=corr_emp))
corr.plot <- corr.plot + 
  geom_abline(intercept = 0, slope = 1, size = .5)+
  geom_line(aes(linetype=factor(nfsr), color = factor(nfsr)),size = 1)+
  scale_x_continuous(name = "Correlación teórica", breaks = seq(0,1,.1))+
  scale_y_continuous(name = "Correlación empírica",breaks = seq(0,1,.1))+
  facet_wrap(~nfsr)+
  theme_bw()
corr.plot