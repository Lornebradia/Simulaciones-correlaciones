#====================================#
#   SIMULACIONES CORRELACIONES       #
#   Simulaciones empleando la        #
#     regla CLAST para el            #
#     análisis de correlaciones      #
#====================================#

# CREACIÓN PREVIA DEL DATA FRAME

# Condiciones de simulación
library(MASS)
num.sim <- 10^4 # número de simulaciones
nfsr <- c(16, 20, 24, 30, 40) # tamaño muestral inicial de referencia
corr.teo <- c(0,.20,.60,.80)

# Data Frame ----
datos.corrs <- data.frame(corr_teo = rep(corr.teo,each=num.sim*length(nfsr)))
datos.corrs$nfsr <- rep(nfsr,each=num.sim)
datos.corrs$corr_emp <- NA
datos.corrs$t <- NA
datos.corrs$pvalue <- NA
datos.corrs$decision <- NA
rm(num.sim,nfsr,corr.teo)
# set.seed(1234)

source(file="simCorr.R")
# source(file="fisherZ.R")

for (i in 1:dim(datos.corrs)[1]){
  datos.corrs$corr_emp[i,2:4] <- simCorr(n = datos.corrs$nfsr[i], r = datos.corrs$corr_teo[i])
}

rm(i)

r.corr <- aggregate.data.frame(datos.corrs, by = list(datos.corrs$corr_teo,datos.corrs$nfsr),FUN=mean)

library(ggplot2)
corr.plot <- ggplot(r.corr,aes(x=corr_teo,y=corr_emp))
corr.plot <- corr.plot + 
  geom_abline(intercept = 0, slope = 1, size = 4, alpha =.25)+
  geom_line(aes(linetype=factor(nfsr), color = factor(nfsr)),size = 1)+
  scale_x_continuous(name = "Correlación teórica", breaks = seq(0,1,.1))+
  scale_y_continuous(name = "Correlación empírica",breaks = seq(0,1,.1))+
  facet_wrap(~nfsr)+
  theme_bw()
ggsave(plot=corr.plot, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/corr.fix.png",height=8, width=12,dpi=600)
corr.plot

