#====================================#
#   SIMULACIONES CORRELACIONES       #
#   Simulaciones empleando la        #
#     REGLA CLAST para    #
#     análisis de correlaciones      #
#====================================#

# Condiciones de simulación-----
library(MASS)
num.sim <- 10^4 # número de simulaciones
nfsr <- c(16, 20, 24, 30, 40) # tamaño muestral inicial de referencia
corr.teo <- c(0,.20,.40,.60,.80)

# Data Frame ----
datos.corrsclast <- data.frame(corr_teo = rep(corr.teo,each=num.sim*length(nfsr)))
datos.corrsclast$nfsr <- rep(nfsr,each=num.sim)
datos.corrsclast$nstop <- NA
datos.corrsclast$corr_emp <- NA
datos.corrsclast$tstat <- NA
datos.corrsclast$pvalue <- NA
datos.corrsclast$decision <- NA
rm(num.sim,nfsr,corr.teo)
# set.seed(1234)

source(file="simCorrclast.R")
# source(file="fisherZ.R")

for (i in 1:dim(datos.corrsclast)[1]){
  datos.corrsclast[i,3:7] <- simCorrclast(n = datos.corrsclast$nfsr[i], r = datos.corrsclast$corr_teo[i],asup=.25)
}

rm(i)

datos.corrsclast$decision <- ifelse(datos.corrsclast$pvalue < .01, 1,0)

for (i in 1:250000){
  if (datos.corrsclast$nstop[i]==datos.corrsclast$nfsr[i]*1.5){
    if (datos.corrsclast$pvalue[i] < .05){
      datos.corrsclast$decision[i] <- 1
    }
  }
}

r.corrclast <- aggregate.data.frame(datos.corrsclast, by = list(datos.corrsclast$corr_teo,datos.corrsclast$nfsr),FUN=mean)

# Gráficos -----
library(ggplot2)

clastcorr.plot <- ggplot(r.corrclast,aes(x=corr_teo,y=corr_emp))
clastcorr.plot <- clastcorr.plot + 
  geom_abline(intercept = 0, slope = 1, size = 4, alpha =.25)+
  geom_line(aes(linetype=factor(nfsr), color = factor(nfsr)),size = 1)+
  scale_x_continuous(name = "Correlación teórica", breaks = seq(0,1,.1))+
  scale_y_continuous(name = "Correlación empírica",breaks = seq(0,1,.1))+
  facet_wrap(~nfsr)+
  theme_bw()
ggsave(plot=clastcorr.plot, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/corrclast.png",height=8, width=12,dpi=600)

clastcorr.pow <- ggplot(r.corrclast,aes(x=corr_teo,y=decision, color = factor(nfsr), shape = factor(nfsr), linetype = factor(nfsr)))
clastcorr.pow <- clastcorr.pow +
  geom_point(size = 3)+
  geom_line()+
  theme_bw()+
  scale_x_continuous(name="Correlación teórica")+
  scale_y_continuous(name="Proporción empírica de rechazos")+
  scale_shape_discrete(name="N fijo")+
  scale_color_discrete(name="N fijo")+
  scale_linetype_discrete(name="N fijo")+
  geom_hline(yintercept=.05, linetype=2,alpha=.5,size=1)
clastcorr.pow
ggsave(plot=clastcorr.pow, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/corpowclast.png",height=8, width=12,dpi=600)


