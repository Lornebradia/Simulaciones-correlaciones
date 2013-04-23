#====================================#
#   SIMULACIONES CORRELACIONES       #
#   Simulaciones empleando la        #
#     REGLA CLAST para    #
#     análisis de correlaciones      #
#====================================#

# Condiciones de simulación-----
library(MASS)
num.sim <- 10^4 # número de simulaciones
nfsr <- c(20, 30, 40, 50, 60) # tamaño muestral inicial de referencia
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

# Bucle de simulación ----

source(file="simCorrclast.R")
# source(file="fisherZ.R")


for (i in 1:dim(datos.corrsclast)[1]){
  datos.corrsclast[i,3:7] <- simCorrclast(n = datos.corrsclast$nfsr[i], r = datos.corrsclast$corr_teo[i],asup=.25)
}

rm(i)

# Tabla Resumen -----

datos.corrsclast$zfisher <- .5*log((1+datos.corrsclast$corr_emp)/(1-datos.corrsclast$corr_emp))
datos.corrsclast$varteo.zfisher <- 1/(datos.corrsclast$nstop-3)

r.corrclast <- aggregate.data.frame(datos.corrsclast, by = list(datos.corrsclast$corr_teo,datos.corrsclast$nfsr),FUN=mean)
r.corrclast$Group.1 <- r.corrclast$Group.2 <- NULL

# Calcula la varianza empírica de las Z agrupadas por corr_teo y nfsr

r.corrclast$varemp.zfisher <- aggregate(datos.corrsclast$zfisher, by=list(datos.corrsclast$corr_teo,datos.corrsclast$nfsr),FUN=var)$x

# Gráficos -----
library(ggplot2)
source("/Users/lbraschi/Documents/R Cosas/multiplot.R")

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
  scale_y_continuous(name="Proporción empírica de rechazos",limits=c(0,1))+
  scale_shape_discrete(name="N fijo")+
  scale_color_discrete(name="N fijo")+
  scale_linetype_discrete(name="N fijo")+
  geom_hline(yintercept=.05, linetype=2,alpha=.5,size=1)
clastcorr.pow
ggsave(plot=clastcorr.pow, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/corpowclast.png",height=8, width=12,dpi=600)

clastcorr.n <- ggplot(r.corrclast,aes(x=corr_teo,y=nstop,color = factor(nfsr), shape = factor(nfsr), linetype = factor(nfsr)))
clastcorr.n <- clastcorr.n + 
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme_bw()+
  scale_x_continuous(name="Correlación teórica")+
  scale_y_continuous(name="N stop")+
  scale_shape_discrete(name="N fijo")+
  scale_color_discrete(name="N fijo")+
  scale_linetype_discrete(name="N fijo")

clastcorr.n

# histh0clast <- ggplot(datos.corrsclast[datos.corrsclast$corr_teo==0,],aes(x=tstat,y=..density..))+
#   geom_histogram(binwidth=1/10,fill="steelblue",color="black",prb=T)+
#   theme_bw()+
#   theme(plot.title=element_text(size=18))+
#   ggtitle("Histograma H0=VERDADERA, CLAST")
# ggsave(plot=histh0clast, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/histh0clast.png",height=8, width=12,dpi=600)

clastcorr.varzteo <- ggplot(r.corrclast, aes(x=corr_teo,y=varteo.zfisher, color=factor(nfsr),shape=factor(nfsr), linetype=factor(nfsr)))
clastcorr.varzteo <- clastcorr.varzteo + 
  geom_point(size = 3)+
  geom_line()+
  theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold"))+
  scale_x_continuous(name="Correlación teórica")+
  scale_y_continuous(name="Varianza teórica")+
  scale_shape_discrete(name="N fijo")+
  scale_color_discrete(name="N fijo")+
  scale_linetype_discrete(name="N fijo")+
  ggtitle("Varianza de Z de Fisher\na partir de los tamaños de parada")

clastcorr.varzteo

clastcorr.varzemp <- ggplot(r.corrclast, aes(x=corr_teo,y=varemp.zfisher, color=factor(nfsr),shape=factor(nfsr), linetype=factor(nfsr)))
clastcorr.varzemp <- clastcorr.varzemp + 
  geom_point(size = 3)+
  geom_line()+
  theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold"))+
  scale_x_continuous(name="Correlación teórica")+
  scale_y_continuous(name="Varianza empírica")+
  scale_shape_discrete(name="N fijo")+
  scale_color_discrete(name="N fijo")+
  scale_linetype_discrete(name="N fijo")+
  ggtitle("Varianza de Z de Fisher\ncalculada empíricamente")

clastcorr.varzemp