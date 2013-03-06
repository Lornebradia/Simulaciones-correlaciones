#====================================#
#   SIMULACIONES CORRELACIONES       #
#   Simulaciones empleando la        #
#     REGLA TAMAÑO PREFIJADO para    #
#     análisis de correlaciones      #
#====================================#

# Condiciones de simulación-----
library(MASS)
num.sim <- 10^4 # número de simulaciones
nfsr <- c(16, 20, 24, 30, 40) # tamaño muestral inicial de referencia
corr.teo <- c(0,.20,.40,.60,.80)

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
  datos.corrs[i,3:6] <- simCorr(n = datos.corrs$nfsr[i], r = datos.corrs$corr_teo[i])
}

rm(i)

r.corr <- aggregate.data.frame(datos.corrs, by = list(datos.corrs$corr_teo,datos.corrs$nfsr),FUN=mean)

# Gráficos -----
library(ggplot2)

corr.plot <- ggplot(r.corr,aes(x=corr_teo,y=corr_emp))
corr.plot <- corr.plot + 
  geom_abline(intercept = 0, slope = 1, size = 4, alpha =.25)+
  geom_line(aes(linetype=factor(nfsr), color = factor(nfsr)),size = 1)+
  scale_x_continuous(name = "Correlación teórica", breaks = seq(0,1,.1))+
  scale_y_continuous(name = "Correlación empírica",breaks = seq(0,1,.1))+
  facet_wrap(~nfsr)+
  theme_bw()
ggsave(plot=corr.plot, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/corrfix.png",height=8, width=12,dpi=600)

corr.pow <- ggplot(r.corr,aes(x=corr_teo,y=decision, color = factor(nfsr), shape = factor(nfsr), linetype = factor(nfsr)))
corr.pow <- corr.pow +
  geom_point(size = 3)+
  geom_line()+
  theme_bw()+
  scale_x_continuous(name="Correlación teórica")+
  scale_y_continuous(name="Proporción empírica de rechazos")+
  scale_shape_discrete(name="N fijo")+
  scale_color_discrete(name="N fijo")+
  scale_linetype_discrete(name="N fijo")+
  geom_hline(yintercept=.05, linetype=2,alpha=.5,size=1)
ggsave(plot=corr.pow, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/corrpow.png",height=8, width=12,dpi=600)

histh0 <- ggplot(datos.corrs[datos.corrs$corr_teo==0,],aes(x=tstat,y=..density..))+
  geom_histogram(binwidth=1/10,fill="steelblue",color="black",prb=T)+
  theme_bw()+geom_density(color="red",size=1,adjust=2)+
  theme(plot.title=element_text(size=18))+
  ggtitle("Histograma H0=VERDADERA, RTP")
ggsave(plot=histh0, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/histh0.png",height=8, width=12,dpi=600)