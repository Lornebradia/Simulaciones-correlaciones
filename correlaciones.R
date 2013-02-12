#====================================#
#   SIMULACIONES CORRELACIONES       #
#   Simulaciones empleando la        #
#     regla CLAST para el            #
#     análisis de correlaciones      #
#====================================#

# CREACIÓN PREVIA DEL DATA FRAME

# Condiciones de simulación
num.sim <- 10^2 # número de simulaciones
nfsr <- c(16, 20, 24, 30, 40) # tamaño muestral inicial de referencia
corr.teo <- c(0,.20,.60,.80)

require(ecodist) # paquete donde se encuentra la función 'corgen', generadora de datos correlacionados

# Data Frame ----
datos.corrs <- data.frame(corr_teo = rep(corr.teo,each=num.sim*length(nfsr)))
datos.corrs$nfsr <- rep(nfsr,each=num.sim)
# set.seed(1234)

simCorrs <- function(len,r){
  temp <- corgen(len=len,r=r)
  cor_emp <- cor(temp$x,temp$y)
  return(cor_emp)
}
datos.corrs$corr_emp <- NA

for (i in 1:2000){
  datos.corrs$corr_emp[i] <- simCorrs(len=datos.corrs$nfsr[i],r=datos.corrs$corr_teo[i])
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