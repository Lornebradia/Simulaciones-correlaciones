#====================================#
#   SIMULACIONES CORRELACIONES       #
#   Simulaciones empleando la        #
#     REGLA TAMAÑO PREFIJADO para    #
#     análisis de correlaciones      #
#====================================#

# Condiciones de simulación-----
library(ggplot2)
library(grid)
library(psych)

num.sim <- 10000 # número de simulaciones
nfsr <- c(20, 30, 40, 50, 60, 70) # tamaño muestral inicial de referencia
corr.teo <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,.99)

# Data Frame ----
datos.corrs <- data.frame(corr_teo = rep(corr.teo,each=num.sim*length(nfsr)))
datos.corrs$nfsr <- rep(nfsr,each=num.sim)
datos.corrs$decision <- datos.corrs$pvalue <- datos.corrs$t <- datos.corrs$corr_emp <- NA
rm(num.sim,nfsr,corr.teo)

# Bucle de simulación ------
source(file="code/SimCorr.R")
set.seed(388547)
datos.sim <- t(mapply(SimCorrchol, datos.corrs$nfsr, datos.corrs$corr_teo))

datos.corrs[,3:6] <- datos.sim

# Tabla Resumen -----
datos.corrs$zfisher <- fisherz(datos.corrs$corr_emp)
datos.corrs$zfisher_teo <- fisherz(datos.corrs$corr_teo)
datos.corrs <- datos.corrs[c("corr_teo","zfisher_teo","nfsr","corr_emp","t","pvalue","decision","zfisher")]
datos.corrs$varteo.zfisher <- 1/(datos.corrs$nfsr-3)

# Calcula los límites para el intervalo de confianza de Z
datos.corrs$ic95inf_z <- datos.corrs$zfisher+qnorm(.025)*sqrt(datos.corrs$varteo.zfisher)
datos.corrs$ic95sup_z <- datos.corrs$zfisher+qnorm(.975)*sqrt(datos.corrs$varteo.zfisher)

# Transforma los límites a correlaciones
# He realizado el intervalo de confianza con la transformación a r y sale exactamente lo mismo que con Z
datos.corrs$ic95inf_r <- fisherz2r(datos.corrs$ic95inf_z)
datos.corrs$ic95sup_r <- fisherz2r(datos.corrs$ic95sup_z)
datos.corrs$ic95 <- ifelse(datos.corrs$zfisher_teo>datos.corrs$ic95inf_z 
                           & datos.corrs$zfisher_teo < datos.corrs$ic95sup_z, 
                           yes=0,
                           no=1)


library(plyr)

r.corr <- ddply(datos.corrs, .(corr_teo,nfsr),summarise, 
                EPR = mean(decision),
                zfisher = mean(zfisher),
                corr_emp = mean(corr_emp),
                theo_var_z = mean(varteo.zfisher),
                emp_var_z = var(zfisher),
                cover_ic = mean(ic95),
                limit_z_inf = mean(ic95inf_z),
                limit_z_sup = mean(ic95sup_z),
                limit_r_inf = mean(ic95inf_r),
                limit_r_sup = mean(ic95sup_r),
                .progress = "text")

r.corr$nfsr <- ordered(r.corr$nfsr) # Convierte nfsr en factor ordenado

# Calcula la varianza empírica de las Z agrupadas por corr_teo y nfsr
r.corr$emp_var_z <- ddply(datos.corrs, 
                          .(corr_teo, nfsr), 
                          summarise, 
                          emp_var_z = var(zfisher),
                          .progress = "text")[,3]

# Gráficos -----
source(file="code/herramientasgraficas.R")

corr.plot <- corrEmpvsTeo(r.corr, "corr_teo", "corr_emp-corr_teo", "nfsr")+
  facet_wrap(~nfsr)+
  ggtitle(label="Empirical vs. theoretical correlation,\nFixed Sampling Rule")

corr.pow <- graf.pot(r.corr, "corr_teo", "EPR", "nfsr") + 
  ggtitle(label = "Empirical proportion of rejections,\nFixed Sampling Rule")

corr.varzteo <- corrVarZ(r.corr, ejex="corr_teo", "theo_var_z", "nfsr")+
  ggtitle("Varianza de Z de Fisher\na partir de los tamaños de parada")

corr.varzemp <- corrVarZ(r.corr, ejex="corr_teo", "emp_var_z", "nfsr")+
  ggtitle("Varianza de Z de Fisher\ncalculada empíricamente")

corr.ic95r <- ggplot(r.corr, aes(x=as.numeric(as.character(nfsr)),y=corr_emp,ymin=limit_r_inf,ymax=limit_r_sup, color = nfsr))+
  geom_pointrange(size = 1)+
  coord_flip()+
  theme_bw()+  
  theme(plot.title = element_text(size = 18, face = "bold"), panel.grid.major.x = element_line(colour = "black", linetype=2))+
  scale_y_continuous(name = "Correlación empírica", breaks = seq(0,1,.1))+
  scale_x_continuous(name = "N fijo")+
  facet_grid(corr_teo~.)+
  ggtitle("Cobertura de los intervalos de confianza\nconstruidos con r de Pearson\nRegla Fija")

corr.ic95z <- ggplot(r.corr, aes(x=as.numeric(as.character(nfsr)),y=zfisher,ymin=limit_z_inf,ymax=limit_z_sup, color = nfsr))+
  geom_pointrange(size = 1)+
  coord_flip()+
  theme_bw()+  
  theme(plot.title = element_text(size = 18, face = "bold"), 
        panel.grid.major.x = element_line(colour = "black", linetype=2),
        axis.text.x = element_text(angle = 90))+
  scale_y_continuous(name = "Z fisher", breaks = signif(digits=2, fisherz(c(seq(0,.8,.1),.99))))+
  scale_x_continuous(name = "N fijo")+
  facet_grid(corr_teo~.)+
  ggtitle("Cobertura de los intervalos de confianza\nconstruidos con Z de Fisher\nRegla Fija")

cobertura_rtp <- ggplot(r.corr, aes(x = corr_teo, y = 1-cover_ic, linetype = as.factor(nfsr), color = as.factor(nfsr)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0.95, linetype = 1,size = 2, alpha = .25)+
  theme_bw()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 18, face = "bold"))+
  scale_x_continuous(name = "Correlación teórica", breaks = seq(0,1,.1))+
  scale_y_continuous(name = "Cobertura", limits = c(.92, .97), breaks = seq(.92,.97,.005))+
  scale_linetype_discrete(name = "N Fijo")+
  scale_colour_discrete(name = "N Fijo")+
  ggtitle("Proporción de cobertura de los intervalos de confianza\nRegla Fija")

