#====================================#
#   SIMULACIONES CORRELACIONES       #
#   Simulaciones empleando la        #
#     REGLA CLAST para               #
#     análisis de correlaciones      #
#====================================#

# Condiciones de simulación-----
num.sim <- 10000 # número de simulaciones
nfsr <- c(20, 30, 40, 50, 60) # tamaño muestral inicial de referencia
corr.teo <- seq(0,1,.1)
corr.teo[11] <- .99

# Data Frame ----
datos.corrsclast<- data.frame(corr_teo = rep(corr.teo,each=num.sim*length(nfsr)))
datos.corrsclast$nfsr <- rep(nfsr,each=num.sim)
datos.corrsclast$decision <- datos.corrsclast$pvalue <- datos.corrsclast$t <- datos.corrsclast$corr_emp <- datos.corrsclast$nstop<- NA
rm(num.sim,nfsr,corr.teo)

# Bucle de simulación ------
source(file="code/SimCorrclast.R")
set.seed(1234)
datos.simclast <- t(mapply(SimCorrclast, datos.corrsclast$nfsr, datos.corrsclast$corr_teo, asup = .25))

datos.corrsclast[,3:7] <- datos.simclast


# Tabla Resumen -----
library(psych)
datos.corrsclast$zfisher <- fisherz(datos.corrsclast$corr_emp) 
datos.corrsclast$zfisher_teo <- fisherz(datos.corrsclast$corr_teo)
datos.corrsclast <- datos.corrsclast[c("corr_teo","zfisher_teo","nfsr","nstop","corr_emp","t","pvalue","decision","zfisher")]
datos.corrsclast$varteo.zfisher <- 1/(datos.corrsclast$nstop-3)

# Calcula los límites para el intervalo de confianza de Z
datos.corrsclast$ic95inf_z <- datos.corrsclast$zfisher+qnorm(.025)*sqrt(datos.corrsclast$varteo.zfisher)
datos.corrsclast$ic95sup_z <- datos.corrsclast$zfisher+qnorm(.975)*sqrt(datos.corrsclast$varteo.zfisher)

# Transforma los límites a correlaciones
datos.corrsclast$ic95inf_r <- fisherz2r(datos.corrsclast$ic95inf_z)
datos.corrsclast$ic95sup_r <- fisherz2r(datos.corrsclast$ic95sup_z)
datos.corrsclast$ic95 <- ifelse(datos.corrsclast$zfisher_teo>datos.corrsclast$ic95inf_z 
                           & datos.corrsclast$zfisher_teo < datos.corrsclast$ic95sup_z, 
                           yes=0,
                           no=1)

library(plyr)

r.corrclast <- ddply(datos.corrsclast, .(corr_teo,nfsr),summarise, 
                nstop = mean(nstop),     
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


# Calcula la varianza empírica de las Z agrupadas por corr_teo y nfsr

r.corrclast$emp_var_z <- ddply(datos.corrsclast, .(corr_teo, nfsr), summarise, emp_var_z = var(zfisher), .progress = "text")[,3]

# Gráficos -----
library(ggplot2)
library(grid)
source("/Users/lbraschi/Documents/R_Cosas/multiplot.R")
source(file="code/herramientasgraficas.R")

clastcorr.plot <- corrEmpvsTeo(clastcorr.plot, "corr_teo", "corr_emp", "nfsr")
   ggtitle(label = "Estimation of correlation, CLAST rule")+
  
ggsave(plot=clastcorr.plot, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/corrclast.png",height=8, width=8,dpi=600)

clastcorr.pow <- ggplot(r.corrclast,aes(x=corr_teo,y=EPR, color = factor(nfsr), shape = factor(nfsr), linetype = factor(nfsr)))
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

clastcorr.varzteo <- ggplot(r.corrclast, aes(x=corr_teo,y=theo_var_z, color=factor(nfsr),shape=factor(nfsr), linetype=factor(nfsr)))+ 
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold"))+
  scale_x_continuous(name="Correlación teórica")+
  scale_y_continuous(name="Varianza teórica")+
  scale_shape_discrete(name="N fijo")+
  scale_color_discrete(name="N fijo")+
  scale_linetype_discrete(name="N fijo")+
  ggtitle("Varianza de Z de Fisher\na partir de los tamaños de parada")

clastcorr.varzteo

clastcorr.varzemp <- ggplot(r.corrclast, aes(x=corr_teo,y=emp_var_z, color=factor(nfsr),shape=factor(nfsr), linetype=factor(nfsr)))
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

clastcorr.ic95r <- ggplot(r.corrclast, aes(x=nfsr,y=corr_emp,ymin=limit_r_inf,ymax=limit_r_sup))+
  geom_pointrange(size = 1)+
  coord_flip()+
  theme_bw()+  
  theme(plot.title = element_text(size = 18, face = "bold"), panel.grid.major.x = element_line(colour = "black", linetype=2))+
  scale_y_continuous(name = "Correlación empírica", breaks = c(0,.20,.40,.60,.80))+
  scale_x_continuous(name = "N fijo")+
  facet_grid(corr_teo~.)+
  ggtitle("Cobertura de los intervalos de confianza construidos con r de Pearson\nRegla CLAST")
ggsave(plot=clastcorr.ic95r, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/clastcorric95r.png",height=8, width=12,dpi=600)

clastcorr.ic95r

clastcorr.ic95z <- ggplot(r.corrclast, aes(x=nfsr,y=zfisher,ymin=limit_z_inf,ymax=limit_z_sup))+
  geom_pointrange(size = 1)+
  coord_flip()+
  theme_bw()+  
  theme(plot.title = element_text(size = 18, face = "bold"), panel.grid.major.x = element_line(colour = "black", linetype=2))+
  scale_y_continuous(name = "Z fisher", breaks = signif(digits=2,r_to_z(c(0,.20,.40,.60,.80))))+
  scale_x_continuous(name = "N fijo")+
  facet_grid(corr_teo~.)+
  ggtitle("Cobertura de los intervalos de confianza construidos con Z de Fisher\nRegla CLAST")
ggsave(plot=clastcorr.ic95z, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/clastcorric95z.png",height=8, width=12,dpi=600)

clastcorr.ic95z

cobertura_clast <- ggplot(r.corrclast, aes(x = corr_teo, y = 1-cover_ic, linetype = as.factor(nfsr), color = as.factor(nfsr)))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0.95, linetype = 1,size = 2, alpha = .25)+
  theme_bw()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 18, face = "bold"))+
  scale_x_continuous(name = "Correlación teórica")+
  scale_y_continuous(name = "Cobertura", limits = c(.92, .97), breaks = seq(.92,.97,.005))+
  scale_linetype_discrete(name = "N Fijo")+
  scale_colour_discrete(name = "N Fijo")+
  ggtitle("Proporción de cobertura de los intervalos de confianza\nRegla CLAST")
ggsave(plot=cobertura_clast, "/Users/lbraschi/Documents/Investigación/Tesis/simulacion_correlaciones/coberturaclast.png",height=8, width=8,dpi=600)

cobertura_clast

# Gráficos Manuel ------

clastcorr_emp <- ggplot(r.corrclast, aes(x = nfsr, y = corr_emp, color = as.factor(corr_teo)))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 12),
        panel.grid.major.y = element_line(size = 1, color = "grey"))+
  scale_x_continuous(name = "Tamaño fijo de la muestra")+
  scale_y_continuous(name = "Correlación muestral media")+
  scale_color_discrete(name = "Correlación poblacional r")

clastcorr_sesgo <- ggplot(r.corrclast, aes(x = nfsr, y = sesgo, color = as.factor(corr_teo)))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 12),
        panel.grid.major.y = element_line(size = 1, color = "grey"))+
  scale_x_continuous(name = "Tamaño fijo de la muestra")+
  scale_y_continuous(name = "Sesgo de correlación")+
  scale_color_discrete(name = "Correlación poblacional r")

clastcorr_varempnfijo <- ggplot(r.corrclast, aes(x = nfsr, y = emp_var_z, color =as.factor(corr_teo)))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme_bw()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 12))+
  scale_x_continuous(name = "Tamaño fijo de la muestra")+
  scale_y_continuous(name = "Varianza empírica Z media")+
  scale_color_discrete(name = "Correlación poblacional r")

clastcorr_varempnfijo

clastcor.vars <- ggplot(r.corrclast.melt[r.corrclast.melt$nfsr == 60,], aes(x = corr_teo, y = value, linetype = variable))+
  geom_line(size = 1)+
  theme(panel.background = element_rect(fill = "white"), axis.line = element_line(colour = "black"))+
  scale_y_continuous(name = "", breaks = seq(0,.4,.02), limits = c(0,.4))+
  scale_x_continuous(name = expression(rho), breaks = seq(0,1,.2))+
  scale_linetype_manual(name = "",values = c("dashed","solid"), labels = c("Theoretical var(Z)", "Empirical var(Z)"))+
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 10, colour = "black"), axis.text.y = element_text(colour = "black"))+
  theme(legend.position = "bottom", legend.text = element_text(size = 12), legend.title = element_text(size = 12), legend.key.width = unit(2,"cm"))+
  theme(plot.title = element_text(size = 18, face = "bold")) +
  ggtitle(label = "Variance of correlation estimation\nN = 60")
ggsave(plot=clastcor.vars, filename="clastcorvars60.png",width=7, height=6, dpi = 100) 
  
clastcor.vars