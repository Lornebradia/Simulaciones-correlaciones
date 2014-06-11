# Gráfico potencia

graf.pot <- function(data, ejex, ejey, tipo){
  # Grafica la potencia de la prueba
  require(ggplot2)
  require(grid)
  pow <- ggplot(data, aes_string(x = ejex, y = ejey, color = tipo, shape = tipo, linetype = tipo))+
    geom_point(size = 3)+
    geom_line(size = .75)+
    theme_bw()+
    theme(legend.position="bottom", 
          legend.text=element_text(size = 15), 
          legend.title = element_text(size = 15), 
          legend.key.width = unit(1.5,"cm"))+
    theme(axis.line = element_line(colour = "black"))+
    theme(plot.title = element_text(size = 18, face = "bold"))+
    theme(axis.title = element_text(size=18),axis.text = element_text(size=15, color = "black")) +
    scale_x_continuous(name=expression(rho), breaks = seq(0,1,.1))+
    scale_y_continuous(name="Proportion of rejections",limits=c(0,1), breaks = seq(0,1,.1))+
    scale_shape_discrete(name="N fix")+
    scale_color_discrete(name="N fix")+
    scale_linetype_discrete(name="N fix")+
    geom_hline(yintercept=.05, linetype=2,alpha=.5,size=1)
  return(pow)
}

# Gráfico efectos empiricos vs teoricos

corrEmpvsTeo <- function (data, ejex, ejey, nfsr) {
  require(ggplot2)
  require(grid)
  corr.plot <- ggplot(data,aes_string(x = ejex, y = ejey, color = nfsr, linetype = nfsr, shape = nfsr))+ 
    geom_line(size = .75)+
    geom_point()+
    theme_bw()+
    theme(legend.position="bottom", 
          legend.text=element_text(size = 15), 
          legend.title = element_text(size = 15), 
          legend.key.width = unit(1.5,"cm"))+
    theme(axis.line = element_line(colour = "black"))+
    theme(plot.title = element_text(size = 18, face = "bold"))+
    theme(axis.title.x = element_text(size=18),axis.text = element_text(size=15, color = "black")) +
    theme(axis.title.y = element_text(size=18, angle=0)) +
    scale_x_continuous(name = expression(rho),breaks=seq(0,1,.1)) +
    scale_y_continuous(name = expression(hat(rho)), breaks=seq(0,1,.1),limits=c(-0.075,1)) +
    geom_abline(intercept = 0, slope = 1, size = 2, alpha = .5) +
    scale_shape_discrete("")+
    scale_linetype_discrete("")+
    scale_color_discrete("")+
    theme(plot.margin = unit(c(.5,.5,0,0),"cm"))
  return(corr.plot)
}
  
