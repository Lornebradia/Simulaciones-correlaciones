# PRUEBAS ALFA SUPERIOR PARA REGLA CLAST #

num.sim <- 10^4 # número de simulaciones

sim.alfa <- data.frame(asup=rep(c(.10,.15,.20,.25),each=num.sim))
sim.alfa$nstop <- NA
sim.alfa$corr_teo <- NA
sim.alfa$tstad <- NA
sim.alfa$tstat <- NA
sim.alfa$decision <- NA

for (i in 1:dim(sim.alfa)[1]){
  sim.alfa[i,2:6] <- simCorrclast(n=24,r=0,asup=sim.alfa$asup[i])
}

sim.alfa$decision <- ifelse(sim.alfa$)

for (i in 1:40000){
  if (sim.alfaclast$nstop[i]==36){
    if (sim.alfaclast$pvalue[i] < .05){
      sim.alfaclast$decision[i] <- 1
    }
  }
}


sim.resumen <- aggregate.data.frame()