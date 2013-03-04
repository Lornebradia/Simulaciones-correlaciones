# PRUEBAS ALFA SUPERIOR PARA REGLA CLAST #

num.sim <- 10^4 # número de simulaciones

sim.alfa <- data.frame(asup=rep(c(.10,.15,.20,.25),each=num.sim))
sim.alfa$nstop <- NA
sim.alfa$corr_emp <- NA
sim.alfa$tstat <- NA
sim.alfa$pvalue <- NA
sim.alfa$decision <- NA
rm(num.sim)

for (i in 1:dim(sim.alfa)[1]){
  sim.alfa[i,2:6] <- simCorrclast(n=24,r=0,asup=sim.alfa$asup[i])
}

sim.alfa$decision <- ifelse(sim.alfa$pvalue <.01,1,0)

for (i in 1:40000){
  if (sim.alfa$nstop[i]==36){
    if (sim.alfa$pvalue[i] < .05){
      sim.alfa$decision[i] <- 1
    }
  }
}

r.simalfa <- aggregate.data.frame(sim.alfa,  by = list(sim.alfa$asup),FUN=mean)
