#!/usr/bin/env Rscript

args = commandArgs(TRUE)
idx = as.integer(args)

njobs = rep(1:10, 4)
rs = rep(c(0, 0.1, 0.2, 0.3), each=10)

library(devtools)
# install_github("dragontaoran/TwoPhaseReg")
library(TwoPhaseReg)

njob = njobs[idx]
r = rs[idx]
NSIM = 1000
nid = 1000
nid2 = 400
n_per_id = 5
true_beta1 = 0.3
true_beta2 = 0.3
true_alpha2 = 0.4
true_gamma1 = 0.5
true_gamma2 = 0.5
psi11 = 1
psi22 = 1
N_SIEVE = 10

set.seed(12345+5000*njob)
fn_out = paste0("res/intercept_r", r, "_njob", njob, ".Rdata")

results_est = matrix(NA, nrow=NSIM, ncol=6)
colnames(results_est) = c("intercept", "X", "Z", "Time", "Time_X", "Time_Z")
results_se = matrix(NA, nrow=NSIM, ncol=6)
colnames(results_se) = c("intercept", "X", "Z", "Time", "Time_X", "Time_Z")
results_vc = matrix(NA, nrow=NSIM, ncol=4)

for (nsim in 1:NSIM) {
        
    ### generate data
    U2 = runif(nid)
    simX = runif(nid)
    simZ = r*simX+U2
    simT = rep(seq(-1, 1, length.out=n_per_id), nid)
    simb0 = rnorm(nid, sd=sqrt(psi11))
    simb1 = rnorm(nid, sd=sqrt(psi22))
    simY = rep(true_beta1*simX+true_gamma1*simZ, each=n_per_id)
    simY = simY+true_alpha2*simT
    simY = simY+true_beta2*rep(simX, each=n_per_id)*simT
    simY = simY+true_gamma2*rep(simZ, each=n_per_id)*simT
    simY = simY+rep(simb0, each=n_per_id)
    simY = simY+rep(simb1, each=n_per_id)*simT
    simY = simY+rnorm(nid*n_per_id)
    simID = rep(1:nid, each=n_per_id)
    Bspline_Z = matrix(NA, nrow=nid, ncol=N_SIEVE)
    cut_z = cut(simZ, breaks=quantile(simZ, probs=seq(0, 1, 1/N_SIEVE)), include.lowest = TRUE)
    for (i in 1:N_SIEVE) {
        Bspline_Z[,i] = as.numeric(cut_z == names(table(cut_z))[i])
    }
    colnames(Bspline_Z) = paste("bs", 1:N_SIEVE, sep="")
    
    Q = rep(NA, nid)
    for (i in 1:nid) {
        idxx = which(simID == i)
        tmp = lm(simY[idxx]~simT[idxx])
        Q[i] = coef(tmp)[1]
    }
    order_Q = order(Q)
    id_phase2 = c(order_Q[1:(nid2/2)], order_Q[(nid-(nid2/2)+1):nid])
    simX[-id_phase2] = NA
    
    data = data.frame(Y=simY, Time=simT, ID=simID, X=rep(simX, each=n_per_id), Z=rep(simZ, each=n_per_id), Bspline_Z[simID,])
    
    res = smle_lmm(Y = "Y", Time = "Time", ID = "ID", X = "X", Z = "Z", Bspline_Z = colnames(Bspline_Z), data = data, ZT=TRUE)
    results_est[nsim,] = t(res$coefficients[,1])
    results_se[nsim,] = t(res$coefficients[,2])
    results_vc[nsim,] = t(res$vc)
    
    if (nsim%%50 == 0) {
        print(paste(nsim, "replicates done."))
    }
}

save(list=c("results_est", "results_se", "results_vc"), file=fn_out)





