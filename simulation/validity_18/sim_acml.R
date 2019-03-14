args = commandArgs(TRUE)
njob = as.integer(args[1])
r = as.numeric(args[2])
goal = args[3]
wd = args[4]

library(mvtnorm)
library(numDeriv)
library(stats)
source("~/tmp/validity_17/acml.R")

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
inits = c(0, true_beta1, true_gamma1, true_alpha2, true_beta2, true_gamma2, log(psi11), log(psi22), 0, 0)
NoWeighting = rep(1, nid2*n_per_id)
if (goal %in% c("intercept", "slope")) {
    samprobs = cbind(rep(1, nid2*n_per_id), rep(0, nid2*n_per_id), rep(1, nid2*n_per_id))
    w.function = rep(goal, nid2*n_per_id)
} else if (goal == "both") {
    samprobs = cbind(rep(0, nid2*n_per_id), rep(1, nid2*n_per_id))
    w.function = rep("bivar", nid2*n_per_id)
}

set.seed(12345+5000*njob)
setwd(wd)
fn_out = paste0(njob, ".RData")

results_est = matrix(NA, nrow=NSIM, ncol=6)
colnames(results_est) = c("intercept", "X", "Z", "Time", "Time_X", "Time_Z")
results_se = matrix(NA, nrow=NSIM, ncol=6)
colnames(results_se) = c("intercept", "X", "Z", "Time", "Time_X", "Time_Z")

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
    data = data.frame(Y=simY, Time=simT, id=simID, Z=rep(simZ, each=n_per_id), Weights=1)
    
    Q = matrix(NA, nid, 2)
    for (i in 1:nid) {
        idxx = which(simID == i)
        tmp = lm(simY[idxx]~simT[idxx])
        Q[i,] = coef(tmp)
    }
    order_Q = matrix(NA, nid, 2)
    order_Q[,1] = order(Q[,1])
    order_Q[,2] = order(Q[,2])

    if (goal == "intercept") {
        id_phase2 = c(order_Q[1:(nid2/2),1], order_Q[(nid-(nid2/2)+1):nid,1])
        cutpoints = cbind(rep(Q[id_phase2[nid2/2],1], nid2*n_per_id), rep(Q[id_phase2[nid2/2+1],1], nid2*n_per_id))
    } else if (goal == "slope") {
        id_phase2 = c(order_Q[1:(nid2/2),2], order_Q[(nid-(nid2/2)+1):nid,2])
        cutpoints = cbind(rep(Q[id_phase2[nid2/2],2], nid2*n_per_id), rep(Q[id_phase2[nid2/2+1],2], nid2*n_per_id))
    } else if (goal == "both") {
        id_phase2_1 = c(order_Q[1:(nid2/4),1], order_Q[(nid-(nid2/4)+1):nid,1])
        ids_remain = (1:nid)[-id_phase2_1]
        nid_remain = length(ids_remain)
        order_Q2 = order(Q[ids_remain,2])
        id_phase2_2 = ids_remain[c(order_Q2[1:(nid2/4)], order_Q2[(nid_remain-(nid2/4)+1):nid_remain])]
        id_phase2 = c(id_phase2_1, id_phase2_2)
        cutpoints = cbind(rep(Q[id_phase2_1[nid2/4],1], nid2*n_per_id), rep(Q[id_phase2_1[nid2/4+1],1], nid2*n_per_id),
                          rep(Q[id_phase2_2[nid2/4],2], nid2*n_per_id), rep(Q[id_phase2_2[nid2/4+1],2], nid2*n_per_id))
    }
    
    simX[-id_phase2] = NA
    data$X = rep(simX, each=n_per_id)
    data2 = data[which(!is.na(data$X)),]
    
    res = acml.lmem2(formula.fixed=Y~X+Z+Time+Time*X+Time*Z, 
                     formula.random=~Time, 
                     data=data2,
                     id="id", 
                     InitVals=inits, 
                     ProfileCol=NA,
                     cutpoints=cutpoints,
                     Weights="Weights", 
                     SampProb=samprobs,
                     w.function=w.function)
    results_est[nsim,] = res$coefficients[1:6]
    results_se[nsim,] = sqrt(diag(res$covariance)[1:6])
    
    if (nsim%%50 == 0) {
        print(paste(nsim, "replicates done."))
    }
}

save(list=c("results_est", "results_se"), file=fn_out)
