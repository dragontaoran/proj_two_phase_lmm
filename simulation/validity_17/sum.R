#!/usr/bin/env Rscript

NJOB = 10
r = c(0, 0.1, 0.2, 0.3)
nr = length(r)
true_beta1 = 0.3
true_beta2 = 0.3
true_gamma1 = 0.5
true_gamma2 = 0.5
true_eta = 0.4
Za = qnorm(0.975)

fn_out = "sum.tab"
sink(fn_out)
cat("r\tCovariate\tInt_Bias\tInt_SE\tInt_SEE\tInt_CP\t\tSlo_Bias\tSlo_SE\tSlo_SEE\tSlo_CP\n")
sink()

out_covar = c("X","Z", "T", "XT", "ZT")
out_empty = rep("", length(out_covar))

for (j in 1:nr) {
    #### intercept 
    load(paste0("res/intercept_r", r[j], "_njob1.Rdata"))
    res_est = results_est
    res_se = results_se
    for (i in 2:NJOB) {
        load(paste0("res/intercept_r", r[j], "_njob", i, ".Rdata"))
        res_est = rbind(res_est, results_est)
        res_se = rbind(res_se, results_se)
    }
    print(dim(res_est))
    print(dim(res_se))
    
    X_bias = mean(res_est[,"X"])-true_beta1
    X_se = sd(res_est[,"X"])
    X_see = mean(res_se[,"X"])
    X_cp = mean((res_est[,"X"]-Za*res_se[,"X"] <= true_beta1) & (res_est[,"X"]+Za*res_se[,"X"] >= true_beta1))
    
    Z_bias = mean(res_est[,"Z"])-true_gamma1
    Z_se = sd(res_est[,"Z"])
    Z_see = mean(res_se[,"Z"])
    Z_cp = mean((res_est[,"Z"]-Za*res_se[,"Z"] <= true_gamma1) & (res_est[,"Z"]+Za*res_se[,"Z"] >= true_gamma1))
    
    T_bias = mean(res_est[,"Time"])-true_eta
    T_se = sd(res_est[,"Time"])
    T_see = mean(res_se[,"Time"])
    T_cp = mean((res_est[,"Time"]-Za*res_se[,"Time"] <= true_eta) & (res_est[,"Time"]+Za*res_se[,"Time"] >= true_eta))
    
    XT_bias = mean(res_est[,"Time_X"])-true_beta2
    XT_se = sd(res_est[,"Time_X"])
    XT_see = mean(res_se[,"Time_X"])
    XT_cp = mean((res_est[,"Time_X"]-Za*res_se[,"Time_X"] <= true_beta2) & (res_est[,"Time_X"]+Za*res_se[,"Time_X"] >= true_beta2))
    
    ZT_bias = mean(res_est[,"Time_Z"])-true_gamma2
    ZT_se = sd(res_est[,"Time_Z"])
    ZT_see = mean(res_se[,"Time_Z"])
    ZT_cp = mean((res_est[,"Time_Z"]-Za*res_se[,"Time_Z"] <= true_gamma2) & (res_est[,"Time_Z"]+Za*res_se[,"Time_Z"] >= true_gamma2))
    
    out_bias_int = c(X_bias, Z_bias, T_bias, XT_bias, ZT_bias)
    out_se_int = c(X_se, Z_se, T_se, XT_se, ZT_se)
    out_see_int = c(X_see, Z_see, T_see, XT_see, ZT_see)
    out_cp_int = c(X_cp, Z_cp, T_cp, XT_cp, ZT_cp)

    #### slope    
    load(paste0("res/slope_r", r[j], "_njob1.Rdata"))
    res_est = results_est
    res_se = results_se
    for (i in 2:NJOB) {
        load(paste0("res/slope_r", r[j], "_njob", i, ".Rdata"))
        res_est = rbind(res_est, results_est)
        res_se = rbind(res_se, results_se)
    }
    print(dim(res_est))
    print(dim(res_se))
    
    X_bias = mean(res_est[,"X"])-true_beta1
    X_se = sd(res_est[,"X"])
    X_see = mean(res_se[,"X"])
    X_cp = mean((res_est[,"X"]-Za*res_se[,"X"] <= true_beta1) & (res_est[,"X"]+Za*res_se[,"X"] >= true_beta1))
    
    Z_bias = mean(res_est[,"Z"])-true_gamma1
    Z_se = sd(res_est[,"Z"])
    Z_see = mean(res_se[,"Z"])
    Z_cp = mean((res_est[,"Z"]-Za*res_se[,"Z"] <= true_gamma1) & (res_est[,"Z"]+Za*res_se[,"Z"] >= true_gamma1))
    
    T_bias = mean(res_est[,"Time"])-true_eta
    T_se = sd(res_est[,"Time"])
    T_see = mean(res_se[,"Time"])
    T_cp = mean((res_est[,"Time"]-Za*res_se[,"Time"] <= true_eta) & (res_est[,"Time"]+Za*res_se[,"Time"] >= true_eta))
    
    XT_bias = mean(res_est[,"Time_X"])-true_beta2
    XT_se = sd(res_est[,"Time_X"])
    XT_see = mean(res_se[,"Time_X"])
    XT_cp = mean((res_est[,"Time_X"]-Za*res_se[,"Time_X"] <= true_beta2) & (res_est[,"Time_X"]+Za*res_se[,"Time_X"] >= true_beta2))
    
    ZT_bias = mean(res_est[,"Time_Z"])-true_gamma2
    ZT_se = sd(res_est[,"Time_Z"])
    ZT_see = mean(res_se[,"Time_Z"])
    ZT_cp = mean((res_est[,"Time_Z"]-Za*res_se[,"Time_Z"] <= true_gamma2) & (res_est[,"Time_Z"]+Za*res_se[,"Time_Z"] >= true_gamma2))
    
    out_bias_slo = c(X_bias, Z_bias, T_bias, XT_bias, ZT_bias)
    out_se_slo = c(X_se, Z_se, T_se, XT_se, ZT_se)
    out_see_slo = c(X_see, Z_see, T_see, XT_see, ZT_see)
    out_cp_slo = c(X_cp, Z_cp, T_cp, XT_cp, ZT_cp)
    
    out_r = c(r[j], rep("", length(out_covar)-1))
    
    out = data.frame(out_r, out_covar, out_bias_int, out_se_int, out_see_int, out_cp_int, 
                     out_empty, out_bias_slo, out_se_slo, out_see_slo, out_cp_slo)
    write.table(out, file=fn_out, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t")
}
