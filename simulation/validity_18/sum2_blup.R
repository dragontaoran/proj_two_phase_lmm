r = c(0, 1, 2, 3)
nsieve_set = c(20)
nr = length(r)
true_beta1 = 0.3
true_beta2 = 0.3
true_gamma1 = 0.5
true_gamma2 = 0.5
true_eta = 0.4
Za = qnorm(0.975)
goal_set = c("intercept", "slope", "both")
out_covar = c("$X$","$Z$", "$\\bT$", "$X\\bT$", "$Z\\bT$")
out_empty = rep("", length(out_covar))

for (nsieve in nsieve_set) {
	for (goal in goal_set) {
	    
	    fn_out = paste0("sum_blup_", goal, "_nsieve", nsieve, ".tab")
	    sink(fn_out)
	    cat("r\tCovariate\tBias\tSE\tSEE\tCP\tRE\n")
	    sink()
	    
	    for (j in 1:nr) {
	        out_r = c(r[j], rep("", length(out_covar)-1))
	         
	        load(paste0("results/blup_", goal, "_r", r[j], "_nsieve", nsieve, ".RData"))
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
	        
	        out_bias_smle = c(X_bias, Z_bias, T_bias, XT_bias, ZT_bias)
	        out_se_smle = c(X_se, Z_se, T_se, XT_se, ZT_se)
	        out_see_smle = c(X_see, Z_see, T_see, XT_see, ZT_see)
	        out_cp_smle = c(X_cp, Z_cp, T_cp, XT_cp, ZT_cp)
	        
	        #### ods
	        load(paste0("results/ods_", goal, "_r", r[j], "_nsieve", nsieve, ".RData"))
	        print(dim(res_est))
	        print(dim(res_se))
	        
	        ods_X_se = sd(res_est[,"X"])
	        ods_Z_se = sd(res_est[,"Z"])
	        ods_T_se = sd(res_est[,"Time"])
	        ods_XT_se = sd(res_est[,"Time_X"])
	        ods_ZT_se = sd(res_est[,"Time_Z"])
	
	        out_re_ods = c(ods_X_se/X_se, ods_Z_se/Z_se, ods_T_se/T_se, ods_XT_se/XT_se, ods_ZT_se/ZT_se)^2
	        out = data.frame(out_r, out_covar, out_bias_smle, out_se_smle, out_see_smle, out_cp_smle, out_re_ods)
	        write.table(out, file=fn_out, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t")
	    }    
	}
}
