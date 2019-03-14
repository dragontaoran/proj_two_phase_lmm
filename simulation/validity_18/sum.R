NJOB = 10
r_set = c(0, 1, 2, 3)
nsieve_set = c(20)
goal_set = c("intercept", "slope", "both")
design_set = c("ods", "blup")

dir.create("results", showWarnings=FALSE)

for (design in design_set) {
    for (goal in goal_set) {
        for (r in r_set) {
			for (nsieve in nsieve_set) {
            	load(paste0("res/", design, "_", goal, "_r", r, "_nsieve", nsieve, "/1.RData"))
            	res_est = results_est
            	res_se = results_se
            	res_vc = results_vc
            	for (i in 2:NJOB) {
            	    load(paste0("res/", design, "_", goal, "_r", r, "_nsieve", nsieve, "/", i, ".RData"))
            	    res_est = rbind(res_est, results_est)
            	    res_se = rbind(res_se, results_se)
            	    res_vc = rbind(res_vc, results_vc)
            	}
            	print(c(dim(res_est), dim(res_se), dim(res_vc)))
            	fn.out = paste0("results/", design, "_", goal, "_r", r, "_nsieve", nsieve, ".RData")
            	save(list=c("res_est", "res_se", "res_vc"), file=fn.out)
			}
        }    
    }   
}
