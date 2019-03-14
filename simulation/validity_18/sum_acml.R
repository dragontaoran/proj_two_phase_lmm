NJOB = 10
r_set = c(2, 3, 4)
# goal_set = c("intercept", "slope", "both")
goal_set = c("intercept", "slope", "both")
design = c("ods")

dir.create("results", showWarnings=FALSE)

for (goal in goal_set) {
    for (r in r_set) {
        load(paste0("res/acml_", design, "_", goal, "_r", r, "/1.RData"))
        res_est = results_est
        res_se = results_se
        for (i in 2:NJOB) {
            load(paste0("res/acml_", design, "_", goal, "_r", r, "/", i, ".RData"))
            res_est = rbind(res_est, results_est)
            res_se = rbind(res_se, results_se)
        }
        print(c(dim(res_est), dim(res_se)))
        fn.out = paste0("results/acml_", design, "_", goal, "_r", r, ".RData")
        save(list=c("res_est", "res_se"), file=fn.out)
    }    
}
