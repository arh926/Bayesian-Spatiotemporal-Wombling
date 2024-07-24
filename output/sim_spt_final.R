# rm(list = ls())
# sys.name = Sys.info()["sysname"]
# if(sys.name == "Linux"){
#   setwd("/home/aritrah-dsph/Dropbox/Repos/sptwombling/R/")
# }else if(sys.name == "Darwin"){
#   setwd("/Users/aritrah/Dropbox/Repos/sptwombling/R/")
# }else if(sys.name == "Windows"){
#   setwd("C:/Users/ah3758/Dropbox/Repos/sptwombling/R/")
# }
#
# require(parallel)
# require(coda)
# require(xtable)
#
#
# fn.list = list.files("cov_fn/")
# sapply(fn.list, function(x){
#   source(paste("cov_fn/", x, sep = ""))
#   return(0)
# })
#
# source("generate_spt_data.R")
# source("collapsed-mh-spt.R")
# source("spsample.R")
# source("spt_gradients.R")
#
#
# ####################################
# # Generate Spatiotemporal Process  #
# ####################################
# seed = NULL
# Ns = c(30, 50, 100)
# Nt = c(3, 6, 9)
# nrep = 10
#
# pattern = "2"
# cov.type = "matern2"
#
# niter = 5e3; nburn = 0.5 * niter; report = 1e2
# #################
# # ALREADY SAVED #
# #################
# for(i in 1:length(Ns)){
#   for(j in 1:length(Nt)){
#     N = Ns[i] * Nt[j]
#
#     results.sim = mclapply(1:nrep, function(x){
#       spt.sim1 = generate_spt_data(Ns = Ns[i], Nt = Nt[j],
#                                    pattern.type = pattern, tau = 1,
#                                    gradients = FALSE,
#                                    derived.geom = FALSE, # create plots for this 02/26
#                                    seed = seed)
#       coords = spt.sim1$coords
#       t = spt.sim1$t
#       y = spt.sim1$y
#
#       chain_y = hlmBayes_mh.spt(coords = coords, t = t, y = y, cov.type = cov.type,
#                                 niter = niter, nburn = nburn, report = report)
#
#       tau2.ci = quantile(chain_y$tau2, probs = c(0.5, 0.025, 0.975))
#       if((1 >= tau2.ci[2]) & (1 <= tau2.ci[3])) bad = FALSE else bad = TRUE
#       while(bad){
#
#         spt.sim1 = generate_spt_data(Ns = Ns[i], Nt = Nt[j],
#                                      pattern.type = pattern, tau = 1,
#                                      gradients = FALSE,
#                                      derived.geom = FALSE, # create plots for this 02/26
#                                      seed = seed)
#         coords = spt.sim1$coords
#         t = spt.sim1$t
#         y = spt.sim1$y
#
#         chain_y = hlmBayes_mh.spt(coords = coords, t = t, y = y, cov.type = cov.type,
#                                   niter = niter, nburn = nburn, report = report)
#         chain_y
#         tau2.ci = quantile(chain_y$tau2, probs = c(0.5, 0.025, 0.975))
#         if((1 >= tau2.ci[2]) & (1 <= tau2.ci[3])) bad = FALSE else bad = TRUE
#       }
#
#       chain_y
#
#     }, mc.cores = 10)
#     # save(results.sim, file = paste0("../data/Simulation 1/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData")) # pattern 1
#     save(results.sim, file = paste0("../data/Simulation 2/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData")) # pattern 2
#     cat("Iteration Setting:: Ns:- ", Ns[i], "Nt:- ", Nt[j], "\t", "time::", as.character(Sys.time()), "\n")
#   }
# }
# Check::
# for(i in 1:length(Ns)){
#   for(j in 1:length(Nt)){
#     # load(paste0("../data/Simulation 1/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData"))
#     load(paste0("../data/Simulation 2/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData"))
#     tau2.res = do.call(rbind, lapply(results.sim, function(x) quantile(x$tau2, probs = c(0.5, 0.025, 0.975))))
#     print(tau2.res)
#     cat("Tau2.est::", round(median(tau2.res[, 1]), 2), "Median Length::", round(median(apply(tau2.res, 1, function(x) x[3]-x[2])), 2), "\n")
#   }
# }
#####################################
# Post-MCMC Sampling for Beta and Z #
#####################################
# for(i in 1:length(Ns)){
#   for(j in 1:length(Nt)){
#     # load(paste0("../data/Simulation 1/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData"))
#     load(paste0("../data/Simulation 2/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData"))
#     for(k in 1:nrep){
#       y = results.sim[[k]]$y
#
#       coords = results.sim[[k]]$coords
#       t = results.sim[[k]]$t
#       N = length(t) * nrow(coords)
#
#       phis = results.sim[[k]]$phis
#       phit = results.sim[[k]]$phit
#       sig2 = results.sim[[k]]$sig2
#       tau2 = results.sim[[k]]$tau2
#
#       z.beta.sample = spsample(y = y, coords = coords, t = t,
#                                phis = phis, phit = phit, sig2 = sig2, tau2 = tau2,
#                                cov.type = cov.type, silent = FALSE)
#       results.sim[[k]]$z = z.beta.sample$z # z + beta
#       results.sim[[k]]$beta = z.beta.sample$beta
#     }
#     # save(results.sim, file = paste0("../data/Simulation 1/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData"))
#     save(results.sim, file = paste0("../data/Simulation 2/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData"))
#     cat("Iteration Setting:: Ns:- ", Ns[i], "Nt:- ", Nt[j], "\n")
#   }
# }

# Check::
# sim.results = array(NA, dim = c(length(Ns), length(Nt), nrep, 2))
# for(i in 1:length(Ns)){
#   for(j in 1:length(Nt)){
#     load(paste0("../data/Simulation 1/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData"))
#     spt.est = do.call(rbind, lapply(results.sim, function(x){
#       est = cbind(x$y, t(apply(x$z, 2, function(x){
#         c(median(x), coda::HPDinterval(as.mcmc(x)))
#       })))
#       c(sqrt(mean((est[,1] - est[,2])^2)), mean(apply(est, 1, function(s) ifelse(s[1] >= s[3] & s[1] <= s[4], 1, 0))))
#     }))
#     sim.results[i, j, , ]  = spt.est
#   }
# }
# sim.results[3, 3, ,]

#####################################
# Gradient and Curvature Estimation #
#####################################
grid.points <- expand.grid(x = seq(0, 1, by = 0.1),
                           y = seq(0, 1, by = 0.1))

for(i in 1:length(Ns)){
  for(j in 1:length(Nt)){
    load(paste0("../data/Simulation 1/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData"))
    # load(paste0("../data/Simulation 2/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData"))
    t = 1:Nt[j]
    # True Values
    true.grad.sx <- true.grad.sy <- true.curv.sxx <- true.curv.sxy <- true.curv.syy <- true.grad.t <- true.grad.sxt <- true.grad.syt <- true.curv.sxxt <- true.curv.sxyt <- true.curv.syyt <- true.grad.tt <- true.grad.sxtt <- true.grad.sytt <- true.curv.sxxtt <- true.curv.sxytt <- true.curv.syytt <- list()
    # Pattern-1
    for(i.t in t){
      true.grad.sx[[i.t]] <- 30 * pi * cos(3 * pi * grid.points[,1])
      true.grad.sy[[i.t]] <- -30 * pi * sin(3 * pi * grid.points[,2]) * cos(i.t * pi/7)
      true.curv.sxx[[i.t]] <- -90 * pi^2 * sin(3 * pi * grid.points[,1])
      true.curv.sxy[[i.t]] <- rep(0, nrow(grid.points))
      true.curv.syy[[i.t]] <- -90 * pi^2 * cos(3 * pi * grid.points[,2]) * cos(i.t * pi/7)
      true.grad.t[[i.t]] <- -10 * pi * cos(3 * pi * grid.points[,2]) * sin(i.t * pi/7)/7
      true.grad.sxt[[i.t]] <- rep(0, nrow(grid.points))
      true.grad.syt[[i.t]] <- 30 * pi^2 * sin(3 * pi * grid.points[,2]) * sin(i.t * pi/7)/7
      true.curv.sxxt[[i.t]] <- rep(0, nrow(grid.points))
      true.curv.sxyt[[i.t]] <- rep(0, nrow(grid.points))
      true.curv.syyt[[i.t]] <- 90 * pi^3 * cos(3 * pi * grid.points[,2]) * sin(i.t * pi/7)/7
      true.grad.tt[[i.t]] <- -10 * pi^2 * cos(3 * pi * grid.points[,2]) * cos(i.t * pi/7)/49
      true.grad.sxtt[[i.t]] <- rep(0, nrow(grid.points))
      true.grad.sytt[[i.t]] <- 30 * pi^3 * sin(3 * pi * grid.points[,2]) * cos(i.t * pi/7)/49
      true.curv.sxxtt[[i.t]] <- rep(0, nrow(grid.points))
      true.curv.sxytt[[i.t]] <- rep(0, nrow(grid.points))
      true.curv.syytt[[i.t]] <- 90 * pi^4 * cos(3 * pi * grid.points[,2]) * cos(i.t * pi/7)/49
    }

    # # Pattern-2
    # for(i.t in t){
    #   true.grad.sx[[i.t]] = 30 * pi * cos(3 * pi * grid.points[, 1]) * cos(3 * pi * grid.points[, 2]) * cos(pi * i.t/7)
    #   true.grad.sy[[i.t]] = -30 * pi * sin(3 * pi * grid.points[, 1]) * sin(3 * pi * grid.points[,2]) * cos(pi * i.t/7)
    #   true.curv.sxx[[i.t]] = -90 * pi^2 * sin(3 * pi * grid.points[,1]) * cos(3 * pi * grid.points[,2])* cos(pi * i.t/7)
    #   true.curv.sxy[[i.t]] = -90 * pi^2 * cos(3 * pi * grid.points[,1]) * sin(3 * pi * grid.points[,2])* cos(pi * i.t/7)
    #   true.curv.syy[[i.t]] = -90 * pi^2 * sin(3 * pi * grid.points[,1]) * cos(3 * pi * grid.points[,2]) * cos(pi * i.t/7)
    #   true.grad.t[[i.t]] = -10 * pi/7 * sin(3 * pi * grid.points[,1]) * cos(3 * pi * grid.points[, 2]) * sin(pi * i.t/7)
    #   true.grad.sxt[[i.t]] = -30 * pi^2/7 * cos(3 * pi * grid.points[,1]) * cos(3 * pi * grid.points[, 2]) * sin(pi * i.t/7)
    #   true.grad.syt[[i.t]] = 30 * pi^2/7 * sin(3 * pi * grid.points[, 1]) * sin(3 * pi * grid.points[, 2]) * sin(pi * i.t/7)
    #   true.curv.sxxt[[i.t]] = 90 * pi^3/7 * sin(3 * pi * grid.points[, 1]) * cos(3 * pi * grid.points[,2]) * sin(pi * i.t/7)
    #   true.curv.sxyt[[i.t]] = 90 * pi^3/7 * cos(3 * pi * grid.points[, 1]) * sin(3 * pi * grid.points[,2]) * sin(pi * i.t/7)
    #   true.curv.syyt[[i.t]] = 90 * pi^3/7 * sin(3 * pi * grid.points[, 1]) * cos(3 * pi * grid.points[,2]) * sin(pi * i.t/7)
    #   true.grad.tt[[i.t]] = -10 * pi^2/49 * sin(3 * pi * grid.points[, 1]) * cos(3 * pi * grid.points[,2]) * cos(pi * i.t/7)
    #   true.grad.sxtt[[i.t]] = -30 * pi^3/49 * sin(3 * pi * grid.points[,1]) * sin(3 * pi * grid.points[,2]) * cos(pi * i.t/7)
    #   true.grad.sytt[[i.t]] = 30 * pi^3/49 * sin(3 * pi * grid.points[,1]) * sin(3 * pi * grid.points[,2]) * cos(pi * i.t/7)
    #   true.curv.sxxtt[[i.t]] = 90 * pi^4/49 * sin(3 * pi * grid.points[,1]) * cos(3 * pi * grid.points[,2]) * cos(pi * i.t/7)
    #   true.curv.sxytt[[i.t]] = 90 * pi^4/49 * cos(3 * pi * grid.points[,1]) * sin(3 * pi * grid.points[,2]) * cos(pi * i.t/7)
    #   true.curv.syytt[[i.t]] = 90 * pi^4/49 * sin(3 * pi * grid.points[,1]) * cos(3 * pi * grid.points[,2]) * cos(pi * i.t/7)
    # }

    true = list(true.grad.sx = true.grad.sx,
                true.grad.sy = true.grad.sy,
                true.curv.sxx = true.curv.sxx,
                true.curv.sxy = true.curv.sxy,
                true.curv.syy = true.curv.syy,
                true.grad.t = true.grad.t,
                true.grad.sxt = true.grad.sxt,
                true.grad.syt = true.grad.syt,
                true.curv.sxxt = true.curv.sxxt,
                true.curv.sxyt = true.curv.sxyt,
                true.curv.syyt = true.curv.syyt,
                true.grad.tt = true.grad.tt,
                true.grad.sxtt = true.grad.sxtt,
                true.grad.sytt = true.grad.sytt,
                true.curv.sxxtt = true.curv.sxxtt,
                true.curv.sxytt = true.curv.sxytt,
                true.curv.syytt = true.curv.syytt)

    if(i == 1 & j == 1) print(as.character(Sys.time()))

    MSPE = CP = list()
    for(k in 1:nrep){
      spt.gradients = spt_gradients(model = results.sim[[k]], cov.type = cov.type, grid.points = grid.points, true = true, plots = FALSE)
      MSPE[[k]] = spt.gradients$mspe
      CP[[k]] = 1 - do.call(rbind, lapply(spt.gradients$plot.fn.cp, function(x) x$CP)); colnames(CP) = colnames(MSPE)
    }
    mspe.cp.reps = list(MSPE = MSPE, CP = CP)
    # save(mspe.cp.reps, file = paste0("../data/Simulation 1/ns-", Ns[i], "-nt-", Nt[j], "-mspe-cp-reps.RData"))
    save(mspe.cp.reps, file = paste0("../data/Simulation 2/ns-", Ns[i], "-nt-", Nt[j], "-mspe-cp-reps.RData"))
    cat("Iteration Setting:: Ns:- ", Ns[i], "Nt:- ", Nt[j], "\t", "time::", as.character(Sys.time()), "\n")
  }
}

tau2.c = c()

for(i in 1:length(Ns)){
  for(j in 1:length(Nt)){
    load(paste0("../data/Simulation 2/ns-", Ns[i], "-nt-", Nt[j], "-reps.RData"))
    tau2.c = rbind(tau2.c,
                   c(median(unlist(lapply(results.sim, function(x) x$tau2))), coda::HPDinterval(coda::as.mcmc(unlist(lapply(results.sim, function(x) x$tau2))))))
  }
}
round(tau2.c, 2)

mean.results <- sd.results <- cp.results <- c()
for(i in 1:length(Ns)){
  for(j in 1:length(Nt)){
    load(paste0("../data/Simulation 2/ns-", Ns[i], "-nt-", Nt[j], "-mspe-cp-reps.RData"))
    cp.results <- rbind(cp.results,
                        apply(do.call(rbind,lapply(mspe.cp.reps$CP, function(x) apply(x, 2, mean))), 2, median))
    mean.results <- rbind(mean.results,
                          apply(do.call(rbind,lapply(mspe.cp.reps$MSPE, function(x) apply(x, 2, function(x.r) sqrt(mean(x.r^2))))), 2, median))
    sd.results <- rbind(sd.results,
                        apply(do.call(rbind,lapply(mspe.cp.reps$MSPE, function(x) apply(x, 2, function(x.r) sqrt(mean(x.r^2))))), 2, sd))
  }
}
rownames(mean.results) = rownames(sd.results) = rownames(cp.results) = c(Ns[1] * Nt, Ns[2] * Nt, Ns[3] * Nt)
colnames(cp.results) = colnames(mean.results)


####################
# For LaTeX tables #
####################


xtable(round(mean.results, 2))

sd.cp.combined <- matrix(NA, nrow = 9, ncol = 17)
for(i in 1:9){
  for(j in 1:17){
    sd.cp.combined[i,j] = paste0("(", sprintf("%.2f", sd.results[i,j]), "), (", sprintf("%.1f", cp.results[i,j]), ")")
  }
}
xtable(sd.cp.combined)


