require(parallel)

load("R/application/eeg/con_eeg.RData")

source("R/collapsed-mh-spt.R")
source("R/spsample.R")
source('R/plot_mcmc.R')
source('R/cov_fn/matern5by2.R')

coords = con_eeg[, c("x", "y")]
y = as.vector(t(as.matrix(con_eeg[, 3:8])))
niter = 5e3; nburn = 0.5 * niter; report = 1e2
t = 1:6

model = hlmBayes_mh.spt(coords = coords, t = t, y = y, cov.type = "matern2",
                        niter = niter, nburn = nburn, report = report)
phis = model$phis
phit = model$phit
sig2 = model$sig2
tau2 = model$tau2

z.beta.sample = spsample(y = y, coords = coords, t = t,
                         phis = phis, phit = phit, sig2 = sig2, tau2 = tau2,
                         cov.type = "matern2", silent = FALSE)
model$z = z.beta.sample$z
model$beta = z.beta.sample$beta

thin_id = seq(1, (niter - nburn), by = 20)

model.summary = rbind(c(median(sig2[thin_id]), coda::HPDinterval(coda::as.mcmc(sig2[thin_id]))),
                      c(median(tau2[thin_id]), coda::HPDinterval(coda::as.mcmc(tau2[thin_id]))),
                      c(median(phis[thin_id]), coda::HPDinterval(coda::as.mcmc(phis[thin_id]))),
                      c(median(phit[thin_id]), coda::HPDinterval(coda::as.mcmc(phit[thin_id]))),
                      c(median(model$beta[thin_id]), coda::HPDinterval(coda::as.mcmc(model$beta[thin_id]))))
rownames(model.summary) = c("sig2", "tau2", "phis", "phit", "(Intercept)")
colnames(model.summary) = c("Estimate", "lower.hpd", "upper.hpd")
xtable::xtable(round(model.summary, 4))

z.est = t(apply(model$z[thin_id,] - model$beta[thin_id], 2, function(x){
  c(median(x), coda::HPDinterval(coda::as.mcmc(x)))
}))
z.est


par(mfrow = c(5,3))
par(mar = rep(2.2, 4))
plot_mcmc(samples = model$beta[thin_id], cnames = "beta")
plot_mcmc(samples = model$z[thin_id, 100], cnames = "z")
plot_mcmc(samples = model$sig2[thin_id], cnames = "sigma2")
plot_mcmc(samples = model$tau2[thin_id], cnames = "tau2")
plot_mcmc(samples = model$phis[thin_id], cnames = "phis")
plot_mcmc(samples = model$phit[thin_id], cnames = "phit")
save(model, file = "R/application/eeg/con_eeg_model.RData")

