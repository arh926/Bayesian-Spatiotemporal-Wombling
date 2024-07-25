load("R/application/eeg/alc_eeg_model.RData")
source("R/spt_gradients.R")
require(parallel)
require(geodata)
require(sp)
require(sf)
require(ggplot2)
require(metR)

b.box.shape = rbind(range(model$coords$x), range(model$coords$y)); rownames(b.box.shape) = c("x", "y"); colnames(b.box.shape) = c("min", "max")
x.seq = seq(b.box.shape["x", "min"] * 2, b.box.shape["x", "max"] * 2, length.out = 30)
y.seq = seq(b.box.shape["y", "min"] * 2, b.box.shape["y", "max"] * 2, length.out = 30)
grid = expand.grid(x = x.seq, y = y.seq)

grid.shape = data.frame(grid[sqrt(grid$x^2 + grid$y^2) < 0.7,]);  dim(grid.shape)
plot(grid.shape); points(model$coords, pch = 16)

# spt.gradients_alc = spt_gradients(model = model,
#                                   cov.type = "matern2",
#                                   grid.points = grid.shape,
#                                   true = NULL, plots = FALSE)
# save(spt.gradients_alc, file = "R/application/eeg/alc_gradients.RData")
load("R/application/eeg/alc_gradients.RData")
do.call(rbind, lapply(spt.gradients_alc, function(x) unlist(lapply(x, function(y) sum(y$signif)))))[,-1]

latency = seq(0, 1000, length.out = 256)
t.pts = round(seq(2, 256, length.out = 20))

gg.arrange = list()
for(j in 1:17){
  df.topo.gg = col.y = list()
  for(i in 1:6){
    derivs = spt.gradients_alc[[i]][[j + 2]][,1]
    col.y[[i]] = sapply(spt.gradients_alc[[i]][[j + 2]]$signif, function(x){
      if(x == 0) return("grey")
      else if(x == 1) return("green")
      else return("cyan")
    })

    surf = mba.surf(cbind(grid.shape, derivs),
                    no.X = 300, no.Y = 300, h = 5, m = 2, extend = TRUE)$xyz.est

    gg.grid = expand.grid(x = surf$x, y = surf$y)
    incircle.id = apply(gg.grid, 1, function(x) sqrt(x[1]^2 + x[2]^2) < 0.7)

    df.topo.gg[[i]] = data.frame(cbind(gg.grid[incircle.id,], z = as.vector(surf$z)[incircle.id], t = round(latency[i], 2)))
  }
  df.topo.gg.full = data.frame(do.call(rbind, df.topo.gg))
  df.topo.gg.full$t = as.factor(df.topo.gg.full$t)
  levels(df.topo.gg.full$t) = paste0(round(latency[t.pts][4:9]), "ms")

  if(j == 1){
    gg.arrange[[j]] = ggplot() + theme_topo() +
      geom_raster(df.topo.gg.full, mapping = aes(x = x, y = y, fill =  z)) +
      labs(fill = "") +
      scale_fill_gradientn(colours = jet.colors(10),
                           label = function(x) sprintf("%.1f", x),
                           guide = "colourbar") +
      geom_contour2(df.topo.gg.full,
                    mapping = aes(x = x, y = y, z = z, label = round(after_stat(level), 2)),
                    size = 0.1, label_size = 2.5) +
      geom_point(data = grid.shape,
                 aes(x = x, y = y),
                 size = 1,
                 fill = unlist(col.y),  stroke = 0.5, pch = 21) +
      facet_wrap(~t, ncol = 6) +
      geom_path(data = maskRing,
                aes(x, y, z = NULL, fill =NULL),
                colour = "white",
                size = 6) +
      geom_path(data = headShape,
                aes(x, y, z = NULL, fill = NULL),
                size = 1.5) +
      geom_path(data = nose,
                aes(x, y, z = NULL, fill = NULL),
                size = 1.5) +
      theme(strip.text.x = element_text(size = 25),
            legend.key.height = unit(0.7, 'cm'),
            legend.key.width = unit(0.3, 'cm'),
            legend.text = element_text(size = 15),
            plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "cm")) +
      coord_equal()
  }else{
    gg.arrange[[j]] = ggplot() + theme_topo() +
      geom_raster(df.topo.gg.full, mapping = aes(x = x, y = y, fill =  z)) +
      labs(fill = "") +
      scale_fill_gradientn(colours = jet.colors(10),
                           label = function(x) sprintf("%.1f", x),
                           guide = "colourbar") +
      geom_contour2(df.topo.gg.full,
                    mapping = aes(x = x, y = y, z = z, label = round(after_stat(level), 2)),
                    size = 0.1, label_size = 2.5) +
      geom_point(data = grid.shape,
                 aes(x = x, y = y),
                 size = 1,
                 fill = unlist(col.y),  stroke = 0.5, pch = 21) +
      facet_wrap(~t, ncol = 6) +
      geom_path(data = maskRing,
                aes(x, y, z = NULL, fill =NULL),
                colour = "white",
                size = 6) +
      geom_path(data = headShape,
                aes(x, y, z = NULL, fill = NULL),
                size = 1.5) +
      geom_path(data = nose,
                aes(x, y, z = NULL, fill = NULL),
                size = 1.5) +
      theme(strip.text.x = element_blank(),
            legend.key.height = unit(0.7, 'cm'),
            legend.key.width = unit(0.3, 'cm'),
            legend.text = element_text(size = 15),
            plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "cm")) +
      coord_equal()
  }
}


pdf("R/application/eeg/full_grad_plot_eeg_alc.pdf", width = 11, height = 5)
gg.arrange
dev.off()
