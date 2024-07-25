load("~/Dropbox/Desktop-Files/spatiotemporal_gradient/space-time/new/application/eeg/con_eeg.RData")
load("~/Dropbox/Desktop-Files/spatiotemporal_gradient/space-time/new/application/eeg/con_eeg_model.RData")
require(raster)
require(sp)
require(MBA)
require(spWombling)
require(sptwombling)
require(rgl)

coords = con_eeg[, c("y", "x")]
levels.curves = c(-0.5, 0.5, 5, 4, 4, 3)
id.curves = c(1, 1, 1, 1, 1, 1)
curves.pm = list()

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

par(mfrow = c(2, 3))
par(mar = rep(3, 4))
for(i in 1:6){
  y = con_eeg[, (i + 2)]

  surf = mba.surf(cbind(coords, erp = y),
                  no.X = 300, no.Y = 300, h = 5, m = 2, extend = TRUE,
                  b.box = cbind(range(coords$x)*2, range(coords$y)*2))$xyz.est
  grid.surf = expand.grid(x = surf$x, y = surf$y)
  incircle.id = matrix(apply(grid.surf, 1, function(x) sqrt(x[1]^2 + x[2]^2) < 0.7),
                       nrow = 300, ncol = 300)
  surf$z = sapply(1:300, function(x.1){
    sapply(1:300, function(x.2){
      if(incircle.id[x.1,x.2]) surf$z[x.1, x.2] = surf$z[x.1, x.2] else surf$z[x.1, x.2] = NA
    })
  })

  image(surf, xaxs = "i", yaxs = "i", col = jet.colors(100),
        axes = TRUE)
  contour(surf, add = TRUE, lwd = 0.1)
  points(coords[,c("x", "y")], pch = 16, cex = 0.5)
  lines(headShape); lines(nose)

  raster.obj =  raster(surf)
  x = raster::rasterToContour(raster.obj, nlevel = 10)
  x.levels <- as.numeric(as.character(x$level))
  curves.pm.subset = subset(x, level == levels.curves[i])
  curves.pm[[i]] = curves.pm.subset@lines[[1]]@Lines[[id.curves[i]]]@coords

  lines(curves.pm[[i]], lwd = 3, col = "black")
}
# anticlockwise orientation
curves.pm[[1]] = curves.pm[[1]][rev(1:nrow(curves.pm[[1]])),]
curves.pm[[4]] = curves.pm[[4]][rev(1:nrow(curves.pm[[4]])),]

# original curves
surf.womb_c = do.call(rbind, sapply(1:6, function(x) cbind(curves.pm[[x]], x), simplify = FALSE))
# partitioned curves
surf.womb = partition_curve(curve.list = curves.pm)

open3d()
bg3d(color = "white")
plot3d(x = surf.womb[,1], y = surf.womb[,2], z = surf.womb[,3], lwd = 2,
       xlab = "x", ylab  = "y", zlab = "t", ann = FALSE, axes = FALSE)
box3d()
grid3d(side = c("x", "y", "z"))
# rglwidget()

surf.trid = surf_triangulate(curves_part = surf.womb)
# ; rglwidget()
# Run Once:: usr.mat3d = par3d()$userMatrix

wombling.df = data.frame(do.call(rbind, surf.trid$wombling.df)); nrow(wombling.df)

# WM.obj = sptwombling(model = model, wombling.df = wombling.df)
# save(WM.obj, file = "~/Dropbox/Desktop-Files/spatiotemporal_gradient/space-time/new/application/eeg/wm_alc.RData")

load("~/Dropbox/Desktop-Files/spatiotemporal_gradient/space-time/new/application/eeg/wm_con.RData")
WM.obj$time["elapsed"]/3600

WM.obj$WM = lapply(WM.obj$WM, function(x){
  x$signif = sapply(x$signif, function(y){
    if(y == 1) return("green")
    else if(y == -1) return("cyan")
    else return("grey")
  })
  x
})
############################
# Overall Wombling Measure #
############################
round(do.call(rbind, lapply(WM.obj$WM, function(x){
  apply(x[,-4], 2, sum)
}))/sum(wombling.df$area) * 1e2, 2)

#############################
# Wombling measures by Time #
#############################
WM.bytime = lapply(WM.obj$WM, function(x){
  round(rbind(t12 = apply(x[1:18,-4], 2, sum),
              t23 = apply(x[19:36,-4], 2, sum),
              t34 = apply(x[37:54,-4], 2, sum),
              t45 = apply(x[55:72,-4], 2, sum),
              t56 = apply(x[73:90,-4], 2, sum)), 6)
})
WM.bytime

for(i in 1:length(WM.obj$WM)){
  open3d()
  bg3d(color = "white")
  plot3d(x = surf.womb_c[,1], y = surf.womb_c[,2], z = surf.womb_c[,3], lwd = 2,
         xlab = "x", ylab  = "y", zlab = "t", ann = FALSE, axes = FALSE)
  box3d()
  grid3d(side = c("x", "y", "z"))
  # rglwidget()
  plot_surf_tri_sig(curves_part = surf.womb, wm.signif = WM.obj$WM[[i]])
  view3d(userMatrix = usr.mat3d) 
  rgl.snapshot(paste0(names(WM.obj$WM)[i],"_con", fmt = ".jpg"))
}

