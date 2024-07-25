setwd("~/Dropbox/Desktop-Files/spatiotemporal_gradient/space-time/new/application/")
load("canada wildfires/model_sepoct.RData")
load("states-NE-NMA_shape.RData")
require(raster)
require(sf)
require(sp)
require(sptwombling)
require(spWombling)
require(rgl)

state_shape.sp = as(state_shape, "Spatial")

levels.curves = c(14, 20, 25, 12)
id.curves = c(1, 1, 1, 17)
curves.pm = list()

coords = pm2.5_final.noNAsepoct[, c("long", "lat")]
par(mfrow = c(2, 2))
par(mar = rep(2, 4))
for(i in 1:4){
  y = pm2.5_final.noNAsepoct[, (i + 5)]
  raster.obj =  sp_plot(11,"Spectral",
                        cbind(coords, y),
                        legend = FALSE,
                        contour.plot = TRUE,
                        shape = state_shape.sp,
                        points.plot = TRUE,
                        raster.surf = TRUE,
                        extend = TRUE)
  x = raster::rasterToContour(raster.obj, nlevel = 10)
  x.levels <- as.numeric(as.character(x$level))
  curves.pm.subset = subset(x, level == levels.curves[i])
  curves.pm[[i]] = curves.pm.subset@lines[[1]]@Lines[[id.curves[i]]]@coords

  lines(curves.pm[[i]], lwd = 3, col = "black")
}

curves.pm[[3]] = curves.pm[[3]][rev(1:nrow(curves.pm[[3]])),]
# original curves
surf.womb_c = do.call(rbind, sapply(1:4, function(x) cbind(curves.pm[[x]], x), simplify = TRUE))
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
# rglwidget()

wombling.df = data.frame(do.call(rbind, surf.trid$wombling.df)); nrow(wombling.df)

# WM.obj = sptwombling(model = model, wombling.df = wombling.df)
# save(WM.obj, file = "R/application/canada wildfires/wm_sepoct.Rdata")
load("canada wildfires/wm_sepoct.Rdata")

WM.obj$time["elapsed"]/3600 # 16.10hrs

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
xtable::xtable(t(round(do.call(rbind, lapply(WM.obj$WM, function(x){
  apply(x[,-4], 2, sum)
}))/sum(wombling.df$area), 2)))

#############################
# Wombling measures by Time #
#############################
WM.bytime = lapply(WM.obj$WM, function(x){
  round(rbind(t12 = apply(x[1:18,-4], 2, sum),
              t23 = apply(x[19:36,-4], 2, sum),
              t34 = apply(x[37:54,-4], 2, sum)), 2)
})
WM.bytime

# Run Once:: usr.mat3d = par3d()$userMatrix
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
  rgl.snapshot(paste0(names(WM.obj$WM)[i],"_so", fmt = ".jpg"))
}
