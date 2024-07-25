setwd("~/Dropbox/Desktop-Files/spatiotemporal_gradient/space-time/new/")
load("application/sierra-nevada-precip/model_precipCA.RData")
##########################
# Wombling Precipitation #
##########################
require(geodata)
require(sp)
require(MBA)
require(metR)
require(sf)
require(ggplot2)
require(cowplot)
require(sptwombling)
require(coda)
require(parallel)

usa_shape = st_as_sf(gadm(country = "USA", level = 1, path = "application/sierra-nevada-precip/"))
ca_shape = subset(usa_shape, NAME_1 == "California")
ca_shape.sp = as(ca_shape, "Spatial")
sierra_nevada.shp = read_sf("application/sierra-nevada-precip/Sierra_Nevada_Conservancy_Subregions/Sierra_Nevada_Conservancy_Subregions.shp")
sierra_nevada.shp = st_transform(sierra_nevada.shp, crs = st_crs(usa_shape))
sierra_nevada.shp.sp = as(sierra_nevada.shp, "Spatial")
plot(sierra_nevada.shp.sp)

sc.north = subset(sierra_nevada.shp.sp, OBJECTID == 4)
sc.south = subset(sierra_nevada.shp.sp, OBJECTID == 3)
sierra = data.frame(rbind(sc.north@polygons[[1]]@Polygons[[1]]@coords[7600:6900,], sc.south@polygons[[1]]@Polygons[[2]]@coords[6100:5000,]))

plot(ca_shape.sp)
lines(sierra_nevada.shp.sp, col = "darkgreen")
lines(sierra, type = "l", col = "darkred")
legend("bottomleft", col = c("black", "darkgreen", "darkred"), legend = c("CA Boundary", "Sierra Boundary" , "Wombling Surface"), lty = c(1, 1, 1))

curves.sierra = list()

curves.sierra[[1]] = curves.sierra[[2]] = curves.sierra[[3]] = curves.sierra[[4]] = sierra

surf.womb_c = do.call(rbind, sapply(1:4, function(x) cbind(curves.sierra[[x]], x), simplify = FALSE))

# partitioned curves
surf.womb = partition_curve(curve.list = curves.sierra); rownames(surf.womb) = NULL

open3d()
bg3d(color = "white")
plot3d(x = surf.womb[,1], y = surf.womb[,2], z = surf.womb[,3], lwd = 2,
       xlab = "x", ylab  = "y", zlab = "t", ann = FALSE, axes = FALSE)
box3d()
grid3d(side = c("x", "y", "z"))
rglwidget()

surf.trid = surf_triangulate(curves_part = surf.womb); rglwidget()

wombling.df = data.frame(do.call(rbind, surf.trid$wombling.df)); nrow(wombling.df)

sum(wombling.df$area)

WM.obj = sptwombling(model = model, wombling.df = wombling.df)
save(WM.obj, file = "wm_prcp.Rdata")

WM.obj$time["elapsed"]/3600 # >36hrs

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
})), 2)/sum(wombling.df$area)))

#############################
# Wombling measures by Time #
#############################
WM.bytime = lapply(WM.obj$WM, function(x){
  round(rbind(t12 = apply(x[1:18,-4], 2, sum),
              t23 = apply(x[19:36,-4], 2, sum),
              t34 = apply(x[37:54,-4], 2, sum)), 2)
})
WM.bytime = lapply(WM.bytime, function(x){
  sig = apply(x, 1, function(y){
    if(y[2]>0) return(1)
    else if(y[3]<0) return(-1)
    else return(0)
  })
  data.frame(cbind(x, sig = sig))
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
  rgl.snapshot(paste0(names(WM.obj$WM)[i],"_prcp", fmt = ".jpg"))
}

