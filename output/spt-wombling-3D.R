setwd("...") # set a working directory of your choice
source("vcrossprod.R")
# Load the models from spt_modelfit_vignette
load("mcmc-pattern-hlm-1.RData")
set.seed(1234)
Ns = 100; Nt = 9; N = Ns*Nt
t = seq(1,Nt, length.out=Nt)
coords = matrix(runif(2*Ns),nc=2)
tau = 1

# plot(coords, xlab="coords.x",ylab="coords.y")
# co-ordinates are observed for each time point
sim.pattern = array(NA, c(Ns,Nt))
## create synthetic y or use package function
for(j in 1:9){
  sim.pattern[,j] = rnorm(Ns,
                          mean = 10 * (sin(3 * pi * coords[,1]) + cos(3 * pi * coords[,2]) * cos(t[j] * pi/7)),
                          sd = tau)
}
y = c()
for(i in 1:100){
  y = c(y,sim.pattern[i,1:9])
}
mean(y); var(y); range(y)

require(raster)
require(sp)
require(spWombling)
require(sptwombling)
wb.1 = wb1 =  wb.2 = wb2 = wb.0 = wb0 = list()
id.lines = id.levels = matrix(NA, nrow = 9, ncol = 2)

length.p = 10
tr.points = 10
###########################
# Selecting Closed Curves #
###########################
id.lines[1,] = c(1, 2)
id.lines[2,] = c(2, 2)
id.lines[3,] = c(1, 1)
id.lines[4,] = c(1, 1)
id.lines[5,] = c(1, 2)
id.lines[6,] = c(1, 2)
id.lines[7,] = c(1, 2)
id.lines[8,] = c(1, 2)
id.lines[9,] = c(1, 2)

id.levels[1, ] = c("-16", "16")
id.levels[2, ] = c("-14", "14")
id.levels[3, ] = c("-12", "10")
id.levels[4, ] = c("-10", "10")
id.levels[5, ] = c("-14", "14")
id.levels[6, ] = c("-16", "16")
id.levels[7, ] = c("-18", "18")
id.levels[8, ] = c("-18", "16")
id.levels[9, ] = c("-14", "14")

########################
# Selecting Open Curve #
########################
id.lines.0 = rep(4, 9)
id.lines.0[4] = 3

#######################
# Plotting the Curves #
#######################

par(mfrow = c(3, 3))
for(j in 1:Nt){
  par(mar=rep(2,4))
  raster.obj =  sp_plot(11,"PiYG",
                        cbind(coords,sim.pattern[,j]),
                        zlim = c(min(y) - 0.5, max(y) + 0.5),
                        legend = FALSE,
                        contour.plot = TRUE,
                        points.plot = TRUE,
                        raster.surf = TRUE,
                        extend = TRUE)
  x = raster::rasterToContour(raster.obj, nlevel = 20)
  x.levels <- as.numeric(as.character(x$level))

  wb.1[[j]] = subset(x, level == id.levels[j,1])
  wb.2[[j]] = subset(x, level == id.levels[j,2])
  wb.0[[j]] = subset(x, level == 0)

  lines(wb.1[[j]]@lines[[1]]@Lines[[id.lines[j,1]]]@coords, lwd = 5, col = "yellow")
  lines(wb.2[[j]]@lines[[1]]@Lines[[id.lines[j,2]]]@coords, lwd = 5, col = "yellow")
  lines(wb.0[[j]]@lines[[1]]@Lines[[id.lines.0[j]]]@coords, lwd = 5, col = "red")

  wb1[[j]] = wb.1[[j]]@lines[[1]]@Lines[[id.lines[j,1]]]@coords
  wb2[[j]] = wb.2[[j]]@lines[[1]]@Lines[[id.lines[j,2]]]@coords
  wb0[[j]] = wb.0[[j]]@lines[[1]]@Lines[[id.lines.0[j]]]@coords

}


########################
# Creating a Partition #
########################
surf.womb.1 = partition_curve(curve.list = wb1)
surf.womb.2 = partition_curve(curve.list = wb2)
surf.womb.0 = partition_curve(curve.list = wb0)

wb.C = rbind(surf.womb.1, surf.womb.2, surf.womb.0)
require(rgl)
open3d()
plot3d(x = wb.C[,1], y = wb.C[,2], z = wb.C[,3], lwd = 2,
       xlim = c(0,1), ylim = c(0,1), zlim = c(0.5, 9.5),
       xlab = "x", ylab  = "y", zlab = "t")
rglwidget()

###############################
# Generating Triangular Plane #
###############################
# surfrace A
surf.trid.1 = surf_triangulate(curves_part = surf.womb.1) #; rglwidget()
# surface B
surf.trid.2 = surf_triangulate(curves_part = surf.womb.2) #; rglwidget()
# surface C
surf.trid.0 = surf_triangulate(curves_part = surf.womb.0) #; rglwidget()

wombling.df1 = data.frame(do.call(rbind, surf.trid.1$wombling.df)); nrow(wombling.df1)
wombling.df2 = data.frame(do.call(rbind, surf.trid.2$wombling.df)); nrow(wombling.df2)
wombling.df0 = data.frame(do.call(rbind, surf.trid.0$wombling.df)); nrow(wombling.df0)

sum(wombling.df1$area)
sum(wombling.df2$area)
sum(wombling.df0$area)

###################################
# Spatiotemporal Surface Wombling #
###################################
WM.obj.1 = sptwombling(model = results[[2]], wombling.df = wombling.df1) # takes 28hrs approx.
WM.obj.2 = sptwombling(model = results[[2]], wombling.df = wombling.df2) # takes 28hrs approx.
WM.obj.0 = sptwombling(model = results[[2]], wombling.df = wombling.df0) # takes 28hrs approx.
