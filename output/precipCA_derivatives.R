load("R/application/sierra-nevada-precip/model_precipCA.RData")
source("R/spt_gradients.R")
require(parallel)
require(geodata)
require(sp)
require(sf)

usa_shape = st_as_sf(gadm(country = "USA", level = 1, path = "R/application/canada wildfires/"))
ca_shape = subset(usa_shape, NAME_1 == "California")
ca_shape.sp = as(ca_shape, "Spatial")
#############################
# Creating grid             #
# for derivative estimation #
#############################
b.box.shape = state_shape.sp@bbox
x.seq = seq(b.box.shape["x", "min"], b.box.shape["x", "max"], length.out = 30)
y.seq = seq(b.box.shape["y", "min"], b.box.shape["y", "max"], length.out = 30)
grid = st_as_sf(expand.grid(longitude = x.seq, latitude = y.seq),
                coords = c("longitude", "latitude"),
                crs = st_crs(ca_shape))

grid.shape = st_intersection(grid, ca_shape)
grid.shape.points = st_coordinates(grid.shape); dim(grid.shape.points)
grid.shape.points = data.frame(grid.shape.points)
plot(grid.shape.points)
spt.gradients_precipCA = spt_gradients(model = model,
                                       cov.type = "matern2",
                                       grid.points = grid.shape.points,
                                       true = NULL, plots = FALSE)
save(spt.gradients_precipCA, file = "R/application/sierra-nevada-precip/precipCA_gradients.RData")
do.call(rbind, lapply(spt.gradients_precipCA, function(x) unlist(lapply(x, function(y) sum(y$signif)))))[,-1]

gg.arrange = list()
for(j in 1:17){
  df.gg = col.y = list()
  for(i in 1:4){
    precip.data.derivs = spt.gradients_precipCA[[i]][[j + 2]][,1]
    col.y[[i]] = sapply(spt.gradients_precipCA[[i]][[j + 2]]$signif, function(x){
      if(x == 0) return("grey")
      else if(x == 1) return("green")
      else return("cyan")
    })

    surf = mba.surf(cbind(grid.shape.points, precip.data.derivs),
                    no.X = 300, no.Y = 300, h = 5, m = 2, extend = TRUE, sp = TRUE)$xyz.est
    proj4string(surf) = proj4string(ca_shape.sp)
    surf.tmp <- try(surf[!is.na(over(surf, ca_shape.sp))[,1],], silent = TRUE)
    surf <- as.image.SpatialGridDataFrame(surf.tmp)

    gg.grid = expand.grid(surf$x, surf$y)
    colnames(gg.grid) = c("long", "lat")
    df.gg[[i]] = cbind(gg.grid, z = as.vector(surf$z), t = i)
    cat(month.abb[i + 8], "...", "\t")
  }
  df.gg.full = data.frame(do.call(rbind, df.gg))
  df.gg.full$t = as.factor(df.gg.full$t)
  levels(df.gg.full$t) = paste(month.abb[9:12], "., 2022", sep = "")

  if(j == 1){
    gg.arrange[[j]] = ggplot() + theme_cowplot(12) +
      geom_raster(df.gg.full, mapping = aes(x = long, y = lat, fill =  z)) +
      labs(x = "Longitude", y = "Latitude", fill = "") +
      scale_fill_distiller(palette = "RdBu",
                           label = function(x) sprintf("%.2f", x), na.value = "white", direction  = 1) +
      geom_contour2(df.gg.full, mapping = aes(x = long, y = lat, z = z, label = after_stat(level)), size = 0.1, label_size = 2.5) +
      geom_point(data = data.frame(grid.shape.points),
                 aes(x = X, y = Y),
                 size = 1,
                 fill = unlist(col.y),  stroke = 0.5, pch = 21) +
      facet_wrap(~t, ncol = 4) +
      geom_sf(data = ca_shape, fill = "transparent") +
      geom_sf(data = sierra_nevada.shp, fill = "transparent",
              color = "black", lwd = 0.8) +
      theme(strip.text.x = element_text(size = 20),
            axis.line = element_line(linewidth = 0.3),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.key.height = unit(1.5, 'cm'),
            legend.key.width = unit(0.6, 'cm'),
            legend.text = element_text(size = 20),
            plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "cm"))
  }else{
    gg.arrange[[j]] = ggplot() + theme_cowplot(12) +
      geom_raster(df.gg.full, mapping = aes(x = long, y = lat, fill =  z)) +
      labs(x = "Longitude", y = "Latitude", fill = "") +
      scale_fill_distiller(palette = "RdBu",
                           label = function(x) sprintf("%.2f", x), na.value = "white", direction  = 1) +
      geom_contour2(df.gg.full, mapping = aes(x = long, y = lat, z = z, label = after_stat(level)), size = 0.1, label_size = 2.5) +
      geom_point(data = data.frame(grid.shape.points),
                 aes(x = X, y = Y),
                 size = 1,
                 fill = unlist(col.y),  stroke = 0.5, pch = 21) +
      facet_wrap(~t, ncol = 4) +
      geom_sf(data = ca_shape, fill = "transparent") +
      geom_sf(data = sierra_nevada.shp, fill = "transparent",
              color = "black", lwd = 0.8) +
      theme(strip.text.x = element_blank(),
            axis.line = element_line(linewidth = 0.3),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.key.height = unit(1.5, 'cm'),
            legend.key.width = unit(0.6, 'cm'),
            legend.text = element_text(size = 20),
            plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "cm"))
  }

}


pdf("R/application/sierra-nevada-precip/full_grad_plot.pdf", width = 12, height = 4)
gg.arrange
dev.off()

