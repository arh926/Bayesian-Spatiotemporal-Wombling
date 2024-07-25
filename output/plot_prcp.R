##########################
# Plotting Precipitation #
##########################
require(geodata)
require(sp)
require(MBA)
require(metR)
require(sf)
require(ggplot2)
require(cowplot)

usa_shape = st_as_sf(gadm(country = "USA", level = 1, path = "R/application/sierra-nevada-precip/"))
ca_shape = subset(usa_shape, NAME_1 == "California")
ca_shape.sp = as(ca_shape, "Spatial")
sierra_nevada.shp = read_sf("R/application/sierra-nevada-precip/Sierra_Nevada_Conservancy_Subregions/Sierra_Nevada_Conservancy_Subregions.shp")


coords = precip_finalCA.noNA[, c("long", "lat")]

pdf("R/application/sierra-nevada-precip/spt_plotsPRCP-sep-dec-100h.pdf", width = 12, height = 4)
df.gg = list()
for(i in 9:12){
  precip.data = precip_finalCA.noNA[, (i + 6)]/100 # in cms

  surf = mba.surf(cbind(coords, precip.data),
                  no.X = 300, no.Y = 300, h = 5, m = 2, extend = TRUE, sp = TRUE)$xyz.est
  proj4string(surf) = proj4string(ca_shape.sp)
  surf.tmp <- try(surf[!is.na(over(surf, ca_shape.sp))[,1],], silent = TRUE)
  surf <- as.image.SpatialGridDataFrame(surf.tmp)

  gg.grid = expand.grid(surf$x, surf$y)
  colnames(gg.grid) = c("long", "lat")
  df.gg[[i]] = cbind(gg.grid, z = as.vector(surf$z), t = i)
  cat(month.abb[i], "...", "\t")
}
df.gg.full = data.frame(do.call(rbind, df.gg))
df.gg.full$t = as.factor(df.gg.full$t)
levels(df.gg.full$t) = paste(month.abb[9:12], "., 2022", sep = "")

ggplot() + theme_cowplot(12) +
  geom_raster(df.gg.full, mapping = aes(x = long, y = lat, fill =  z)) +
  labs(x = "Longitude", y = "Latitude", fill = "") +
  scale_fill_distiller(palette = "RdBu",
                       label = function(x) sprintf("%.2f", x), na.value = "white", direction  = 1) +
  geom_contour2(df.gg.full, mapping = aes(x = long, y = lat, z = z, label = after_stat(level)), size = 0.1, label_size = 2.5) +
  geom_point(data = data.frame(coords),
             aes(x = long, y = lat),
             size = 1) +
  facet_wrap(~t, ncol = 4) +
  geom_sf(data = ca_shape, fill = "transparent") +
  geom_sf(data = sierra_nevada.shp, fill = "transparent",
          color = "white", lwd = 0.8) +
  theme(strip.text.x = element_text(size = 20),
        axis.line = element_line(linewidth = 0.3),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.key.height = unit(1.5, 'cm'),
        legend.key.width = unit(0.6, 'cm'),
        legend.text = element_text(size = 20),
        plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "cm"))
dev.off()


