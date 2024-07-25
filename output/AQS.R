setwd("...")
##########################################
# AIR QUALITY DATA FOR CANADA WILD FIRES #
#           (EAST COAST)                 #
##########################################
AQS_east = sapply(list.files("data/Canada Wildfire 2023/"), function(x) read.csv(paste0("data/Canada Wildfire 2023/", x)), simplify = FALSE)
#########################################
# Testing for May, June and Sep. & Oct. #
#########################################
# June 6 -- June 9 window: https://ysph.yale.edu/news-article/canadian-wildfire-smoke-associated-with-increased-asthma-cases-in-nyc/
# Sep 29 -- Oct. 2 window: NY engulfed in smoke
month_date_id = c(paste0("09/", sprintf("%02d", 29:30),"/2023"), paste0("10/", sprintf("%02d", 1:2),"/2023"))
# paste0("06/", sprintf("%02d", 6:9),"/2023")
# c(paste0("09/", sprintf("%02d", 29:30),"/2023"), paste0("10/", sprintf("%02d", 1:2),"/2023"))


AQS_east.month = lapply(AQS_east, function(x){
  id.month = which(x$Date %in% month_date_id)
  x[id.month,]
})

#######################
# Reformatting Data   #
# Sites x Time Format #
#######################
state.site.data = sapply(1:length(AQS_east.month), function(st){
  state.data = AQS_east.month[[st]]
  state.name = state.data$STATE[1]
  site.state.data = sapply(unique(state.data$Site.ID), function(s){
    state.data[which(state.data$Site.ID == s),]
  }, simplify = FALSE)
  site.names = unlist(lapply(site.state.data, function(y) y$Site.Name[1]))
  site.data.final = cbind(State = state.name, Site.Name = site.names, data.frame(do.call(rbind, lapply(site.state.data, function(x){
    pm2.5 = aggregate(x$Daily.Mean.PM2.5.Concentration, list(x$Date), mean)
    pm2.5.final = rep(NA, length(month_date_id))
    pm2.5.final[match(pm2.5[,1], month_date_id)] = pm2.5[,2]
    site.data = c(x$Site.ID[1],
                  x$SITE_LATITUDE[1],
                  x$SITE_LONGITUDE[1], pm2.5.final)
    names(site.data) = c("id", "lat", "long", month_date_id)
    site.data
  }))))
  site.data.final
}, simplify = FALSE)

pm2.5_final = do.call(rbind, state.site.data)
pm2.5_final.noNA = na.exclude(pm2.5_final)
pm2.5_final.noNAsepoct = pm2.5_final.noNA
dim(pm2.5_final.noNA)

########################################
# Make Spatiotemporal Plots :: ggplot2 #
########################################
require(geodata)
require(sp)
require(MBA)
require(metR)
require(sf)
require(ggplot2)
require(cowplot)

usa_shape = st_as_sf(gadm(country = "USA", level = 1, path = "/Users/aritrah/Library/CloudStorage/Dropbox/Desktop-Files/spatiotemporal_gradient/space-time/new/application/canada wildfires/"))
state_shape = subset(usa_shape, NAME_1 %in% unique(pm2.5_final.noNA$State))
save(state_shape, file = "/Users/aritrah/Library/CloudStorage/Dropbox/Desktop-Files/spatiotemporal_gradient/space-time/new/application/states-NE-NMA.RData")
save(pm2.5_final.noNAsepoct, file = "/Users/aritrah/Library/CloudStorage/Dropbox/Desktop-Files/spatiotemporal_gradient/space-time/new/application/pm2.5-sep-oct.RData")
state_shape.sp = as(state_shape, "Spatial")

coords = pm2.5_final.noNA[, c("long", "lat")]

pdf("R/application/canada wildfires/spt_plotsPM-gg-jun69.pdf", width = 12, height = 10)
month.id.loop = 1:4
df.gg = list()
for(i in month.id.loop){
  pm2.5.data = pm2.5_final.noNA[, (i + 5)]

  surf = mba.surf(cbind(coords, pm2.5.data),
                  no.X = 300, no.Y = 300, h = 5, m = 2, extend = TRUE, sp = TRUE)$xyz.est
  proj4string(surf) = proj4string(state_shape.sp)
  surf.tmp <- try(surf[!is.na(over(surf, state_shape.sp))[,1],], silent = TRUE)
  surf <- as.image.SpatialGridDataFrame(surf.tmp)

  gg.grid = expand.grid(surf$x, surf$y)
  colnames(gg.grid) = c("long", "lat")
  df.gg[[i]] = cbind(gg.grid, z = as.vector(surf$z), t = i)
  cat(month_date_id[i], "...", "\t")
}
df.gg.full = data.frame(do.call(rbind, df.gg))
df.gg.full$t = as.factor(df.gg.full$t)
levels(df.gg.full$t) = paste("June ", month.id.loop, ", 2023", sep = "")
# c(paste("Sep. ", 29:30, ", 2023", sep = ""), paste("Oct. ", 1:2, ", 2023", sep = ""))


ggplot() + theme_cowplot(12) +
  geom_raster(df.gg.full, mapping = aes(x = long, y = lat, fill =  z)) +
  labs(x = "Longitude", y = "Latitude", fill = "PM2.5") +
  scale_fill_distiller(palette = "Spectral",
                       label = function(x) sprintf("%.2f", x), na.value = "white") +
  geom_contour2(df.gg.full, mapping = aes(x = long, y = lat, z = z, label = after_stat(level)), size = 0.1, label_size = 2.5) +
  geom_point(data = data.frame(coords),
             aes(x = long, y = lat),
             size = 1.2) +
  facet_wrap(~t, ncol = 2) +
  geom_sf(data = state_shape, fill = "transparent") +
  theme(strip.text.x = element_text(size = 30),
        axis.line = element_line(linewidth = 0.3),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.key.height = unit(4, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.text = element_text(size = 25),
        plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "cm"))
dev.off()
