require(readODS)
# Downloaded from: https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt
stations_data = data.frame(read_ods("R/application/sierra-nevada-precip/ghcnd_stations_CA.ods", sheet = 1))
stations_name = data.frame(read_ods("R/application/sierra-nevada-precip/ghcnd_stations_CA.ods", sheet = 2))[,-c(5:7)]
stations_name[, 2] = apply(stations_name, 1, function(x){
  if(is.na(x[4])) x[4] = " "
  paste(x[2], x[3], x[4], sep = " ")
})
stations_name = stations_name[, -c(3:4)]
colnames(stations_name) = c("state", "name")

start.dt = "2022-01-01"
end.dt = "2022-12-31"
limit = 1000

prcp_monthly.tmp = matrix(NA, ncol = 12, nrow = nrow(stations_data))
for(i in 1:nrow(stations_data)){
  precip.station = try(ncdc(datasetid = "GHCND",
                            stationid = paste0("GHCND:", stations_data[i, "id"]),
                            datatypeid = "PRCP",
                            startdate = start.dt,
                            enddate = end.dt,
                            limit = limit,
                            add_units = TRUE))
  if(class(precip.station) == "try-error"){
    # Handling no data found
    prcp_monthly.tmp[i,] = NA
    cat(stations_data[i, "id"], "Miss/No Service...","\n")
  }else if(nrow(precip.station$data) == 0){
    # Handling no data found
    prcp_monthly.tmp[i,] = NA
    cat(stations_data[i, "id"], "Miss/No Service...","\n")
  }else{
    # Handling Missing Month
    month.ids = unlist(lapply(strsplit(precip.station$data$date, "-"),
                              function(x) x[2]))
    agg.tmp = aggregate(precip.station$data$value,
                        list(month.ids), sum)
    miss.month = match(agg.tmp[,1],sprintf("%02d", 1:12))
    final.agg.tmp = rep(NA, 12)
    final.agg.tmp[miss.month] = agg.tmp[,2]

    cat(stations_data[i, "id"], "Done...","\n")
    prcp_monthly.tmp[i,] = final.agg.tmp
  }
  Sys.sleep(10)
}

precip_finalCA = cbind(stations_data, prcp_monthly.tmp)
precip_finalCA = cbind(stations_name, precip_finalCA)
colnames(precip_finalCA) = c( "state",
                              "name",
                              "id",
                              "lat",
                              "long",
                              "elev.in.mts", month.abb)
head(precip_finalCA)
save(precip_finalCA, file = "R/application/sierra-nevada-precip/precip_finalCA.RData")

dim(precip_finalCA)
precip_finalCA.noNA = na.exclude(precip_finalCA)
dim(precip_finalCA.noNA)
