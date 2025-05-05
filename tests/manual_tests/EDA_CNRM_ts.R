## scoping the problem with CNRM future time series. 


library(climr)
library(dplyr)

# Get lat, long, and elevation for 7 locations in BC
test_pts<-data.frame(id = seq(1,6,by=1),
                     lon = c(-120.1879,-120.4258,-121.9251,-120.3030,-127.5062,-127.6785),
                     lat = c(55, 57.4644, 59.9900, 55.2420, 54.0191, 54.1638),
                     elev = c(441.9092,901.2709,461.7851,926.7590,1098.2932,1022.2858)
)

ds_out <- downscale(
  xyz = test_pts,
  which_refmap = "refmap_climr",
  gcm_hist_years = 1960:2014,
  gcm_ssp_years = 2015:2100,
  ssps = c("ssp245"),
  max_run = 3,
  gcms = list_gcms()[3:4],
  db_option = "local",
  vars = c("Tmax_sm", "PPT_sm"))

ds_out2<-ds_out[!is.na(GCM),]

par(mfrow=c(3,2))
for(i in 1:3){
  x <- ds_out2[GCM=="CNRM-ESM2-1" & id == i & RUN == "r1i1p1f2", PERIOD]
  y <- ds_out2[GCM=="CNRM-ESM2-1" & id == i & RUN == "r1i1p1f2", Tmax_sm]
  plot(x,y, type="l", main="CNRM-ESM2-1")
  
  x <- ds_out2[GCM=="CanESM5" & id == i & RUN == "r2i1p1f1", PERIOD]
  y <- ds_out2[GCM=="CanESM5" & id == i & RUN == "r2i1p1f1", Tmax_sm]
  plot(x,y, type="l", main="CanESM5")
}

par(mfrow=c(2,2))
for(i in unique(ds_out2[GCM=="CNRM-ESM2-1", RUN])){
  x <- ds_out2[GCM=="CNRM-ESM2-1" & id == 1 & RUN == i, PERIOD]
  y <- ds_out2[GCM=="CNRM-ESM2-1" & id == 1 & RUN == i, Tmax_sm]
  plot(x,y, type="l", main=i)
  
}

ds_out <- downscale(
  xyz = test_pts[1,],
  gcm_hist_years = 1960:2014,
  gcm_ssp_years = 2015:2100,
  ssps = c("ssp245"),
  max_run = 1,
  gcms = list_gcms(),
  vars = c("Tmax_sm", "PPT_sm"))

par(mfrow=c(3,5), mar=c(2,2,2,0.1), mgp=c(3, 0.25, 0), tck=-0.01)
for(gcm in list_gcms()){
  x <- ds_out[GCM==gcm & RUN != "ensembleMean", PERIOD]
  y <- ds_out[GCM==gcm & RUN != "ensembleMean", Tmax_sm]
  plot(x,y, type="l", main=gcm)
}

ds_out <- downscale(
  xyz = test_pts[1,],
  which_refmap = "auto",
  gcm_hist_years = 1960:2014,
  gcm_ssp_years = 2015:2100,
  ssps = list_ssps()[-4],
  max_run = 3,
  gcms = list_gcms()[4],
  vars = c("Tmax_sm", "PPT_sm"))

par(mfrow=c(3,3), mar=c(2,2,2,0.1), mgp=c(3, 0.25, 0), tck=-0.01)
for(ssp in list_ssps()[-4]){
  runs <- unique(ds_out[RUN != "ensembleMean" & SSP==ssp, RUN])
  for(run in runs){
    x <- ds_out[RUN != "ensembleMean" & SSP==ssp & RUN==run, PERIOD]
    y <- ds_out[RUN != "ensembleMean" & SSP==ssp & RUN==run, Tmax_sm]
    plot(x,y, type="l", main=paste(ssp, run))
  }
}

ds_out <- downscale(
  xyz = test_pts[1,],
  gcm_hist_years = 1960:2014,
  gcm_ssp_years = 2015:2100,
  ssps = list_ssps()[2],
  max_run = 1,
  gcms = list_gcms()[4],
  db_option = "local",
  vars = list_vars(set="Seasonal"))

par(mfrow=c(4,3), mar=c(2,2,2,0.1), mgp=c(3, 0.25, 0), tck=-0.01)
vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_"))
ssp = list_ssps()[2]
run <- unique(ds_out[RUN != "ensembleMean" & SSP==ssp, RUN])
for(var in vars){
    x <- ds_out[RUN != "ensembleMean" & RUN==run, PERIOD]
    y <- ds_out[RUN != "ensembleMean" & RUN==run, get(var)]
    plot(x,y, type="l", main=paste(var))
}

ds_out <- downscale(
  xyz = test_pts[1,],
  which_refmap = "auto",
  gcm_periods = list_gcm_periods(),
  ssps = list_ssps()[2],
  max_run = 3,
  gcms = list_gcms()[4],
  vars = c("Tmax_sm", "PPT_sm", "Tmin_sm"))

par(mfrow=c(3,3), mar=c(2,2,2,0.1), mgp=c(3, 0.25, 0), tck=-0.01)
  for(run in runs){
    vars = c("Tmax_sm", "Tmin_sm", "PPT_sm")
    for(var in vars){
    x <- ds_out[RUN != "ensembleMean" & RUN==run, PERIOD]
    y <- ds_out[RUN != "ensembleMean" & RUN==run, get(var)]
    plot(y, main=paste(var, run), xaxt="n")
    axis(1, at=1:5, labels = list_gcm_periods())
  }
}

