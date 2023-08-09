#### BGC compare hypercube

library(climRpnw)
library(terra)
library(data.table)

###WNA
list_normal()
normal <- normal_input("Normal_1961_1990MP")
dem1 <- rast(file.path(data_path(),"inputs_pkg/normal/Normal_1961_1990MP/Normal_1961_1990MP.wlrdem.tif"))

dem <- dem1[[nlyr(dem1)]]
in_coords <- as.data.frame(dem, xy = TRUE, na.rm = FALSE)
clim_vars <- c("Tmin_wt","Tmax_wt","PPT_wt","Tmin_sm","Tmax_sm","PPT_sm")

res_wna <- downscale(in_coords, normal = normal, return_normal = TRUE, vars = clim_vars)
setDT(res_wna)

for(var in clim_vars){
  temp <- copy(dem)
  values(temp) <- res_wna[[var]]
  if(var == clim_vars[1]){
    wna_res <- temp
  }else{
    add(wna_res) <- temp
  }
}
names(wna_res) <- clim_vars
plot(wna_res[[1]])
writeRaster(wna_res,"WNA_climbc.tif")

### BC
normal <- normal_input("Normal_1961_1990MP_BC")
dem1 <- rast(file.path(data_path(),"inputs_pkg/normal/Normal_1961_1990MP_BC/Normal_1961_1990MP_BC.wlrdem.tif"))

dem <- dem1[[nlyr(dem1)]]
in_coords <- as.data.frame(dem, xy = TRUE, na.rm = FALSE)

gcms <- list_gcm()

for(gcm_nm in gcms[-1]){
  gcm_nm <- gcms[9]
  cat(gcm_nm)
  gcm <- gcm_input(
    gcm = gcm_nm,
    ssp = c("ssp245"),
    period = "2061_2080",
    max_run = 0
  )
  
  res_bc <- downscale(in_coords, normal = normal, gcm = gcm, vars = clim_vars)
  setDT(res_bc)
  
  for(var in clim_vars){
    temp <- copy(dem)
    values(temp) <- res_bc[[var]]
    if(var == clim_vars[1]){
      bc_res <- temp
    }else{
      add(bc_res) <- temp
    }
  }
  names(bc_res) <- clim_vars
  writeRaster(bc_res,paste0("BC_GCM_Vars/Mod",gcm_nm,".tif"), overwrite = T)
  rm(res_bc,bc_res)
}

#######################################################################3
clim_vars <- c("CMD","DD5")

bins <- 10
target_hypercube <- rast("WNA_climbc.tif")
target_hypercube <- target_hypercube[[clim_vars]]
target_classified <- terra::sapp(target_hypercube, \(x,...) setValues(x,values(classify(x,bins))))

concat_fn <- function(...){
  data <- list(...)
  out <- data[[1]]
  for(i in 2:length(data)){
    out <- out + data[[i]]*10^(i-1)
  }
  return(out)
}
target_id <- lapp(target_classified,concat_fn)
target_freq <- as.data.table(freq(target_id)) ##target_freq has counts of all bins
setorder(target_freq,-count)
ranges <- minmax(target_hypercube)

##split into same bins as full data set
##should update this for efficiency

all_mods <- list.files("BC_GCM_Vars/")


for(fname in all_mods){
  cat(".")
  compare_classified <- rast(paste0("BC_GCM_Vars/",fname))
  compare_classified <- compare_classified[[clim_vars]]
  for(i in 1:nlyr(compare_classified)){
    values(compare_classified[[i]]) <- values(terra::classify(compare_classified[[i]], seq(ranges[1,i],ranges[2,i],length.out = bins+1),
                                                              include.lowest = T)) 
  }
  
  compare_id <- lapp(compare_classified,concat_fn)
  compare_freq <- as.data.table(freq(compare_id))
  target_freq[compare_freq, sample_num := i.count,  on = "value"]
  target_freq[is.na(sample_num), sample_num := 0]
  missed_bins <- target_freq$sample_num
  
  rast_final <- terra::classify(target_id, target_freq[,.(value,sample_num)])
  
  if(fname == all_mods[1]){
    comb_rast <- rast_final
  }else{
    comb_rast <- comb_rast + rast_final
  }
  
}

plot(comb_rast)
writeRaster(comb_rast,"Combined_Mods_WillsVars.tif")

#ranges <- ranges[,names(sample_hypercube)]

plot(rast_final)
