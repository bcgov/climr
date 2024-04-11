library(terra)
library(stringr)
library(data.table)
library(climr)

period_start <- c(2001,2021,2041,2061,2081)
period_end <- c(2020,2040,2060,2080,2100)


monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# ==========================================
# step 3: export rasters of change
# ==========================================

dirs <- list.dirs("../Common_Files/CMIP6_GCMs/")
gcms <- list_gcm() 

startyear.ref <- 1961
endyear.ref <- 1990
na_ext <- ext(c(-180, -52.5, 14, 84))

gcm <- gcms[1]
for(gcm in gcms[1:5]) {
  
  #process the climate elements
  dir <- paste("../Common_Files/CMIP6_GCMs/", gcm, sep="\\")
  files <- list.files(dir)
  element.list <- sapply(strsplit(files, "_"), "[", 1)
  scenario.list <- sapply(strsplit(files, "_"), "[", 4)
  ripf.list <- sapply(strsplit(files, "_"), "[", 5)
  run.list <- paste(scenario.list, ripf.list, sep="_")
  elements <- unique(element.list)[c(3,4,1)]
  runs <- unique(run.list)
  scenarios <- unique(scenario.list)
  
  # run=runs[1]
  element=elements[1]
  for(element in elements) {
    
    # read in the raw time series for the historical runs
    s <- which(element.list==element & scenario.list=="historical")
    files.ref <- files[s]
    runs.ref <- unique(run.list[s])
    run.ref <- runs.ref[1]
    for(run.ref in runs.ref) {
      files.run <- files.ref[grep(run.ref, files.ref)]
      if(gcm=="GFDL-ESM4") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear.ref:2014)]
      if(gcm=="INM-CM5-0") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear.ref:2014)]
      
      # if(gcm=="AWI-CM-1-1-MR") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear.ref:2014)]
      # if(gcm=="EC-Earth3") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear.ref:2014)]
      # if(gcm=="MPI-ESM1-2-HR") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear.ref:2014)]
      # if(gcm=="GISS-E2-1-G") files.run <- files.run[3:4]
      # if(gcm=="INM-CM5-0") files.run <- files.run[2]
      temp <- terra::rast(paste(dir, files.run, sep="\\"))
      dates <- time(temp)
      ref.months <- month(dates)
      
      m=1
      for(m in 1:12) {
        r <- temp[[ref.months == m]]
        if(element=="pr") r <- r*86400*monthdays[m] else r <- r-273.15  #convert units to /month (86400 seconds / day) and to degrees C from Kelvins
        
        ref.yrs <- year(time(r))
        ref <- mean(r[[ref.yrs %in% startyear.ref:endyear.ref]])
        #hist <- mean(r[[ref.yrs %in% 2001:2014]])
        
        assign(paste("ref", monthcodes[m], sep="."), if(run.ref==runs.ref[1]) ref else c(get(paste("ref", monthcodes[m], sep=".")),ref))
        #assign(paste("hist", monthcodes[m], sep="."), if(run.ref==runs.ref[1]) ref else brick(c(get(paste("hist", monthcodes[m], sep=".")), hist)))
        
      }
      print(run.ref)
    }
    
    # compile reference period mean
    m=1
    for(m in 1:12) {
      ref <- mean(get(paste("ref", monthcodes[m], sep=".")))
      ref.ensembleMean <- if(m==1) ref else c(ref.ensembleMean, ref)
    }
    names(ref.ensembleMean) <- paste(gcm, element, monthcodes, "reference", "ensembleMean", startyear.ref, endyear.ref, sep="_")
    compiled <- ref.ensembleMean
    
    # compile future periods
    scenario <- "ssp126"
    for(scenario in scenarios[-1]) {
      s <- which(element.list==element & scenario.list==scenario)
      files.proj <- files[s]
      runs.proj <- unique(run.list[s])
      s_hist <- which(element.list==element & scenario.list=="historical")
      files.hist <- files[s_hist]
      runs.hist <- unique(run.list[s_hist])
      run_list <- list()
      for(run.proj in runs.proj) {
        files.run <- files.proj[grep(run.proj, files.proj)]
        run_nm <- gsub(".*_","", run.proj)
        files.run.hist <- files.hist[grep(run_nm, files.hist)]
        if(length(files.run.hist) == 0) next
        if(gcm=="GFDL-ESM4") files.run.hist <- files.run.hist[which(str_sub(files.run.hist, -9,-6)%in%startyear.ref:2014)]
        if(gcm=="INM-CM5-0") files.run.hist <- files.run.hist[which(str_sub(files.run.hist, -9,-6)%in%startyear.ref:2014)]
        #if(gcm=="EC-Earth3") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%year)]
        #if(gcm=="MPI-ESM1-2-HR") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%(startyear-5):(endyear+5))]
        if(gcm=="GFDL-ESM4"){
          tempx <- terra::rast(paste(dir, files.run, sep="\\"))
          temp_hist <- terra::rast(paste(dir, files.run.hist, sep="\\"))
          tempx <- project(tempx, temp_hist)
          temp_proj <- crop(tempx, temp_hist)
        }else{
          temp_proj <- terra::rast(paste(dir, files.run, sep="\\"))
          temp_hist <- terra::rast(paste(dir, files.run.hist, sep="\\"))
        }

        proj.yrs <- year(time(temp_proj))
        proj.yrs.hist <- year(time(temp_hist))
        if(element == "pr") {
          temp_proj <- temp_proj * 86400 * mean(monthdays)
          temp_hist <- temp_hist * 86400 * mean(monthdays)
        }else{
          temp_proj <- temp_proj - 273.15
          temp_hist <- temp_hist - 273.15
        } 
        
        for(yri in 1:5){
          if(yri == 1){
            r_period <- c(temp_hist[[proj.yrs.hist %in% period_start[yri]:period_end[yri]]], 
                          temp_proj[[proj.yrs %in% period_start[yri]:period_end[yri]]])
          }else{
            r_period <- temp_proj[[proj.yrs %in% period_start[yri]:period_end[yri]]]
          }
          mnths_period <- months(time(r_period))
          for(m in 1:12) {
            ref_temp <- mean(r_period[[mnths_period == month.name[m]]])
            period_mean <- if(m==1) ref_temp else c(period_mean, ref_temp)
          }
          names(period_mean) <- paste(gcm, element, monthcodes, scenario, run.proj, period_start[yri], period_end[yri], sep="_")
          future_pers <- if(yri == 1) period_mean else c(future_pers,period_mean)
        }

        run_list[[run.proj]] <- future_pers
        print(run.proj)
      }
      
      t1 <- sds(run_list)
      proj.ensembleMean <- app(t1, mean)
      proj.months <- rep(1:12, nlyr(proj.ensembleMean)/12)
      proj.yrs <- rep(c("2001_2020","2021_2040","2041_2060","2061_2080","2081_2100"), each = 12)
      names(proj.ensembleMean) <- paste(gcm, element, monthcodes[proj.months], scenario, "ensembleMean", proj.yrs, sep="_")
      
      proj.runs <- rast(t1)
      names(proj.runs) <- gsub(paste0(".*",gcm),gcm, names(proj.runs))
      
      compiled <- c(compiled, proj.ensembleMean, proj.runs)
      print(scenario)
    }
    
    # write data out for the GCMxElement 
    dir.create(sprintf("../Common_Files/GCM_Periods/%s", gcm), recursive = TRUE)
    terra::writeRaster(compiled, paste(sprintf("../Common_Files/GCM_Periods/%s/", gcm), gcm, element, "tif", sep="."), overwrite=TRUE)

    print(element)
  }
  print(gcm)
}

