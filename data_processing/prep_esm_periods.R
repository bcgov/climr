library(terra)
library(stringr)
library(data.table)

period_start <- c(2001,2021,2041,2061,2081)
period_end <- c(2020,2040,2060,2080,2100)


monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# ==========================================
# step 3: export rasters of change
# ==========================================

dirs <- list.dirs("C:\\Users\\kdaust\\LocalFiles\\CMIP6_GCMs")
gcms <- unique(sapply(strsplit(dirs, "/"), "[", 2))
select <- c(2,4,5,8,9,10,12,13,15,16,18)
gcms <- gcms[select]

startyear.ref <- 1961
endyear.ref <- 1990

minyear <- 2020
maxyear <- 2100

gcm <- gcms[1]
for(gcm in gcms[-c(1:4)]) {
  
  #process the climate elements
  dir <- paste("C:\\Users\\kdaust\\LocalFiles\\CMIP6_GCMs", gcm, sep="\\")
  files <- list.files(dir)
  element.list <- sapply(strsplit(files, "_"), "[", 1)
  scenario.list <- sapply(strsplit(files, "_"), "[", 4)
  ripf.list <- sapply(strsplit(files, "_"), "[", 5)
  run.list <- paste(scenario.list, ripf.list, sep="_")
  elements <- unique(element.list)[c(3,4,1)]
  runs <- unique(run.list)
  scenarios <- unique(scenario.list)
  
  # run=runs[1]
  element=elements[3]
  for(element in elements) {
    
    # read in the raw time series for the historical runs
    s <- which(element.list==element & scenario.list=="historical")
    files.ref <- files[s]
    runs.ref <- unique(run.list[s])
    run.ref <- runs.ref[1]
    for(run.ref in runs.ref) {
      files.run <- files.ref[grep(run.ref, files.ref)]
      if(gcm=="AWI-CM-1-1-MR") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear.ref:2014)]
      if(gcm=="EC-Earth3") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear.ref:2014)]
      if(gcm=="MPI-ESM1-2-HR") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear.ref:2014)]
      if(gcm=="GISS-E2-1-G") files.run <- files.run[3:4]
      if(gcm=="INM-CM5-0") files.run <- files.run[2]
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
    scenario <- "ssp245"
    for(scenario in scenarios[-1]) {
      s <- which(element.list==element & scenario.list==scenario)
      files.proj <- files[s]
      runs.proj <- unique(run.list[s])
      run.proj <- runs.proj[2]
      run_list <- list()
      for(run.proj in runs.proj) {
        files.run <- files.proj[grep(run.proj, files.proj)]
        #if(gcm=="EC-Earth3") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%year)]
        #if(gcm=="MPI-ESM1-2-HR") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%(startyear-5):(endyear+5))]
        temp <- terra::rast(paste(dir, files.run, sep="\\"))
        proj.months <- month(time(temp))
        proj.yrs <- year(time(temp))
        if(element == "pr") {
          temp <- temp * 86400 * mean(monthdays)
          # for(m in 1:12) {
          #   temp[[which(proj.months == m)]] <- temp[[which(proj.months == m)]] * 86400 * monthdays[m]
          # }
        }else{
          temp <- temp - 273.15
        } 
        r <- temp[[proj.yrs %in% minyear:maxyear]]
        proj.months <- month(time(r))
        proj.yrs <- year(time(r))
        names(r) <- paste(gcm, element, monthcodes[proj.months], run.proj, proj.yrs, sep="_")
        run_list[[run.proj]] <- r
        print(run.proj)
      }
      
      t1 <- sds(run_list)
      proj.ensembleMean <- app(t1, mean)
      proj.months <- month(time(proj.ensembleMean))
      proj.yrs <- year(time(proj.ensembleMean))
      names(proj.ensembleMean) <- paste(gcm, element, monthcodes[proj.months], scenario, "ensembleMean", proj.yrs, sep="_")
      
      proj.runs <- rast(t1)
      names(proj.runs) <- gsub(paste0(".*",gcm),gcm, names(proj.runs))
      
      compiled <- c(compiled, proj.ensembleMean, proj.runs)
      print(scenario)
    }
    
    # write data out for the GCMxElement 
    dir.create(sprintf("C:\\Users\\kdaust\\LocalFiles\\ProcessedGCMs/gcm/%s/gcmData", gcm), recursive = TRUE)
    # writeRaster(compiled, paste(sprintf("C:\\Users\\kdaust\\LocalFiles\\ProcessedGCMs/gcm/%s/gcmData", gcm), gcm, element, "tif", sep="."), overwrite=TRUE, format="CDF", varname=element, varunit=if(element=="pr") "mm" else "degC", 
    #             longname="", xname="latitude",   yname="longitude",zname="index",
    #             zunit="numeric")
    terra::writeRaster(compiled, paste(sprintf("C:\\Users\\kdaust\\LocalFiles\\ProcessedGCMs/gcm/%s/gcmData", gcm), gcm, element, "tif", sep="."), overwrite=TRUE)
    write.csv(names(compiled), paste(sprintf("C:\\Users\\kdaust\\LocalFiles\\ProcessedGCMs/gcm/%s/gcmData", gcm), gcm, element, "csv", sep=".")) # this is the metadata for each raster
    
    print(element)
  }
  print(gcm)
}

