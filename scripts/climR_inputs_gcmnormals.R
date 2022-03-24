
# Objective: Export 1961-1990 baseline and 20-year normals of tmin, tmax and precipitation for all runs of 13 models and 4 scenarios
# Colin Mahony colin.mahony@gov.bc.ca

library(raster)
library(stringr)

monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# ==========================================
# step 3: export rasters of change
# ==========================================

dirs <- list.dirs("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Data\\CMIP6\\25GCMs")
gcms <- unique(sapply(strsplit(dirs, "/"), "[", 2))
select <- c(3,5,7,13,14,15,16,21,22,24,25,27,32)
gcms <- gcms[select]

startyear.ref <- 1961
endyear.ref <- 1990

gcm <- gcms[4]
for(gcm in gcms){
  
  #process the climate elements
  dir <- paste("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Data\\CMIP6\\25GCMs", gcm, sep="\\")
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
  for(element in elements){
    
    # read in the raw time series for the historical runs
    s <- which(element.list==element & scenario.list=="historical")
    files.ref <- files[s]
    runs.ref <- unique(run.list[s])
    run.ref <- runs.ref[1]
    for(run.ref in runs.ref){
      files.run <- files.ref[grep(run.ref, files.ref)]
      if(gcm=="AWI-CM-1-1-MR") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear.ref:2014)]
      if(gcm=="EC-Earth3") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear.ref:2014)]
      if(gcm=="MPI-ESM1-2-HR") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear.ref:2014)]
      if(gcm=="GISS-E2-1-G") files.run <- files.run[3:4]
      if(gcm=="INM-CM5-0") files.run <- files.run[2]
      j=1
      for(j in 1:length(files.run)){ #did this rather than stack import becuase it preserves the variable (month) names
        temp2 <- brick(paste(dir, files.run[j], sep="\\"))
        temp <- if(j==1) temp2 else brick(c(temp, temp2))
        print(j)
      }
      
      # separate runs into months. 
      m=1
      for(m in 1:12){
        r <- temp[[which(substr(names(temp),7,8)==monthcodes[m])]]
        if(element=="pr") r <- r*86400*monthdays[m] else r <- r-273.15  #convert units to /month (86400 seconds / day) and to degrees C from Kelvins
        
        years <- if(substr(names(r),1,1)[1]=="X") substr(names(r),2,5) else substr(names(r),1,4)
        
        ref <- mean(r[[which(years%in%startyear.ref:endyear.ref)]])
        hist <- mean(r[[which(years%in%2001:2014)]])
        
        assign(paste("ref", monthcodes[m], sep="."), if(run.ref==runs.ref[1]) ref else brick(c(get(paste("ref", monthcodes[m], sep=".")), ref)))
        assign(paste("hist", monthcodes[m], sep="."), if(run.ref==runs.ref[1]) ref else brick(c(get(paste("hist", monthcodes[m], sep=".")), hist)))
        
      }
      print(run.ref)
    }
    
    # compile reference period mean
    m=1
    for(m in 1:12){
      ref <- mean(get(paste("ref", monthcodes[m], sep=".")))
      ref.ensembleMean <- if(m==1) ref else brick(c(ref.ensembleMean, ref))
    }
    names(ref.ensembleMean) <- paste(gcm, element, monthcodes, "reference", "ensembleMean", startyear.ref, endyear.ref, sep="_")
    compiled <- ref.ensembleMean
    

    # compile future periods
    scenario <- "ssp245"
    for(scenario in scenarios[-1]){
      startyear <- 2001
      for(startyear in seq(2001, 2100, 20)){
        endyear <- startyear+19
        # Calculate Change
        s <- which(element.list==element & scenario.list==scenario)
        files.proj <- files[s]
        runs.proj <- unique(run.list[s])
        run.proj <- runs.proj[1]
        for(run.proj in runs.proj){
          files.run <- files.proj[grep(run.proj, files.proj)]
          if(gcm=="EC-Earth3") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%startyear:endyear)]
          if(gcm=="MPI-ESM1-2-HR") files.run <- files.run[which(str_sub(files.run, -9,-6)%in%(startyear-5):(endyear+5))]
          j=1
          for(j in 1:length(files.run)){ #did this rather than stack import becuase it preserves the variable (month) names
            temp2 <- brick(paste(dir, files.run[j], sep="\\"))
            temp <- if(j==1) temp2 else brick(c(temp, temp2))
            print(j)
          }
          
          # separate runs into months. 
          m=1
          for(m in 1:12){
            r <- temp[[which(substr(names(temp),7,8)==monthcodes[m])]]
            if(element=="pr") r <- r*86400*monthdays[m] else r <- r-273.15  #convert units to /month (86400 seconds / day) and to degrees C from Kelvins
            
            years <- if(substr(names(r),1,1)[1]=="X") substr(names(r),2,5) else substr(names(r),1,4)
            
            proj <- mean(r[[which(years%in%startyear:endyear)]])
            if(startyear==2001) proj <- (proj*6 + mean(get(paste("hist", monthcodes[m], sep=".")))*14)/20 #combine with historical run for 2001-2020 period
            
            assign(paste("proj", monthcodes[m], sep="."), if(run.proj==runs.proj[1]) proj else brick(c(get(paste("proj", monthcodes[m], sep=".")), proj)))
            
          }
          print(run.proj)
        }
        
        # Concatenate monthly future Rasters (Ensemble Mean)
        for(m in 1:12){
          proj <- mean(get(paste("proj", monthcodes[m], sep=".")))
          proj.ensembleMean <- if(m==1) proj else brick(c(proj.ensembleMean, proj))
          # print(m)
        }
        names(proj.ensembleMean) <- paste(gcm, element, monthcodes, scenario, "ensembleMean", startyear, endyear, sep="_")
        
        # Concatenate monthly future Rasters (indiv.runs)
        for(m in 1:12){
          proj <- get(paste("proj", monthcodes[m], sep="."))
          names(proj) <- paste(gcm, element, monthcodes[m], runs.proj, startyear, endyear, sep="_")
          proj.runs <- if(m==1) proj else brick(c(proj.runs, proj))
          # print(m)
        }
        compiled <- stack(compiled, proj.ensembleMean, proj.runs)

        print(paste(startyear, gcm))
      }
      print(scenario)
    }
    
    # write data out for the GCMxElement 
    writeRaster(compiled, paste(sprintf("inputs_raw/gcm/%s/gcmData", gcm), gcm, element, "nc", sep="."), overwrite=TRUE, format="CDF", varname=element, varunit=if(element=="pr") "mm" else "degC", 
                longname="", xname="latitude",   yname="longitude",zname="index",
                zunit="numeric")
    write.csv(names(compiled), paste(sprintf("inputs_raw/gcm/%s/gcmIndex", gcm), gcm, element, "csv", sep=".")) # this is the metadata for each raster
    
    print(element)
  }
  print(gcm)
}


gcm <- "GFDL-ESM4"
element <- "tasmin"
test <- brick(paste(sprintf("inputs_raw/gcm/%s/gcmData", gcm), gcm, element, "nc", sep="."))
index <- read.csv(paste(sprintf("inputs_raw/gcm/%s/gcmIndex", gcm), gcm, element, "csv", sep="."), )
names(test) <- index[,-1]
test
# names(test)
plot(test[[1]])
plot(test[[111]])
