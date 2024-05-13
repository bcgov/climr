# compare climateBC and climr

# install.packages('C:/Users/CMAHONY/OneDrive - Government of BC/Data/Climatebc_v750/ClimateNAr_1.2.0.zip', repos=NULL, type='source')
# remotes::install_github("bcgov/climr@devl")

library(ClimateNAr)
library(climr)
library(data.table)

#two locations (smithers and kamloops)
xyz <- data.frame(lon = c(-127.1530,-120.326), lat = c(54.785, 50.681), elev = c(475, 351), id = 1:2)


wkDir = 'C:/Users/CMAHONY/OneDrive - Government of BC/Data/Climatebc_v750/'
exe <- "ClimateNA_v7.50.exe"
xyz.climbc <- data.frame(id1=xyz$id, id2=1:dim(xyz)[1], lat=xyz$lat, long=xyz$lon, el=xyz$elev)
write.csv(xyz.climbc, paste0(wkDir, "xyz.csv"), row.names = F)
# clm <- ClimateNA_API(ClimateBC_NA='BC', latLonEl,period='Normal_1961_1990.nrm', MSY='Y');
# inputFile = paste0(wkDir, "xyz.csv")
# outputFile = paste0(wkDir, "xyz_Normal_1961_1990MSY.csv")
# period = 'Normal_1961_1990.nrm'
# ClimateNA_cmdLine(exe, wkDir, period, MSY='MSY',inputFile, outputFile)

# Climate data
climbc <- read.csv(paste0(wkDir, "xyz_Normal_1961_1990MSY.csv"))
climr <- climr_downscale(xyz, vars=list_variables(), which_normal = "normal_bc")

# Basic comparison of monthly variables
vars <- list_variables()[grep("01|04|07|10", list_variables())]
par(mar=c(5,3,0.1,0.1), mgp=c(2,0.25,0))
n <- length(vars)
plot(0, type="o", xaxt="n", xlab="", ylab="climr/ClimateBC", xlim = c(1,n), ylim=c(0,18), col="white", tck=0)
axis(1, at=1:n, labels = vars, las=2, cex=0.7, tck=0)
lines(c(-99,99), c(1,1), lty=2, col="gray")
for(var in vars){
  i <- which(vars==var)
  x <- climbc[, which(names(climbc)==var)]
  y <- climr[, get(var)]
  lines(c(i,i),c(-99,99), col="gray")
  points(c(i,i),y/x, pch=c(1,4))
}

# Basic comparison of annual variables
vars <- list_variables()[-grep("01|02|03|04|05|06|07|08|09|10|11|12|wt|sp|sm|at|PPT|Tave|Tmax|Tmin", list_variables())]
par(mar=c(4,3,0.1,0.1), mgp=c(2,0.25,0))
n <- length(vars)
plot(0, type="o", xaxt="n", xlab="", ylab="climr/ClimateBC", xlim = c(1,n), ylim=c(0.5,2), col="white", tck=0)
axis(1, at=1:n, labels = vars, las=2, cex=0.7, tck=0)
lines(c(-99,99), c(1,1), lty=2, col="gray")
for(var in vars){
  i <- which(vars==var)
  x <- climbc[, which(names(climbc)==var)]
  y <- climr[, get(var)]
  lines(c(i,i),c(-99,99), col="gray")
  points(c(i,i),y/x, pch=c(1,4))
}

# Basic comparison of PAS variables
vars <- list_variables()[grep("PAS", list_variables())]
par(mar=c(4,3,0.1,0.1), mgp=c(2,0.25,0))
n <- length(vars)
plot(0, type="o", xaxt="n", xlab="", ylab="climr/ClimateBC", xlim = c(1,n), ylim=c(0,18), col="white", tck=0)
axis(1, at=1:n, labels = vars, las=2, cex=0.7, tck=0)
lines(c(-99,99), c(1,1), lty=2, col="gray")
for(var in vars){
  i <- which(vars==var)
  x <- climbc[, which(names(climbc)==var)]
  y <- climr[, get(var)]
  lines(c(i,i),c(-99,99), col="gray")
  points(c(i,i),y/x, pch=c(1,4))
}

# Basic comparison of CMI variables
vars <- list_variables()[grep("CMI", list_variables())]
par(mar=c(4,3,0.3,0.1), mgp=c(2,0.25,0))
n <- length(vars)
plot(0, type="o", xaxt="n", xlab="", ylab="climr/ClimateBC", xlim = c(1,n), ylim=c(-100,100), col="white", tck=0)
axis(1, at=1:n, labels = vars, las=2, cex=0.7, tck=0)
lines(c(-99,99), c(1,1), lty=2, col="gray")
for(var in vars){
  i <- which(vars==var)
  x <- climbc[, which(names(climbc)==var)]
  y <- climr[, get(var)]
  lines(c(i,i),c(-999,999), col="gray")
  points(c(i,i),y/x, pch=c(1,4))
}

# Calculate CMI from basic variables and compare to climateBC
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# Functions: 
# PET
calc_PET <- function(tave, tmmin, tmmax, alt, rh) {
  # D <- 0.5 * (.calc_SVP(tmmax) - .calc_SVP(tmmin)) - .calc_SVP(tmmin - 2.5) # Hogg 1997 at face value
  # D <- 0.5 * (.calc_SVP(tmmax) + .calc_SVP(tmmin)) - .calc_SVP(tmmin - 2.5) # intuitive modification (average of SVP of tmin and tmax to get average daily SVP)
  es <- 0.5 * (.calc_SVP(tmmax) + .calc_SVP(tmmin)); D <- es - .calc_SVP(tmmin)*rh/100  # from https://andrewsforest.oregonstate.edu/sites/default/files/lter/data/studies/ms01/dewpt_vpd_calculations.pdf
  pet <- ifelse(
    tave > 10, 93 * D * exp(alt / 9300),
    ifelse(tave > -5, (6.2 * tave + 31) * D * exp(alt / 9300), 0)
  )
  return(pet)
}

# Saturated Vapour Pressure at a temperature t
calc_SVP <- function(t) {
  svp <- .calc_SVP(t)
  i <- which(t < 0)
  svp[i] <- svp[i] * (1 + (t[i] * 0.01))
  return(svp)
}

# Magnus-Tetens empirical formula for saturation vapour pressure, with units of degrees C and kPa
.calc_SVP <- function(t) {
  return(0.6105 * exp((17.273 * t) / (t + 237.3)))
}

# test the calc_svp functions
x <- -20:20
y1 <- .calc_SVP(x)
y2 <- calc_SVP(x)
plot(x, y1)
points(x, y2, pch=4)
# these are reasonable values (https://www.researchgate.net/figure/Dependence-of-water-vapour-pressure-saturation-deficit-SD-in-kPa-and-vapour-pressure_fig2_258775628)

# annual cycle of SVP at Kamloops
i=2
tave <- climbc[i, which(names(climbc)%in%paste0("Tave", monthcodes))]
tmmin <- climbc[i, which(names(climbc)%in%paste0("Tmin", monthcodes))]
tmmax <- climbc[i, which(names(climbc)%in%paste0("Tmax", monthcodes))]
plot(1:12, .calc_SVP(tmmax), ylim = c(0, 4))
points(1:12, .calc_SVP(tmmin), pch=4)
points(1:12, .calc_SVP(tmmin - 2.5))

# annual cycle of VPD at Kamloops (using hogg formula at face value)
i=2
tave <- climbc[i, which(names(climbc)%in%paste0("Tave", monthcodes))]
tmmin <- climbc[i, which(names(climbc)%in%paste0("Tmin", monthcodes))]
tmmax <- climbc[i, which(names(climbc)%in%paste0("Tmax", monthcodes))]
D <- 0.5 * (.calc_SVP(tmmax) - .calc_SVP(tmmin)) - .calc_SVP(tmmin - 2.5)
plot(1:12, D)

# annual cycle of VPD at Kamloops (modifying hogg formula to average of tmin and tmax)
i=2
tave <- climbc[i, which(names(climbc)%in%paste0("Tave", monthcodes))]
tmmin <- climbc[i, which(names(climbc)%in%paste0("Tmin", monthcodes))]
tmmax <- climbc[i, which(names(climbc)%in%paste0("Tmax", monthcodes))]
D <- 0.5 * (.calc_SVP(tmmax) + .calc_SVP(tmmin)) - .calc_SVP(tmmin - 2.5)
plot(1:12, D)

# Plot Comparing CMI from scratch vs climateBC
vars <- list_variables()[grep("CMI", list_variables())][-1]
y <- climbc[, which(names(climbc)%in%vars)]
par(mar=c(4,3,0.3,0.1), mgp=c(2,0.25,0))
plot(0, type="o", xaxt="n", xlab="", ylab="CMI", xlim = c(1,length(vars)+1), ylim=range(y), col="white", tck=0)
axis(1, at=1:length(vars), labels = vars, las=2, cex=0.7, tck=0)
lines(c(-99,99), c(1,1), lty=2, col="gray")

# annual cycle of CMI from corrected hogg equation
for(i in 1:2){
  tave <- climbc[i, which(names(climbc)%in%paste0("Tave", monthcodes))]
  tmmin <- climbc[i, which(names(climbc)%in%paste0("Tmin", monthcodes))]
  tmmax <- climbc[i, which(names(climbc)%in%paste0("Tmax", monthcodes))]
  rh <- climbc[i, which(names(climbc)%in%paste0("RH", monthcodes))]
  pr <- climbc[i, which(names(climbc)%in%paste0("PPT", monthcodes))]
  alt <- rep(climbc[i, which(names(climbc)=="Elevation")], 12)
  PET <- unlist(calc_PET(tave, tmmin, tmmax, alt, rh))
  CMI <- (pr - PET)/10 #convert from mm to centimeters
  if(i==1) points(1:12, CMI, type = "b", pch = 19, col = "red", )
  if(i==2) points(1:12, CMI, type = "b", pch = 4, col = "red", lwd=2)
  print(sum(CMI))
}

# annual cycle of CMI from ClimateBC
for(var in vars){
  i <- which(vars==var)
  y <- climbc[, which(names(climbc)==var)]
  # lines(c(i,i),c(-999,999), col="gray")
  points(c(i,i),y, pch=c(1,4))
  if(i==12) text(c(i,i),y,c("Smithers", "Kamloops"), pos=4)
}
# ClimateBC values are replicated


####################
## investigate the effect of two different methods of calculating VPD

#three locations (Smithers, Kamloops, Saskatoon)
xyz <- data.frame(lon = c(-127.1530,-120.326, -106.669), lat = c(54.785, 50.681, 52.158), elev = c(475, 351, 499), id = 1:3)
clim <- climr_downscale(xyz, vars=list_variables(), which_normal = "normal_na")
locations <- c("Smithers", "Kamloops", "Saskatoon")

# Functions: 
calc_PET_trad <- function(tave, tmmin, tmmax, alt, rh) {
  D <- 0.5 * (.calc_SVP(tmmax) + .calc_SVP(tmmin)) - .calc_SVP(tmmin - 2.5) # intuitive modification (average of SVP of tmin and tmax to get average daily SVP)
  # es <- 0.5 * (.calc_SVP(tmmax) + .calc_SVP(tmmin)); D <- es - .calc_SVP(tmmin)*rh/100  # from https://andrewsforest.oregonstate.edu/sites/default/files/lter/data/studies/ms01/dewpt_vpd_calculations.pdf
  pet <- ifelse(
    tave > 10, 93 * D * exp(alt / 9300),
    ifelse(tave > -5, (6.2 * tave + 31) * D * exp(alt / 9300), 0)
  )
  return(pet)
}
calc_PET_RH <- function(tave, tmmin, tmmax, alt, rh) {
  # D <- 0.5 * (.calc_SVP(tmmax) + .calc_SVP(tmmin)) - .calc_SVP(tmmin - 2.5) # intuitive modification (average of SVP of tmin and tmax to get average daily SVP)
  es <- 0.5 * (.calc_SVP(tmmax) + .calc_SVP(tmmin)); D <- es - .calc_SVP(tmmin)*rh/100  # from https://andrewsforest.oregonstate.edu/sites/default/files/lter/data/studies/ms01/dewpt_vpd_calculations.pdf
  pet <- ifelse(
    tave > 10, 93 * D * exp(alt / 9300),
    ifelse(tave > -5, (6.2 * tave + 31) * D * exp(alt / 9300), 0)
  )
  return(pet)
}

# Plot Comparing the two methods
vars <- list_variables()[grep("CMI", list_variables())][-1]
par(mar=c(4,3,0.3,0.1), mgp=c(2,0.25,0))
plot(0, type="o", xaxt="n", xlab="", ylab="CMI", xlim = c(1,length(vars)+2), ylim=c(-15, 7), col="white", tck=0)
axis(1, at=1:length(vars), labels = vars, las=2, cex=0.7, tck=0)
lines(c(-99,99), c(1,1), lty=2, col="gray")
legend("bottomright", legend=c("Tmin - 2.5 method", "RH method"), bty="n", lty=c(2,1), col=c(2,1), lwd=c(2,1))

for(i in 1:3){
  tave <- clim[i, paste0("Tave", monthcodes), with = FALSE]
  tmmin <- clim[i, paste0("Tmin", monthcodes), with = FALSE]
  tmmax <- clim[i, paste0("Tmax", monthcodes), with = FALSE]
  rh <- clim[i, paste0("RH", monthcodes), with = FALSE]
  pr <- clim[i, paste0("PPT", monthcodes), with = FALSE]
  alt <- rep(xyz[i, "elev"], 12)
  PET_trad <- unlist(calc_PET_trad(tave, tmmin, tmmax, alt, rh))
  PET_RH <- unlist(calc_PET_RH(tave, tmmin, tmmax, alt, rh))
  CMI_trad <- unlist((pr - PET_trad)/10) #convert from mm to centimeters
  CMI_RH <- unlist((pr - PET_RH)/10) #convert from mm to centimeters
  points(1:12, CMI_trad, type = "b", pch = i, col = "red", lty=2, lwd=2)
  points(1:12, CMI_RH, type = "b", pch = i)
  text(12,CMI_trad[12],locations[i], pos=4)
  print(sum(CMI))
}





