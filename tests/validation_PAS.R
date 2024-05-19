# compare climateBC and climr

# install.packages('C:/Users/CMAHONY/OneDrive - Government of BC/Data/Climatebc_v750/ClimateNAr_1.2.0.zip', repos=NULL, type='source')
# remotes::install_github("bcgov/climr@devl")

library(ClimateNAr)
library(climr)
library(data.table)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

#two locations (smithers and kamloops)
xyz <- data.frame(lon = c(-127.1530,-120.326), lat = c(54.785, 50.681), elev = c(475, 351), id = 1:2)
locations <- c("Smithers", "Kamloops")


wkDir = "C:/Users/CMAHONY/OneDrive - Government of BC/Data/Climatebc_v750/"
exe <- "ClimateNA_v7.50.exe"
xyz.climbc <- data.frame(id1=xyz$id, id2=1:dim(xyz)[1], lat=xyz$lat, long=xyz$lon, el=xyz$elev)
write.csv(xyz.climbc, paste0(wkDir, "xyz.csv"), row.names = F)
# clm <- ClimateNA_API(ClimateBC_NA='BC', latLonEl,period='Normal_1961_1990.nrm', MSY='Y');
# inputFile = paste0(wkDir, "xyz.csv")
# outputFile = paste0(wkDir, "xyz_Normal_1961_1990MSY.csv")
# period = 'Normal_1961_1990.nrm'
# ClimateNA_cmdLine(exe, wkDir, period, MSY='MSY',inputFile, outputFile)

# Climate data
climbc <- as.data.table(read.csv(paste0(wkDir, "xyz_Normal_1961_1990MSY.csv")))
climr <- climr_downscale(xyz, vars=list_variables(), which_normal = "normal_bc")

# Basic comparison of PAS variables (relative)
vars <- list_variables()[grep("PAS", list_variables())]
par(mar=c(4,3,0.1,0.1), mgp=c(2,0.25,0))
n <- length(vars)
plot(0, type="o", xaxt="n", xlab="", ylab="climr/ClimateBC", xlim = c(1,n), ylim=c(0,18), col="white", tck=0)
axis(1, at=1:n, labels = vars, las=2, cex=0.7, tck=0)
lines(c(-99,99), c(1,1), lty=2, col="gray")
for(var in vars){
  i <- which(vars==var)
  x <- climbc[, get(var)]
  y <- climr[, get(var)]
  lines(c(i,i),c(-99,99), col="gray")
  points(c(i,i),y/x, pch=c(1,4))
}

# Basic comparison of PAS variables (absolute)
vars <- paste0("PAS", monthcodes)
pas.climbc <- climbc[, vars, with = FALSE]
pas.climr <- climr[, vars, with = FALSE]
par(mar=c(4,3,0.1,0.1), mgp=c(2,0.25,0))
plot(0, type="o", xaxt="n", xlab="", ylab="PAS (mm)", xlim = c(1,14), yaxs="i", ylim=c(0,max(pas.climr)*1.05), col="white", tck=0)
axis(1, at=1:12, labels = vars, las=2, cex=0.7, tck=0)
legend("bottomright", legend=c("climr", "ClimateBC"), bty="n", lty=c(2,1), col=c(2,1), lwd=c(2,1))
for(i in 1:2){
  y.climbc <- pas.climbc[i,]
  y.climr <- pas.climr[i,]
  points(1:12,y.climr, type = "b", pch=i, col=i, lty=2, lwd=2)
  points(1:12,y.climbc, type = "b", pch=i, col=i)
  text(12,y.climr[[12]],locations[i], pos=4)
}

# comparison of all elements (absolute)
data(variables)
elements <- unique(variables[Category=="Monthly", Code_Element])
for(element in elements){
# Basic comparison of PAS variables (absolute)
vars <- if(length(grep("DD", element)) == 1) paste0(element, "_", monthcodes) else paste0(element, monthcodes)
pas.climbc <- climbc[, vars, with = FALSE]
pas.climr <- climr[, vars, with = FALSE]
par(mar=c(4,3,0.1,0.1), mgp=c(2,0.25,0))
plot(0, type="o", xaxt="n", xlab="", ylab="element", xlim = c(1,14), ylim=range(pas.climr), col="white", tck=0)
axis(1, at=1:12, labels = vars, las=2, cex=0.7, tck=0)
legend("bottomright", legend=c("climr", "ClimateBC"), bty="n", lty=c(2,1), col=c(2,1), lwd=c(2,1))
for(i in 1:2){
  y.climbc <- pas.climbc[i,]
  y.climr <- pas.climr[i,]
  points(1:12,y.climr, type = "b", pch=i, col=i, lty=2, lwd=2)
  points(1:12,y.climbc, type = "b", pch=i, col=i)
  text(12,y.climr[[12]],locations[i], pos=4)
}
}

