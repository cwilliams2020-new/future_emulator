#!/usr/bin/env Rscript

# Reads in emulated data from text files (created by cjrw1a*) for individual locations, and plots.  Working (as of 29/6/23) for all emulated variables.  Note that, because 
# data had to be transferred to Bristol server (too large for PC), this script here is now redundant and instead is run on Bristol server.

##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################

# install.packages("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/Emulator/Packages/GP_0.1.4/GP",repos = NULL, type="source")
# install.packages("plotrix")

################## Load libraries and setup workspace ########################################

.libPaths(c("C:/Users/cw18831/OneDrive - University of Bristol/Documents/R/win-library/4.1", .libPaths()))
setwd("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/Emulator/2015_Bristol_5D_v001/R")

library(maps)
library(ggplot2)
library(viridis)
library(plyr)
library(plotrix)
library(gstat)
library(fields)
library(ncdf4)
library(colorspace)
library(gdata)

require(colorspace)
require(graphics)

##############################################################################################################################################################
######################################################### EMULATED DATA (I.E. HADCM3 RESOLUTION) #############################################################
##############################################################################################################################################################

# Set some constants

nyears = 1001
nmembers = 90
nscenarios = 4

nlocations_all = 4 # Total number of locations
nlocations_land = 1 # Land locations
nlocations_sea = 3 # Ocean locations

times <- seq(1,nyears,by=1)
members <- seq(1,nmembers,by=1)
scenarios = c("nat", "rcp26", "rcp45", "rcp85")

# Set some figure constants: When it comes to the figures, this can be done here because each figure will show all ensemble members.  

plotspath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/Plots/",sep="")

xlabels = seq(1,nyears,by=100)

fs = 1 # Size of font
fs2 = 0.5 # Size of font
fs3 = 1.75 # Size of font
lns = 1.5 # Width of lines
lns2 = 1 # Width of lines
pts = 0.75 # Size of points
lb = 0.01 # Left border
rb = 0.985 # Right border
bb = 0.04 # Bottom border
ht = (1-bb)/1 # Height of panel

# Locations and grid boxes:
#   Korean Peninsula = 127.5E, 37.5N, = GB 83, 22                   O      KP       O
#   Ocean to the W = 123.75E, 37.5N = GB 82, 22                            O        
#   Ocean to the S = 127.5E, 35N = GB 83, 23                      
#   Ocean to the E = 131.25E, 37.5N = GB 84, 22

# Map of locations: All                               Land only                     Ocean only

                    # loc2    loc1    loc4            X       loc1    X             loc1    X    loc3
                    #         loc3                            X                             loc2

################ Temperature ###########################

print("Creating temperature plot")

# Create array to hold anomalies at each individual location i.e. time by location by scenario by ensemble members, as well as ensemble means/medians i.e. time by location
# by scenario.  This needs to be done outside ensemble member loop.

data_loc_anom = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_mean = array(0,c(nyears,nlocations_all,nscenarios))
data_loc_med = array(0,c(nyears,nlocations_all,nscenarios))

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file is read from a separate directory according to ensemble member
        
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      # Read each file, according to location, scenario and member

      data = read.table(paste(datapath,"Em_output_data_temp_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), header=TRUE)
      
      # For every location, scenario and member, store the anomalies (3rd column), ensemble means (5th column) and ensemble medians (6th column) in array

      data_loc_anom[,loc,scen,mem] = data[,3]
      data_loc_mean[,loc,scen] = data[,5]
      data_loc_med[,loc,scen] = data[,6]
      
    }
    
  }
  
  print(loc)
  
}

# Visualise

ylabels = c("-10", "", "-5", "", "0", "", "5", "", "10")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (4*4 = 16)

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"emul_anom_temp_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(times, data_loc_anom[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(-10, 10), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(times, data_loc_anom[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(times, data_loc_anom[,loc,scen,67], type="l", col="red", lwd=lns) # Then plot "best" ensemble number for each scenario
    lines(times, data_loc_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(times, data_loc_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(-10, 10, by = 2.5), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Temperature", side=2, line=3.5, font=1, cex=fs)
    mtext("anomaly (degC)", side=2, line=2, font=1, cex=fs)
    
    if (scen == 1) {      # Only include legend for natural scenario
      legend(800, 11, c("Ensemble members", "Best ensemble member", "Ensemble mean", "Ensemble median"), pch=c(NA), lty=1, lwd=lns, cex=fs, pt.cex=pts, col=c("grey80", "red", "black", "blue"), bty="n", ncol=1)
    }
    
    par(las=1)
    dev.off()
    
  }
  
}

stop()

################ Precipitation ###########################

print("Creating precipitation plot")

# Create array to hold anomalies at each individual location i.e. time by location by scenario by ensemble members, as well as ensemble means/medians i.e. time by location
# by scenario.  This needs to be done outside ensemble member loop.

data_loc_anom = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_mean = array(0,c(nyears,nlocations_all,nscenarios))
data_loc_med = array(0,c(nyears,nlocations_all,nscenarios))

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file is read from a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      # Read each file, according to location, scenario and member
      
      data = read.table(paste(datapath,"Em_output_data_precip_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), header=TRUE)
      
      # For every location, scenario and member, store the anomalies (3rd column), ensemble means (5th column) and ensemble medians (6th column) in array
      
      data_loc_anom[,loc,scen,mem] = data[,3]
      data_loc_mean[,loc,scen] = data[,5]
      data_loc_med[,loc,scen] = data[,6]
      
    }
    
  }
  
  print(loc)
  
}

# Visualise

ylabels = c("-10", "0", "10", "20", "30", "40", "50")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (4*4 = 16)

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"emul_anom_precip_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(times, data_loc_anom[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(-10, 50), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(times, data_loc_anom[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(times, data_loc_anom[,loc,scen,67], type="l", col="red", lwd=lns) # Then plot "best" ensemble number for each scenario
    lines(times, data_loc_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(times, data_loc_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(-10, 50, by = 10), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Precipitation", side=2, line=3.5, font=1, cex=fs)
    mtext("anomaly (mm/month)", side=2, line=2, font=1, cex=fs)
    
    if (scen == 1) {      # Only include legend for natural scenario
      legend(800, 11, c("Ensemble members", "Best ensemble member", "Ensemble mean", "Ensemble median"), pch=c(NA), lty=1, lwd=lns, cex=fs, pt.cex=pts, col=c("grey80", "red", "black", "blue"), bty="n", ncol=1)
    }
    
    par(las=1)
    dev.off()
    
  }
  
}

################ Evapotranspiration ###########################

# In case anyone is wondering how evapotranspiration can possibly be a filled field (i.e. global, land and ocean), given that transpiration is a plant-based process, 
# then actually, here, evapotranspiration = evaporation from surface and canopy, + transpiration from canopy.  Over bare soil, this = evaporation from land
# surface.  Over ocean, this = evaporation from ocean.

print("Creating evapotranspiration plot")

# Create array to hold anomalies at each individual location i.e. time by location by scenario by ensemble members, as well as ensemble means/medians i.e. time by location
# by scenario.  This needs to be done outside ensemble member loop.

data_loc_anom = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_mean = array(0,c(nyears,nlocations_all,nscenarios))
data_loc_med = array(0,c(nyears,nlocations_all,nscenarios))

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file is read from a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      # Read each file, according to location, scenario and member
      
      data = read.table(paste(datapath,"Em_output_data_evap_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), header=TRUE)
      
      # For every location, scenario and member, store the anomalies (3rd column), ensemble means (5th column) and ensemble medians (6th column) in array
      
      data_loc_anom[,loc,scen,mem] = data[,3]
      data_loc_mean[,loc,scen] = data[,5]
      data_loc_med[,loc,scen] = data[,6]
      
    }
    
  }
  
  print(loc)
  
}

# Visualise

ylabels = c("-60", "", "-40", "", "-20", "", "0", "", "20", "")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (4*4 = 16)

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"emul_anom_evap_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(times, data_loc_anom[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(-60, 30), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(times, data_loc_anom[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(times, data_loc_anom[,loc,scen,67], type="l", col="red", lwd=lns) # Then plot "best" ensemble number for each scenario
    lines(times, data_loc_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(times, data_loc_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(-60, 30, by = 10), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Evaporation", side=2, line=3.5, font=1, cex=fs)
    mtext("anomaly (mm/month)", side=2, line=2, font=1, cex=fs)
    
    if (scen == 1) {      # Only include legend for natural scenario
      legend(800, 11, c("Ensemble members", "Best ensemble member", "Ensemble mean", "Ensemble median"), pch=c(NA), lty=1, lwd=lns, cex=fs, pt.cex=pts, col=c("grey80", "red", "black", "blue"), bty="n", ncol=1)
    }
    
    par(las=1)
    dev.off()
    
  }
  
}

################ Sea ice ###########################

print("Creating sea ice plot")

# Create array to hold anomalies at each individual location i.e. time by location by scenario by ensemble members, as well as ensemble means/medians i.e. time by location
# by scenario.  This needs to be done outside ensemble member loop.  Ocean points only.

data_loc_anom = array(0,c(nyears,nlocations_sea,nscenarios,nmembers))
data_loc_mean = array(0,c(nyears,nlocations_sea,nscenarios))
data_loc_med = array(0,c(nyears,nlocations_sea,nscenarios))

for (loc in 1:nlocations_sea) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file is read from a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      # Read each file, according to location, scenario and member
      
      data = read.table(paste(datapath,"Em_output_data_sice_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), header=TRUE)
      
      # For every location, scenario and member, store the means (Even though they are called "anom" here, 2nd column), ensemble means (5th column) and ensemble medians (6th column) in array
      
      data_loc_anom[,loc,scen,mem] = data[,2]
      data_loc_mean[,loc,scen] = data[,5]
      data_loc_med[,loc,scen] = data[,6]
      
    }
    
  }
  
  print(loc)
  
}

# Currently giving negative values when clearly sea ice cannot be negative. So need to have a statement saying that if sea ice < 0, then sea ice = 0.  
# Easiest way of doing this is to identify where is negative and where is positive, then multiply array by this binary result so that negative values are multiplied by 0 
# (i.e. become 0) and positive values are multiplied by 1 (i.e. stay the same).

data_loc_anom = data_loc_anom * (data_loc_anom >= 0.0)
data_loc_mean = data_loc_mean * (data_loc_mean >= 0.0)
data_loc_med = data_loc_med * (data_loc_med >= 0.0)

# Visualise

ylabels = c("0", "5", "10", "15", "20", "25", "30")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (3*4 = 12)

for (loc in 1:nlocations_sea) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"emul_anom_sice_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(times, data_loc_anom[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(0, 30), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(times, data_loc_anom[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(times, data_loc_anom[,loc,scen,67], type="l", col="red", lwd=lns) # Then plot "best" ensemble number for each scenario
    lines(times, data_loc_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(times, data_loc_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(0, 30, by = 5), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Sea ice (%)", side=2, line=3.5, font=1, cex=fs)
    
    if (scen == 1) {      # Only include legend for natural scenario
      legend(800, 11, c("Ensemble members", "Best ensemble member", "Ensemble mean", "Ensemble median"), pch=c(NA), lty=1, lwd=lns, cex=fs, pt.cex=pts, col=c("grey80", "red", "black", "blue"), bty="n", ncol=1)
    }
    
    par(las=1)
    dev.off()
    
  }
  
}

################ Soil moisture ###########################

print("Creating soil moisture plot")

# Create array to hold anomalies at each individual location i.e. time by location by scenario by ensemble members, as well as ensemble means/medians i.e. time by location
# by scenario.  This needs to be done outside ensemble member loop.  Land points only.

data_loc_anom = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_mean = array(0,c(nyears,nlocations_land,nscenarios))
data_loc_med = array(0,c(nyears,nlocations_land,nscenarios))

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file is read from a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      # Read each file, according to location, scenario and member
      
      data = read.table(paste(datapath,"Em_output_data_sm_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), header=TRUE)
      
      # For every location, scenario and member, store the anomalies (3rd column), ensemble means (5th column) and ensemble medians (6th column) in array
      
      data_loc_anom[,loc,scen,mem] = data[,3]
      data_loc_mean[,loc,scen] = data[,5]
      data_loc_med[,loc,scen] = data[,6]
      
    }
    
  }
  
  print(loc)
  
}

# Visualise

ylabels = c("-3", "", "-2", "", "-1", "", "0", "", "1", "", "2", "", "3")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (1*4 = 4)

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"emul_anom_sm_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(times, data_loc_anom[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(-3, 3), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(times, data_loc_anom[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(times, data_loc_anom[,loc,scen,67], type="l", col="red", lwd=lns) # Then plot "best" ensemble number for each scenario
    lines(times, data_loc_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(times, data_loc_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(-3, 3, by = 0.5), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Soil moisture", side=2, line=3.5, font=1, cex=fs)
    mtext("anomaly (kg/m-2)", side=2, line=2, font=1, cex=fs)
    
    if (scen == 1) {      # Only include legend for natural scenario
      legend(800, 11, c("Ensemble members", "Best ensemble member", "Ensemble mean", "Ensemble median"), pch=c(NA), lty=1, lwd=lns, cex=fs, pt.cex=pts, col=c("grey80", "red", "black", "blue"), bty="n", ncol=1)
    }
    
    par(las=1)
    dev.off()
    
  }
  
}

################ Snow depth ###########################

print("Creating snow depth plot")

# Create array to hold anomalies at each individual location i.e. time by location by scenario by ensemble members, as well as ensemble means/medians i.e. time by location
# by scenario.  This needs to be done outside ensemble member loop.  Land points only.

data_loc_anom = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_mean = array(0,c(nyears,nlocations_land,nscenarios))
data_loc_med = array(0,c(nyears,nlocations_land,nscenarios))

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file is read from a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      # Read each file, according to location, scenario and member
      
      data = read.table(paste(datapath,"Em_output_data_snow_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), header=TRUE)
      
      # For every location, scenario and member, store the means (even though they are called "anom" here, 2nd column), ensemble means (5th column) and ensemble medians (6th column) in array
      
      data_loc_anom[,loc,scen,mem] = data[,2]
      data_loc_mean[,loc,scen] = data[,5]
      data_loc_med[,loc,scen] = data[,6]
      
    }
    
  }
  
  print(loc)
  
}

# Visualise

# For ease of reading (because values are so small), convert from m to mm

data_loc_anom = data_loc_anom * 1000
data_loc_mean = data_loc_mean * 1000
data_loc_med = data_loc_med * 1000

ylabels = c("0", "0.5", "1", "1.5", "2", "2.5", "3", "3.5", "4")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (1*4 = 4)

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"emul_anom_snow_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(times, data_loc_anom[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(0, 4), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(times, data_loc_anom[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(times, data_loc_anom[,loc,scen,67], type="l", col="red", lwd=lns) # Then plot "best" ensemble number for each scenario
    lines(times, data_loc_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(times, data_loc_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(0, 4, by = 0.5), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Snow depth (mm)", side=2, line=3.5, font=1, cex=fs)

    if (scen == 1) {      # Only include legend for natural scenario
      legend(800, 11, c("Ensemble members", "Best ensemble member", "Ensemble mean", "Ensemble median"), pch=c(NA), lty=1, lwd=lns, cex=fs, pt.cex=pts, col=c("grey80", "red", "black", "blue"), bty="n", ncol=1)
    }
    
    par(las=1)
    dev.off()
    
  }
  
}

################ Soil temperature ###########################

print("Creating soil temperature plot")

# Create array to hold anomalies at each individual location i.e. time by location by scenario by ensemble members, as well as ensemble means/medians i.e. time by location
# by scenario.  This needs to be done outside ensemble member loop.  Land points only.

data_loc_anom = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_mean = array(0,c(nyears,nlocations_land,nscenarios))
data_loc_med = array(0,c(nyears,nlocations_land,nscenarios))

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file is read from a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      # Read each file, according to location, scenario and member
      
      data = read.table(paste(datapath,"Em_output_data_stemp_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), header=TRUE)
      
      # For every location, scenario and member, store the anomalies (3rd column), ensemble means (5th column) and ensemble medians (6th column) in array
      
      data_loc_anom[,loc,scen,mem] = data[,3]
      data_loc_mean[,loc,scen] = data[,5]
      data_loc_med[,loc,scen] = data[,6]
      
    }
    
  }
  
  print(loc)
  
}

# Visualise

ylabels = c("-10", "", "-5", "", "0", "", "5", "", "10")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (1*4 = 4)

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"emul_anom_stemp_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(times, data_loc_anom[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(-10, 10), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(times, data_loc_anom[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(times, data_loc_anom[,loc,scen,67], type="l", col="red", lwd=lns) # Then plot "best" ensemble number for each scenario
    lines(times, data_loc_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(times, data_loc_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(-10, 10, by = 2.5), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Soil temperature", side=2, line=3.5, font=1, cex=fs)
    mtext("anomaly (degC)", side=2, line=2, font=1, cex=fs)
    
    if (scen == 1) {      # Only include legend for natural scenario
      legend(800, 11, c("Ensemble members", "Best ensemble member", "Ensemble mean", "Ensemble median"), pch=c(NA), lty=1, lwd=lns, cex=fs, pt.cex=pts, col=c("grey80", "red", "black", "blue"), bty="n", ncol=1)
    }
    
    par(las=1)
    dev.off()
    
  }
  
}

################ Vegetation ###########################

print("Creating vegetation plot")

# Create array to hold anomalies at each individual location i.e. time by location by scenario by ensemble members, as well as ensemble means/medians i.e. time by location
# by scenario.  This needs to be done outside ensemble member loop.  Land points only.

data_loc_anom = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_mean = array(0,c(nyears,nlocations_land,nscenarios))
data_loc_med = array(0,c(nyears,nlocations_land,nscenarios))

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file is read from a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      # Read each file, according to location, scenario and member
      
      data = read.table(paste(datapath,"Em_output_data_veg_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), header=TRUE)
      
      # For every location, scenario and member, store the anomalies (3rd column), ensemble means (5th column) and ensemble medians (6th column) in array
      
      data_loc_anom[,loc,scen,mem] = data[,3]
      data_loc_mean[,loc,scen] = data[,5]
      data_loc_med[,loc,scen] = data[,6]
      
    }
    
  }
  
  print(loc)
  
}

# Visualise

ylabels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (1*4 = 4)

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"emul_anom_veg_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(times, data_loc_anom[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(0, 5), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(times, data_loc_anom[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(times, data_loc_anom[,loc,scen,67], type="l", col="red", lwd=lns) # Then plot "best" ensemble number for each scenario
    lines(times, data_loc_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(times, data_loc_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(0, 5, by = 0.5), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Vegetation", side=2, line=3.5, font=1, cex=fs)
    mtext("anomaly (LAI)", side=2, line=2, font=1, cex=fs)
    
    if (scen == 1) {      # Only include legend for natural scenario
      legend(800, 11, c("Ensemble members", "Best ensemble member", "Ensemble mean", "Ensemble median"), pch=c(NA), lty=1, lwd=lns, cex=fs, pt.cex=pts, col=c("grey80", "red", "black", "blue"), bty="n", ncol=1)
    }
    
    par(las=1)
    dev.off()
    
  }
  
}

################ Wind speed ###########################

print("Creating wind speed plot")

# Create array to hold anomalies at each individual location i.e. time by location by scenario by ensemble members, as well as ensemble means/medians i.e. time by location
# by scenario.  This needs to be done outside ensemble member loop.

data_loc_anom = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_mean = array(0,c(nyears,nlocations_all,nscenarios))
data_loc_med = array(0,c(nyears,nlocations_all,nscenarios))

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file is read from a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      # Read each file, according to location, scenario and member
      
      data = read.table(paste(datapath,"Em_output_data_wspeed_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), header=TRUE)
      
      # For every location, scenario and member, store the anomalies (3rd column), ensemble means (5th column) and ensemble medians (6th column) in array
      
      data_loc_anom[,loc,scen,mem] = data[,3]
      data_loc_mean[,loc,scen] = data[,5]
      data_loc_med[,loc,scen] = data[,6]
      
    }
    
  }
  
  print(loc)
  
}

# Visualise

ylabels = c("-2", "", "-1", "", "0", "", "1", "", "2")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (4*4 = 16)

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"emul_anom_wspeed_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(times, data_loc_anom[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(-2, 2), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(times, data_loc_anom[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(times, data_loc_anom[,loc,scen,67], type="l", col="red", lwd=lns) # Then plot "best" ensemble number for each scenario
    lines(times, data_loc_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(times, data_loc_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(-2, 2, by = 0.5), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Wind speed", side=2, line=3.5, font=1, cex=fs)
    mtext("anomaly (m/s)", side=2, line=2, font=1, cex=fs)
    
    if (scen == 1) {      # Only include legend for natural scenario
      legend(800, 11, c("Ensemble members", "Best ensemble member", "Ensemble mean", "Ensemble median"), pch=c(NA), lty=1, lwd=lns, cex=fs, pt.cex=pts, col=c("grey80", "red", "black", "blue"), bty="n", ncol=1)
    }
    
    par(las=1)
    dev.off()
    
  }
  
}

print("Finished!")




