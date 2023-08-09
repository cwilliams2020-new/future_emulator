#!/usr/bin/env Rscript

# Does the same as cjrw_*, but only for downscaled data.
# CW's KAERI version 2 Of writing out and plotting, reading in downscaled temperature and precipitation (both downscaling methods) arrays and identifing 
# locations (7 locations as specified by Minjeong), and plotting.  Working (as of 11/4/23) for all downscaled locations. Note that, because data had to be transferred to 
# Bristol server (too large for PC), this script here is now redundant and instead is run on Bristol server.

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

############################ Set some constants #################################

nyears = 1001
nmembers = 90
nscenarios = 4

years <- seq(1,nyears,by=1)
sim <- seq(1,nmembers,by=1)
scenarios = c("nat", "rcp26", "rcp45", "rcp85")

# Set some figure constants

plotspath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/Plots/All members/Downscaled",sep="")

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

##############################################################################################################################################################
######################################################### DOWNSCALED DATA (I.E. CRU RESOLUTION) ##############################################################
##############################################################################################################################################################

#################### Set up region/locations ################################################

# Set up region in grid boxes (CRU resolution)

lon_min=610
lon_max=620
lat_min=247
lat_max=260

# To calculate GB in regional grid, based on a longitude and latitude:
#   Calculate closest global GB to longitude and latitude, then global GB - region min) + 1 = location lon, (global GB - region min) + 1 = location lat
# So far, they have given 7 locations:
# Lat, lon (d/m/s)	        	    Global CRU grid box	  Calculation				                Regional CRU grid box
# 37°43'18.04"N, 126°35'10.20"E	  256, 614		          (256-247)+1=10, (614-610)+1=5		  10, 5
# 37°43'18.04"N, 128°27'59.96"E	  256, 617		          (256-247)+1=10, (617-610)+1=8	  	10, 8
# 36°19'2.81"N, 126°46'34.43"E	  253, 614		          (253-247)+1=7, (614-610)+1=5		  7, 5
# 36°19'2.81"N, 127°57'27.28"E	  253, 616		          (253-247)+1=7, (616-610)+1=7		  7, 7
# 36°19'2.81"N, 129° 4'1.92"E	    253, 619		          (253-247)+1=7, (619-610)+1=10	  	7, 10
# 34°57'27.90"N, 126°42'46.65"E	  250, 614		          (250-247)+1=4, (614-610)+1=5		  4, 5
# 35°10'30.47"N, 128°26'20.67"E	  251, 617		          (251-247)+1=5, (617-610)+1=8		  5, 8

nlocations = 7 # Number of locations

global_GB_all = array(0,c(2,nlocations)) # Create array to hold global GB at each location, then either manually input locations or read in a file

global_GB_all[1,1] = 256
global_GB_all[2,1] = 614
global_GB_all[1,2] = 256
global_GB_all[2,2] = 617
global_GB_all[1,3] = 253
global_GB_all[2,3] = 614
global_GB_all[1,4] = 253
global_GB_all[2,4] = 616
global_GB_all[1,5] = 253
global_GB_all[2,5] = 619
global_GB_all[1,6] = 250
global_GB_all[2,6] = 614
global_GB_all[1,7] = 251
global_GB_all[2,7] = 617

regional_GB_all = array(0,c(2,nlocations)) # Create array to hold regional GB at each location

# Calculate grid box number in regional array, based on grid box number from global array (which itself has been calculated from a latitude/longitude)

for (loc in 1:nlocations){
  
  regional_GB_all[1,loc] = (global_GB_all[1,loc]-lat_min)+1
  regional_GB_all[2,loc] = (global_GB_all[2,loc]-lon_min)+1
  
}

################## Load temperature data ########################################

# Open first file to get longitudes, latitudes and times (which will always be the same regardless of ensemble member or downscaling technique)

nc_file = nc_open("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/1/Em_output_data_Temp_region_bc_natural_0_to_1MyrAP.nc")
longs = ncvar_get(nc_file,"longitude")
lats = ncvar_get(nc_file,"latitude")
times = ncvar_get(nc_file,"time")

# Get dimension sizes for longitudes, latitudes, times, scenarios and ensemble members

nx = length(longs)
ny = length(lats)
nt = length(times)

# Create sequence of numbers from 1-90, corresponding to ensemble members

member <- seq(1,nmembers,by=1)

# Create array to hold data at each individual location i.e. time by location by scenario by ensemble members.  This needs to be done outside ensemble member loop.

temp_loc_bc = array(0,c(nt,nlocations,nscenarios,nmembers))
temp_loc_stat = array(0,c(nt,nlocations,nscenarios,nmembers))

for (memnumber in 1:nmembers) { # Begin ensemble member loop, to read through all 90 ensemble members
  
print(member[memnumber])
  
# Array over region of interest containing (bias correction) temperature, for each scenario

#print("Loading data for bias correction")

temp_all_bc = array(0,c(nx,ny,nt,nscenarios)) # Create array to hold data array at every location in region i.e. longitude by latitude by time by scenario

nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Temp_region_bc_natural_0_to_1MyrAP.nc",sep=""))
temp_all_bc[,,,1] = ncvar_get(nc_file,"SAT")
nc_close(nc_file)

nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Temp_region_bc_rcp26_0_to_1MyrAP.nc",sep=""))
temp_all_bc[,,,2] = ncvar_get(nc_file,"SAT")
nc_close(nc_file)

nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Temp_region_bc_rcp45_0_to_1MyrAP.nc",sep=""))
temp_all_bc[,,,3] = ncvar_get(nc_file,"SAT")
nc_close(nc_file)

nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Temp_region_bc_rcp85_0_to_1MyrAP.nc",sep=""))
temp_all_bc[,,,4] = ncvar_get(nc_file,"SAT")
nc_close(nc_file)

# Array over region of interest containing (physical-statistical) temperature, for each scenario

#print("Loading data for physical-statistical")

temp_all_stat = array(0,c(nx,ny,nt,nscenarios)) # Create array to hold data array at every location in region i.e. longitude by latitude by time by scenario

nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Temp_region_stat_natural_0_to_1MyrAP.nc",sep=""))
temp_all_stat[,,,1] = ncvar_get(nc_file,"SAT")
nc_close(nc_file)

nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Temp_region_stat_rcp26_0_to_1MyrAP.nc",sep=""))
temp_all_stat[,,,2] = ncvar_get(nc_file,"SAT")
nc_close(nc_file)

nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Temp_region_stat_rcp45_0_to_1MyrAP.nc",sep=""))
temp_all_stat[,,,3] = ncvar_get(nc_file,"SAT")
nc_close(nc_file)

nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Temp_region_stat_rcp85_0_to_1MyrAP.nc",sep=""))
temp_all_stat[,,,4] = ncvar_get(nc_file,"SAT")
nc_close(nc_file)

# Flip data to be right way up, for all timesteps and scenarios

temp_all_stat_new = array(0,c(nx,ny,nt,nscenarios))

n = 14
for (y in c(1:14)){
  temp_all_stat_new[1:11,y,,] = temp_all_stat[(1:11),n,,]
  n = n-1
}

################## Extract data at locations for each downscaling method ########################################

### Bias correction

for (loc in 1:nlocations){
  
  for (scen in 1:nscenarios){
    
    temp_loc_bc[,loc,scen,memnumber]=temp_all_bc[regional_GB_all[2,loc],regional_GB_all[1,loc],,scen]
    
  }
  
}

### Physical-statistical


for (loc in 1:nlocations){
  
  for (scen in 1:nscenarios){
    
    temp_loc_stat[,loc,scen,memnumber]=temp_all_stat_new[regional_GB_all[2,loc],regional_GB_all[1,loc],,scen]
    
  }
  
}

} # Close ensemble member loop

# At every location and for each scenario, calculate mean/median across all members (i.e. 1001 years by 7 locations by 4 scenarios)

temp_loc_bc_mean = array(0,c(nyears,nlocations,nscenarios))
temp_loc_bc_med = array(0,c(nyears,nlocations,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations) {
    
    for (scen in 1:nscenarios) {
    
      temp_loc_bc_mean[year,loc,scen]=mean(temp_loc_bc[year,loc,scen,])
      temp_loc_bc_med[year,loc,scen]=median(temp_loc_bc[year,loc,scen,])
    
    }
    
  }
  
}


temp_loc_stat_mean = array(0,c(nyears,nlocations,nscenarios))
temp_loc_stat_med = array(0,c(nyears,nlocations,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations) {
    
    for (scen in 1:nscenarios) {
      
      temp_loc_stat_mean[year,loc,scen]=mean(temp_loc_stat[year,loc,scen,])
      temp_loc_stat_med[year,loc,scen]=median(temp_loc_stat[year,loc,scen,])
      
    }
    
  }
  
}

################## Load precipitation data ########################################

# Open first file to get longitudes, latitudes and times (which will always be the same regardless of ensemble member or downscaling technique)

nc_file = nc_open("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/1/Em_output_data_Precip_region_bc_natural_0_to_1MyrAP.nc")
longs = ncvar_get(nc_file,"longitude")
lats = ncvar_get(nc_file,"latitude")
times = ncvar_get(nc_file,"time")

# Get dimension sizes for longitudes, latitudes, times, scenarios and ensemble members

nx = length(longs)
ny = length(lats)
nt = length(times)

# Create sequence of numbers from 1-90, corresponding to ensemble members

member <- seq(1,nmembers,by=1)

# Create array to hold data at each individual location i.e. time by location by scenario by ensemble members.  This needs to be done outside ensemble member loop.

precip_loc_bc = array(0,c(nt,nlocations,nscenarios,nmembers))
precip_loc_stat = array(0,c(nt,nlocations,nscenarios,nmembers))

for (memnumber in 1:nmembers) { # Begin ensemble member loop, to read through all 90 ensemble members
  
  print(member[memnumber])
  
  # Array over region of interest containing (bias correction) temperature, for each scenario
  
  #print("Loading data for bias correction")
  
  precip_all_bc = array(0,c(nx,ny,nt,nscenarios)) # Create array to hold data array at every location in region i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Precip_region_bc_natural_0_to_1MyrAP.nc",sep=""))
  precip_all_bc[,,,1] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Precip_region_bc_rcp26_0_to_1MyrAP.nc",sep=""))
  precip_all_bc[,,,2] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Precip_region_bc_rcp45_0_to_1MyrAP.nc",sep=""))
  precip_all_bc[,,,3] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Precip_region_bc_rcp85_0_to_1MyrAP.nc",sep=""))
  precip_all_bc[,,,4] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  # Array over region of interest containing (physical-statistical) temperature, for each scenario
  
  #print("Loading data for physical-statistical")
  
  precip_all_stat = array(0,c(nx,ny,nt,nscenarios)) # Create array to hold data array at every location in region i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Precip_region_stat_natural_0_to_1MyrAP.nc",sep=""))
  precip_all_stat[,,,1] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Precip_region_stat_rcp26_0_to_1MyrAP.nc",sep=""))
  precip_all_stat[,,,2] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Precip_region_stat_rcp45_0_to_1MyrAP.nc",sep=""))
  precip_all_stat[,,,3] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",member[memnumber],"/Em_output_data_Precip_region_stat_rcp85_0_to_1MyrAP.nc",sep=""))
  precip_all_stat[,,,4] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  # Flip data to be right way up, for all timesteps and scenarios
  
  precip_all_stat_new = array(0,c(nx,ny,nt,nscenarios))
  
  n = 14
  for (y in c(1:14)){
    precip_all_stat_new[1:11,y,,] = precip_all_stat[(1:11),n,,]
    n = n-1
  }
  
  ################## Extract data at locations for each downscaling method ########################################
  
  ### Bias correction
  
  for (loc in 1:nlocations){
    
    for (scen in 1:nscenarios){
      
      precip_loc_bc[,loc,scen,memnumber]=precip_all_bc[regional_GB_all[2,loc],regional_GB_all[1,loc],,scen]
      
    }
    
  }
  
  ### Physical-statistical
  
  
  for (loc in 1:nlocations){
    
    for (scen in 1:nscenarios){
      
      precip_loc_stat[,loc,scen,memnumber]=precip_all_stat_new[regional_GB_all[2,loc],regional_GB_all[1,loc],,scen]
      
    }
    
  }
  
} # Close ensemble member loop

# At every location and for each scenario, calculate mean/median across all members (i.e. 1001 years by 7 locations by 4 scenarios)

precip_loc_bc_mean = array(0,c(nyears,nlocations,nscenarios))
precip_loc_bc_med = array(0,c(nyears,nlocations,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations) {
    
    for (scen in 1:nscenarios) {
      
      precip_loc_bc_mean[year,loc,scen]=mean(precip_loc_bc[year,loc,scen,])
      precip_loc_bc_med[year,loc,scen]=median(precip_loc_bc[year,loc,scen,])
      
    }
    
  }
  
}


precip_loc_stat_mean = array(0,c(nyears,nlocations,nscenarios))
precip_loc_stat_med = array(0,c(nyears,nlocations,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations) {
    
    for (scen in 1:nscenarios) {
      
      precip_loc_stat_mean[year,loc,scen]=mean(precip_loc_stat[year,loc,scen,])
      precip_loc_stat_med[year,loc,scen]=median(precip_loc_bc[year,loc,scen,])
      
    }
    
  }
  
}

################## Visualise ##################################################################################

print("Creating temperature (bias correction) plots")

ylabels = c("0", "2", "4", "6", "8", "10", "12", "14", "16")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (7*4 = 28)

for (loc in 1:nlocations) {
  
  for (scen in 1:nscenarios) {
  
   png(file=paste(plotspath,"down_bc_temp_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
  
   par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
  
   plot(years, temp_loc_bc[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(0, 16), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
  
   for (mem in 1:nmembers) {
    lines(years, temp_loc_bc[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
   }
  
   lines(years, temp_loc_bc_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
   lines(years, temp_loc_bc_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
   
   axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
   axis(2, at = seq(0, 16, by = 2), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
   par(las=0, xpd=TRUE,lwd=lns)
   mtext("Time AP (kyr)", side=1, line=1.5, font=1, cex=fs)
   mtext("Temperature ("~ degree*C*")", side=2, line=2, font=1, cex=fs)
   par(las=1)
   dev.off()
  
  }
  
}

print("Creating temperature (physical-statistical) plots")

ylabels = c("0", "2", "4", "6", "8", "10", "12", "14", "16")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (7*4 = 28)

for (loc in 1:nlocations) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"down_stat_temp_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(years, temp_loc_stat[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(0, 16), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(years, temp_loc_stat[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(years, temp_loc_stat_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(years, temp_loc_stat_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(0, 16, by = 2), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Time AP (kyr)", side=1, line=1.5, font=1, cex=fs)
    mtext("Temperature ("~ degree*C*")", side=2, line=2, font=1, cex=fs)
    par(las=1)
    dev.off()
    
  }
  
}

print("Creating precipitation (bias correction) plots")

ylabels = c("80", "", "100", "", "120", "", "140", "", "160")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (7*4 = 28)

for (loc in 1:nlocations) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"down_bc_precip_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(years, precip_loc_bc[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(80, 160), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(years, precip_loc_bc[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(years, precip_loc_bc_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(years, precip_loc_bc_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(80, 160, by = 10), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Time AP (kyr)", side=1, line=1.5, font=1, cex=fs)
    mtext("Precipitation (mm/month)", side=2, line=2, font=1, cex=fs)
    par(las=1)
    dev.off()
    
  }
  
}

print("Creating precipitation (physical-statistical) plots")

ylabels = c("80", "", "100", "", "120", "", "140", "", "160")

# Do this within 2 loops, locations and scenarios, so end up with an image for every location and every scenario (7*4 = 28)

for (loc in 1:nlocations) {
  
  for (scen in 1:nscenarios) {
    
    png(file=paste(plotspath,"down_stat_precip_allmems_loc_",loc,"_",scenarios[scen],".png", sep = ""), width=3000, height=800, res=300)
    
    par(fig=c(lb,rb,(bb),(bb+ht)), mar=c(2,4.5,0.5,3), pty="m", mgp=c(2,0.5,0), las=1, lwd=lns)
    
    plot(years, precip_loc_stat[,loc,scen,1], type="l", col="grey80", cex=lns, xlab="", ylab="", xlim=c(1, 1001), ylim=c(80, 160), xaxt="n", yaxt="n", xaxs="i", yaxs="i", axes=FALSE)
    
    for (mem in 1:nmembers) {
      lines(years, precip_loc_stat[,loc,scen,mem], type="l", col="grey80", lwd=lns) # Plot each member for each scenario
    }
    
    lines(years, precip_loc_stat_mean[,loc,scen], type="l", col="black", lwd=lns) # Then plot mean of all members for each scenario
    lines(years, precip_loc_stat_med[,loc,scen], type="l", col="blue", lwd=lns) # Then plot median of all members for each scenario
    
    axis(1, at = seq(1, 1001, by = 100), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
    axis(2, at = seq(80, 160, by = 10), labels=ylabels, tck=0.01, lwd=lns, cex.axis=fs)
    par(las=0, xpd=TRUE,lwd=lns)
    mtext("Time AP (kyr)", side=1, line=1.5, font=1, cex=fs)
    mtext("Precipitation (mm/month)", side=2, line=2, font=1, cex=fs)
    par(las=1)
    dev.off()
    
  }
  
}

print("Finished!")
