#!/usr/bin/env Rscript

# Reads in emulated data as global .nc files and then extracts individual locations, Writing out text files to be used in cjrw1b*.  Working (as of 29/6/23) for all emulated 
# variables.  Note that, because data had to be transferred to Bristol server (too large for PC), this script here is now redundant and instead is run on Bristol server.

# Note: Global means that are read in here are filled fields i.e. there is no land sea mask applied to any year, for any variable.  For anomalies/variance, LSM exists. This means that those 
# variables that require the land sea mask (e.g. soil moisture, soil temperature, vegetation) contain values for every year.  This is fine for Korean Peninsula, 
# because it never entirely disappears i.e. whatever the sea level is, the Korean Peninsula is always there i.e. there are always valid data.  However, this is 
# more of a problem elsewhere (e.g. Finland), which appears and disappears according to sea level (e.g. for the first ~87 kya, this grid box is counted as 
# ocean and therefore these variables = NaN).  So if wanting to change to a new region, need to check the land sea mask (and whether or not the grid box 
# disappears when sea level changes) before extracting data.

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

members <- seq(1,nmembers,by=1)
scenarios = c("nat", "rcp26", "rcp45", "rcp85")

# Even though, when visualised here, data are upside down, they are still the right way up in terms of selecting locations (from main emulator script), so don't flip

# Locations and grid boxes:
#   Korean Peninsula = 127.5E, 37.5N, = GB 83, 22                   O      KP       O
#   Ocean to the W = 123.75E, 37.5N = GB 82, 22                            O        
#   Ocean to the S = 127.5E, 35N = GB 83, 23                      
#   Ocean to the E = 131.25E, 37.5N = GB 84, 22

nlocations_all = 4 # Total number of locations
nlocations_land = 1 # Land locations
nlocations_sea = 3 # Ocean locations

loc_all = array(0,c(nlocations_all,2)) # Create array to hold GB at each location (all 4 locations by 2 GB (lon by lat)), then manually input locations
loc_land = array(0,c(nlocations_land,2)) # Create array to hold GB at each location over land (1 location by 2 GB (lon by lat)), then manually input locations
loc_sea = array(0,c(nlocations_sea,2)) # Create array to hold GB at each location over ocean (3 locations by 2 GB (lon by lat)), then manually input locations

# Assign locations      Map of locations (when all GB are considered, KP always = loc1)

loc_all[1,1] = 83       # loc2    loc1    loc4
loc_all[1,2] = 22       #         loc3
loc_all[2,1] = 82
loc_all[2,2] = 22
loc_all[3,1] = 83
loc_all[3,2] = 23
loc_all[4,1] = 84
loc_all[4,2] = 22

#                       Map of locations (when only land GB are considered)

loc_land[1,1] = 83      # X       loc1    X
loc_land[1,2] = 22      #         X  

#                       Map of locations (when only ocean GB are considered)

loc_sea[1,1] = 82       # loc1    X    loc3
loc_sea[1,2] = 22       #         loc2
loc_sea[2,1] = 83
loc_sea[2,2] = 23
loc_sea[3,1] = 84
loc_sea[3,2] = 22

# Open any file to get longitudes, latitudes and times (which will always be the same regardless of variable or ensemble member)

nc_file = nc_open("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/1/Em_output_data_means_Temp_Global_natural_0_to_1MyrAP_cjrw.nc")
longs = ncvar_get(nc_file,"longitude")
lats = ncvar_get(nc_file,"latitude")
times = ncvar_get(nc_file,"time")

# Get dimension sizes for longitudes, latitudes

nx = length(longs)
ny = length(lats)

################ Temperature ###########################

print("Writing out temperature")

# Create arrays to hold data at each individual location i.e. time by location by scenario by ensemble members.  This needs to be done outside ensemble member loop.
# All locations

data_loc_means = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_anom = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_var = array(0,c(nyears,nlocations_all,nscenarios,nmembers))

for (memnumber in 1:nmembers) { # Begin ensemble member loop, to read through all 90 ensemble members
  
  print(members[memnumber])
  
  data_all_means = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold means at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Temp_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,1] = ncvar_get(nc_file,"SAT")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Temp_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,2] = ncvar_get(nc_file,"SAT")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Temp_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,3] = ncvar_get(nc_file,"SAT")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Temp_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,4] = ncvar_get(nc_file,"SAT")
  nc_close(nc_file)
  
  data_all_anom = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold anomalies at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Temp_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,1] = ncvar_get(nc_file,"SAT_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Temp_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,2] = ncvar_get(nc_file,"SAT_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Temp_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,3] = ncvar_get(nc_file,"SAT_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Temp_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,4] = ncvar_get(nc_file,"SAT_anomaly")
  nc_close(nc_file)
  
  data_all_var = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold variances at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Temp_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,1] = ncvar_get(nc_file,"SAT_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Temp_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,2] = ncvar_get(nc_file,"SAT_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Temp_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,3] = ncvar_get(nc_file,"SAT_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Temp_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,4] = ncvar_get(nc_file,"SAT_var")
  nc_close(nc_file)
  
  # Extract data at locations
  
  for (loc in 1:nlocations_all){
    
    for (scen in 1:nscenarios){
      
      data_loc_means[,loc,scen,memnumber]=data_all_means[loc_all[loc,1],loc_all[loc,2],,scen]
      data_loc_anom[,loc,scen,memnumber]=data_all_anom[loc_all[loc,1],loc_all[loc,2],,scen]
      data_loc_var[,loc,scen,memnumber]=data_all_var[loc_all[loc,1],loc_all[loc,2],,scen]
      
    }
    
  }
  
} # Close ensemble member loop

# At every location and for each scenario, calculate mean and median across all members (i.e. 1001 years by 4 locations by 4 scenarios, where each value is the mean/median across ensemble members)
# Only do this for anomalies for now, as these are what will be plotted

print("Calculating ensemble mean/median")

data_loc_anom_mean = array(0,c(nyears,nlocations_all,nscenarios))
data_loc_anom_med = array(0,c(nyears,nlocations_all,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations_all) {
    
    for (scen in 1:nscenarios) {
      
      data_loc_anom_mean[year,loc,scen]=mean(data_loc_anom[year,loc,scen,])
      data_loc_anom_med[year,loc,scen]=median(data_loc_anom[year,loc,scen,])
      
    }
    
  }
  
}

# Write out - where each file contains means, and anomalies and variances for a given location, scenario and ensemble member, as well as the ensemble mean/median (which will be the 
# same for each location and scenario)

print("Writing out")

ncol = 6

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file goes into a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      data_to_save = array(c((times), round(data_loc_means[,loc,scen,mem], digits = 4), round(data_loc_anom[,loc,scen,mem], digits = 4), round(data_loc_var[,loc,scen,mem], digits = 4), round(data_loc_anom_mean[,loc,scen], digits = 4), round(data_loc_anom_med[,loc,scen], digits = 4)), c(length(rev(times)),ncol))
      colnames(data_to_save) = c("Time_kyrAP", "Temp_degC", "dTemp_degC", "vTemp_degC", "mean_dTemp_degC", "med_dTemp_degC")
      write.table(data_to_save, file = paste(datapath,"Em_output_data_temp_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
      
    }
    
  }
  
}

stop()

################ Precipitation ###########################

print("Writing out precipitation")

# Create arrays to hold data at each individual location i.e. time by location by scenario by ensemble members.  This needs to be done outside ensemble member loop.
# All locations

data_loc_means = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_anom = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_var = array(0,c(nyears,nlocations_all,nscenarios,nmembers))

for (memnumber in 1:nmembers) { # Begin ensemble member loop, to read through all 90 ensemble members
  
  print(members[memnumber])
  
  data_all_means = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold means at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Precip_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,1] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Precip_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,2] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Precip_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,3] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Precip_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,4] = ncvar_get(nc_file,"precip")
  nc_close(nc_file)
  
  data_all_anom = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold anomalies at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Precip_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,1] = ncvar_get(nc_file,"precip_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Precip_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,2] = ncvar_get(nc_file,"precip_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Precip_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,3] = ncvar_get(nc_file,"precip_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Precip_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,4] = ncvar_get(nc_file,"precip_anomaly")
  nc_close(nc_file)
  
  data_all_var = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold variances at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Precip_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,1] = ncvar_get(nc_file,"precip_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Precip_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,2] = ncvar_get(nc_file,"precip_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Precip_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,3] = ncvar_get(nc_file,"precip_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Precip_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,4] = ncvar_get(nc_file,"precip_var")
  nc_close(nc_file)
  
  # Extract data at locations
  
  for (loc in 1:nlocations_all){
    
    for (scen in 1:nscenarios){
      
      data_loc_means[,loc,scen,memnumber]=data_all_means[loc_all[loc,1],loc_all[loc,2],,scen]
      data_loc_anom[,loc,scen,memnumber]=data_all_anom[loc_all[loc,1],loc_all[loc,2],,scen]
      data_loc_var[,loc,scen,memnumber]=data_all_var[loc_all[loc,1],loc_all[loc,2],,scen]
      
    }
    
  }
  
} # Close ensemble member loop

# At every location and for each scenario, calculate mean and median across all members (i.e. 1001 years by 4 locations by 4 scenarios, where each value is the mean/median across ensemble members)
# Only do this for anomalies for now, as these are what will be plotted

print("Calculating ensemble mean/median")

data_loc_anom_mean = array(0,c(nyears,nlocations_all,nscenarios))
data_loc_anom_med = array(0,c(nyears,nlocations_all,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations_all) {
    
    for (scen in 1:nscenarios) {
      
      data_loc_anom_mean[year,loc,scen]=mean(data_loc_anom[year,loc,scen,])
      data_loc_anom_med[year,loc,scen]=median(data_loc_anom[year,loc,scen,])
      
    }
    
  }
  
}

# Write out - where each file contains means, and anomalies and variances for a given location, scenario and ensemble member, as well as the ensemble mean/median (which will be the 
# same for each location and scenario)

print("Writing out")

ncol = 6

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file goes into a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      data_to_save = array(c((times), round(data_loc_means[,loc,scen,mem], digits = 4), round(data_loc_anom[,loc,scen,mem], digits = 4), round(data_loc_var[,loc,scen,mem], digits = 4), round(data_loc_anom_mean[,loc,scen], digits = 4), round(data_loc_anom_med[,loc,scen], digits = 4)), c(length(rev(times)),ncol))
      colnames(data_to_save) = c("Time_kyrAP", "Precip_mm/month", "dPrecip_mm/month", "vPrecip_mm/month", "mean_dPrecip_mm/month", "med_dPrecip_mm/month")
      write.table(data_to_save, file = paste(datapath,"Em_output_data_precip_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
      
    }
    
  }
  
}

################ Evapotranspiration ###########################

# In case anyone is wondering how evapotranspiration can possibly be a filled field (i.e. global, land and ocean), given that transpiration is a plant-based process, 
# then actually, here, evapotranspiration = evaporation from surface and canopy, + transpiration from canopy.  Over bare soil, this = evaporation from land
# surface.  Over ocean, this = evaporation from ocean.

print("Writing out evapotranspiration")

# Create arrays to hold data at each individual location i.e. time by location by scenario by ensemble members.  This needs to be done outside ensemble member loop.
# All locations

data_loc_means = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_anom = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_var = array(0,c(nyears,nlocations_all,nscenarios,nmembers))

for (memnumber in 1:nmembers) { # Begin ensemble member loop, to read through all 90 ensemble members
  
  print(members[memnumber])
  
  data_all_means = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold means at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Evapotrans_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,1] = ncvar_get(nc_file,"evapotrans")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Evapotrans_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,2] = ncvar_get(nc_file,"evapotrans")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Evapotrans_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,3] = ncvar_get(nc_file,"evapotrans")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Evapotrans_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,4] = ncvar_get(nc_file,"evapotrans")
  nc_close(nc_file)

  data_all_anom = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold anomalies at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Evapotrans_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,1] = ncvar_get(nc_file,"evapotrans_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Evapotrans_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,2] = ncvar_get(nc_file,"evapotrans_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Evapotrans_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,3] = ncvar_get(nc_file,"evapotrans_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Evapotrans_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,4] = ncvar_get(nc_file,"evapotrans_anomaly")
  nc_close(nc_file)
  
  data_all_var = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold variances at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Evapotrans_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,1] = ncvar_get(nc_file,"evapotrans_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Evapotrans_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,2] = ncvar_get(nc_file,"evapotrans_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Evapotrans_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,3] = ncvar_get(nc_file,"evapotrans_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Evapotrans_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,4] = ncvar_get(nc_file,"evapotrans_var")
  nc_close(nc_file)
  
# Extract data at locations

  for (loc in 1:nlocations_all){
    
    for (scen in 1:nscenarios){
      
      data_loc_means[,loc,scen,memnumber]=data_all_means[loc_all[loc,1],loc_all[loc,2],,scen]
      data_loc_anom[,loc,scen,memnumber]=data_all_anom[loc_all[loc,1],loc_all[loc,2],,scen]
      data_loc_var[,loc,scen,memnumber]=data_all_var[loc_all[loc,1],loc_all[loc,2],,scen]
      
    }
    
  }
  
} # Close ensemble member loop

# At every location and for each scenario, calculate mean and median across all members (i.e. 1001 years by 4 locations by 4 scenarios, where each value is the mean/median across ensemble members)
# Only do this for anomalies for now, as these are what will be plotted

print("Calculating ensemble mean/median")

data_loc_anom_mean = array(0,c(nyears,nlocations_all,nscenarios))
data_loc_anom_med = array(0,c(nyears,nlocations_all,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations_all) {
    
    for (scen in 1:nscenarios) {
      
      data_loc_anom_mean[year,loc,scen]=mean(data_loc_anom[year,loc,scen,])
      data_loc_anom_med[year,loc,scen]=median(data_loc_anom[year,loc,scen,])
      
    }
    
  }
  
}

# Write out - where each file contains means, and anomalies and variances for a given location, scenario and ensemble member, as well as the ensemble mean/median (which will be the 
# same for each location and scenario)

print("Writing out")

ncol = 6

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file goes into a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      data_to_save = array(c((times), round(data_loc_means[,loc,scen,mem], digits = 4), round(data_loc_anom[,loc,scen,mem], digits = 4), round(data_loc_var[,loc,scen,mem], digits = 4), round(data_loc_anom_mean[,loc,scen], digits = 4), round(data_loc_anom_med[,loc,scen], digits = 4)), c(length(rev(times)),ncol))
      colnames(data_to_save) = c("Time_kyrAP", "Evap_mm/month", "dEvap_mm/month", "vEvap_mm/month", "mean_dEvap_mm/month", "med_dEvap_mm/month")
      write.table(data_to_save, file = paste(datapath,"Em_output_data_evap_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
      
    }
    
  }
  
}

################ Sea ice ###########################

print("Writing out sea ice")

# Create arrays to hold data at each individual location i.e. time by location by scenario by ensemble members.  This needs to be done outside ensemble member loop.
# Ocean locations only

data_loc_means = array(0,c(nyears,nlocations_sea,nscenarios,nmembers))
data_loc_anom = array(0,c(nyears,nlocations_sea,nscenarios,nmembers))
data_loc_var = array(0,c(nyears,nlocations_sea,nscenarios,nmembers))

for (memnumber in 1:nmembers) { # Begin ensemble member loop, to read through all 90 ensemble members
  print(members[memnumber])
  
  data_all_means = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold means at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Sice_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,1] = ncvar_get(nc_file,"seaice")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Sice_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,2] = ncvar_get(nc_file,"seaice")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Sice_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,3] = ncvar_get(nc_file,"seaice")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Sice_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,4] = ncvar_get(nc_file,"seaice")
  nc_close(nc_file)
  
  data_all_anom = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold anomalies at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Sice_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,1] = ncvar_get(nc_file,"seaice_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Sice_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,2] = ncvar_get(nc_file,"seaice_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Sice_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,3] = ncvar_get(nc_file,"seaice_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Sice_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,4] = ncvar_get(nc_file,"seaice_anomaly")
  nc_close(nc_file)
  
  data_all_var = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold variances at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Sice_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,1] = ncvar_get(nc_file,"seaice_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Sice_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,2] = ncvar_get(nc_file,"seaice_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Sice_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,3] = ncvar_get(nc_file,"seaice_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Sice_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,4] = ncvar_get(nc_file,"seaice_var")
  nc_close(nc_file)
  
# Currently giving negative values when clearly sea ice cannot be negative. So need to have a statement saying that if sea ice < 0, then sea ice = 0.  
# Easiest way of doing this is to identify where is negative and where is positive, then multiply array by this binary result so that negative values are multiplied by 0 
# (i.e. become 0) and positive values are multiplied by 1 (i.e. stay the same).
  
  data_all_means = data_all_means * (data_all_means >= 0.0)
  data_all_anom = data_all_anom * (data_all_anom >= 0.0)
  data_all_var = data_all_var * (data_all_var >= 0.0)
  
# Extract data at locations
  
  for (loc in 1:nlocations_sea){
    
    for (scen in 1:nscenarios){
      
      data_loc_means[,loc,scen,memnumber]=data_all_means[loc_sea[loc,1],loc_sea[loc,2],,scen]
      data_loc_anom[,loc,scen,memnumber]=data_all_anom[loc_sea[loc,1],loc_sea[loc,2],,scen]
      data_loc_var[,loc,scen,memnumber]=data_all_var[loc_sea[loc,1],loc_sea[loc,2],,scen]
      
    }
    
  }
  
} # Close ensemble member loop

# At every location and for each scenario, calculate mean and median across all members (i.e. 1001 years by 4 locations by 4 scenarios, where each value is the mean/median across ensemble members)
# Only do this for means for now, as these are what will be plotted

print("Calculating ensemble mean/median")

data_loc_anom_mean = array(0,c(nyears,nlocations_sea,nscenarios))
data_loc_anom_med = array(0,c(nyears,nlocations_sea,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations_sea) {
    
    for (scen in 1:nscenarios) {
      
      data_loc_anom_mean[year,loc,scen]=mean(data_loc_means[year,loc,scen,]) # For sea ice, calculate ensemble mean/median based on means, not anomalies
      data_loc_anom_med[year,loc,scen]=median(data_loc_means[year,loc,scen,])
      
    }
    
  }
  
}

# Write out - where each file contains means, and anomalies and variances for a given location, scenario and ensemble member, as well as the ensemble mean/median (which will be the 
# same for each location and scenario)

print("Writing out")

ncol = 6

for (loc in 1:nlocations_sea) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file goes into a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      data_to_save = array(c((times), round(data_loc_means[,loc,scen,mem], digits = 4), round(data_loc_anom[,loc,scen,mem], digits = 4), round(data_loc_var[,loc,scen,mem], digits = 4), round(data_loc_anom_mean[,loc,scen], digits = 4), round(data_loc_anom_med[,loc,scen], digits = 4)), c(length(rev(times)),ncol))
      colnames(data_to_save) = c("Time_kyrAP", "SeaIceConcentration_%", "dSeaIceConcentration_%", "vSeaIceConcentration_%", "mean_SeaIceConcentration_%", "med_SeaIceConcentration_%")
      write.table(data_to_save, file = paste(datapath,"Em_output_data_sice_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
      
    }
    
  }
  
}

################ Soil moisture ###########################

print("Writing out soil moisture")

# Create arrays to hold data at each individual location i.e. time by location by scenario by ensemble members.  This needs to be done outside ensemble member loop.
# Land locations only (of which there is only 1)

data_loc_means = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_anom = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_var = array(0,c(nyears,nlocations_land,nscenarios,nmembers))

for (memnumber in 1:nmembers) { # Begin ensemble member loop, to read through all 90 ensemble members
  
  print(members[memnumber])
  
  data_all_means = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold means at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"Em_output_data_means_SM_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,1] = ncvar_get(nc_file,"sm")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_SM_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,2] = ncvar_get(nc_file,"sm")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_SM_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,3] = ncvar_get(nc_file,"sm")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_SM_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,4] = ncvar_get(nc_file,"sm")
  nc_close(nc_file)
  
  data_all_anom = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold anomalies at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_SM_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,1] = ncvar_get(nc_file,"sm_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_SM_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,2] = ncvar_get(nc_file,"sm_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_SM_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,3] = ncvar_get(nc_file,"sm_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_SM_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,4] = ncvar_get(nc_file,"sm_anomaly")
  nc_close(nc_file)
  
  data_all_var = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold variances at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_SM_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,1] = ncvar_get(nc_file,"sm_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_SM_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,2] = ncvar_get(nc_file,"sm_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_SM_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,3] = ncvar_get(nc_file,"sm_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_SM_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,4] = ncvar_get(nc_file,"sm_var")
  nc_close(nc_file)
  
  # Extract data at locations
  
  for (loc in 1:nlocations_land){
    
    for (scen in 1:nscenarios){
      
      data_loc_means[,loc,scen,memnumber]=data_all_means[loc_land[loc,1],loc_land[loc,2],,scen]
      data_loc_anom[,loc,scen,memnumber]=data_all_anom[loc_land[loc,1],loc_land[loc,2],,scen]
      data_loc_var[,loc,scen,memnumber]=data_all_var[loc_land[loc,1],loc_land[loc,2],,scen]
      
    }
    
  }
  
} # Close ensemble member loop

# At every location and for each scenario, calculate mean and median across all members (i.e. 1001 years by 4 locations by 4 scenarios, where each value is the mean/median across ensemble members)
# Only do this for anomalies for now, as these are what will be plotted

print("Calculating ensemble mean/median")

data_loc_anom_mean = array(0,c(nyears,nlocations_land,nscenarios))
data_loc_anom_med = array(0,c(nyears,nlocations_land,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations_land) {
    
    for (scen in 1:nscenarios) {
      
      data_loc_anom_mean[year,loc,scen]=mean(data_loc_anom[year,loc,scen,])
      data_loc_anom_med[year,loc,scen]=median(data_loc_anom[year,loc,scen,])
      
    }
    
  }
  
}

# Write out - where each file contains means, and anomalies and variances for a given location, scenario and ensemble member, as well as the ensemble mean/median (which will be the 
# same for each location and scenario)

print("Writing out")

ncol = 6

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file goes into a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      data_to_save = array(c((times), round(data_loc_means[,loc,scen,mem], digits = 4), round(data_loc_anom[,loc,scen,mem], digits = 4), round(data_loc_var[,loc,scen,mem], digits = 4), round(data_loc_anom_mean[,loc,scen], digits = 4), round(data_loc_anom_med[,loc,scen], digits = 4)), c(length(rev(times)),ncol))
      colnames(data_to_save) = c("Time_kyrAP", "SoilMoisture_kg/m-2", "dSoilMoisture_kg/m-2", "vSoilMoisture_kg/m-2", "mean_dSoilMoisture_kg/m-2", "med_dSoilMoisture_kg/m-2")
      write.table(data_to_save, file = paste(datapath,"Em_output_data_sm_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
      
    }
    
  }
  
}


################ Snow depth ###########################

print("Writing out snow depth ")

# Create arrays to hold data at each individual location i.e. time by location by scenario by ensemble members.  This needs to be done outside ensemble member loop.
# Land locations only (of which there is only 1)

data_loc_means = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_anom = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_var = array(0,c(nyears,nlocations_land,nscenarios,nmembers))

for (memnumber in 1:nmembers) { # Begin ensemble member loop, to read through all 90 ensemble members
  
  print(members[memnumber])
  
  data_all_means = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold means at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_SnDepth_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,1] = ncvar_get(nc_file,"snow")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_SnDepth_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,2] = ncvar_get(nc_file,"snow")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_SnDepth_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,3] = ncvar_get(nc_file,"snow")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_SnDepth_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,4] = ncvar_get(nc_file,"snow")
  nc_close(nc_file)
  
  data_all_anom = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold anomalies at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_SnDepth_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,1] = ncvar_get(nc_file,"snow_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_SnDepth_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,2] = ncvar_get(nc_file,"snow_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_SnDepth_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,3] = ncvar_get(nc_file,"snow_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_SnDepth_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,4] = ncvar_get(nc_file,"snow_anomaly")
  nc_close(nc_file)
  
  data_all_var = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold variances at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_SnDepth_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,1] = ncvar_get(nc_file,"snow_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_SnDepth_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,2] = ncvar_get(nc_file,"snow_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_SnDepth_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,3] = ncvar_get(nc_file,"snow_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_SnDepth_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,4] = ncvar_get(nc_file,"snow_var")
  nc_close(nc_file)
  
  # Extract data at locations
  
  for (loc in 1:nlocations_land){
    
    for (scen in 1:nscenarios){
      
      data_loc_means[,loc,scen,memnumber]=data_all_means[loc_land[loc,1],loc_land[loc,2],,scen]
      data_loc_anom[,loc,scen,memnumber]=data_all_anom[loc_land[loc,1],loc_land[loc,2],,scen]
      data_loc_var[,loc,scen,memnumber]=data_all_var[loc_land[loc,1],loc_land[loc,2],,scen]
      
    }
    
  }
  
} # Close ensemble member loop

# At every location and for each scenario, calculate mean and median across all members (i.e. 1001 years by 4 locations by 4 scenarios, where each value is the mean/median across ensemble members)
# Only do this for means for now, as these are what will be plotted

print("Calculating ensemble mean/median")

data_loc_anom_mean = array(0,c(nyears,nlocations_land,nscenarios))
data_loc_anom_med = array(0,c(nyears,nlocations_land,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations_land) {
    
    for (scen in 1:nscenarios) {
      
      data_loc_anom_mean[year,loc,scen]=mean(data_loc_means[year,loc,scen,]) # For snow depth, calculate ensemble mean based on means, not anomalies
      data_loc_anom_med[year,loc,scen]=median(data_loc_means[year,loc,scen,])
      
    }
    
  }
  
}

# Write out - where each file contains means, and anomalies and variances for a given location, scenario and ensemble member, as well as the ensemble mean/median (which will be the 
# same for each location and scenario)

print("Writing out")

ncol = 6

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file goes into a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      data_to_save = array(c((times), round(data_loc_means[,loc,scen,mem], digits = 4), round(data_loc_anom[,loc,scen,mem], digits = 4), round(data_loc_var[,loc,scen,mem], digits = 4), round(data_loc_anom_mean[,loc,scen], digits = 4), round(data_loc_anom_med[,loc,scen], digits = 4)), c(length(rev(times)),ncol))
      colnames(data_to_save) = c("Time_kyrAP", "SnDepth_m", "dSnDepth_m", "vSnDepth_m", "mean_SnDepth_m", "med_SnDepth_m")
      write.table(data_to_save, file = paste(datapath,"Em_output_data_snow_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
      
    }
    
  }
  
}

################ Soil temperature ###########################

print("Writing out soil temperature")

# Create arrays to hold data at each individual location i.e. time by location by scenario by ensemble members.  This needs to be done outside ensemble member loop.
# Land locations only (of which there is only 1)

data_loc_means = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_anom = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_var = array(0,c(nyears,nlocations_land,nscenarios,nmembers))

for (memnumber in 1:nmembers) { # Begin ensemble member loop, to read through all 90 ensemble members
  
  print(members[memnumber])
  
  data_all_means = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold means at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_STemp_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,1] = ncvar_get(nc_file,"stemp")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_STemp_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,2] = ncvar_get(nc_file,"stemp")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_STemp_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,3] = ncvar_get(nc_file,"stemp")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_STemp_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,4] = ncvar_get(nc_file,"stemp")
  nc_close(nc_file)
  
  data_all_anom = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold anomalies at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_STemp_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,1] = ncvar_get(nc_file,"stemp_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_STemp_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,2] = ncvar_get(nc_file,"stemp_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_STemp_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,3] = ncvar_get(nc_file,"stemp_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_STemp_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,4] = ncvar_get(nc_file,"stemp_anomaly")
  nc_close(nc_file)
  
  data_all_var = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold variances at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_STemp_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,1] = ncvar_get(nc_file,"stemp_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_STemp_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,2] = ncvar_get(nc_file,"stemp_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_STemp_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,3] = ncvar_get(nc_file,"stemp_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_STemp_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,4] = ncvar_get(nc_file,"stemp_var")
  nc_close(nc_file)
  
  # Extract data at locations
  
  for (loc in 1:nlocations_land){
    
    for (scen in 1:nscenarios){
      
      data_loc_means[,loc,scen,memnumber]=data_all_means[loc_land[loc,1],loc_land[loc,2],,scen]
      data_loc_anom[,loc,scen,memnumber]=data_all_anom[loc_land[loc,1],loc_land[loc,2],,scen]
      data_loc_var[,loc,scen,memnumber]=data_all_var[loc_land[loc,1],loc_land[loc,2],,scen]
      
    }
    
  }
  
} # Close ensemble member loop

# At every location and for each scenario, calculate mean and median across all members (i.e. 1001 years by 4 locations by 4 scenarios, where each value is the mean/median across ensemble members)
# Only do this for anomalies for now, as these are what will be plotted

print("Calculating ensemble mean/median")

data_loc_anom_mean = array(0,c(nyears,nlocations_land,nscenarios))
data_loc_anom_med = array(0,c(nyears,nlocations_land,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations_land) {
    
    for (scen in 1:nscenarios) {
      
      data_loc_anom_mean[year,loc,scen]=mean(data_loc_anom[year,loc,scen,])
      data_loc_anom_med[year,loc,scen]=median(data_loc_anom[year,loc,scen,])
      
    }
    
  }
  
}

# Write out - where each file contains means, and anomalies and variances for a given location, scenario and ensemble member, as well as the ensemble mean/median (which will be the 
# same for each location and scenario)

print("Writing out")

ncol = 6

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file goes into a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      data_to_save = array(c((times), round(data_loc_means[,loc,scen,mem], digits = 4), round(data_loc_anom[,loc,scen,mem], digits = 4), round(data_loc_var[,loc,scen,mem], digits = 4), round(data_loc_anom_mean[,loc,scen], digits = 4), round(data_loc_anom_med[,loc,scen], digits = 4)), c(length(rev(times)),ncol))
      colnames(data_to_save) = c("Time_kyrAP", "SoilTemp_degC", "dSoilTemp_degC", "vSoilTemp_degC", "mean_dSoilTemp_degC", "med_dSoilTemp_degC")
      write.table(data_to_save, file = paste(datapath,"Em_output_data_stemp_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
      
    }
    
  }
  
}

################ Vegetation ###########################

print("Writing out vegetation")

# Create arrays to hold data at each individual location i.e. time by location by scenario by ensemble members.  This needs to be done outside ensemble member loop.
# Land locations only (of which there is only 1)

data_loc_means = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_anom = array(0,c(nyears,nlocations_land,nscenarios,nmembers))
data_loc_var = array(0,c(nyears,nlocations_land,nscenarios,nmembers))

for (memnumber in 1:nmembers) { # Begin ensemble member loop, to read through all 90 ensemble members
  
  print(members[memnumber])
  
  data_all_means = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold means at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Veg_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,1] = ncvar_get(nc_file,"veg")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Veg_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,2] = ncvar_get(nc_file,"veg")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Veg_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,3] = ncvar_get(nc_file,"veg")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_Veg_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,4] = ncvar_get(nc_file,"veg")
  nc_close(nc_file)
  
  data_all_anom = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold anomalies at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Veg_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,1] = ncvar_get(nc_file,"veg_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Veg_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,2] = ncvar_get(nc_file,"veg_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Veg_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,3] = ncvar_get(nc_file,"veg_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_Veg_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,4] = ncvar_get(nc_file,"veg_anomaly")
  nc_close(nc_file)
  
  data_all_var = array(0,c(nx,ny,nyears,nscenarios)) # Create array to hold variances at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Veg_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,1] = ncvar_get(nc_file,"veg_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Veg_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,2] = ncvar_get(nc_file,"veg_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Veg_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,3] = ncvar_get(nc_file,"veg_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_Veg_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,4] = ncvar_get(nc_file,"veg_var")
  nc_close(nc_file)
  
  # Extract data at locations
  
  for (loc in 1:nlocations_land){
    
    for (scen in 1:nscenarios){
      
      data_loc_means[,loc,scen,memnumber]=data_all_means[loc_land[loc,1],loc_land[loc,2],,scen]
      data_loc_anom[,loc,scen,memnumber]=data_all_anom[loc_land[loc,1],loc_land[loc,2],,scen]
      data_loc_var[,loc,scen,memnumber]=data_all_var[loc_land[loc,1],loc_land[loc,2],,scen]
      
    }
    
  }
  
} # Close ensemble member loop

# At every location and for each scenario, calculate mean and median across all members (i.e. 1001 years by 4 locations by 4 scenarios, where each value is the mean/median across ensemble members)
# Only do this for anomalies for now (means don't mean anything here), as these are what will be plotted

print("Calculating ensemble mean/median")

data_loc_anom_mean = array(0,c(nyears,nlocations_land,nscenarios))
data_loc_anom_med = array(0,c(nyears,nlocations_land,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations_land) {
    
    for (scen in 1:nscenarios) {
      
      data_loc_anom_mean[year,loc,scen]=mean(data_loc_anom[year,loc,scen,])
      data_loc_anom_med[year,loc,scen]=median(data_loc_anom[year,loc,scen,])
      
    }
    
  }
  
}

# Write out - where each file contains means, and anomalies and variances for a given location, scenario and ensemble member, as well as the ensemble mean/median (which will be the 
# same for each location and scenario)

print("Writing out")

ncol = 6

for (loc in 1:nlocations_land) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file goes into a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      data_to_save = array(c((times), round(data_loc_means[,loc,scen,mem], digits = 4), round(data_loc_anom[,loc,scen,mem], digits = 4), round(data_loc_var[,loc,scen,mem], digits = 4), round(data_loc_anom_mean[,loc,scen], digits = 4), round(data_loc_anom_med[,loc,scen], digits = 4)), c(length(rev(times)),ncol))
      colnames(data_to_save) = c("Time_kyrAP", "TotalLeafAreaIndex", "dTotalLeafAreaIndex", "vTotalLeafAreaIndex", "mean_dTotalLeafAreaIndex", "med_dTotalLeafAreaIndex")
      write.table(data_to_save, file = paste(datapath,"Em_output_data_veg_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
      
    }
    
  }
  
}

################ Wind speed ###########################

print("Writing out wind speed")

# Create arrays to hold data at each individual location i.e. time by location by scenario by ensemble members.  This needs to be done outside ensemble member loop.
# All locations

data_loc_means = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_anom = array(0,c(nyears,nlocations_all,nscenarios,nmembers))
data_loc_var = array(0,c(nyears,nlocations_all,nscenarios,nmembers))

for (memnumber in 1:nmembers) { # Begin ensemble member loop, to read through all 90 ensemble members
  
  print(members[memnumber])
  
  data_all_means = array(0,c(nx+1,ny-1,nyears,nscenarios)) # Create array to hold means at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_WSpeed_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,1] = ncvar_get(nc_file,"wspeed")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_WSpeed_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,2] = ncvar_get(nc_file,"wspeed")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_WSpeed_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,3] = ncvar_get(nc_file,"wspeed")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_means_WSpeed_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_means[,,,4] = ncvar_get(nc_file,"wspeed")
  nc_close(nc_file)
  
  data_all_anom = array(0,c(nx+1,ny-1,nyears,nscenarios)) # Create array to hold anomalies at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_WSpeed_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,1] = ncvar_get(nc_file,"wspeed_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_WSpeed_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,2] = ncvar_get(nc_file,"wspeed_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_WSpeed_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,3] = ncvar_get(nc_file,"wspeed_anomaly")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_anom_WSpeed_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_anom[,,,4] = ncvar_get(nc_file,"wspeed_anomaly")
  nc_close(nc_file)
  
  data_all_var = array(0,c(nx+1,ny-1,nyears,nscenarios)) # Create array to hold variances at every global location i.e. longitude by latitude by time by scenario
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_WSpeed_Global_natural_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,1] = ncvar_get(nc_file,"wspeed_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_WSpeed_Global_RCP2.6_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,2] = ncvar_get(nc_file,"wspeed_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_WSpeed_Global_RCP4.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,3] = ncvar_get(nc_file,"wspeed_var")
  nc_close(nc_file)
  
  nc_file = nc_open(paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",members[memnumber],"/Em_output_data_var_WSpeed_Global_RCP8.5_0_to_1MyrAP_cjrw.nc",sep=""))
  data_all_var[,,,4] = ncvar_get(nc_file,"wspeed_var")
  nc_close(nc_file)
  
  # Extract data at locations
  
  for (loc in 1:nlocations_all){
    
    for (scen in 1:nscenarios){
      
      data_loc_means[,loc,scen,memnumber]=data_all_means[loc_all[loc,1],loc_all[loc,2],,scen]
      data_loc_anom[,loc,scen,memnumber]=data_all_anom[loc_all[loc,1],loc_all[loc,2],,scen]
      data_loc_var[,loc,scen,memnumber]=data_all_var[loc_all[loc,1],loc_all[loc,2],,scen]
      
    }
    
  }
  
} # Close ensemble member loop

# At every location and for each scenario, calculate mean and median across all members (i.e. 1001 years by 4 locations by 4 scenarios, where each value is the mean/median across ensemble members)
# Only do this for anomalies for now, as these are what will be plotted

print("Calculating ensemble mean/median")

data_loc_anom_mean = array(0,c(nyears,nlocations_all,nscenarios))
data_loc_anom_med = array(0,c(nyears,nlocations_all,nscenarios))

for (year in 1:nyears) {
  
  for (loc in 1:nlocations_all) {
    
    for (scen in 1:nscenarios) {
      
      data_loc_anom_mean[year,loc,scen]=mean(data_loc_anom[year,loc,scen,])
      data_loc_anom_med[year,loc,scen]=median(data_loc_anom[year,loc,scen,])
      
    }
    
  }
  
}


# Write out - where each file contains means, and anomalies and variances for a given location, scenario and ensemble member, as well as the ensemble mean/median (which will be the 
# same for each location and scenario)

print("Writing out")

ncol = 6

for (loc in 1:nlocations_all) {
  
  for (scen in 1:nscenarios) {
    
    for (mem in 1:nmembers) {
      
      # Create path so that each text file goes into a separate directory according to ensemble member
      
      datapath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice0/",mem,"/",sep="")
      
      data_to_save = array(c((times), round(data_loc_means[,loc,scen,mem], digits = 4), round(data_loc_anom[,loc,scen,mem], digits = 4), round(data_loc_var[,loc,scen,mem], digits = 4), round(data_loc_anom_mean[,loc,scen], digits = 4), round(data_loc_anom_med[,loc,scen], digits = 4)), c(length(rev(times)),ncol))
      colnames(data_to_save) = c("Time_kyrAP", "WSpeed_m/s", "dWSpeed_m/s", "vWSpeed_m/s", "mean_dWSpeed_m/s", "med_dWSpeed_m/s")
      write.table(data_to_save, file = paste(datapath,"Em_output_data_wspeed_loc",loc,"_",scenarios[scen],"_mem",mem,"_0_to_1MyrAP_cjrw.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
      
    }
    
  }
  
}

print("Finished!")




