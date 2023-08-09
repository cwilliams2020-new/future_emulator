#!/usr/bin/env Rscript

# CW's KAERI version 5 of CW's KAERI version 1 (which itself = SKB emulator v8), Where now (25/5/23):
#  Does the same as version 4, but with virtually all of the file saving and plotting removed, instead writing out global emulated data as netcdf file for
#  every variable (not just temperature and precipitation), then loading data for the downscaling, before going to downscaling script.  All writing out 
#  (i.e. extracting timeseries from either emulated or downscaled data) is now down in in cjrw_locations_plotting.R

##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################

# Emulator script for the reconstruction of all climate variables ######
# modice+tdab emulator (gl) AND modlowice emulator (ig), calibrated on temp
# For RCP emissions scenarios (updated in response to SL) 

# Need to run this once the first time using the emulator:
# install.packages("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/Emulator/Packages/GP_0.1.4/GP",repos = NULL, type="source")
# Will not be able to load plotrix library, so separately (at command line), do install.packages("plotrix")
# install.packages("plotrix")

# Load libraries and setup workspace ########################################

.libPaths(c("C:/Users/cw18831/OneDrive - University of Bristol/Documents/R/win-library/4.1", .libPaths()))
setwd("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/Emulator/2015_Bristol_5D_v001/R")

print('Emulator_all_vars')

library(maps)
library(ggplot2)
library(viridis)
library(plyr)
library(plotrix)
library(gstat)
library(fields)
library(ncdf4)
library(colorspace)
library(gdata) # CW: This is needed for keep function

require(colorspace)
require(graphics)

noice = 0 # Flag for forcing emulator, downscaling etc. to avoid ice over the sites

# UNSURE WHAT THIS IS FOR
par_orig = par()

# Take second input argument at the variable to load and first input as the ensemble member to load
# Allow the Rscript function to take arguments (number corresponding to the CGSLM file name)
args = commandArgs(trailingOnly=TRUE)
paste("Loading variable",args[2],"for LHC ensemble member",args[1],sep=" ")
if(length(args)!=0){
  EmulVars = args[2]
  Sim = args[1]
}else{

  EmulVars = 'Temp' # Working (filled field)
#  EmulVars = 'Precip' # Working (filled field)
#  EmulVars = 'Evapotrans' # Working (filled field)
#  EmulVars = 'SIce' # Working (requires reversed LSM)
#  EmulVars = 'SM' # Working (requires LSM)
#  EmulVars = 'SnDepth' # Working (filled field)
#  EmulVars = 'STemp' # Working (requires LSM)
#  EmulVars = 'Veg' # Working (requires LSM)
#  EmulVars = 'WSpeed' # Working (filled field)
  
#  Sim = 'Corr' # 'Orig' or 'Corr' or 1-90 where 67 = corr

  Sim <- seq(1,90,by=1)

}

for (simnumber in 1:90) {

print(Sim[simnumber])
  
if (EmulVars == "Temp" || EmulVars == "Precip") {
  
  runemulator = 1
  runBC = 1
  runsaving = 1
  runstatsplots = 1
  
} else {
  
  runemulator = 1
  runBC = 0
  runsaving = 1
  runstatsplots = 0
  
}

# Set filepaths for output plots, data etc.  This has now been moved downwards, within location loop, so that output can be saved according to location


if (runemulator==1){


    
  ## Load and calibrate the correct emulator ########################################################
  source(file.path('Loading', paste("Emul_in_", EmulVars, ".R", sep="")))

    ## Setup data for emulator ########################################################################
  
  print("Starting to setup emulator")
  Sys.time()
  check1 = Sys.time()
  
  # Select whether or not to use the actual or the nearest grid box to each site.  CW: no longer needed
  # use_actual_GB = 1
  # use_actual_GB = 0

  # Load forcing data (either using Nat's original code, the corrected code or the full ensemble) and Load glacial state data
  source("Loading/forcings_1myr_AP.R")
  source("Loading/Rnum_1myr_AP.R")
  
  if (Sim[simnumber] == "Corr"){
    modelInputs <- loadForcing(Sim[simnumber]) # This loads all scenarios for the current LHC member
    data_input_Rnum_rcp_1myr_AP <- loadRnum(Sim[simnumber]) # This loads the ice sheet state for all scenarios
  } else {
    modelInputs <- loadForcing(as.numeric(Sim[simnumber])) # This loads all scenarios for the current LHC member
    data_input_Rnum_rcp_1myr_AP <- loadRnum(as.numeric(Sim[simnumber])) # This loads the ice sheet state for all scenarios
  }
  
  # Take just the ice sheet state for each scenario (i.e. leave off the dates)
  Rnum_rcp = data.frame(data_input_Rnum_rcp_1myr_AP[,2:dim(data_input_Rnum_rcp_1myr_AP)[2]])
  
  
  # Load some more data that should remain the same for all LHC members
  source("../Data/2018-08-01 Final report/Insol_Laskar_jul_65N_1myr_AP.R") # Orbital data
  nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice.qrparm.mask.nc")
  lsm_tdstb = ncvar_get(nc_file,"lsm")
  lons_orig = ncvar_get(nc_file,"longitude")
  nc_close(nc_file)
  source('../Data/Data - dTeq dT500/dTeq_dT500_veg_snp_srf_n9_ann_tdab.R') # Loads ice_surf_tdab_19kyr
  
  # UNSURE WHAT THIS IS FOR
  Exp_num_tdab = 26
  if (EmulVars == "Temp_noice"){
    ice_config = "_no_ice"
  } else { ice_config = ""}
  
  # Set some constants
  times = matrix(seq(0, 1000000, 1000)) # years for each future timeslice
  nx = 96 # number of lon grid cells
  
  if (EmulVars == "WSpeed"){ # CW: 73 latitudes for all variables except Wind speed, which = 72
    ny = 72
  } else { ny = 73}
  
  nx2 = nx/2 # used for centring Greenwich Meridian
  nt = length(times) # number of timeslices
  ns = 4 # number of emissions scenarios
  K = 273.15 # Kelvin-Celsius conversion
  
  
  ## Generate lat-lon grid and grid box area for calculating global weighted mean #####################
  lons = array(0:(nx-1))*3.75
  lons[1:(nx+1)] = c(lons[(nx2+1):nx]-360,lons[1:nx2],lons[nx2+1]) # Centre the Greenwich Meridian
  
  lats = array(0:(ny-1))*2.5-90
  lons_orig = lons_orig-180
  
  gb_lat_length = 2.5 # degrees
  gb_lat_length_pole = gb_lat_length/2 # degrees
  gb_lon_length = 3.75 # degrees
  
  lats_true = array(0,c((ny+1),1))
  lats_true[1,] = lats[1];
  lats_true[ny+1,] = lats[ny];
  lats_true[2,] = lats_true[1,]+gb_lat_length_pole;
  lats_true[ny,] = lats_true[ny+1,]-gb_lat_length_pole;
  
  for (ncol in 3:(ny-1)){lats_true[ncol,] = lats_true[ncol-1,]+gb_lat_length;}
  
  lons_true = array(0,c((nx+2),1))
  lons_true[1,] = lons[1];
  lons_true[2,] = lons[1]+(gb_lon_length/2);
  lons_true[nx+2,] = lons[nx+1];
  lons_true[nx+1,] = lons[nx+1]-(gb_lon_length/2);
  
  for (nrow in 3:nx){lons_true[nrow,] = lons_true[nrow-1,]+gb_lon_length;}
  
  # Convert latitude and longitude to radians
  lats_true_rad = lats_true*(pi/180);
  lons_true_rad = lons_true*(pi/180);
  
  # Calculate grid box areas manually
  gb_area_manual_split = array(0,c((nx+1),ny))
  gb_area_manual = array(0,c(nx,ny))
  
  for (ncol in 1:ny){
    for (nrow in 1:nx+1){
      gb_area_manual_split[nrow,ncol] = abs(sin(lats_true_rad[ncol,])-sin(lats_true_rad[ncol+1,]))*abs(lons_true_rad[nrow,]-lons_true_rad[nrow+1,])/(4*pi)
    }
  }
  
  gb_area_manual_split[1,] = gb_area_manual_split[(nx+1),]
  gb_area_manual_total = sum(sum(gb_area_manual_split))
  
  gb_area_manual[1,] = gb_area_manual_split[1,]+gb_area_manual_split[nx+1,]
  gb_area_manual[2:nx,] = gb_area_manual_split[2:nx,]
  
  
  ## Use pythag. to calculate eccentricity value (hypotenuse) ########################################
  eccentricity_val = modelInputs$RCP26
  colnames(eccentricity_val, do.NULL = TRUE, prefix = "col")
  colnames(eccentricity_val) = c("co2","obliquity","eccentricity","l. of perihelion","ice")
  
  for (i in 1:length(eccentricity_val[,1])) {
    eccentricity_val[i,3] = sqrt((modelInputs$RCP26[i,3]^2) + (modelInputs$RCP26[i,4]^2)) # Calculate Ecc
    eccentricity_val[i,4] = atan2(modelInputs$RCP26[i,3]*pi/180,modelInputs$RCP26[i,4]*pi/180) # Calculate Per
    eccentricity_val[i,4] = eccentricity_val[i,4]*180/pi
  }
  
  for (i in 1:length(eccentricity_val[,1])) {
    if (eccentricity_val[i,4] < 0) {
      eccentricity_val[i,4] = eccentricity_val[i,4] + 360; # Correct Per to be between 0 and 360 (from -180 to 180)
    }
  }
  
  
  ## Define the global PI SAT  #############################
  # used for converting anomalies to absolute
  model_output_tdst_big = array(0,c((nx+1),ny))
  model_output_tdst = model_output_tdst[,,1] 
  
  # Centre the Greenwich Meridian
  for (y in c(1:ny)){
    model_output_tdst_big[1:(nx+1),y] = c(model_output_tdst[(nx2+1):nx,y],model_output_tdst[1:nx2,y],model_output_tdst[(nx2+1),y])
  }
  
  # Define locations #################################################
  
  # Load data.  This needs to be done before the location loop.  See ~/R/locationfiles_README.txt for further details on what these files contain
  
  cont_paramdat = read.table(file.path('location_pairs_HadCM3res_testSK.res'), sep=" ") # Grid box numbers for 2 locations (lon,lat) at HadCM3 resolution
  location_pairs_HadCM3res = data.matrix(cont_paramdat)
  rm(list=ls()[(ls() %in% c('cont_paramdat','cont_param_dim'))])
  
  cont_paramdat = read.table(file.path('location_pairs_CRUres_testSK.res'), sep=" ") # Grid box numbers for 2 locations (lon,lat) at CRU resolution
  location_pairs_CRUres = data.matrix(cont_paramdat)
  rm(list=ls()[(ls() %in% c('cont_paramdat','cont_param_dim'))])
  
  cont_paramdat = read.table(file.path('location_regional_testSK.res'), sep=" ") # Grid box numbers for region, values: nx, ny, min lon, max lon, min lat, max lat, extra min lat, extra max lat
  location_region = data.matrix(cont_paramdat)
  rm(list=ls()[(ls() %in% c('cont_paramdat','cont_param_dim'))])
  
  cont_paramdat = read.table(file.path('location_regional_HadCM3res_testSK.res'), sep=" ") # Grid box numbers for region (HadCM3), values: min lon, max lon, min lat, max lat
  location_region_had = data.matrix(cont_paramdat)
  rm(list=ls()[(ls() %in% c('cont_paramdat','cont_param_dim'))])
  
  cont_paramdat = read.table(file.path('location_pairs_downBC_testSK.res'), sep=" ") # Grid box numbers  for 2 locations (lon,lat) used for bias correction downscaling
  location_pairs_downBC = data.matrix(cont_paramdat)
  rm(list=ls()[(ls() %in% c('cont_paramdat','cont_param_dim'))])

  cont_paramdat = read.table(file.path('location_pairs_downPS_testSK.res'), sep=" ") # Grid box numbers for 2 locations (lon,lat) used for physical-statistical downscaling
  location_pairs_downPS = data.matrix(cont_paramdat)
  rm(list=ls()[(ls() %in% c('cont_paramdat','cont_param_dim'))])

  cont_paramdat = read.table(file.path('location_figs_map1_testSK.res'), sep=" ") # Longitudes and latitudes for plotting (first spatial map, which here = Figure 4.5)
  location_figs_map1 = data.matrix(cont_paramdat)
  rm(list=ls()[(ls() %in% c('cont_paramdat','cont_param_dim'))])
  
  cont_paramdat = read.table(file.path('location_figs_map2_testSK.res'), sep=" ") # Longitudes and latitudes for plotting (second spatial map, which here = Figure 4.17)
  location_figs_map2 = data.matrix(cont_paramdat)
  rm(list=ls()[(ls() %in% c('cont_paramdat','cont_param_dim'))])

  location_pairs = 1
  
  for (loc_pairs in 1:location_pairs){    # Begin location loop.  Don't actually need this any more, because Firstly emulated data are being written out as
                                          # a global array, with timeseries extracted later, and secondly downscaling locations are taken from regional 
                                          # array, not individual locations.  But keep, set as 1:1, so can be changed if necessary.
    
# Setup pathnames for output, figures, etc
    
    if (Sim[simnumber] == "Corr"){
      resultfilepath = "C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/Plots/"
      dataoutfilepath = "C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/"
      datainfilepath = "C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Input/"
    } else { # CW: Included new else statement to accommodate ensemble member numbers, each in own directory
      resultfilepath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/Plots/noice",noice,"/",Sim[simnumber],"/",sep="")
      dataoutfilepath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/noice",noice,"/",Sim[simnumber],"/",sep="")
      datainfilepath = paste("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Input/noice",noice,"/",Sim[simnumber],"/",sep="")
      
    }
    
  # On model grid:
  # KAERI actual location = 127.38 lon (+180 = 307.38), 36.35 lat - nearest HadCM3 location = 127.5 (+180 = 307.5), 37.5, which is grid box 83,22 (because data has been centred on GM, so 
  # grid box 83 = 127.5 + 180 = 307.5). No need to worry about nearest GB (either modice or lowice), because South Korea (and indeed North Korea) is only one GB and is surrounded by sea 
  # apart from further inland e.g. and China, i.e. (North/South Korea is in middle, China is above and top left).  Both pairs will be the same and will never change.
  #   1	1	0
  #   0	1	0
  #   0	0	0  
  
  Forsmark_lon_GB = location_pairs_HadCM3res[loc_pairs+loc_pairs-1,1]
  Forsmark_lat_GB = location_pairs_HadCM3res[loc_pairs+loc_pairs-1,2]
  Olkiluoto_lon_GB = location_pairs_HadCM3res[loc_pairs+loc_pairs,1]
  Olkiluoto_lat_GB = location_pairs_HadCM3res[loc_pairs+loc_pairs,2]
  
  # On CRU grid:
  # KAERI actual location = 127.38 lon (+180 = 307.38), 36.35 lat - nearest CRU location = 127.25 (+180 = 307.25), 36.25, which is grid box 615, 253 (because data is already centred on GM, so grid box 615 = 127.25 + 180 = 307.25)
  # For other locations in South Korea, could be any one of 51 GB at CRU resolution
  # The next 4 variable names are used both here and throughout downscaling code. Both pairs will be the same and will never change (because downscaling will be taken from regional array).
  
  Forsmark_lon_GB_CRU = location_pairs_CRUres[loc_pairs+loc_pairs-1,1]
  Forsmark_lat_GB_CRU = location_pairs_CRUres[loc_pairs+loc_pairs-1,2]
  Olkiluoto_lon_GB_CRU = location_pairs_CRUres[loc_pairs+loc_pairs,1]
  Olkiluoto_lat_GB_CRU = location_pairs_CRUres[loc_pairs+loc_pairs,2]
  
  print(location_pairs_CRUres[loc_pairs+loc_pairs-1,1])
  print(location_pairs_CRUres[loc_pairs+loc_pairs-1,2])
  print(location_pairs_CRUres[loc_pairs+loc_pairs,1])
  print(location_pairs_CRUres[loc_pairs+loc_pairs,2])
  
  
  # For selecting South Korea using CRU (126.25-129.75 lon (which = 306.25-309.75), 33.25-37.25 lat. This is GB 613-620 lon, 247-255 lat, but will be different in this file, because 
  # later and in downscaling code, these numbers are subtracted or added from nx_cru (720), ny_cru (360) or nx2_cru (nx_cru/2 = 360). So (using South Korea example) 
  # lons = nx2_cru+min,nx_cru-max (i.e. 360+253 = 613 and 720-100 = 620) and lats = ny_cru-min,ny_cru-max (i.e. 360-113 = 247 and 360-105 = 255).  
  # So for lons GB 613 = 306.25 (or, if -180, 126.25) and GB 620 = 309.75 (or, if -180, 129.75), then for lats GB 247 = 33.25 and GB 255 = 37.25.  This explains nx and ny, because 
  # 620-613=7 (then add 1 to include both limits in the array, which = 8) and 255-247=8 (then add 1 to include both limits in the array, which = 9). So area becomes: 8 9 253 100 113 105 114 106
  
  # For selecting entire Korean Peninsular using CRU (124.75-129.75 lon (which = 304.75-309.75), 33.25-39.75 lat. This is GB 611-620 lon, 247-260 lat, but will be different in this file, because 
  # later and in downscaling code, these numbers are subtracted or added from nx_cru (720), ny_cru (360) or nx2_cru (nx_cru/2 = 360).  So (using entire Korean Peninsular example) 
  # lons = nx2_cru+min,nx_cru-max (i.e. 360+250 = 610 and 720-100 = 620) and lats = ny_cru-min,ny_cru-max (i.e. 360-113 = 247 and 360-100 = 260).  
  # So for lons GB 610 = 304.75 (or, if -180, 124.75) and GB 620 = 309.75 (or, if -180, 129.75), then for lats GB 247 = 33.25 and GB 260 = 39.25.  This explains nx and ny, because 
  # 620-610=10 (then add 1 to include both limits in the array, which = 11) and 260-247=13 (then add 1 to include both limits in the array, which = 14).  So area becomes: 11 14 250 100 113 100 114 101

  # The next 6 variable names are used both here and throughout downscaling code
  
  # CW: for unknown reasons, precipitation downscaling uses nx_cru_adj_scand and nx_cru_orig_scand interchangeably, both of which are the same, 
  # so create another array identical to former one but with latter name
  
  nx_cru_adj_scand = location_region[loc_pairs,1] 
  nx_cru_orig_scand = location_region[loc_pairs,1]
  ny_cru_scand = location_region[loc_pairs,2]
  lons_cru_orig_scand_min = location_region[loc_pairs,3]
  lons_cru_orig_scand_max = location_region[loc_pairs,4]
  lats_cru_scand_min = location_region[loc_pairs,5]
  lats_cru_scand_max = location_region[loc_pairs,6]
  lats_cru_orig_scand_min = location_region[loc_pairs,7] #ATKA - Not sure why there is a different area used sometimes.  CW: this is only used throughout downscaling code, but is 1 larger than lats
  lats_cru_orig_scand_max = location_region[loc_pairs,8] #ATKA - Not sure why there is a different area used sometimes.  CW: this is only used throughout downscaling code, but is 1 larger than lats
  
  # For selecting South Korea using HadCM3 - This will be the same as above, because South Korea is only one grid box (i.e. 127.5 (+180 = 307.5), 37.5, which is grid box 83,22
  
  lons_had_min = location_region_had[loc_pairs,1]
  lons_had_max = location_region_had[loc_pairs,2]
  lats_had_min = location_region_had[loc_pairs,3]
  lats_had_max = location_region_had[loc_pairs,4]
  
  # Note that for both of these, they are picked out of above (already sub-setted) box, not out of entire CRU array. So (using South Korea example), KAERI lon = GB 615, which is 2 
  # grid boxes from the above smaller array, i.e. 613+3 = 616, which = 306.25+3 grid boxes (which is the same as 1.5°) = 307.75 (or, if using -180, = 127.75).  
  # Likewise KAERI lat = GB 253, which is 7 grid boxes from the above smaller array i.e. 247+7 = 254, which = 33.25+7 grid boxes (which is the same as 3.5°) = 36.75.  
  # Note that this (i.e. GB 616,254 or 127.75,36.75) is 1 GB different (further East and further North) than location defined above (i.e. GB 615,253 or 127.25,36.25), to include both
  # limits in the array e.g. 613+3=616, but this is actually 615 because you are counting 613 itself).  So answer becomes 3,7.
  
  Forsmark_lon_GB_CRU_scand = location_pairs_downBC[loc_pairs+loc_pairs-1,1] # CW: this is only used throughout downscaling code (bias correction)
  Forsmark_lat_GB_CRU_scand = location_pairs_downBC[loc_pairs+loc_pairs-1,2]
  Olkiluoto_lon_GB_CRU_scand = location_pairs_downBC[loc_pairs+loc_pairs,1] 
  Olkiluoto_lat_GB_CRU_scand = location_pairs_downBC[loc_pairs+loc_pairs,2]
  
  Forsmark_lon_GB_CRU_scand2 = location_pairs_downPS[loc_pairs+loc_pairs-1,1] # CW: this is only used throughout downscaling code (physical-statistical)
  Forsmark_lat_GB_CRU_scand2 = location_pairs_downPS[loc_pairs+loc_pairs-1,2] 
  Olkiluoto_lon_GB_CRU_scand2 = location_pairs_downPS[loc_pairs+loc_pairs,1]
  Olkiluoto_lat_GB_CRU_scand2 = location_pairs_downPS[loc_pairs+loc_pairs,2]
  
  # Longitudes and latitudes for plotting (first spatial map, which here = Figure 4.5, zooming out to cover a larger area)
  
  lons_map1_min = location_figs_map1[loc_pairs,1]
  lons_map1_max = location_figs_map1[loc_pairs,2]
  lats_map1_min = location_figs_map1[loc_pairs,3]
  lats_map1_max = location_figs_map1[loc_pairs,4]

  # Longitudes and latitudes for plotting (second spatial map, which here = Figure 4.17, zooming in)
  
  lons_map2_min = location_figs_map2[loc_pairs,1]
  lons_map2_max = location_figs_map2[loc_pairs,2]
  lats_map2_min = location_figs_map2[loc_pairs,3]
  lats_map2_max = location_figs_map2[loc_pairs,4]
  
  tdstb_sweden = model_output_tdst_big[Forsmark_lon_GB,Forsmark_lat_GB]
  tdstb_finland = model_output_tdst_big[Olkiluoto_lon_GB,Olkiluoto_lat_GB]

  # Load CRU climate data for bias correction later ##################################################
  # Import CRU gridded climatology data (v2.1)
  if (EmulVars == "Temp"){
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/clim.6190.lan.tmp.nc")
    cru_monthly = ncvar_get(nc_file,"tmp")
    lats_cru = ncvar_get(nc_file,"lat")
    lons_cru_orig = ncvar_get(nc_file,"lon")
    nc_close(nc_file)
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_temp_mm_1_5m_interp.nc")
    temp_tdstb_interp_orig = ncvar_get(nc_file,"temp_mm_1_5m")
    nc_close(nc_file)
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_temp_mm_1_5m_interp_smooth.nc")
    temp_tdstb_interp_smooth_orig = ncvar_get(nc_file,"temp_mm_1_5m")
    nc_close(nc_file)
    
    # Convert SAT to Deg C
    temp_tdstb_interp_orig = temp_tdstb_interp_orig - K
    temp_tdstb_interp_smooth_orig = temp_tdstb_interp_smooth_orig - K
  }
  if (EmulVars == "Precip"){
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/clim.6190.lan.pre.nc")
    cru_monthly = ncvar_get(nc_file,"pre")
    lats_cru = ncvar_get(nc_file,"lat")
    lons_cru_orig = ncvar_get(nc_file,"lon")
    nc_close(nc_file)
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_precip_mm_srf_interp.nc")
    precip_tdstb_interp_orig = ncvar_get(nc_file,"precip_mm_srf")
    nc_close(nc_file)
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_precip_mm_srf_interp_smooth.nc")
    precip_tdstb_interp_smooth_orig = ncvar_get(nc_file,"precip_mm_srf")
    nc_close(nc_file)
  }
  
  if (EmulVars == "Temp" || EmulVars == "Precip"){
    # Convert CRU Data from monthly means to annual
    nx_cru = 720
    ny_cru = 360
    nx2_cru = nx_cru/2
    
    cru_ann = array(0,c(nx_cru,ny_cru))
    
    for (y in c(1:ny_cru)) {
      for (x in c(1:nx_cru)) {
        cru_ann[x,y] = mean(cru_monthly[x,y,1:12])
      }
    }
    
    cru = cru_ann
    lons_cru_orig = lons_cru_orig-180
    
    # Extract CRU data just for sites
    cru_sweden = array(cru[Forsmark_lon_GB_CRU,Forsmark_lat_GB_CRU])
    cru_finland = array(cru[Olkiluoto_lon_GB_CRU,Olkiluoto_lat_GB_CRU])

    # Calculate anomaly (absolute difference; bias) between observed (CRU) and modelled (tdstb) pre-industrial temperature
    if (EmulVars == "Temp"){
      PI_anom_tdstb_cru_sweden_a_site_only = tdstb_sweden - cru_sweden
      PI_anom_tdstb_cru_finland_a_site_only = tdstb_finland - cru_finland
    }
    if (EmulVars == "Precip"){
      PI_anom_tdstb_cru_sweden_a_site_only = tdstb_sweden / cru_sweden
      PI_anom_tdstb_cru_finland_a_site_only = tdstb_finland / cru_finland
    }
  }
  
  print("Starting to run emulator")
  check2 = Sys.time()
  print(check2-check1)
  
  # START LOOP GOING THROUGH EACH SCENARIO ###########################################################
  # Pre-allocate some variables which are needed beyond the loop
  my_tim_emissions_e_truemean = array(0,c(nt,ns))
  scenario_means_orig = array(0,c(nx,ny,nt,ns))
  my_tim_sweden_emissions_e = array(0,c(nt,ns))
  my_tim_anom_sweden_emissions_e = array(0,c(nt,ns))
  my_var_anom_sweden_emissions_e = array(0,c(nt,ns))
  my_tim_finland_emissions_e = array(0,c(nt,ns))
  my_tim_anom_finland_emissions_e = array(0,c(nt,ns))
  my_var_anom_finland_emissions_e = array(0,c(nt,ns))
  
  
  
  for (scen in 1:ns){
    # Unpacking the current scenario from modelInputs from their list format
    model_input = modelInputs[scen]
    
    # Scenario_... are okay to change - only used in this section of script
    Scenario_emissions = data.frame(model_input)
    
    # set CO2 to be log (natural) of CO2
    Scenario_emissions[,1] = log(Scenario_emissions[,1])
    
    #ATKA - for noice runs, include the following:
    if (noice == 1){
      
      ice_coverage_lower = -52.59 #-72.59 + 20
      
      for (n in 1:dim(Scenario_emissions)[1]){
        if (Scenario_emissions[n,5] < ice_coverage_lower){ # ASSUME ICE AT SITES
          Scenario_emissions[n,5] = ice_coverage_lower
        }
      }
    }
    #ATKA - end insertion
    
    # XScenario_...
    # rescaled to make it compatible with the 'X' used to calibrate the emulator
    XScenario_emissions = sweep(Scenario_emissions,  2, attr(X_all, "scaled:center"), '-')
    XScenario_emissions = sweep(XScenario_emissions, 2, attr(X_all, "scaled:scale"), '/')
    
    
    ## Run emulator using input data for next 1 Myr ################################################
    
    # run emulator (feed every row of XScenario into emulator, put this in a list,
    # and repack this into an array.
    
    OUT_gl_emissions = lapply(seq(nrow(XScenario_emissions)), function(i) pe_p((XScenario_emissions[i,, drop=FALSE]) , E_gl ) )
    OUT_ig_emissions = lapply(seq(nrow(XScenario_emissions)), function(i) pe_p((XScenario_emissions[i,, drop=FALSE]) , E_ig ) )
    
    # rm(list=ls()[(ls() %in% c('E_gl', 'E_ig'))]) # Is this line needed? It removes the emulator info that is required for the second, third and fourth iterations
    
    # extract mean and variance
    scenario_means_gl_emissions_anom = simplify2array( lapply(OUT_gl_emissions, function(i) i$mean))
    scenario_means_ig_emissions_anom = simplify2array( lapply(OUT_ig_emissions, function(i) i$mean))
    scenario_var_gl_emissions_anom = simplify2array( lapply(OUT_gl_emissions, function(i) i$var))
    scenario_var_ig_emissions_anom = simplify2array( lapply(OUT_ig_emissions, function(i) i$var))
    
    rm(list=ls()[(ls() %in% c('OUT_gl_emissions', 'OUT_gl_rcp26', 'OUT_gl_rcp45', 'OUT_gl_rcp6', 'OUT_gl_rcp85'))])
    rm(list=ls()[(ls() %in% c('OUT_ig_emissions', 'OUT_ig_rcp26', 'OUT_ig_rcp45', 'OUT_ig_rcp6', 'OUT_ig_rcp85'))])
    
    
    # attach times as an attribute
    attr(scenario_means_gl_emissions_anom, 'times') = times
    attr(scenario_means_ig_emissions_anom, 'times') = times
    attr(scenario_var_gl_emissions_anom, 'times') = times
    attr(scenario_var_ig_emissions_anom, 'times') = times
    
    
    # Extract data depending on glacial state to create full timeseries
    glacial_state_emissions = array(0,c(length(Scenario_emissions[,1]),1))
    
    for (n in 1:length(Scenario_emissions[,1])){
      if (Scenario_emissions[n,5] < 0){ # GLACIAL
        glacial_state_emissions[n,1] = 0
      } else { # INTERGLACIAL
        glacial_state_emissions[n,1] = 1
      }
    }
    
    scenario_means_emissions_anom = array(0,c(dim(scenario_means_gl_emissions_anom)[1],dim(scenario_means_gl_emissions_anom)[2],dim(scenario_means_gl_emissions_anom)[3]))
    scenario_var_emissions_anom = array(0,c(dim(scenario_var_gl_emissions_anom)[1],dim(scenario_var_gl_emissions_anom)[2],dim(scenario_var_gl_emissions_anom)[3]))
    
    for (n in 1:dim(scenario_means_gl_emissions_anom)[3]){
      if (glacial_state_emissions[n,1] == 0){ # GLACIAL
        #  message(n, " Glacial")
        scenario_means_emissions_anom[,,n] = scenario_means_gl_emissions_anom[,,n]
        scenario_var_emissions_anom[,,n] = scenario_var_gl_emissions_anom[,,n]
      } else { # INTERGLACIAL
        #  message(n, " Interglacial")
        scenario_means_emissions_anom[,,n] = scenario_means_ig_emissions_anom[,,n]
        scenario_var_emissions_anom[,,n] = scenario_var_ig_emissions_anom[,,n]
      }
    }
    
    rm(list=ls()[(ls() %in% c('scenario_means_gl_emissions_anom', 'scenario_means_ig_emissions_anom', 'scenario_var_gl_emissions_anom', 'scenario_var_ig_emissions_anom'))])
    
    ## Format results from emulator ################################################################
    
    # Calculate true global mean annual temperature evolution for next 1 Myr ------------------------
    # Calculate true global mean annual temperature (anom compared to tdstb)
    
    # These variables (e.g. my_tim_emissions_e_truemean) are used in plotting the global mean later on for Figure 4-1 (# FIGURE 20 - Plot evolution of global mean annual temperature anomaly for 1 Myr AP ---------------------------------------------------------)
    # Therefore, the following has been moved outside of the loop so it can be pre-allocated: my_tim_emissions_e_truemean = array(0,c(nt,1))
    
    for (y in 1:nt){
      my_tim_emissions_e_truemean[y,scen] = sum(sum((scenario_means_emissions_anom[,,y]*gb_area_manual[,])))
    }
    
    
    # Unit corrections where necessary
    if (EmulVars == "Precip"){
      if (scen == 1){
        
        model_output_tdst[,] = model_output_tdst[,]*60*60*24*30 #ATKA - changed index from [,,] to [,]
        model_output_tdst_big[,] = model_output_tdst_big[,]*60*60*24*30 #ATKA - changed index from [,,] to [,]
      }
      scenario_var_emissions_anom = scenario_var_emissions_anom*30
      scenario_means_emissions_anom = scenario_means_emissions_anom*30
    }
    if (EmulVars == "Evapotrans"){
      if (scen == 1){
        model_output_tdst[,] = model_output_tdst[,]*30 #CW - changed index from [,,] to [,]
      }
      scenario_var_emissions_anom = scenario_var_emissions_anom*30
      scenario_means_emissions_anom = scenario_means_emissions_anom*30
    }
    if (EmulVars == "SnDepth"){
      if (scen == 1){
        model_output_tdst[,] = model_output_tdst[,]/1000 #CW - changed index from [,,] to [,]
      }
      scenario_var_emissions_anom[scenario_var_emissions_anom < 0] = 0
      scenario_means_emissions_anom[scenario_means_emissions_anom < 0] = 0
    }
    if (EmulVars == "STemp"){
      if (scen == 1){
        model_output_tdst = model_output_tdst - K
      }
    }
    # if (EmulVars == "Temp"){
    #   model_output_tdst = model_output_tdst - K
    # }
    if (EmulVars == "Veg"){
      if (scen == 1){
        model_output_tdst[,] = model_output_tdst[,]/1000 #CW - changed index from [,,] to [,]
      }
      scenario_var_emissions_anom[scenario_var_emissions_anom < 0] = 0
      scenario_means_emissions_anom[scenario_means_emissions_anom < 0] = 0
    }
    
    
    # Centre Greenwich Meridian for mapping ------------------------
    # CW: This changes array to be firstly Eurocentric (i.e. Europe/Africa in the middle) and secondly to be slightly larger (hence "big") in longitudes/rows (97 instead of 96)
    # This is where the anomalies come from, so need to use "big" arrays
    
    scenario_means_emissions_big_anom = array(0,c((nx+1),ny,nt))
    for (e in c(1:nt)){
      for (y in c(1:ny)){
        scenario_means_emissions_big_anom[1:(nx+1),y,e] = c(scenario_means_emissions_anom[(nx2+1):nx,y,e],scenario_means_emissions_anom[1:nx2,y,e],scenario_means_emissions_anom[(nx2+1),y,e])
      }
    }
    
    scenario_var_emissions_big_anom = array(0,c((nx+1),ny,nt))
    for (e in c(1:nt)){
      for (y in c(1:ny)){
        scenario_var_emissions_big_anom[1:(nx+1),y,e] = c(scenario_var_emissions_anom[(nx2+1):nx,y,e],scenario_var_emissions_anom[1:nx2,y,e],scenario_var_emissions_anom[(nx2+1),y,e])
      }
    }
    
    
    # Calculate actual global annual mean (add pre-industrial back on) ------------------------
    # There are differences in corrections carried out here depending on what variable is being calculated
    #ATKA - MAKE SURE THIS IS UPDATED CORRECTLY FOR EACH VARIABLE
    
    if (EmulVars == "SIce"){
      #model_output_tdst_zero = model_output_tdst
      #model_output_tdst_big_zero = model_output_tdst_big
      
      #model_output_tdst_zero[is.na(model_output_tdst)] = 0
      #model_output_tdst_big_zero[is.na(model_output_tdst_big)] = 0
      
      model_output_tdst[is.na(model_output_tdst)] = 0
      model_output_tdst_big[is.na(model_output_tdst_big)] = 0
    }
    
    #ATKA - Somewhere here a LSM needs applied for variables: Veg, SM, STemp and SIce. See ~line 500 in original scripts for those variables
    
    
    scenario_means_emissions_big = array(0,c((nx+1),ny,nt))
    
    for (e in c(1:nt)){
      scenario_means_orig[,,e,scen] = scenario_means_emissions_anom[,,e]+model_output_tdst
    }
    
    for (e in c(1:nt)){
      scenario_means_emissions_big[,,e] = scenario_means_emissions_big_anom[,,e]+model_output_tdst_big
    }
    
    # Extract site data
    my_tim_sweden_emissions_e[,scen] = matrix(scenario_means_emissions_big[Forsmark_lon_GB,Forsmark_lat_GB,])
    my_tim_anom_sweden_emissions_e[,scen] = matrix(scenario_means_emissions_big_anom[Forsmark_lon_GB,Forsmark_lat_GB,])
    my_var_anom_sweden_emissions_e[,scen] = scenario_var_emissions_big_anom[Forsmark_lon_GB,Forsmark_lat_GB,]
    my_tim_finland_emissions_e[,scen] = matrix(scenario_means_emissions_big[Olkiluoto_lon_GB,Olkiluoto_lat_GB,])
    my_tim_anom_finland_emissions_e[,scen] = matrix(scenario_means_emissions_big_anom[Olkiluoto_lon_GB,Olkiluoto_lat_GB,])
    my_var_anom_finland_emissions_e[,scen] = scenario_var_emissions_big_anom[Olkiluoto_lon_GB,Olkiluoto_lat_GB,]
    
    #ATKA - I am not sure if it better to add another dimension to scenario_means_emissions_anom for use later or to reinstate variable names. For now, I have done the latter:
    # Reinstate variables for each scenario - used for saving data later
    
    # CW: Inserted new section, as below (for anomalies) to deal with mean values, needed for sea ice only
    if (scen == 1){scenario_means_zero_emissions_big = scenario_means_emissions_big} # CW: These are the mean values coming out of the emulator, using "big" array
    else if (scen == 2){scenario_means_rcp26_big = scenario_means_emissions_big}
    else if (scen == 3){scenario_means_rcp45_big = scenario_means_emissions_big}
    else {scenario_means_rcp85_big = scenario_means_emissions_big}

    if (scen == 1){scenario_means_zero_emissions_big_anom = scenario_means_emissions_big_anom} # CW: These are the anomalies coming out of the emulator, using "big" array
    else if (scen == 2){scenario_means_rcp26_big_anom = scenario_means_emissions_big_anom}
    else if (scen == 3){scenario_means_rcp45_big_anom = scenario_means_emissions_big_anom}
    else {scenario_means_rcp85_big_anom = scenario_means_emissions_big_anom}
    
    # CW: Inserted new section, as above, to deal with uncertainty
    if (scen == 1){scenario_var_zero_emissions_big_anom = scenario_var_emissions_big_anom} # CW: These are the variances coming out of the emulator, using "big" array
    else if (scen == 2){scenario_var_rcp26_big_anom = scenario_var_emissions_big_anom}
    else if (scen == 3){scenario_var_rcp45_big_anom = scenario_var_emissions_big_anom}
    else {scenario_var_rcp85_big_anom = scenario_var_emissions_big_anom}
    
  } # End of the scenario loop for the emulator
  
  print("Emulator finished")
  check3 = Sys.time()
  print(check3-check2)
}

## FIGURE 4-1 - Plot evolution of global mean annual temperature anomaly for 1 Myr AP - see version 3

if (runBC==1){
  ## Load in data for bias correction #####################################################################
  # Only run emulator if calculating temperature or precipitation
  if (EmulVars == "Temp" || EmulVars == "Precip"){
    
    print("Loading data for bias correction")
    check4 = Sys.time()
    
    # Import CRU gridded data (v2.1)
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/cru-elevation.nc") # CRU orography (not Eurocentric)
    orog_cru = ncvar_get(nc_file,"elv")
    orog_cru_orig = orog_cru
    nc_close(nc_file)
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice_orog.nc") # HadCM3 orography using modern sea level/ice
    orog_modice_orig = ncvar_get(nc_file,"ht")
    nc_close(nc_file)
    orog_modice = array(0,c(nx,ny))
    n = ny_cru
    for (y in c(1:ny)){
      orog_modice[1:(nx),y] = c(orog_modice_orig[(nx2+1):nx,y],orog_modice_orig[1:nx2,y]) # Flips longitudes
      n = n-1
    }
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice_orog_interp.nc") # HadCM3 orography using modern sea level/ice, interpolated onto CRU resolution (blocky)
    orog_modice_interp_orig = ncvar_get(nc_file,"ht")
    nc_close(nc_file)
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice_orog_interp_smooth.nc") # HadCM3 orography using modern sea level/ice, interpolated onto CRU resolution (smoothed)
    orog_modice_interp_smooth_orig = ncvar_get(nc_file,"ht")
    nc_close(nc_file)
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_q_mm_1_5m_interp_smooth.nc") # HadCM3 1.5m q, interpolated onto CRU resolution (smoothed)
    q_tdstb_interp_smooth_orig = ncvar_get(nc_file,"q_mm_1_5m")
    nc_close(nc_file)
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_u_mm_p_850_interp_smooth.nc") # HadCM3 850mb u, interpolated onto CRU resolution (smoothed)
    u_850_tdstb_interp_smooth_orig = ncvar_get(nc_file,"u_mm_p")
    nc_close(nc_file)
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_v_mm_p_850_interp_smooth.nc") # HadCM3 850mb v, interpolated onto CRU resolution (smoothed)
    v_850_tdstb_interp_smooth_orig = ncvar_get(nc_file,"v_mm_p")
    nc_close(nc_file)
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_p_mm_msl_interp_smooth.nc") # HadCM3 MSLP, interpolated onto CRU resolution (smoothed)
    mslp_tdstb_interp_smooth_orig = ncvar_get(nc_file,"p_mm_msl")
    nc_close(nc_file)
    
    # Convert SAT to R interpolated data
    bilinterp_data <- list( x= 1:96, y=1:73, z= model_output_tdst)
    bilinterp_lon <- seq( 1,96,,720)
    bilinterp_lat <- seq( 1,73,,360)
    make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid
    interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
    bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))
    #image.plot( as.surface( loc1, bilinterp_new))
    # image.plot(bilinterp_newdata[,c(ny_cru:1)])
    
    if (EmulVars == "Temp"){
      temp_tdstb_interp_smooth_orig_cdo=array(temp_tdstb_interp_smooth_orig,c(nx_cru,ny_cru))
      temp_tdstb_interp_smooth_orig=array(bilinterp_newdata,c(nx_cru,ny_cru))
      
      # I think this reverses the dataset
      n = ny_cru
      for (y in c(1:ny_cru)){
        for (x in c(1:nx_cru)){
          temp_tdstb_interp_smooth_orig[x,y] = bilinterp_newdata[x,n]
        }
        n = n - 1
      }
      
      # image.plot(temp_tdstb_interp_smooth_orig)
      # image.plot(temp_tdstb_interp_smooth_orig_cdo)
    }
    
    # Reformat CRU data so it's consistent with Hadley
    
    orog_cru = array(0,c(nx_cru,ny_cru))
    temp_cru = array(0,c(nx_cru,ny_cru))
    
    # Again, I think this reverses the dataset
    n = ny_cru
    for (y in c(1:ny_cru)){
      orog_cru[1:nx_cru,y] = orog_cru_orig[(1:nx_cru),n]
      n = n-1
    }
    
    n = ny_cru
    for (y in c(1:ny_cru)){
      temp_cru[1:nx_cru,y] = cru_ann[(1:nx_cru),n]
      n = n-1
    }
    
    lons_cru_orig = lons_cru_orig-180
    
    
    
    # Regrid HadCM3 PI data to same format as CRU
    
    model_output_tdstb_regrid = array(0,c(nx,ny))
    n = ny
    for (y in c(1:ny)){
      model_output_tdstb_regrid[1:nx,y] = c(model_output_tdst[(nx2+1):nx,n],model_output_tdst[1:nx2,n])
      n = n-1
    }
    
    # Bilinearly interp HadCM3 PI data to CRU grid
    
    bilinterp_lon <- seq( 1,96,,720)
    bilinterp_lat <- seq( 1,73,,360)
    make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid
    
    
    bilinterp_data <- list( x= 1:96, y=1:73, z= model_output_tdstb_regrid[,])
    interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
    bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))
    
    temp_tdstb_interp_smooth_orig = array(NaN,c(nx_cru,ny_cru))
    temp_tdstb_interp_smooth_orig[,] = bilinterp_newdata
    
    
    # Assign grid boxes with no orography data (LT 0) as missing values
    
    cru_ann[which(orog_cru<=0,arr.ind=TRUE)] = NaN
    orog_cru[which(orog_cru<=0,arr.ind=TRUE)] = NaN
    cru_ann[is.na(orog_cru)] = NaN
    orog_cru[is.na(orog_cru)] = NaN
    temp_tdstb_interp_smooth_orig[which(is.na(orog_cru))] = NaN
    
    
    # Select Scandinavia (lat 52-72, lon 3-31)
    lons_cru_orig_scand = lons_cru_orig[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max)]
    lats_cru_scand = lats_cru[(ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)]
    
    temp_tdstb_interp_smooth_orig_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
    temp_tdstb_interp_smooth_orig_scand[,] = temp_tdstb_interp_smooth_orig[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max))]
    
    # Calculate anomaly (absolute difference; bias) between observed (CRU) and modelled (tdstb) pre-industrial temperature
    
    temp_PI_anom_tdstb_cru_interp_smooth = array(NaN,c(nx_cru,ny_cru,1))
    if (EmulVars == "Temp"){
      temp_PI_anom_tdstb_cru_interp_smooth[,,1] = temp_tdstb_interp_smooth_orig - cru_ann
    } else {temp_PI_anom_tdstb_cru_interp_smooth[,,1] = temp_tdstb_interp_smooth_orig / cru_ann}
    
    # Import orography data (Singarayer + Valdes, and Peltier 1deg)
    .
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/lowice_orog_interp_smooth.nc")
    orog_lowice_interp_smooth_orig = ncvar_get(nc_file,"ht")
    nc_close(nc_file)
    
    orog_tdab_interp_smooth_orig = array(0,c(nx_cru,ny_cru,Exp_num_tdab))
    tdab_exp_nam_list = letters[seq( from = 1, to = 26 )]
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/qrparm.orog_tdab28k_interp_smooth.nc")
    for (num_exp in c(1:Exp_num_tdab)){
      tdab_exp_nam = paste("tdab",tdab_exp_nam_list[num_exp],sep="")
      orog_tdab_interp_smooth_orig[,,num_exp] = ncvar_get(nc_file,tdab_exp_nam)
    }
    nc_close(nc_file)
    
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/iceht.nc")
    orog_peltier_1deg_all_orig = ncvar_get(nc_file,'orogo')
    orog_peltier_1deg_all_times = ncvar_get(nc_file,'time')
    lons_peltier = ncvar_get(nc_file,'longitude_1')
    lats_peltier = ncvar_get(nc_file,'latitude_1')
    nc_close(nc_file)
    
    nx_peltier = length(lons_peltier)
    ny_peltier = length(lats_peltier)
    nx2_peltier = nx_peltier/2
    
    tdab_times = c(seq(1,22),24,26)  # removing last (114 m) tdab
    
    orog_peltier_1deg_orig = array(0,c(nx_peltier,ny_peltier,length(tdab_times)))
    
    for (num_exp in c(1:length(tdab_times))){
      loc_peltier = which.min(abs(tdab_times[num_exp] - orog_peltier_1deg_all_times))
      orog_peltier_1deg_orig[,,num_exp] = orog_peltier_1deg_all_orig[,,loc_peltier]
    }
    
    
    # Reformat grid
    
    orog_peltier_1deg_orig_regrid = orog_peltier_1deg_orig
    for (num_exp in c(1:length(tdab_times))){
      for (y in c(1:ny_peltier)){
        orog_peltier_1deg_orig_regrid[1:(nx_peltier),y,num_exp] = c(orog_peltier_1deg_orig[(nx2_peltier+1):nx_peltier,y,num_exp],orog_peltier_1deg_orig[1:nx2_peltier,y,num_exp])
      }
    }
    
    
    # Bilinearly interp Peltier orog data to CRU grid
    
    orog_peltier_interp_smooth_orig = array(0,c(nx_cru,ny_cru,length(tdab_times)))
    
    bilinterp_lon <- seq( 1,360,,720)
    bilinterp_lat <- seq( 1,180,,360)
    make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid
    
    for (num_exp in c(1:length(tdab_times))){
      
      bilinterp_data <- list( x= 1:360, y=1:180, z= orog_peltier_1deg_orig_regrid[,,num_exp])
      interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
      bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))
      
      orog_peltier_interp_smooth_orig[,,num_exp] = bilinterp_newdata
      
    }
    
    
    # Calculate 0.5 deg resolution Peltier orog used in HadCM3 ((Peltier yr x - Peltier yr 0) + HadCM3 yr 0)
    orog_peltier_interp_smooth_diff_0kyr = orog_peltier_interp_smooth_orig
    
    for (num_exp in c(1:length(tdab_times))){
      orog_peltier_interp_smooth_diff_0kyr[,,num_exp] = orog_peltier_interp_smooth_orig[,,num_exp] - orog_peltier_interp_smooth_orig[,,1]
    }
    
    # Calculate modice smooth orography
    orog_modice_interp_smooth = array(0,c(nx_cru,ny_cru))
    
    n = ny_cru
    for (y in c(1:ny_cru)){
      orog_modice_interp_smooth[1:(nx_cru),n] = c(orog_modice_interp_smooth_orig[(nx2_cru+1):nx_cru,y],orog_modice_interp_smooth_orig[1:nx2_cru,y])
      n = n-1
    }
    orog_modice_interp_smooth_landsea = orog_modice_interp_smooth
    
    
    # Add Peltier orog diff to 0.5 deg HadCM3 PI orog
    
    orog_peltier_HadCM3_comb_interp_smooth_orig = array(0,c(nx_cru,ny_cru,length(tdab_times)))
    
    for (num_exp in c(1:length(tdab_times))){
      orog_peltier_HadCM3_comb_interp_smooth_orig[,,num_exp] = orog_peltier_interp_smooth_diff_0kyr[,,num_exp] + orog_modice_interp_smooth_landsea
    }
    
    
    # Combine Cru and Peltier orographies into one matrix
    
    orog_all_cru_peltier_interp_orig = array(0,c(nx_cru,ny_cru,26))
    
    orog_all_cru_peltier_interp_orig[,,1] = orog_cru
    orog_all_cru_peltier_interp_orig[,,2] = orog_cru
    
    i = 1
    for (num_exp in c(3:26)){
      j = ny_cru
      for (y in c(1:ny_cru)){
        orog_all_cru_peltier_interp_orig[,y,num_exp] = c(orog_peltier_HadCM3_comb_interp_smooth_orig[,j,i]) # removing first (0 m) and last (114 m) tdab
        j = j - 1
      }
      i = i + 1
    }
    
    
    # Combine 0.5 deg HadCM3 orographies into one matrix
    
    gsl_list_all = c(model_input_all[183,5],model_input_all[1,5],model_input_all[62:85,5]) # removing first (0 m) and last (114 m) tdab
    
    orog_all_interp_smooth_orig = array(0,c(nx_cru,ny_cru,26))
    
    n = ny_cru
    for (y in c(1:ny_cru)){
      orog_all_interp_smooth_orig[1:(nx_cru),n,1] = c(orog_modice_interp_smooth_orig[(nx2_cru+1):nx_cru,y],orog_modice_interp_smooth_orig[1:nx2_cru,y])
      orog_all_interp_smooth_orig[1:(nx_cru),n,2] = c(orog_lowice_interp_smooth_orig[(nx2_cru+1):nx_cru,y],orog_lowice_interp_smooth_orig[1:nx2_cru,y])
      n = n-1
    }
    
    i = 2
    for (num_exp in c(3:26)){
      n = ny_cru
      for (y in c(1:ny_cru)){
        orog_all_interp_smooth_orig[1:(nx_cru),n,num_exp] = c(orog_tdab_interp_smooth_orig[(nx2_cru+1):nx_cru,y,i],orog_tdab_interp_smooth_orig[1:nx2_cru,y,i]) # removing first (0 m) and last (114 m) tdab
        n = n-1
      }
      i = i + 1
    }
    
    
    print("Finished loading data for bias correction")
    check5 = Sys.time()
    print(check5-check4)

  } # Closing off if for Temp and Precip vars only


  if (EmulVars == "Temp"){
    if (noice == 1){
      source("Temp_noice_BC.R")
    }
    else {
      source("Temp_BC_cjrw_v1.R")
    }
  }
  
  if (EmulVars == "Precip"){
    if (noice == 1){
      source("Precip_noice_BC.R")
    }
    else {
      source("Precip_BC_cjrw_v1.R")
    }
  }
}


######################################################## DATA SAVING #############################################################################

######################### Note: A lot of this has been removed.  If it is needed again, see version 3
  
if (runsaving==1){

# save data that is universal for all variables and simulations (e.g. CRU and HadCM3 orography) - see version 3

  # ICE COVER AT OLKILUOTO AND FORSMARK (FIGURES 28 - 39, TS) ---------------------------------------------------------------

  # CW: The variable names "model_input_*" are needed below to apply the LSM, but AKTA removed these and replace them with "modelInputs$*, created in 
  # Loadings/forcings_1myr_AP.R as a data table.  This is loaded here at line 111.  For temp and precip, these variable names are reinstated during
  # downscaling, in the bias correction subscript Temp_BC.R, and hence needs this by the time this bit is reached.  But they are not reinstated for other
  # variables, therefore need to reinstate them here
  
  model_input_zero_emissions_1myr_AP = modelInputs$natural
  model_input_rcp26_1myr_AP = modelInputs$RCP26
  model_input_rcp45_1myr_AP = modelInputs$RCP45
  model_input_rcp85_1myr_AP = modelInputs$RCP85

  # CW: Don't think (90% certainty!) that this ice section is being done, because if statements implies to only do this if the file does NOT exist.  It does,
  # therefore this next section is not carried out. However, when creating new directories for different ensemble members, need to make sure that this file 
  # is always included, because if it doesn't exist then this will be done and variable names e.g. slr_gl_sites_threshold_sweden_lower will not be found 
  # (because these are created during the downscaling).  Actually, no. given that the file is the same (need to check this) for each ensemble member, can 
  # simply point to just one of them i.e. don't need to copy this file into each new directory.

    if (file.exists("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/UpdatedData/Emulator/Output/Em_output_data_ice_at_OF_natural_0_to_1MyrAP.txt") == 0){
    model_input_all_ns_1myr_AP_GSL = array(c(model_input_zero_emissions_1myr_AP[,5],model_input_rcp26_1myr_AP[,5],model_input_rcp45_1myr_AP[,5],model_input_rcp85_1myr_AP[,5]),c(nt,ns))

    print('This will (Probably) not be printed')
    
    ice_cover_olkiluoto_natural = array(0,c(nt,2))
    e = 1
    for (row in 1:nt) {
      if (model_input_all_ns_1myr_AP_GSL[row,e] >= slr_gl_sites_threshold_sweden_lower) {
        ice_cover_olkiluoto_natural[row,1] = "No"
        ice_cover_olkiluoto_natural[row,2] = "No"
      } else if (model_input_all_ns_1myr_AP_GSL[row,e] <= slr_gl_sites_threshold_sweden_upper) {
        ice_cover_olkiluoto_natural[row,1] = "Yes"
        ice_cover_olkiluoto_natural[row,2] = "Yes"
      } else {
        ice_cover_olkiluoto_natural[row,1] = "No"
        ice_cover_olkiluoto_natural[row,2] = "Yes"
      }
    }
    
    ice_cover_olkiluoto_rcp26 = array(0,c(nt,2))
    e = 2
    for (row in 1:nt) {
      if (model_input_all_ns_1myr_AP_GSL[row,e] >= slr_gl_sites_threshold_sweden_lower) {
        ice_cover_olkiluoto_rcp26[row,1] = "No"
        ice_cover_olkiluoto_rcp26[row,2] = "No"
      } else if (model_input_all_ns_1myr_AP_GSL[row,e] <= slr_gl_sites_threshold_sweden_upper) {
        ice_cover_olkiluoto_rcp26[row,1] = "Yes"
        ice_cover_olkiluoto_rcp26[row,2] = "Yes"
      } else {
        ice_cover_olkiluoto_rcp26[row,1] = "No"
        ice_cover_olkiluoto_rcp26[row,2] = "Yes"
      }
    }
    
    ice_cover_olkiluoto_rcp45 = array(0,c(nt,2))
    e = 3
    for (row in 1:nt) {
      if (model_input_all_ns_1myr_AP_GSL[row,e] >= slr_gl_sites_threshold_sweden_lower) {
        ice_cover_olkiluoto_rcp45[row,1] = "No"
        ice_cover_olkiluoto_rcp45[row,2] = "No"
      } else if (model_input_all_ns_1myr_AP_GSL[row,e] <= slr_gl_sites_threshold_sweden_upper) {
        ice_cover_olkiluoto_rcp45[row,1] = "Yes"
        ice_cover_olkiluoto_rcp45[row,2] = "Yes"
      } else {
        ice_cover_olkiluoto_rcp45[row,1] = "No"
        ice_cover_olkiluoto_rcp45[row,2] = "Yes"
      }
    }
    
    ice_cover_olkiluoto_rcp85 = array(0,c(nt,2))
    e = 4
    for (row in 1:nt) {
      if (model_input_all_ns_1myr_AP_GSL[row,e] >= slr_gl_sites_threshold_sweden_lower) {
        ice_cover_olkiluoto_rcp85[row,1] = "No"
        ice_cover_olkiluoto_rcp85[row,2] = "No"
      } else if (model_input_all_ns_1myr_AP_GSL[row,e] <= slr_gl_sites_threshold_sweden_upper) {
        ice_cover_olkiluoto_rcp85[row,1] = "Yes"
        ice_cover_olkiluoto_rcp85[row,2] = "Yes"
      } else {
        ice_cover_olkiluoto_rcp85[row,1] = "No"
        ice_cover_olkiluoto_rcp85[row,2] = "Yes"
      }
    }
    
  }
  
    ncol = 3

# All SAT related files - see version 3
    
  if (EmulVars == "Temp"){  # CW: Begin temperature-only loop (written by AKTA)
    
    
    # SAT ANOMALY AS GLOBAL MEAN (FIGURE 20, TS) ---------------------------------------------------------------
    
    ncol = 2
    
    data_to_save = array(c((times/1000), round(my_tim_emissions_e_truemean[,1], digits = 1)),c(length(rev(times)),ncol))
    colnames(data_to_save) = c("Time_kyrAP", "dTemp_degC")
    write.table(data_to_save, file = paste(dataoutfilepath,"Em_output_data_temp_global_natural_0_to_1MyrAP.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
    
    data_to_save = array(c((times/1000), round(my_tim_emissions_e_truemean[,2], digits = 1)),c(length(rev(times)),ncol))
    colnames(data_to_save) = c("Time_kyrAP", "dTemp_degC")
    write.table(data_to_save, file = paste(dataoutfilepath,"Em_output_data_temp_global_RCP2.6_0_to_1MyrAP.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
    
    data_to_save = array(c((times/1000), round(my_tim_emissions_e_truemean[,3], digits = 1)),c(length(rev(times)),ncol))
    colnames(data_to_save) = c("Time_kyrAP", "dTemp_degC")
    write.table(data_to_save, file = paste(dataoutfilepath,"Em_output_data_temp_global_RCP4.5_0_to_1MyrAP.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
    
    data_to_save = array(c((times/1000), round(my_tim_emissions_e_truemean[,4], digits = 1)),c(length(rev(times)),ncol))
    colnames(data_to_save) = c("Time_kyrAP", "dTemp_degC")
    write.table(data_to_save, file = paste(dataoutfilepath,"Em_output_data_temp_global_RCP8.5_0_to_1MyrAP.txt",sep=""), append = FALSE, sep = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)
    
    

  } # CW: Close off temperature-Only loop (written by AKTA)

# Downscaled data - see version 3

    # CW: Concerning missing values, don't have this part in any if statement i.e. do for every variable
  
    # SAT ANOMALY OVER SCANDINAVIA AT ALL TIME SLICES OVER NEXT 1 MYR (MAP) ---------------------------------------------------------------
    
    # Assign grid boxes with no orography data (NaN) as missing values
    #ATKA - CHECK FROM HERE. MAY BE SOME DUPLICATION OF STUFF LOADED EARLIER OR CHANGES IN DEFINITION OF SCAND
    
    # CW: The orography was originally read in during the downscaling, so needs to be repeated here (not sure if this is necessary for the LSM?)
    
    print('Importing orography')    
    
  nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice_orog.nc")
  orog_modice_orig = ncvar_get(nc_file,"ht")
  nc_close(nc_file)
  orog_modice = array(0,c(nx,ny))
  for (y in c(1:ny)){
    orog_modice[1:(nx),y] = c(orog_modice_orig[(nx2+1):nx,y],orog_modice_orig[1:nx2,y])
  }
  
    for (dummy in 1:1){
      scenario_means_rcp26_big_anom_land_only = scenario_means_emissions_big_anom
      for (n_scen in 1:nt){
        for (x in 1:nx){
          for (y in 1:ny){
            if (is.nan(orog_modice[x,y]) == TRUE)
              scenario_means_rcp26_big_anom_land_only[x,y,n_scen] = NaN
          }
        }
      }
    }

    
  print('Importing LSM')    
    
    # Import land/sea masks
    
    lsim_modice_orig_alltypes = array(0,c(nx,ny,9))
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice.qrfrac.type.nc")
    lsim_modice_orig_alltypes = ncvar_get(nc_file,"field1391")
    nc_close(nc_file)
    
    
    lsim_modice_orig = array(0,c(nx,ny))
    
    for (y in c(1:ny)){
      lsim_modice_orig[1:(nx),y] = c(lsim_modice_orig_alltypes[(nx2+1):nx,y,9],lsim_modice_orig_alltypes[1:nx2,y,9])
    }
    
    
    lsim_lowice_orig_alltypes = array(0,c(nx,ny,9))
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/lowice.qrfrac.type.nc")
    lsim_lowice_orig_alltypes = ncvar_get(nc_file,"field1391")
    nc_close(nc_file)
    
    
    lsim_lowice_orig = array(0,c(nx,ny))
    
    for (y in c(1:ny)){
      lsim_lowice_orig[1:(nx),y] = c(lsim_lowice_orig_alltypes[(nx2+1):nx,y,9],lsim_lowice_orig_alltypes[1:nx2,y,9])
    }
    
    
    lsim_tdab_orig_alltypes = array(0,c(nx,ny,9))
    tdab_exp_nam_list = letters[seq( from = 1, to = 26 )]
    lsim_tdab_orig = array(0,c(nx,ny,length(tdab_exp_nam_list)))
    
    nc_file = nc_open("../orig/Input/2018-08-01 Final report/qrfrac.type_tdab28k.nc")
    for (num_exp in c(1:length(tdab_exp_nam_list))){
      tdab_exp_nam = paste("tdab",tdab_exp_nam_list[num_exp],sep="")
      lsim_tdab_orig_alltypes = ncvar_get(nc_file,tdab_exp_nam)
      
      for (y in c(1:ny)){
        lsim_tdab_orig[1:(nx),y,num_exp] = c(lsim_tdab_orig_alltypes[(nx2+1):nx,y,9],lsim_tdab_orig_alltypes[1:nx2,y,9])
      }
    }
    nc_close(nc_file)
    
    
    gsl_list_all = c(model_input_all[183,5],model_input_all[1,5],model_input_all[62:85,5]) # removing first (0 m) and last (114 m) tdab
    
    lsim_all_orig = array(0,c(nx,ny,length(gsl_list_all)))
    
    for (y in c(1:ny)){
      lsim_all_orig[,y,1] = c(lsim_modice_orig[,y])
      lsim_all_orig[,y,2] = c(lsim_lowice_orig[,y])
    }
    
    
    i = 2
    for (num_exp in c(3:length(gsl_list_all))){
      for (y in c(1:ny)){
        lsim_all_orig[,y,num_exp] = c(lsim_tdab_orig[,y,i]) # removing first (0 m) and last (114 m) tdab
      }
      i = i + 1
    }
    
    # Convert data to correct format for mapping
    
    lsim_all_orig_big = array(0,c((nx+1),ny,length(gsl_list_all)))
    for (e in c(1:length(gsl_list_all)))
      for (y in c(1:ny))
        lsim_all_orig_big[1:(nx+1),y,e] = c(lsim_all_orig[1:nx,y,e],lsim_all_orig[1,y,e])
    
    
    # Set up dimensions
    
    lons_scand_min = lons_had_min
    lons_scand_max = lons_had_max
    
    lats_scand_min = lats_had_min
    lats_scand_max = lats_had_max
    
    lons_scand = lons[lons_scand_min:lons_scand_max]
    lats_scand = lats[(ny-(lats_scand_max-1)):(ny-(lats_scand_min-1))]
    
    lons_nc = seq(from = -180, to = 176.25, by = 3.75 ) # seq(from = -178.125, to = 178.125, by = 3.75 )
    
    print('Begin assigning LSM') 
    
    # Assign land/sea mask to data
    # CW: Inserted new section, as below (for anomalies) to deal with mean values, needed for sea ice only
    
    scenario_means_zero_emissions_big_lsm = scenario_means_zero_emissions_big
    scenario_means_rcp26_big_lsm = scenario_means_rcp26_big
    scenario_means_rcp45_big_lsm = scenario_means_rcp45_big
    scenario_means_rcp85_big_lsm = scenario_means_rcp85_big

    scenario_means_zero_emissions_big_anom_lsm = scenario_means_zero_emissions_big_anom
    scenario_means_rcp26_big_anom_lsm = scenario_means_rcp26_big_anom
    scenario_means_rcp45_big_anom_lsm = scenario_means_rcp45_big_anom
    scenario_means_rcp85_big_anom_lsm = scenario_means_rcp85_big_anom
    
    # CW: Inserted new section, as above, to deal with uncertainty
    
    scenario_var_zero_emissions_big_anom_lsm = scenario_var_zero_emissions_big_anom
    scenario_var_rcp26_big_anom_lsm = scenario_var_rcp26_big_anom
    scenario_var_rcp45_big_anom_lsm = scenario_var_rcp45_big_anom
    scenario_var_rcp85_big_anom_lsm = scenario_var_rcp85_big_anom
    
    num_exp_toDSstat = 1



    # CW: For each of these, inserted new section, as with means, to include uncertainty. So mydata1 applies LSM to means, mydata2 applies LSM to uncertainty
  
  if (EmulVars == "SM" || EmulVars == "STemp" || EmulVars == "Veg" || EmulVars == "SIce"){ # CW: Only assign LSM to these variables
    
    for (num_exp_toDSstat in 1:nt){
      
      gsl_index_ts = which.min(abs(model_input_zero_emissions_1myr_AP[num_exp_toDSstat,5] - gsl_list_all))
      gsl_value_ts = gsl_list_all[[gsl_index_ts]]
      lsim_em_zero_emissions_orig_ts = lsim_all_orig_big[,,gsl_index_ts]
      
      if (EmulVars == "SIce"){ # CW: Currently LSM is masking out ocean, which is correct for most variables but not sea ice.  Instead, need to mask out land.
#                                    Do This using the same function (is.na), but preceded by ! for sea ice only, which does the opposite of the function
        
      mydata = scenario_means_zero_emissions_big_lsm[,,num_exp_toDSstat]
      mydata[!is.na(lsim_em_zero_emissions_orig_ts)] = NaN
      scenario_means_zero_emissions_big_lsm[,,num_exp_toDSstat] = mydata
        
      mydata1 = scenario_means_zero_emissions_big_anom_lsm[,,num_exp_toDSstat]
      mydata1[!is.na(lsim_em_zero_emissions_orig_ts)] = NaN
      scenario_means_zero_emissions_big_anom_lsm[,,num_exp_toDSstat] = mydata1
      
      mydata2 = scenario_var_zero_emissions_big_anom_lsm[,,num_exp_toDSstat]
      mydata2[!is.na(lsim_em_zero_emissions_orig_ts)] = NaN
      scenario_var_zero_emissions_big_anom_lsm[,,num_exp_toDSstat] = mydata2
     
      } else { # CW: if not sea ice, then remove ! so that ocean is masked out
           
      mydata1 = scenario_means_zero_emissions_big_anom_lsm[,,num_exp_toDSstat]
      mydata1[is.na(lsim_em_zero_emissions_orig_ts)] = NaN
      scenario_means_zero_emissions_big_anom_lsm[,,num_exp_toDSstat] = mydata1
        
      mydata2 = scenario_var_zero_emissions_big_anom_lsm[,,num_exp_toDSstat]
      mydata2[is.na(lsim_em_zero_emissions_orig_ts)] = NaN
      scenario_var_zero_emissions_big_anom_lsm[,,num_exp_toDSstat] = mydata2
        
      } # CW: Close off sea ice loop
      
    }
    
    for (num_exp_toDSstat in 1:nt){
      
      gsl_index_ts = which.min(abs(model_input_rcp26_1myr_AP[num_exp_toDSstat,5] - gsl_list_all))
      gsl_value_ts = gsl_list_all[[gsl_index_ts]]
      lsim_em_rcp26_orig_ts = lsim_all_orig_big[,,gsl_index_ts]
      
      if (EmulVars == "SIce"){ # CW: Currently LSM is masking out ocean, which is correct for most variables but not sea ice.  Instead, need to mask out land.
#                                    Do This using the same function (is.na), but preceded by ! for sea ice only, which does the opposite of the function
        
      mydata = scenario_means_rcp26_big_lsm[,,num_exp_toDSstat]
      mydata[!is.na(lsim_em_rcp26_orig_ts)] = NaN
      scenario_means_rcp26_big_lsm[,,num_exp_toDSstat] = mydata
        
      mydata1 = scenario_means_rcp26_big_anom_lsm[,,num_exp_toDSstat]
      mydata1[!is.na(lsim_em_rcp26_orig_ts)] = NaN
      scenario_means_rcp26_big_anom_lsm[,,num_exp_toDSstat] = mydata1
      
      mydata2 = scenario_var_rcp26_big_anom_lsm[,,num_exp_toDSstat]
      mydata2[!is.na(lsim_em_rcp26_orig_ts)] = NaN
      scenario_var_rcp26_big_anom_lsm[,,num_exp_toDSstat] = mydata2

    } else { # CW: if not sea ice, then remove ! so that ocean is masked out

      mydata1 = scenario_means_rcp26_big_anom_lsm[,,num_exp_toDSstat]
      mydata1[is.na(lsim_em_rcp26_orig_ts)] = NaN
      scenario_means_rcp26_big_anom_lsm[,,num_exp_toDSstat] = mydata1
      
      mydata2 = scenario_var_rcp26_big_anom_lsm[,,num_exp_toDSstat]
      mydata2[is.na(lsim_em_rcp26_orig_ts)] = NaN
      scenario_var_rcp26_big_anom_lsm[,,num_exp_toDSstat] = mydata2
      
    } # CW: Close off sea ice loop
      
  }
    
    for (num_exp_toDSstat in 1:nt){
      
      gsl_index_ts = which.min(abs(model_input_rcp45_1myr_AP[num_exp_toDSstat,5] - gsl_list_all))
      gsl_value_ts = gsl_list_all[[gsl_index_ts]]
      lsim_em_rcp45_orig_ts = lsim_all_orig_big[,,gsl_index_ts]
      
      if (EmulVars == "SIce"){ # CW: Currently LSM is masking out ocean, which is correct for most variables but not sea ice.  Instead, need to mask out land.
#                                    Do This using the same function (is.na), but preceded by ! for sea ice only, which does the opposite of the function
        
      mydata = scenario_means_rcp45_big_lsm[,,num_exp_toDSstat]
      mydata[!is.na(lsim_em_rcp45_orig_ts)] = NaN
      scenario_means_rcp45_big_lsm[,,num_exp_toDSstat] = mydata
        
      mydata1 = scenario_means_rcp45_big_anom_lsm[,,num_exp_toDSstat]
      mydata1[!is.na(lsim_em_rcp45_orig_ts)] = NaN
      scenario_means_rcp45_big_anom_lsm[,,num_exp_toDSstat] = mydata1
      
      mydata2 = scenario_var_rcp45_big_anom_lsm[,,num_exp_toDSstat]
      mydata2[!is.na(lsim_em_rcp45_orig_ts)] = NaN
      scenario_var_rcp45_big_anom_lsm[,,num_exp_toDSstat] = mydata2

      } else { # CW: if not sea ice, then remove ! so that ocean is masked out

      mydata1 = scenario_means_rcp45_big_anom_lsm[,,num_exp_toDSstat]
      mydata1[is.na(lsim_em_rcp45_orig_ts)] = NaN
      scenario_means_rcp45_big_anom_lsm[,,num_exp_toDSstat] = mydata1
        
      mydata2 = scenario_var_rcp45_big_anom_lsm[,,num_exp_toDSstat]
      mydata2[is.na(lsim_em_rcp45_orig_ts)] = NaN
      scenario_var_rcp45_big_anom_lsm[,,num_exp_toDSstat] = mydata2
        
      } # CW: Close off sea ice loop
      
    }
    
    for (num_exp_toDSstat in 1:nt){
      
      gsl_index_ts = which.min(abs(model_input_rcp85_1myr_AP[num_exp_toDSstat,5] - gsl_list_all))
      gsl_value_ts = gsl_list_all[[gsl_index_ts]]
      lsim_em_rcp85_orig_ts = lsim_all_orig_big[,,gsl_index_ts]
      
      if (EmulVars == "SIce"){ # CW: Currently LSM is masking out ocean, which is correct for most variables but not sea ice.  Instead, need to mask out land.
#                                    Do This using the same function (is.na), but preceded by ! for sea ice only, which does the opposite of the function

      mydata = scenario_means_rcp85_big_lsm[,,num_exp_toDSstat]
      mydata[!is.na(lsim_em_rcp85_orig_ts)] = NaN
      scenario_means_rcp85_big_lsm[,,num_exp_toDSstat] = mydata
        
      mydata1 = scenario_means_rcp85_big_anom_lsm[,,num_exp_toDSstat]
      mydata1[!is.na(lsim_em_rcp85_orig_ts)] = NaN
      scenario_means_rcp85_big_anom_lsm[,,num_exp_toDSstat] = mydata1
      
      mydata2 = scenario_var_rcp85_big_anom_lsm[,,num_exp_toDSstat]
      mydata2[!is.na(lsim_em_rcp85_orig_ts)] = NaN
      scenario_var_rcp85_big_anom_lsm[,,num_exp_toDSstat] = mydata2

      } else { # CW: if not sea ice, then remove ! so that ocean is masked out

      mydata1 = scenario_means_rcp85_big_anom_lsm[,,num_exp_toDSstat]
      mydata1[is.na(lsim_em_rcp85_orig_ts)] = NaN
      scenario_means_rcp85_big_anom_lsm[,,num_exp_toDSstat] = mydata1
        
      mydata2 = scenario_var_rcp85_big_anom_lsm[,,num_exp_toDSstat]
      mydata2[is.na(lsim_em_rcp85_orig_ts)] = NaN
      scenario_var_rcp85_big_anom_lsm[,,num_exp_toDSstat] = mydata2
        
        
      } # CW: Close off sea ice if statement
      
   }
    
    print('End of assigning LSM') 
    
    save_num = c(1:nt)
    kyr_num = c(0:(nt-1))
    kyr_num_zeros = c(rep("000",10),rep("00",90),rep("0",900),"")
    
  } # CW: Close off LSM if statement    
    
############### Save global data from emulator (i.e. HadCM3 resolution) as netcdf #######

# Note: For Korean Peninsula, this is only one grid box therefore netcdf file, although successfully created, won't be a map but just 1001 single values.
# Regional file no longer written out, as no longer needed (GBs are selected in cjrw1*) - still keeps loop, so can be used if needed, but sets myt to 1:1 i.e. only global

# Means
    
    kyr_num = c(0:(nt-1))
    
    names_t=c("Global","Region")
    names_s=c("natural","RCP2.6","RCP4.5",'RCP8.5')
    
    lons_all_min = c(1,lons_scand_min)
    
    if (EmulVars == "WSpeed"){                                # CW: 96 longitudes for all variables except Wind speed, which = 97
      lons_all_max = c(97,lons_scand_max)
    } else { lons_all_max = c(96,lons_scand_max)}
    
    if (EmulVars == "WSpeed"){                                # CW: 73 latitudes for all variables except Wind speed, which = 72
      lats_all_min = c(72,lats_scand_min)
    } else { lats_all_min = c(73,lats_scand_min)}
    
     lats_all_max = c(1,lats_scand_max)
    
    for (myt in 1:1){
      lons_all = lons[lons_all_min[myt]:lons_all_max[myt]]
      lats_all = lats[(ny-(lats_all_max[myt]-1)):(ny-(lats_all_min[myt]-1))]
      for (mys in 1:ns){
        
        # define global attributes
        nc_institution = "University of Bristol, 2021"
        nc_datasource = "Climate emulator"
        
        if (EmulVars == "Temp"){
          nc_title = "2m air temperature"
        } else if (EmulVars == "Precip"){
          nc_title = "Precipitation"
        } else if (EmulVars == "Evapotrans"){
          nc_title = "Evapotranspiration"
        } else if (EmulVars == "SIce"){
          nc_title = "Sea ice"
        } else if (EmulVars == "SM"){
          nc_title = "Soil moisture"
        } else if (EmulVars == "SnDepth"){
          nc_title = "Snow depth"
        } else if (EmulVars == "STemp"){
          nc_title = "Soil temperature"
        } else if (EmulVars == "Veg"){
          nc_title = "Vegetation"
        } else if (EmulVars == "WSpeed"){
          nc_title = "Wind speed"
        }
        
        nc_file_name <- paste(dataoutfilepath,"Em_output_data_means_",EmulVars,"_",names_t[myt],"_",names_s[mys],"_0_to_1MyrAP_cjrw.nc",sep="")
        
        # define dimensions
        lon_dim <- ncdim_def("longitude","degrees_east",as.double(lons_all))
        lat_dim <- ncdim_def("latitude","degrees_north",as.double(lats_all))
        time_dim <- ncdim_def("time","",as.double(kyr_num))
        surf_dim <- ncdim_def("surface","",as.double(1))
        
        # define variables
        if (EmulVars == "Temp"){
          miss_value <- 2e20
          nc_var_name <- "SAT"
          nc_var_unit <- "degC"
          nc_var_lname <- "2m air temperature"
        } else if (EmulVars == "Precip"){
          miss_value <- 2e20
          nc_var_name <- "precip"
          nc_var_unit <- "mm/mo"
          nc_var_lname <- "Precipitation"
        } else if (EmulVars == "Evapotrans"){
          miss_value <- 2e20
          nc_var_name <- "evapotrans"
          nc_var_unit <- "mm/mo"
          nc_var_lname <- "Evapotranspiration"
        } else if (EmulVars == "SIce"){
          miss_value <- 2e20
          nc_var_name <- "seaice"
          nc_var_unit <- "%"
          nc_var_lname <- "Sea ice"
        } else if (EmulVars == "SM"){
          miss_value <- 2e20
          nc_var_name <- "sm"
          nc_var_unit <- "kg/m2"
          nc_var_lname <- "Soil moisture"
        } else if (EmulVars == "SnDepth"){
          miss_value <- 2e20
          nc_var_name <- "snow"
          nc_var_unit <- "m"
          nc_var_lname <- "Snow depth"
        } else if (EmulVars == "STemp"){
          miss_value <- 2e20
          nc_var_name <- "stemp"
          nc_var_unit <- "degC"
          nc_var_lname <- "Soil temperature"
        } else if (EmulVars == "Veg"){
          miss_value <- 2e20
          nc_var_name <- "veg"
          nc_var_unit <- "LAI"
          nc_var_lname <- "Vegetation"
        } else if (EmulVars == "WSpeed"){
          miss_value <- 2e20
          nc_var_name <- "wspeed"
          nc_var_unit <- "m/s"
          nc_var_lname <- "Wind speed"
        }
        
        # create variable and ncdf file with first variable so dimensions are set
        var_def <- ncvar_def(nc_var_name,nc_var_unit,list(lon_dim,lat_dim,time_dim,surf_dim),miss_value,nc_var_lname,prec="double")
        nc_file <- nc_create(nc_file_name,var_def)

        if (EmulVars == "SM" || EmulVars == "STemp" || EmulVars == "Veg" || EmulVars == "SIce"){ # CW: Only use version with LSM for these variables
          
        # define data to save
        if (mys == 1){
          data_to_save = scenario_means_zero_emissions_big_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 2){
          data_to_save = scenario_means_rcp26_big_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 3){
          data_to_save = scenario_means_rcp45_big_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 4){
          data_to_save = scenario_means_rcp85_big_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        
        } else {
          
        # define data to save
        if (mys == 1){
          data_to_save = scenario_means_zero_emissions_big[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 2){
          data_to_save = scenario_means_rcp26_big[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 3){
          data_to_save = scenario_means_rcp45_big[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 4){
          data_to_save = scenario_means_rcp85_big[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
          

        }
        
        # add data to ncdf file
        ncvar_put(nc_file,var_def,data_to_save)
        
        # close the file
        nc_close(nc_file)
        
        
      }
    }

# Anomalies
    
    kyr_num = c(0:(nt-1))
    
    names_t=c("Global","Region")
    names_s=c("natural","RCP2.6","RCP4.5",'RCP8.5')
    lons_all_min = c(1,lons_scand_min)
    
    if (EmulVars == "WSpeed"){                                # CW: 96 longitudes for all variables except Wind speed, which = 97
      lons_all_max = c(97,lons_scand_max)
    } else { lons_all_max = c(96,lons_scand_max)}
    
    if (EmulVars == "WSpeed"){                                # CW: 73 latitudes for all variables except Wind speed, which = 72
      lats_all_min = c(72,lats_scand_min)
    } else { lats_all_min = c(73,lats_scand_min)}
    
    lats_all_max = c(1,lats_scand_max)

    for (myt in 1:1){
    lons_all = lons[lons_all_min[myt]:lons_all_max[myt]]
    lats_all = lats[(ny-(lats_all_max[myt]-1)):(ny-(lats_all_min[myt]-1))]
    for (mys in 1:ns){
      
      # define global attributes
      nc_institution = "University of Bristol, 2021"
      nc_datasource = "Climate emulator"
      
      if (EmulVars == "Temp"){
        nc_title = "2m air temperature"
      } else if (EmulVars == "Precip"){
        nc_title = "Precipitation"
      } else if (EmulVars == "Evapotrans"){
        nc_title = "Evapotranspiration"
      } else if (EmulVars == "SIce"){
        nc_title = "Sea ice"
      } else if (EmulVars == "SM"){
        nc_title = "Soil moisture"
      } else if (EmulVars == "SnDepth"){
        nc_title = "Snow depth"
      } else if (EmulVars == "STemp"){
        nc_title = "Soil temperature"
      } else if (EmulVars == "Veg"){
        nc_title = "Vegetation"
      } else if (EmulVars == "WSpeed"){
        nc_title = "Wind speed"
      }
         
      nc_file_name <- paste(dataoutfilepath,"Em_output_data_anom_",EmulVars,"_",names_t[myt],"_",names_s[mys],"_0_to_1MyrAP_cjrw.nc",sep="")
      
      # define dimensions
      lon_dim <- ncdim_def("longitude","degrees_east",as.double(lons_all))
      lat_dim <- ncdim_def("latitude","degrees_north",as.double(lats_all))
      time_dim <- ncdim_def("time","",as.double(kyr_num))
      surf_dim <- ncdim_def("surface","",as.double(1))
      
      # define variables
      if (EmulVars == "Temp"){
       miss_value <- 2e20
       nc_var_name <- "SAT_anomaly"
       nc_var_unit <- "degC"
       nc_var_lname <- "2m air temperature anomaly (compared to pre-industrial)"
      } else if (EmulVars == "Precip"){
       miss_value <- 2e20
       nc_var_name <- "precip_anomaly"
       nc_var_unit <- "mm/mo"
       nc_var_lname <- "Precipitation anomaly (compared to pre-industrial)"
      } else if (EmulVars == "Evapotrans"){
        miss_value <- 2e20
        nc_var_name <- "evapotrans_anomaly"
        nc_var_unit <- "mm/mo"
        nc_var_lname <- "Evapotranspiration anomaly (compared to pre-industrial)"
      } else if (EmulVars == "SIce"){
        miss_value <- 2e20
        nc_var_name <- "seaice_anomaly"
        nc_var_unit <- "%"
        nc_var_lname <- "Sea ice anomaly (compared to pre-industrial)"
      } else if (EmulVars == "SM"){
        miss_value <- 2e20
        nc_var_name <- "sm_anomaly"
        nc_var_unit <- "kg/m2"
        nc_var_lname <- "Soil moisture anomaly (compared to pre-industrial)"
      } else if (EmulVars == "SnDepth"){
        miss_value <- 2e20
        nc_var_name <- "snow_anomaly"
        nc_var_unit <- "m"
        nc_var_lname <- "Snow depth anomaly (compared to pre-industrial)"
      } else if (EmulVars == "STemp"){
        miss_value <- 2e20
        nc_var_name <- "stemp_anomaly"
        nc_var_unit <- "degC"
        nc_var_lname <- "Soil temperature anomaly (compared to pre-industrial)"
      } else if (EmulVars == "Veg"){
        miss_value <- 2e20
        nc_var_name <- "veg_anomaly"
        nc_var_unit <- "LAI"
        nc_var_lname <- "Vegetation anomaly (compared to pre-industrial)"
      } else if (EmulVars == "WSpeed"){
        miss_value <- 2e20
        nc_var_name <- "wspeed_anomaly"
        nc_var_unit <- "m/s"
        nc_var_lname <- "Wind speed anomaly (compared to pre-industrial)"
      }
      
      # create variable and ncdf file with first variable so dimensions are set
      var_def <- ncvar_def(nc_var_name,nc_var_unit,list(lon_dim,lat_dim,time_dim,surf_dim),miss_value,nc_var_lname,prec="double")
      nc_file <- nc_create(nc_file_name,var_def)

      if (EmulVars == "SM" || EmulVars == "STemp" || EmulVars == "Veg" || EmulVars == "SIce"){ # CW: Only use version with LSM for these variables
        
        # define data to save
        if (mys == 1){
          data_to_save = scenario_means_zero_emissions_big_anom_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 2){
          data_to_save = scenario_means_rcp26_big_anom_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 3){
          data_to_save = scenario_means_rcp45_big_anom_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 4){
          data_to_save = scenario_means_rcp85_big_anom_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        
      } else {
        
        # define data to save
        if (mys == 1){
          data_to_save = scenario_means_zero_emissions_big_anom[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 2){
          data_to_save = scenario_means_rcp26_big_anom[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 3){
          data_to_save = scenario_means_rcp45_big_anom[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        if (mys == 4){
          data_to_save = scenario_means_rcp85_big_anom[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
        }
        
       }            

      
      # add data to ncdf file
      ncvar_put(nc_file,var_def,data_to_save)
      
      # close the file
      nc_close(nc_file)
      
   
    }
  }
    
# Variance
    
    kyr_num = c(0:(nt-1))
    
    names_t=c("Global","Region")
    names_s=c("natural","RCP2.6","RCP4.5",'RCP8.5')
    lons_all_min = c(1,lons_scand_min)
    
    if (EmulVars == "WSpeed"){                                # CW: 96 longitudes for all variables except Wind speed, which = 97
      lons_all_max = c(97,lons_scand_max)
    } else { lons_all_max = c(96,lons_scand_max)}
    
    if (EmulVars == "WSpeed"){                                # CW: 73 latitudes for all variables except Wind speed, which = 72
      lats_all_min = c(72,lats_scand_min)
    } else { lats_all_min = c(73,lats_scand_min)}
    
    lats_all_max = c(1,lats_scand_max)
    
    for (myt in 1:1){
      lons_all = lons[lons_all_min[myt]:lons_all_max[myt]]
      lats_all = lats[(ny-(lats_all_max[myt]-1)):(ny-(lats_all_min[myt]-1))]
      for (mys in 1:ns){
        
        # define global attributes
        nc_institution = "University of Bristol, 2021"
        nc_datasource = "Climate emulator"
        
        if (EmulVars == "Temp"){
          nc_title = "2m air temperature"
        } else if (EmulVars == "Precip"){
          nc_title = "Precipitation"
        } else if (EmulVars == "Evapotrans"){
          nc_title = "Evapotranspiration"
        } else if (EmulVars == "SIce"){
          nc_title = "Sea ice"
        } else if (EmulVars == "SM"){
          nc_title = "Soil moisture"
        } else if (EmulVars == "SnDepth"){
          nc_title = "Snow depth"
        } else if (EmulVars == "STemp"){
          nc_title = "Soil temperature"
        } else if (EmulVars == "Veg"){
          nc_title = "Vegetation"
        } else if (EmulVars == "WSpeed"){
          nc_title = "Wind speed"
        }
        
        nc_file_name <- paste(dataoutfilepath,"Em_output_data_var_",EmulVars,"_",names_t[myt],"_",names_s[mys],"_0_to_1MyrAP_cjrw.nc",sep="")
        
        # define dimensions
        lon_dim <- ncdim_def("longitude","degrees_east",as.double(lons_all))
        lat_dim <- ncdim_def("latitude","degrees_north",as.double(lats_all))
        time_dim <- ncdim_def("time","",as.double(kyr_num))
        surf_dim <- ncdim_def("surface","",as.double(1))
        
        # define variables
        if (EmulVars == "Temp"){
          miss_value <- 2e20
          nc_var_name <- "SAT_var"
          nc_var_unit <- "degC"
          nc_var_lname <- "2m air temperature variance"
        } else if (EmulVars == "Precip"){
          miss_value <- 2e20
          nc_var_name <- "precip_var"
          nc_var_unit <- "mm/mo"
          nc_var_lname <- "Precipitation variance"
        } else if (EmulVars == "Evapotrans"){
          miss_value <- 2e20
          nc_var_name <- "evapotrans_var"
          nc_var_unit <- "mm/mo"
          nc_var_lname <- "Evapotranspiration variance"
        } else if (EmulVars == "SIce"){
          miss_value <- 2e20
          nc_var_name <- "seaice_var"
          nc_var_unit <- "%"
          nc_var_lname <- "Sea ice variance"
        } else if (EmulVars == "SM"){
          miss_value <- 2e20
          nc_var_name <- "sm_var"
          nc_var_unit <- "kg/m2"
          nc_var_lname <- "Soil moisture variance"
        } else if (EmulVars == "SnDepth"){
          miss_value <- 2e20
          nc_var_name <- "snow_var"
          nc_var_unit <- "m"
          nc_var_lname <- "Snow depth variance"
        } else if (EmulVars == "STemp"){
          miss_value <- 2e20
          nc_var_name <- "stemp_var"
          nc_var_unit <- "degC"
          nc_var_lname <- "Soil temperature variance"
        } else if (EmulVars == "Veg"){
          miss_value <- 2e20
          nc_var_name <- "veg_var"
          nc_var_unit <- "LAI"
          nc_var_lname <- "Vegetation variance"
        } else if (EmulVars == "WSpeed"){
          miss_value <- 2e20
          nc_var_name <- "wspeed_var"
          nc_var_unit <- "m/s"
          nc_var_lname <- "Wind speed variance"
        }
        
        # create variable and ncdf file with first variable so dimensions are set
        var_def <- ncvar_def(nc_var_name,nc_var_unit,list(lon_dim,lat_dim,time_dim,surf_dim),miss_value,nc_var_lname,prec="double")
        nc_file <- nc_create(nc_file_name,var_def)

        if (EmulVars == "SM" || EmulVars == "STemp" || EmulVars == "Veg" || EmulVars == "SIce"){ # CW: Only use version with LSM for these variables
          
          # define data to save
          if (mys == 1){
            data_to_save = scenario_var_zero_emissions_big_anom_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
          }
          if (mys == 2){
            data_to_save = scenario_var_rcp26_big_anom_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
          }
          if (mys == 3){
            data_to_save = scenario_var_rcp45_big_anom_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
          }
          if (mys == 4){
            data_to_save = scenario_var_rcp85_big_anom_lsm[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
          }
          
        } else {
          
          # define data to save
          if (mys == 1){
            data_to_save = scenario_var_zero_emissions_big_anom[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
          }
          if (mys == 2){
            data_to_save = scenario_var_rcp26_big_anom[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
          }
          if (mys == 3){
            data_to_save = scenario_var_rcp45_big_anom[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
          }
          if (mys == 4){
            data_to_save = scenario_var_rcp85_big_anom[lons_all_min[myt]:lons_all_max[myt],c(lats_all_max[myt]:lats_all_min[myt]),c(1:1001)]
          }
          
          
        }            
        
        # add data to ncdf file
        ncvar_put(nc_file,var_def,data_to_save)
        
        # close the file
        nc_close(nc_file)
        
        
      }
    }
    
################## Write out emulated data for all variables - removed (see version 4) ###################
  
} #ATKA - closing off save option

print('Successfully finished')

# CW: End of writing out data section

print("End of location pair, onto the text")

} # CW: Close off location loop
  
######################### Plotting section has been removed.  If it is needed again, see version 3 & 4 ###############
  
  
  keep("Sim", "allvars", "EmulVars", "noice", sure = TRUE) # CW: Wipe all variables except these
  
  print("End of simulation, onto the next...")
  
} # CW: Close off big simulation loop

print("End of all simulations, so back to wrapper and onto the next variable...")

#########################################################################################
