## Downscale Temperature results from emulator ######################################################################

## Does the same as original downscaling (Temp_BC.R), but removing the hardwiring of the locations and replacing instead with arrays read in from text files
## within main script.  Also at end of each scenario (for each downscaling method), saves downscaled regional array (all time steps) as netcdf.

print("Starting downscaling: go put the kettle on.")

check1a = Sys.time()

# Downscale emulated data (1 Myr) to site scale (simple bias correction compared to obs; DSbias_a_site_only + DSbias_b_site_ig_only) ----------------------------------------------------------

# Import CRU gridded climatology data (v2.1)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/clim.6190.lan.tmp.nc")
temp_cru_monthly = ncvar_get(nc_file,"tmp")
lats_cru = ncvar_get(nc_file,"lat")
lons_cru_orig = ncvar_get(nc_file,"lon")
nc_close(nc_file)


# Convert CRU Data from monthly means to annual

nx_cru = 720
ny_cru = 360
nx2_cru = nx_cru/2

temp_cru_ann = array(0,c(nx_cru,ny_cru))

for (y in c(1:ny_cru)) {
  for (x in c(1:nx_cru)) {
    temp_cru_ann[x,y] = mean(temp_cru_monthly[x,y,1:12])
  }
}


temp_cru = temp_cru_ann

lons_cru_orig = lons_cru_orig-180

temp_cru_sweden = array(temp_cru[Forsmark_lon_GB_CRU,Forsmark_lat_GB_CRU])

temp_cru_finland = array(temp_cru[Olkiluoto_lon_GB_CRU,Olkiluoto_lat_GB_CRU])


# Calculate anomaly (absolute difference; bias) between observed (CRU) and modelled (tdstb) pre-industrial temperature

#ATKA - copied from 'corrected' original script
ne = length(times) # timeslice exps
temp_tdstb_sweden = model_output_tdst_big[Forsmark_lon_GB,Forsmark_lat_GB]
temp_tdstb_finland = model_output_tdst_big[Olkiluoto_lon_GB,Olkiluoto_lat_GB]
source('col_bwr.R')
col_bwr = rgb(bwr(20))

temp_PI_anom_tdstb_cru_sweden_a_site_only = temp_tdstb_sweden - temp_cru_sweden

temp_PI_anom_tdstb_cru_finland_a_site_only = temp_tdstb_finland - temp_cru_finland


#ATKA - reinstate variables for each scenario, that I otherwise grouped into one variable:
my_tim_sweden_zero_emissions_e = my_tim_sweden_emissions_e[,1]
my_tim_sweden_rcp26_e = my_tim_sweden_emissions_e[,2]
my_tim_sweden_rcp45_e = my_tim_sweden_emissions_e[,3]
my_tim_sweden_rcp85_e = my_tim_sweden_emissions_e[,4]
my_tim_finland_zero_emissions_e = my_tim_finland_emissions_e[,1]
my_tim_finland_rcp26_e = my_tim_finland_emissions_e[,2]
my_tim_finland_rcp45_e = my_tim_finland_emissions_e[,3]
my_tim_finland_rcp85_e = my_tim_finland_emissions_e[,4]


# Calculate downscaled site temperature (DSbias_a_site_only) * See below ice-covered section for DSbias_b_site_ig_only calculation
my_tim_sweden_zero_emissions_e_DSbias_a_site_only = array(my_tim_sweden_zero_emissions_e - temp_PI_anom_tdstb_cru_sweden_a_site_only,c(ne,1))
my_tim_sweden_rcp26_e_DSbias_a_site_only = array(my_tim_sweden_rcp26_e - temp_PI_anom_tdstb_cru_sweden_a_site_only,c(ne,1))
my_tim_sweden_rcp45_e_DSbias_a_site_only = array(my_tim_sweden_rcp45_e - temp_PI_anom_tdstb_cru_sweden_a_site_only,c(ne,1))
my_tim_sweden_rcp85_e_DSbias_a_site_only = array(my_tim_sweden_rcp85_e - temp_PI_anom_tdstb_cru_sweden_a_site_only,c(ne,1))

my_tim_finland_zero_emissions_e_DSbias_a_site_only = array(my_tim_finland_zero_emissions_e - temp_PI_anom_tdstb_cru_finland_a_site_only,c(ne,1))
my_tim_finland_rcp26_e_DSbias_a_site_only = array(my_tim_finland_rcp26_e - temp_PI_anom_tdstb_cru_finland_a_site_only,c(ne,1))
my_tim_finland_rcp45_e_DSbias_a_site_only = array(my_tim_finland_rcp45_e - temp_PI_anom_tdstb_cru_finland_a_site_only,c(ne,1))
my_tim_finland_rcp85_e_DSbias_a_site_only = array(my_tim_finland_rcp85_e - temp_PI_anom_tdstb_cru_finland_a_site_only,c(ne,1))

plot(times, my_tim_sweden_zero_emissions_e_DSbias_a_site_only, type = 'l', ylim = c(-25, 10))
lines(times, my_tim_finland_zero_emissions_e_DSbias_a_site_only, type = 'l', ylim = c(-25, 10), col = 'red')



# Calculate ice-covered period at sites in downscaled data ----------------------------------------------------------------------------

# Identify downscaled times when sites are covered by ice sheet and assign NA

my_tim_sweden_zero_emissions_e_DSbias_a_site_only_NA_lower = my_tim_sweden_zero_emissions_e_DSbias_a_site_only
my_tim_sweden_rcp26_e_DSbias_a_site_only_NA_lower = my_tim_sweden_rcp26_e_DSbias_a_site_only
my_tim_sweden_rcp45_e_DSbias_a_site_only_NA_lower = my_tim_sweden_rcp45_e_DSbias_a_site_only
my_tim_sweden_rcp85_e_DSbias_a_site_only_NA_lower = my_tim_sweden_rcp85_e_DSbias_a_site_only

my_tim_finland_zero_emissions_e_DSbias_a_site_only_NA_lower = my_tim_finland_zero_emissions_e_DSbias_a_site_only
my_tim_finland_rcp26_e_DSbias_a_site_only_NA_lower = my_tim_finland_rcp26_e_DSbias_a_site_only
my_tim_finland_rcp45_e_DSbias_a_site_only_NA_lower = my_tim_finland_rcp45_e_DSbias_a_site_only
my_tim_finland_rcp85_e_DSbias_a_site_only_NA_lower = my_tim_finland_rcp85_e_DSbias_a_site_only

my_tim_sweden_zero_emissions_e_DSbias_a_site_only_NA_upper = my_tim_sweden_zero_emissions_e_DSbias_a_site_only
my_tim_sweden_rcp26_e_DSbias_a_site_only_NA_upper = my_tim_sweden_rcp26_e_DSbias_a_site_only
my_tim_sweden_rcp45_e_DSbias_a_site_only_NA_upper = my_tim_sweden_rcp45_e_DSbias_a_site_only
my_tim_sweden_rcp85_e_DSbias_a_site_only_NA_upper = my_tim_sweden_rcp85_e_DSbias_a_site_only

my_tim_finland_zero_emissions_e_DSbias_a_site_only_NA_upper = my_tim_finland_zero_emissions_e_DSbias_a_site_only
my_tim_finland_rcp26_e_DSbias_a_site_only_NA_upper = my_tim_finland_rcp26_e_DSbias_a_site_only
my_tim_finland_rcp45_e_DSbias_a_site_only_NA_upper = my_tim_finland_rcp45_e_DSbias_a_site_only
my_tim_finland_rcp85_e_DSbias_a_site_only_NA_upper = my_tim_finland_rcp85_e_DSbias_a_site_only

slr_gl_sites_threshold_sweden_lower = -52.59 #-72.59 + 20 #-66.08 #-46.08 - 20 # SLR threshold when ice sheet is not covering site
slr_gl_sites_threshold_sweden_upper = -92.59 #-72.59 - 20 # SLR threshold when ice sheet is covering site
slr_gl_sites_threshold_finland_lower = -52.59 #-72.59 + 20 #-66.08 #-46.08 - 20 # SLR threshold when ice sheet is not covering site
slr_gl_sites_threshold_finland_upper = -92.59 #-72.59 - 20 # SLR threshold when ice sheet is covering site

#ATKA - reinstating other variables
model_input_zero_emissions_1myr_AP = modelInputs$natural
model_input_rcp26_1myr_AP = modelInputs$RCP26
model_input_rcp45_1myr_AP = modelInputs$RCP45
model_input_rcp85_1myr_AP = modelInputs$RCP85

for (i in 1:ne){
  if (model_input_zero_emissions_1myr_AP[i,5] <= slr_gl_sites_threshold_sweden_upper){
    my_tim_sweden_zero_emissions_e_DSbias_a_site_only_NA_upper[i,1] = NA
  }
  if (model_input_rcp26_1myr_AP[i,5] <= slr_gl_sites_threshold_sweden_upper){
    my_tim_sweden_rcp26_e_DSbias_a_site_only_NA_upper[i,1] = NA
  }
  if (model_input_rcp45_1myr_AP[i,5] <= slr_gl_sites_threshold_sweden_upper){
    my_tim_sweden_rcp45_e_DSbias_a_site_only_NA_upper[i,1] = NA
  }
  if (model_input_rcp85_1myr_AP[i,5] <= slr_gl_sites_threshold_sweden_upper){
    my_tim_sweden_rcp85_e_DSbias_a_site_only_NA_upper[i,1] = NA
  }
}

for (i in 1:ne){
  if ((model_input_zero_emissions_1myr_AP[i,5] <= slr_gl_sites_threshold_sweden_lower)&(model_input_zero_emissions_1myr_AP[i,5] >= slr_gl_sites_threshold_sweden_upper)){
    my_tim_sweden_zero_emissions_e_DSbias_a_site_only_NA_lower[i,1] = NA
  }
  if ((model_input_rcp26_1myr_AP[i,5] <= slr_gl_sites_threshold_sweden_lower)&(model_input_rcp26_1myr_AP[i,5] >= slr_gl_sites_threshold_sweden_upper)){
    my_tim_sweden_rcp26_e_DSbias_a_site_only_NA_lower[i,1] = NA
  }
  if ((model_input_rcp45_1myr_AP[i,5] <= slr_gl_sites_threshold_sweden_lower)&(model_input_rcp45_1myr_AP[i,5] >= slr_gl_sites_threshold_sweden_upper)){
    my_tim_sweden_rcp45_e_DSbias_a_site_only_NA_lower[i,1] = NA
  }
  if ((model_input_rcp85_1myr_AP[i,5] <= slr_gl_sites_threshold_sweden_lower)&(model_input_rcp85_1myr_AP[i,5] >= slr_gl_sites_threshold_sweden_upper)){
    my_tim_sweden_rcp85_e_DSbias_a_site_only_NA_lower[i,1] = NA
  }
}

for (i in 1:ne){
  if (model_input_zero_emissions_1myr_AP[i,5] <= slr_gl_sites_threshold_finland_upper){
    my_tim_finland_zero_emissions_e_DSbias_a_site_only_NA_upper[i,1] = NA
  }
  if (model_input_rcp26_1myr_AP[i,5] <= slr_gl_sites_threshold_finland_upper){
    my_tim_finland_rcp26_e_DSbias_a_site_only_NA_upper[i,1] = NA
  }
  if (model_input_rcp45_1myr_AP[i,5] <= slr_gl_sites_threshold_finland_upper){
    my_tim_finland_rcp45_e_DSbias_a_site_only_NA_upper[i,1] = NA
  }
  if (model_input_rcp85_1myr_AP[i,5] <= slr_gl_sites_threshold_finland_upper){
    my_tim_finland_rcp85_e_DSbias_a_site_only_NA_upper[i,1] = NA
  }
}

for (i in 1:ne){
  if ((model_input_zero_emissions_1myr_AP[i,5] <= slr_gl_sites_threshold_finland_lower)&(model_input_zero_emissions_1myr_AP[i,5] >= slr_gl_sites_threshold_finland_upper)){
    my_tim_finland_zero_emissions_e_DSbias_a_site_only_NA_lower[i,1] = NA
  }
  if ((model_input_rcp26_1myr_AP[i,5] <= slr_gl_sites_threshold_finland_lower)&(model_input_rcp26_1myr_AP[i,5] >= slr_gl_sites_threshold_finland_upper)){
    my_tim_finland_rcp26_e_DSbias_a_site_only_NA_lower[i,1] = NA
  }
  if ((model_input_rcp45_1myr_AP[i,5] <= slr_gl_sites_threshold_finland_lower)&(model_input_rcp45_1myr_AP[i,5] >= slr_gl_sites_threshold_finland_upper)){
    my_tim_finland_rcp45_e_DSbias_a_site_only_NA_lower[i,1] = NA
  }
  if ((model_input_rcp85_1myr_AP[i,5] <= slr_gl_sites_threshold_finland_lower)&(model_input_rcp85_1myr_AP[i,5] >= slr_gl_sites_threshold_finland_upper)){
    my_tim_finland_rcp85_e_DSbias_a_site_only_NA_lower[i,1] = NA
  }
}


# Identify downscaled times when sites are transitioning between ice covered and ice free

slr_gl_sites_sweden_upper = array(0,c(ne,ns))
slr_gl_sites_sweden_upper[,1] = is.na(my_tim_sweden_zero_emissions_e_DSbias_a_site_only_NA_upper)
slr_gl_sites_sweden_upper[,2] = is.na(my_tim_sweden_rcp26_e_DSbias_a_site_only_NA_upper)
slr_gl_sites_sweden_upper[,3] = is.na(my_tim_sweden_rcp45_e_DSbias_a_site_only_NA_upper)
slr_gl_sites_sweden_upper[,4] = is.na(my_tim_sweden_rcp85_e_DSbias_a_site_only_NA_upper)

slr_gl_sites_finland_upper = array(0,c(ne,ns))
slr_gl_sites_finland_upper[,1] = is.na(my_tim_finland_zero_emissions_e_DSbias_a_site_only_NA_upper)
slr_gl_sites_finland_upper[,2] = is.na(my_tim_finland_rcp26_e_DSbias_a_site_only_NA_upper)
slr_gl_sites_finland_upper[,3] = is.na(my_tim_finland_rcp45_e_DSbias_a_site_only_NA_upper)
slr_gl_sites_finland_upper[,4] = is.na(my_tim_finland_rcp85_e_DSbias_a_site_only_NA_upper)

slr_gl_sites_sweden_lower = array(0,c(ne,ns))
slr_gl_sites_sweden_lower[,1] = is.na(my_tim_sweden_zero_emissions_e_DSbias_a_site_only_NA_lower)
slr_gl_sites_sweden_lower[,2] = is.na(my_tim_sweden_rcp26_e_DSbias_a_site_only_NA_lower)
slr_gl_sites_sweden_lower[,3] = is.na(my_tim_sweden_rcp45_e_DSbias_a_site_only_NA_lower)
slr_gl_sites_sweden_lower[,4] = is.na(my_tim_sweden_rcp85_e_DSbias_a_site_only_NA_lower)

slr_gl_sites_finland_lower = array(0,c(ne,ns))
slr_gl_sites_finland_lower[,1] = is.na(my_tim_finland_zero_emissions_e_DSbias_a_site_only_NA_lower)
slr_gl_sites_finland_lower[,2] = is.na(my_tim_finland_rcp26_e_DSbias_a_site_only_NA_lower)
slr_gl_sites_finland_lower[,3] = is.na(my_tim_finland_rcp45_e_DSbias_a_site_only_NA_lower)
slr_gl_sites_finland_lower[,4] = is.na(my_tim_finland_rcp85_e_DSbias_a_site_only_NA_lower)

count_slr_gl_sites_zero_emissions_all_sweden_upper = array(NA,c(100,2))
count_slr_gl_sites_zero_emissions_all_sweden_upper[1,1] = 1
count_slr_gl_sites_zero_emissions_all_sweden_upper[1,2] = 0
count_slr_gl_sites_rcp26_all_sweden_upper = array(NA,c(100,2))
count_slr_gl_sites_rcp26_all_sweden_upper[1,1] = 1
count_slr_gl_sites_rcp26_all_sweden_upper[1,2] = 0
count_slr_gl_sites_rcp45_all_sweden_upper = array(NA,c(100,2))
count_slr_gl_sites_rcp45_all_sweden_upper[1,1] = 1
count_slr_gl_sites_rcp45_all_sweden_upper[1,2] = 0
count_slr_gl_sites_rcp85_all_sweden_upper = array(NA,c(100,2))
count_slr_gl_sites_rcp85_all_sweden_upper[1,1] = 1
count_slr_gl_sites_rcp85_all_sweden_upper[1,2] = 0

count_slr_gl_sites_zero_emissions_all_finland_upper = array(NA,c(100,2))
count_slr_gl_sites_zero_emissions_all_finland_upper[1,1] = 1
count_slr_gl_sites_zero_emissions_all_finland_upper[1,2] = 0
count_slr_gl_sites_rcp26_all_finland_upper = array(NA,c(100,2))
count_slr_gl_sites_rcp26_all_finland_upper[1,1] = 1
count_slr_gl_sites_rcp26_all_finland_upper[1,2] = 0
count_slr_gl_sites_rcp45_all_finland_upper = array(NA,c(100,2))
count_slr_gl_sites_rcp45_all_finland_upper[1,1] = 1
count_slr_gl_sites_rcp45_all_finland_upper[1,2] = 0
count_slr_gl_sites_rcp85_all_finland_upper = array(NA,c(100,2))
count_slr_gl_sites_rcp85_all_finland_upper[1,1] = 1
count_slr_gl_sites_rcp85_all_finland_upper[1,2] = 0

count_slr_gl_sites_zero_emissions_all_sweden_lower = array(NA,c(100,2))
count_slr_gl_sites_zero_emissions_all_sweden_lower[1,1] = 1
count_slr_gl_sites_zero_emissions_all_sweden_lower[1,2] = 0
count_slr_gl_sites_rcp26_all_sweden_lower = array(NA,c(100,2))
count_slr_gl_sites_rcp26_all_sweden_lower[1,1] = 1
count_slr_gl_sites_rcp26_all_sweden_lower[1,2] = 0
count_slr_gl_sites_rcp45_all_sweden_lower = array(NA,c(100,2))
count_slr_gl_sites_rcp45_all_sweden_lower[1,1] = 1
count_slr_gl_sites_rcp45_all_sweden_lower[1,2] = 0
count_slr_gl_sites_rcp85_all_sweden_lower = array(NA,c(100,2))
count_slr_gl_sites_rcp85_all_sweden_lower[1,1] = 1
count_slr_gl_sites_rcp85_all_sweden_lower[1,2] = 0

count_slr_gl_sites_zero_emissions_all_finland_lower = array(NA,c(100,2))
count_slr_gl_sites_zero_emissions_all_finland_lower[1,1] = 1
count_slr_gl_sites_zero_emissions_all_finland_lower[1,2] = 0
count_slr_gl_sites_rcp26_all_finland_lower = array(NA,c(100,2))
count_slr_gl_sites_rcp26_all_finland_lower[1,1] = 1
count_slr_gl_sites_rcp26_all_finland_lower[1,2] = 0
count_slr_gl_sites_rcp45_all_finland_lower = array(NA,c(100,2))
count_slr_gl_sites_rcp45_all_finland_lower[1,1] = 1
count_slr_gl_sites_rcp45_all_finland_lower[1,2] = 0
count_slr_gl_sites_rcp85_all_finland_lower = array(NA,c(100,2))
count_slr_gl_sites_rcp85_all_finland_lower[1,1] = 1
count_slr_gl_sites_rcp85_all_finland_lower[1,2] = 0

count_slr_gl_sites_4rcp_all_finland_upper = array(NA,c(1,4))
count_slr_gl_sites_4rcp_all_sweden_upper = array(NA,c(1,4))

count_slr_gl_sites_4rcp_all_finland_lower = array(NA,c(1,4))
count_slr_gl_sites_4rcp_all_sweden_lower = array(NA,c(1,4))

row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_sweden_upper[i,1] != slr_gl_sites_sweden_upper[(i+1),1]) {
    count_slr_gl_sites_zero_emissions_all_sweden_upper[row,2] = i
    row = row+1
    count_slr_gl_sites_zero_emissions_all_sweden_upper[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_sweden_upper[1] = row

row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_finland_upper[i,1] != slr_gl_sites_finland_upper[(i+1),1]) {
    count_slr_gl_sites_zero_emissions_all_finland_upper[row,2] = i
    row = row+1
    count_slr_gl_sites_zero_emissions_all_finland_upper[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_finland_upper[1] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_sweden_upper[i,2] != slr_gl_sites_sweden_upper[(i+1),2]) {
    count_slr_gl_sites_rcp26_all_sweden_upper[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp26_all_sweden_upper[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_sweden_upper[2] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_finland_upper[i,2] != slr_gl_sites_finland_upper[(i+1),2]) {
    count_slr_gl_sites_rcp26_all_finland_upper[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp26_all_finland_upper[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_finland_upper[2] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_sweden_upper[i,3] != slr_gl_sites_sweden_upper[(i+1),3]) {
    count_slr_gl_sites_rcp45_all_sweden_upper[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp45_all_sweden_upper[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_sweden_upper[3] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_finland_upper[i,3] != slr_gl_sites_finland_upper[(i+1),3]) {
    count_slr_gl_sites_rcp45_all_finland_upper[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp45_all_finland_upper[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_finland_upper[3] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_sweden_upper[i,4] != slr_gl_sites_sweden_upper[(i+1),4]) {
    count_slr_gl_sites_rcp85_all_sweden_upper[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp85_all_sweden_upper[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_sweden_upper[4] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_finland_upper[i,4] != slr_gl_sites_finland_upper[(i+1),4]) {
    count_slr_gl_sites_rcp85_all_finland_upper[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp85_all_finland_upper[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_finland_upper[4] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_sweden_lower[i,1] != slr_gl_sites_sweden_lower[(i+1),1]) {
    count_slr_gl_sites_zero_emissions_all_sweden_lower[row,2] = i
    row = row+1
    count_slr_gl_sites_zero_emissions_all_sweden_lower[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_sweden_lower[1] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_finland_lower[i,1] != slr_gl_sites_finland_lower[(i+1),1]) {
    count_slr_gl_sites_zero_emissions_all_finland_lower[row,2] = i
    row = row+1
    count_slr_gl_sites_zero_emissions_all_finland_lower[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_finland_lower[1] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_sweden_lower[i,2] != slr_gl_sites_sweden_lower[(i+1),2]) {
    count_slr_gl_sites_rcp26_all_sweden_lower[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp26_all_sweden_lower[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_sweden_lower[2] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_finland_lower[i,2] != slr_gl_sites_finland_lower[(i+1),2]) {
    count_slr_gl_sites_rcp26_all_finland_lower[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp26_all_finland_lower[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_finland_lower[2] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_sweden_lower[i,3] != slr_gl_sites_sweden_lower[(i+1),3]) {
    count_slr_gl_sites_rcp45_all_sweden_lower[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp45_all_sweden_lower[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_sweden_lower[3] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_finland_lower[i,3] != slr_gl_sites_finland_lower[(i+1),3]) {
    count_slr_gl_sites_rcp45_all_finland_lower[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp45_all_finland_lower[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_finland_lower[3] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_sweden_lower[i,4] != slr_gl_sites_sweden_lower[(i+1),4]) {
    count_slr_gl_sites_rcp85_all_sweden_lower[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp85_all_sweden_lower[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_sweden_lower[4] = row


row = 1
n = 1
for (i in 1:(ne-1)){
  if (slr_gl_sites_finland_lower[i,4] != slr_gl_sites_finland_lower[(i+1),4]) {
    count_slr_gl_sites_rcp85_all_finland_lower[row,2] = i
    row = row+1
    count_slr_gl_sites_rcp85_all_finland_lower[row,1] = i
  }
}
count_slr_gl_sites_4rcp_all_finland_lower[4] = row


# Extract downscaled time extents for when sites are ice covered

num_ig_sweden_upper = array(NA,c(1,4))
num_ig_finland_upper = array(NA,c(1,4))

num_ig_sweden_lower = array(NA,c(1,4))
num_ig_finland_lower = array(NA,c(1,4))

num_ig_sweden_upper[1] = ceiling((count_slr_gl_sites_4rcp_all_sweden_upper[1]-1)/2)
num_ig_sweden_upper[2] = ceiling((count_slr_gl_sites_4rcp_all_sweden_upper[2]-1)/2)
num_ig_sweden_upper[3] = ceiling((count_slr_gl_sites_4rcp_all_sweden_upper[3]-1)/2)
num_ig_sweden_upper[4] = ceiling((count_slr_gl_sites_4rcp_all_sweden_upper[4]-1)/2)
num_ig_finland_upper[1] = ceiling((count_slr_gl_sites_4rcp_all_finland_upper[1]-1)/2)
num_ig_finland_upper[2] = ceiling((count_slr_gl_sites_4rcp_all_finland_upper[2]-1)/2)
num_ig_finland_upper[3] = ceiling((count_slr_gl_sites_4rcp_all_finland_upper[3]-1)/2)
num_ig_finland_upper[4] = ceiling((count_slr_gl_sites_4rcp_all_finland_upper[4]-1)/2)

num_ig_sweden_lower[1] = ceiling((count_slr_gl_sites_4rcp_all_sweden_lower[1]-1)/2)
num_ig_sweden_lower[2] = ceiling((count_slr_gl_sites_4rcp_all_sweden_lower[2]-1)/2)
num_ig_sweden_lower[3] = ceiling((count_slr_gl_sites_4rcp_all_sweden_lower[3]-1)/2)
num_ig_sweden_lower[4] = ceiling((count_slr_gl_sites_4rcp_all_sweden_lower[4]-1)/2)
num_ig_finland_lower[1] = ceiling((count_slr_gl_sites_4rcp_all_finland_lower[1]-1)/2)
num_ig_finland_lower[2] = ceiling((count_slr_gl_sites_4rcp_all_finland_lower[2]-1)/2)
num_ig_finland_lower[3] = ceiling((count_slr_gl_sites_4rcp_all_finland_lower[3]-1)/2)
num_ig_finland_lower[4] = ceiling((count_slr_gl_sites_4rcp_all_finland_lower[4]-1)/2)

count_slr_gl_sites_zero_emissions_sweden_upper = array(0,c(num_ig_sweden_upper[1],2))
count_slr_gl_sites_rcp26_sweden_upper = array(0,c(num_ig_sweden_upper[2],2))
count_slr_gl_sites_rcp45_sweden_upper = array(0,c(num_ig_sweden_upper[3],2))
count_slr_gl_sites_rcp85_sweden_upper = array(0,c(num_ig_sweden_upper[4],2))

count_slr_gl_sites_zero_emissions_finland_upper = array(0,c(num_ig_finland_upper[1],2))
count_slr_gl_sites_rcp26_finland_upper = array(0,c(num_ig_finland_upper[2],2))
count_slr_gl_sites_rcp45_finland_upper = array(0,c(num_ig_finland_upper[3],2))
count_slr_gl_sites_rcp85_finland_upper = array(0,c(num_ig_finland_upper[4],2))

count_slr_gl_sites_zero_emissions_sweden_lower = array(0,c(num_ig_sweden_lower[1],2))
count_slr_gl_sites_rcp26_sweden_lower = array(0,c(num_ig_sweden_lower[2],2))
count_slr_gl_sites_rcp45_sweden_lower = array(0,c(num_ig_sweden_lower[3],2))
count_slr_gl_sites_rcp85_sweden_lower = array(0,c(num_ig_sweden_lower[4],2))

count_slr_gl_sites_zero_emissions_finland_lower = array(0,c(num_ig_finland_lower[1],2))
count_slr_gl_sites_rcp26_finland_lower = array(0,c(num_ig_finland_lower[2],2))
count_slr_gl_sites_rcp45_finland_lower = array(0,c(num_ig_finland_lower[3],2))
count_slr_gl_sites_rcp85_finland_lower = array(0,c(num_ig_finland_lower[4],2))

row = 2
for (i in 1:num_ig_sweden_upper[1]){
  count_slr_gl_sites_zero_emissions_sweden_upper[i,] = count_slr_gl_sites_zero_emissions_all_sweden_upper[row,]
  row = row+2
}
count_slr_gl_sites_zero_emissions_sweden_upper[,1] = count_slr_gl_sites_zero_emissions_sweden_upper[,1]+1


row = 2
for (i in 1:num_ig_finland_upper[1]){
  count_slr_gl_sites_zero_emissions_finland_upper[i,] = count_slr_gl_sites_zero_emissions_all_finland_upper[row,]
  row = row+2
}
count_slr_gl_sites_zero_emissions_finland_upper[,1] = count_slr_gl_sites_zero_emissions_finland_upper[,1]+1


row = 2
for (i in 1:num_ig_sweden_upper[2]){
  count_slr_gl_sites_rcp26_sweden_upper[i,] = count_slr_gl_sites_rcp26_all_sweden_upper[row,]
  row = row+2
}
count_slr_gl_sites_rcp26_sweden_upper[,1] = count_slr_gl_sites_rcp26_sweden_upper[,1]+1


row = 2
for (i in 1:num_ig_finland_upper[2]){
  count_slr_gl_sites_rcp26_finland_upper[i,] = count_slr_gl_sites_rcp26_all_finland_upper[row,]
  row = row+2
}
count_slr_gl_sites_rcp26_finland_upper[,1] = count_slr_gl_sites_rcp26_finland_upper[,1]+1


row = 2
for (i in 1:num_ig_sweden_upper[3]){
  count_slr_gl_sites_rcp45_sweden_upper[i,] = count_slr_gl_sites_rcp45_all_sweden_upper[row,]
  row = row+2
}
count_slr_gl_sites_rcp45_sweden_upper[,1] = count_slr_gl_sites_rcp45_sweden_upper[,1]+1

row = 2
for (i in 1:num_ig_finland_upper[3]){
  count_slr_gl_sites_rcp45_finland_upper[i,] = count_slr_gl_sites_rcp45_all_finland_upper[row,]
  row = row+2
}
count_slr_gl_sites_rcp45_finland_upper[,1] = count_slr_gl_sites_rcp45_finland_upper[,1]+1


row = 2
for (i in 1:num_ig_sweden_upper[4]){
  count_slr_gl_sites_rcp85_sweden_upper[i,] = count_slr_gl_sites_rcp85_all_sweden_upper[row,]
  row = row+2
}
count_slr_gl_sites_rcp85_sweden_upper[,1] = count_slr_gl_sites_rcp85_sweden_upper[,1]+1

row = 2
for (i in 1:num_ig_finland_upper[4]){
  count_slr_gl_sites_rcp85_finland_upper[i,] = count_slr_gl_sites_rcp85_all_finland_upper[row,]
  row = row+2
}
count_slr_gl_sites_rcp85_finland_upper[,1] = count_slr_gl_sites_rcp85_finland_upper[,1]+1

row = 2
for (i in 1:num_ig_sweden_lower[1]){
  count_slr_gl_sites_zero_emissions_sweden_lower[i,] = count_slr_gl_sites_zero_emissions_all_sweden_lower[row,]
  row = row+2
}
count_slr_gl_sites_zero_emissions_sweden_lower[,1] = count_slr_gl_sites_zero_emissions_sweden_lower[,1]+1

row = 2
for (i in 1:num_ig_finland_lower[1]){
  count_slr_gl_sites_zero_emissions_finland_lower[i,] = count_slr_gl_sites_zero_emissions_all_finland_lower[row,]
  row = row+2
}
count_slr_gl_sites_zero_emissions_finland_lower[,1] = count_slr_gl_sites_zero_emissions_finland_lower[,1]+1


row = 2
for (i in 1:num_ig_sweden_lower[2]){
  count_slr_gl_sites_rcp26_sweden_lower[i,] = count_slr_gl_sites_rcp26_all_sweden_lower[row,]
  row = row+2
}
count_slr_gl_sites_rcp26_sweden_lower[,1] = count_slr_gl_sites_rcp26_sweden_lower[,1]+1

row = 2
for (i in 1:num_ig_finland_lower[2]){
  count_slr_gl_sites_rcp26_finland_lower[i,] = count_slr_gl_sites_rcp26_all_finland_lower[row,]
  row = row+2
}
count_slr_gl_sites_rcp26_finland_lower[,1] = count_slr_gl_sites_rcp26_finland_lower[,1]+1


row = 2
for (i in 1:num_ig_sweden_lower[3]){
  count_slr_gl_sites_rcp45_sweden_lower[i,] = count_slr_gl_sites_rcp45_all_sweden_lower[row,]
  row = row+2
}
count_slr_gl_sites_rcp45_sweden_lower[,1] = count_slr_gl_sites_rcp45_sweden_lower[,1]+1

row = 2
for (i in 1:num_ig_finland_lower[3]){
  count_slr_gl_sites_rcp45_finland_lower[i,] = count_slr_gl_sites_rcp45_all_finland_lower[row,]
  row = row+2
}
count_slr_gl_sites_rcp45_finland_lower[,1] = count_slr_gl_sites_rcp45_finland_lower[,1]+1


row = 2
for (i in 1:num_ig_sweden_lower[4]){
  count_slr_gl_sites_rcp85_sweden_lower[i,] = count_slr_gl_sites_rcp85_all_sweden_lower[row,]
  row = row+2
}
count_slr_gl_sites_rcp85_sweden_lower[,1] = count_slr_gl_sites_rcp85_sweden_lower[,1]+1

row = 2
for (i in 1:num_ig_finland_lower[4]){
  count_slr_gl_sites_rcp85_finland_lower[i,] = count_slr_gl_sites_rcp85_all_finland_lower[row,]
  row = row+2
}
count_slr_gl_sites_rcp85_finland_lower[,1] = count_slr_gl_sites_rcp85_finland_lower[,1]+1

# Convert to years (from row numbers)

gl_periods_sites_zero_emissions_finland_upper = array(0,c(num_ig_finland_upper[1],3))
gl_periods_sites_rcp26_finland_upper = array(0,c(num_ig_finland_upper[2],3))
gl_periods_sites_rcp45_finland_upper = array(0,c(num_ig_finland_upper[3],3))
gl_periods_sites_rcp85_finland_upper = array(0,c(num_ig_finland_upper[4],3))

gl_periods_sites_zero_emissions_finland_lower = array(0,c(num_ig_finland_lower[1],3))
gl_periods_sites_rcp26_finland_lower = array(0,c(num_ig_finland_lower[2],3))
gl_periods_sites_rcp45_finland_lower = array(0,c(num_ig_finland_lower[3],3))
gl_periods_sites_rcp85_finland_lower = array(0,c(num_ig_finland_lower[4],3))

for (col in 1:2){
  for (row in 1:num_ig_finland_upper[1]){
    gl_periods_sites_zero_emissions_finland_upper[row,col] = (times[count_slr_gl_sites_zero_emissions_finland_upper[row,col],1])/1000
  }
}
gl_periods_sites_zero_emissions_finland_upper[,3] = gl_periods_sites_zero_emissions_finland_upper[,2]-gl_periods_sites_zero_emissions_finland_upper[,1]

for (col in 1:2){
  for (row in 1:num_ig_finland_upper[2]){
    gl_periods_sites_rcp26_finland_upper[row,col] = (times[count_slr_gl_sites_rcp26_finland_upper[row,col],1])/1000
  }
}
gl_periods_sites_rcp26_finland_upper[,3] = gl_periods_sites_rcp26_finland_upper[,2]-gl_periods_sites_rcp26_finland_upper[,1]

for (col in 1:2){
  for (row in 1:num_ig_finland_upper[3]){
    gl_periods_sites_rcp45_finland_upper[row,col] = (times[count_slr_gl_sites_rcp45_finland_upper[row,col],1])/1000
  }
}
gl_periods_sites_rcp45_finland_upper[,3] = gl_periods_sites_rcp45_finland_upper[,2]-gl_periods_sites_rcp45_finland_upper[,1]

for (col in 1:2){
  for (row in 1:num_ig_finland_upper[4]){
    gl_periods_sites_rcp85_finland_upper[row,col] = (times[count_slr_gl_sites_rcp85_finland_upper[row,col],1])/1000
  }
}
gl_periods_sites_rcp85_finland_upper[,3] = gl_periods_sites_rcp85_finland_upper[,2]-gl_periods_sites_rcp85_finland_upper[,1]


for (col in 1:2){
  for (row in 1:num_ig_finland_lower[1]){
    gl_periods_sites_zero_emissions_finland_lower[row,col] = (times[count_slr_gl_sites_zero_emissions_finland_lower[row,col],1])/1000
  }
}
gl_periods_sites_zero_emissions_finland_lower[,3] = gl_periods_sites_zero_emissions_finland_lower[,2]-gl_periods_sites_zero_emissions_finland_lower[,1]

for (col in 1:2){
  for (row in 1:num_ig_finland_lower[2]){
    gl_periods_sites_rcp26_finland_lower[row,col] = (times[count_slr_gl_sites_rcp26_finland_lower[row,col],1])/1000
  }
}
gl_periods_sites_rcp26_finland_lower[,3] = gl_periods_sites_rcp26_finland_lower[,2]-gl_periods_sites_rcp26_finland_lower[,1]

for (col in 1:2){
  for (row in 1:num_ig_finland_lower[3]){
    gl_periods_sites_rcp45_finland_lower[row,col] = (times[count_slr_gl_sites_rcp45_finland_lower[row,col],1])/1000
  }
}
gl_periods_sites_rcp45_finland_lower[,3] = gl_periods_sites_rcp45_finland_lower[,2]-gl_periods_sites_rcp45_finland_lower[,1]

for (col in 1:2){
  for (row in 1:num_ig_finland_lower[4]){
    gl_periods_sites_rcp85_finland_lower[row,col] = (times[count_slr_gl_sites_rcp85_finland_lower[row,col],1])/1000
  }
}
gl_periods_sites_rcp85_finland_lower[,3] = gl_periods_sites_rcp85_finland_lower[,2]-gl_periods_sites_rcp85_finland_lower[,1]


# Save to text file

# write.table(x = count_slr_gl_sites_rcp45_sweden_upper, file = "C:/Users/nl6806/OneDrive - University of Bristol/PostDoc/2017-02-15 Posiva + SKB/3a. Emulate future climate/Emulator/For_Bristol/2015_Bristol_5D_v001/R/2018-08-01 Final report/Results/glac_periods_rcp45_sweden_upper.txt", sep = " ", col.names = c("% start_kyr","end_kyr"), row.names = FALSE)

# write.table(x = count_slr_gl_sites_rcp45_finland_upper, file = "C:/Users/nl6806/OneDrive - University of Bristol/PostDoc/2017-02-15 Posiva + SKB/3a. Emulate future climate/Emulator/For_Bristol/2015_Bristol_5D_v001/R/2018-08-01 Final report/Results/glac_periods_rcp45_finland_upper.txt", sep = " ", col.names = c("% start_kyr","end_kyr"), row.names = FALSE)



# Calculate downscaled site temperature only when GSL is > lower threshold (DSbias_b_site_ig_only)

model_input_all_ns_1myr_AP_GSL = array(c(model_input_zero_emissions_1myr_AP[,5],model_input_rcp26_1myr_AP[,5],model_input_rcp45_1myr_AP[,5],model_input_rcp85_1myr_AP[,5]),c(ne,ns))

my_tim_sweden_zero_emissions_e_DSbias_b_site_ig_only = array(NaN,c(ne,1))
my_tim_sweden_rcp26_e_DSbias_b_site_ig_only = array(NaN,c(ne,1))
my_tim_sweden_rcp45_e_DSbias_b_site_ig_only = array(NaN,c(ne,1))
my_tim_sweden_rcp85_e_DSbias_b_site_ig_only = array(NaN,c(ne,1))
my_tim_finland_zero_emissions_e_DSbias_b_site_ig_only = array(NaN,c(ne,1))
my_tim_finland_rcp26_e_DSbias_b_site_ig_only = array(NaN,c(ne,1))
my_tim_finland_rcp45_e_DSbias_b_site_ig_only = array(NaN,c(ne,1))
my_tim_finland_rcp85_e_DSbias_b_site_ig_only = array(NaN,c(ne,1))

for (row in c(1:ne)){
  
  if (model_input_all_ns_1myr_AP_GSL[row,1] >= slr_gl_sites_threshold_sweden_lower) {
    my_tim_sweden_zero_emissions_e_DSbias_b_site_ig_only[row,1] = my_tim_sweden_zero_emissions_e_DSbias_a_site_only[row,1]
    my_tim_finland_zero_emissions_e_DSbias_b_site_ig_only[row,1] = my_tim_finland_zero_emissions_e_DSbias_a_site_only[row,1]
  } else {
    my_tim_sweden_zero_emissions_e_DSbias_b_site_ig_only[row,1] = my_tim_sweden_zero_emissions_e[row]
    my_tim_finland_zero_emissions_e_DSbias_b_site_ig_only[row,1] = my_tim_finland_zero_emissions_e[row]
  }

  if (model_input_all_ns_1myr_AP_GSL[row,2] >= slr_gl_sites_threshold_sweden_lower) {
    my_tim_sweden_rcp26_e_DSbias_b_site_ig_only[row,1] = my_tim_sweden_rcp26_e_DSbias_a_site_only[row,1]
    my_tim_finland_rcp26_e_DSbias_b_site_ig_only[row,1] = my_tim_finland_rcp26_e_DSbias_a_site_only[row,1]
  } else {
    my_tim_sweden_rcp26_e_DSbias_b_site_ig_only[row,1] = my_tim_sweden_rcp26_e[row]
    my_tim_finland_rcp26_e_DSbias_b_site_ig_only[row,1] = my_tim_finland_rcp26_e[row]
  }
  
  if (model_input_all_ns_1myr_AP_GSL[row,3] >= slr_gl_sites_threshold_sweden_lower) {
    my_tim_sweden_rcp45_e_DSbias_b_site_ig_only[row,1] = my_tim_sweden_rcp45_e_DSbias_a_site_only[row,1]
    my_tim_finland_rcp45_e_DSbias_b_site_ig_only[row,1] = my_tim_finland_rcp45_e_DSbias_a_site_only[row,1]
  } else {
    my_tim_sweden_rcp45_e_DSbias_b_site_ig_only[row,1] = my_tim_sweden_rcp45_e[row]
    my_tim_finland_rcp45_e_DSbias_b_site_ig_only[row,1] = my_tim_finland_rcp45_e[row]
  }

  if (model_input_all_ns_1myr_AP_GSL[row,4] >= slr_gl_sites_threshold_sweden_lower) {
    my_tim_sweden_rcp85_e_DSbias_b_site_ig_only[row,1] = my_tim_sweden_rcp85_e_DSbias_a_site_only[row,1]
    my_tim_finland_rcp85_e_DSbias_b_site_ig_only[row,1] = my_tim_finland_rcp85_e_DSbias_a_site_only[row,1]
  } else {
    my_tim_sweden_rcp85_e_DSbias_b_site_ig_only[row,1] = my_tim_sweden_rcp85_e[row]
    my_tim_finland_rcp85_e_DSbias_b_site_ig_only[row,1] = my_tim_finland_rcp85_e[row]
  }
  
}
  
plot(times, my_tim_sweden_zero_emissions_e_DSbias_b_site_ig_only, type = 'l', ylim = c(-25, 10))
lines(times, my_tim_finland_zero_emissions_e_DSbias_b_site_ig_only, type = 'l', ylim = c(-25, 10), col='red')


# Downscale emulated data (1 Myr) to regional scale (simple bias correction compared to obs; DSbias_c + DSbias_d_ig_only) ----------------------------------------------------------

# Import CRU gridded orography data (v2.1)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/cru-elevation.nc")
orog_cru = ncvar_get(nc_file,"elv")
nc_close(nc_file)


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

temp_cru_ann[which(orog_cru<=0,arr.ind=TRUE)] = NaN
orog_cru[which(orog_cru<=0,arr.ind=TRUE)] = NaN
temp_cru_ann[is.na(orog_cru)] = NaN
orog_cru[is.na(orog_cru)] = NaN
temp_tdstb_interp_smooth_orig[which(is.na(orog_cru))] = NaN

lons_cru_orig_scand = lons_cru_orig[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max)]
lats_cru_scand = lats_cru[(ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)]

temp_tdstb_interp_smooth_orig_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
temp_tdstb_interp_smooth_orig_scand[,] = temp_tdstb_interp_smooth_orig[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max))]

#image.plot(lons_cru_orig_scand, lats_cru_scand, temp_tdstb_interp_smooth_orig_scand[,], zlim = c(-30,30))


# Calculate anomaly (absolute difference; bias) between observed (CRU) and modelled (tdstb) pre-industrial temperature

temp_PI_anom_tdstb_cru_interp_smooth = array(NaN,c(nx_cru,ny_cru,1))
temp_PI_anom_tdstb_cru_interp_smooth[,,1] = temp_tdstb_interp_smooth_orig - temp_cru_ann

# image.plot(temp_tdstb_interp_smooth_orig, zlim = c(-30,30))
# image.plot(temp_cru_ann, zlim = c(-30,30))
# image.plot(temp_PI_anom_tdstb_cru_interp_smooth[,,ne_toDSbias], zlim = c(-10,10))


# Import orography data (Singarayer + Valdes, and Peltier 1deg)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice_orog_interp_smooth.nc")
orog_modice_interp_smooth_orig = ncvar_get(nc_file,"ht")
nc_close(nc_file)

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

# image.plot(orog_peltier_interp_smooth_orig[,,20])
# image.plot(orog_peltier_interp_smooth_diff_0kyr[,,20])


# Calculate modice smooth orography

nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice_orog_interp_smooth.nc")
orog_modice_interp_smooth_orig = ncvar_get(nc_file,"ht")
nc_close(nc_file)

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





#ATKA - variables reinstated
scenario_means_zero_emissions_orig = scenario_means_orig[,,,1]
scenario_means_rcp26_orig = scenario_means_orig[,,,2]
scenario_means_rcp45_orig = scenario_means_orig[,,,3]
scenario_means_rcp85_orig = scenario_means_orig[,,,4]

print("Beginning bias correction downscaling for all years")

# Natural ------

# Regrid emulator data to same format as CRU

scenario_means_zero_emissions_orig_regrid = array(0,c(nx,ny,ne))
n = ny
for (e in c(1:ne)){
  for (y in c(1:ny)){
    scenario_means_zero_emissions_orig_regrid[1:nx,y,e] = c(scenario_means_zero_emissions_orig[(nx2+1):nx,n,e],scenario_means_zero_emissions_orig[1:nx2,n,e])
    n = n-1
  }
  n = ny
}


ne_toDSbias = 1

em_zero_emissions_interp_smooth_scand_DSbias_c_sweden = array(NaN,c(ne,ne_toDSbias))
em_zero_emissions_interp_smooth_scand_DSbias_c_finland = array(NaN,c(ne,ne_toDSbias))

em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_sweden = array(NaN,c(ne,ne_toDSbias))
em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_finland = array(NaN,c(ne,ne_toDSbias))

num_exp_toDSbias = 1

# Create array to hold regional downscaled (simple bias correction) data at all 1001 timesteps (extract array size from regional input file i.e. first 2 values, nx and ny)
# For Korean Peninsular, this = 11,14,1001.  For Scandinavia, this = 57,41,1001

final_temp_zero_emissions_region_alltimes = array(0,c(location_region[1],location_region[2],1001)) 

for (num_exp_toDSbias in c(1:ne)){ # Begins year loop

  # Calculate orography to use
  
  gsl_index_ts = which.min(abs(model_input_zero_emissions_1myr_AP[num_exp_toDSbias,5] - gsl_list_all))
  gsl_value_ts = gsl_list_all[[gsl_index_ts]]
  orog_cru_peltier_interp_orig_ts = orog_all_cru_peltier_interp_orig[,,gsl_index_ts]
  
  
  # Interpolate emulated data
  
  bilinterp_lon <- seq( 1,96,,720)
  bilinterp_lat <- seq( 1,73,,360)
  make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid

  bilinterp_data <- list( x= 1:96, y=1:73, z= scenario_means_zero_emissions_orig_regrid[,,num_exp_toDSbias])
  interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
  bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))


  orog_cru_peltier_interp_orig_ts[which(orog_cru_peltier_interp_orig_ts<=0,arr.ind=TRUE)] = NaN
  bilinterp_newdata[is.na(orog_cru_peltier_interp_orig_ts)] = NaN # Assign grid boxes with no orography data (LT 0) as missing values

  temp_em_zero_emissions_interp_smooth_orig = array(NaN,c(nx_cru,ny_cru,ne_toDSbias))
  temp_em_zero_emissions_interp_smooth_orig[,,ne_toDSbias] = bilinterp_newdata
  
  # image.plot(model_output_tdstb_regrid[,])
  
  # image.plot(temp_tdstb_interp_smooth_orig[,])
  
  # image.plot(scenario_means_zero_emissions_orig_regrid[,,ne_toDSbias])

  # image.plot(temp_em_zero_emissions_interp_smooth_orig[,,ne_toDSbias])

  
  # Calculate downscaled regional temperature (DSbias_c)
  
  em_zero_emissions_interp_smooth_DSbias_c = array(NaN,c(nx_cru,ny_cru,ne_toDSbias))
  em_zero_emissions_interp_smooth_DSbias_c[,,ne_toDSbias] = temp_em_zero_emissions_interp_smooth_orig[,,ne_toDSbias] - temp_PI_anom_tdstb_cru_interp_smooth[,,ne_toDSbias]

  # image.plot(temp_em_zero_emissions_interp_smooth_orig[,,ne_toDSbias], zlim = c(-30,30))
  # image.plot(em_zero_emissions_interp_smooth_DSbias_c[,,ne_toDSbias], zlim = c(-30,30))
  
  
  lons_cru_orig_scand = lons_cru_orig[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max)]
  lats_cru_scand = lats_cru[(ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)]
  
  temp_cru_ann_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
  temp_cru_ann_scand[,] = temp_cru_ann[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max))]

  temp_em_zero_emissions_interp_smooth_DSbias_c_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSbias))
  temp_em_zero_emissions_interp_smooth_DSbias_c_scand[,,ne_toDSbias] = em_zero_emissions_interp_smooth_DSbias_c[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)),ne_toDSbias]

  # image.plot(lons_cru_orig_scand, lats_cru_scand, temp_em_zero_emissions_interp_smooth_DSbias_c_scand[,,ne_toDSbias], zlim = c(-10,10))
  
  
  em_zero_emissions_interp_smooth_scand_DSbias_c_sweden[num_exp_toDSbias,ne_toDSbias] = array(temp_em_zero_emissions_interp_smooth_DSbias_c_scand[Forsmark_lon_GB_CRU_scand,Forsmark_lat_GB_CRU_scand,ne_toDSbias])
  
  em_zero_emissions_interp_smooth_scand_DSbias_c_finland[num_exp_toDSbias,ne_toDSbias] = array(temp_em_zero_emissions_interp_smooth_DSbias_c_scand[Olkiluoto_lon_GB_CRU_scand,Olkiluoto_lat_GB_CRU_scand,ne_toDSbias])
  
  
  # Convert data to correct format for mapping
  
  temp_em_zero_emissions_interp_smooth_orig_big = temp_em_zero_emissions_interp_smooth_orig[,,ne_toDSbias]
  temp_em_zero_emissions_interp_smooth_DSbias_c_scand_big = temp_em_zero_emissions_interp_smooth_DSbias_c_scand[,,ne_toDSbias]
 
  # CW: At each time step, save in the larger (final) array 
  
  final_temp_zero_emissions_region_alltimes[,,num_exp_toDSbias] = temp_em_zero_emissions_interp_smooth_DSbias_c_scand_big
  
  message(paste(num_exp_toDSbias))

} # Close year loop

# Save data - SAT DOWNSCALED OVER SCANDINAVIA AT ALL TIME SLICES OVER NEXT 1 MYR - CORRESPONDS TO SIMPLE BIAS CORRECTION (ALTHOUGH INCLUDING OROGRAPHY), WRITTEN OUT
# AS TEXT FILE *Dbias_c* IN MAIN SCRIPT, E.G. em_*_interp_smooth_scand_DSbias_c_finland/sweden

kyr_num = c(0:(ne-1))

# define global attributes
nc_institution = "University of Bristol, 2018"
nc_datasource = "Climate emulator"
    
nc_title = "2m air temperature"
nc_file_name <- paste(dataoutfilepath,"Em_output_data_",EmulVars,"_region_bc_natural_0_to_1MyrAP.nc",sep="")

# define dimensions
lon_dim <- ncdim_def("longitude","degrees_east",as.double(lons_cru_orig_scand))
lat_dim <- ncdim_def("latitude","degrees_north",as.double(lats_cru_scand))
time_dim <- ncdim_def("time","",as.double(kyr_num))
surf_dim <- ncdim_def("surface","",as.double(1))

# define variables
miss_value <- 2e20
nc_var_name <- "SAT"
nc_var_unit <- "degC"
nc_var_lname <- "2m air temperature"

# create variable and ncdf file with first variable so dimensions are set
var_def <- ncvar_def(nc_var_name,nc_var_unit,list(lon_dim,lat_dim,surf_dim,time_dim),miss_value,nc_var_lname,prec="double")
nc_file <- nc_create(nc_file_name,var_def)
      
# define data to save
data_to_save = final_temp_zero_emissions_region_alltimes[,,]
      
# add data to ncdf file
ncvar_put(nc_file,var_def,data_to_save)
      
# put additional attributes into dimension and data variables
ncatt_put(nc_file,"longitude","axis","X" ,verbose=FALSE, definemode=FALSE)
ncatt_put(nc_file,"latitude","axis","Y")
ncatt_put(nc_file,"time","axis","T")
    
# add global attributes
ncatt_put(nc_file,0,"title",nc_title)
ncatt_put(nc_file,0,"institution",nc_institution)
ncatt_put(nc_file,0,"source",nc_datasource)
    
# close the file, writing data to disk
nc_close(nc_file)

#plot(times, em_zero_emissions_interp_smooth_scand_DSbias_c_sweden, type = 'l', ylim = c(-25, 10))
#lines(times, em_zero_emissions_interp_smooth_scand_DSbias_c_finland, type = 'l', ylim = c(-25, 10), col='red')

# Temporarily store first time slice

global_way = final_temp_zero_emissions_region_alltimes[,,1]

image.plot(final_temp_zero_emissions_region_alltimes[,,1], zlim = c(2,17))

# RCP2.6 ------

# Regrid emulator data to same format as CRU

scenario_means_rcp26_orig_regrid = array(0,c(nx,ny,ne))
n = ny
for (e in c(1:ne)){
  for (y in c(1:ny)){
    scenario_means_rcp26_orig_regrid[1:nx,y,e] = c(scenario_means_rcp26_orig[(nx2+1):nx,n,e],scenario_means_rcp26_orig[1:nx2,n,e])
    n = n-1
  }
  n = ny
}


ne_toDSbias = 1

em_rcp26_interp_smooth_scand_DSbias_c_sweden = array(NaN,c(ne,ne_toDSbias))
em_rcp26_interp_smooth_scand_DSbias_c_finland = array(NaN,c(ne,ne_toDSbias))

em_rcp26_interp_smooth_scand_DSbias_d_ig_only_sweden = array(NaN,c(ne,ne_toDSbias))
em_rcp26_interp_smooth_scand_DSbias_d_ig_only_finland = array(NaN,c(ne,ne_toDSbias))

num_exp_toDSbias = 1

# Create array to hold regional downscaled (simple bias correction) data at all 1001 timesteps (extract array size from regional input file i.e. first 2 values, nx and ny)
# For Korean Peninsular, this = 11,14,1001.  For Scandinavia, this = 57,41,1001

final_temp_rcp26_region_alltimes = array(0,c(location_region[1],location_region[2],1001)) 

for (num_exp_toDSbias in c(1:ne)){
  
  # Calculate orography to use
  
  gsl_index_ts = which.min(abs(model_input_rcp26_1myr_AP[num_exp_toDSbias,5] - gsl_list_all))
  gsl_value_ts = gsl_list_all[[gsl_index_ts]]
  orog_cru_peltier_interp_orig_ts = orog_all_cru_peltier_interp_orig[,,gsl_index_ts]

    # Interpolate emulated data
  
  bilinterp_lon <- seq( 1,96,,720)
  bilinterp_lat <- seq( 1,73,,360)
  make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid
  
  bilinterp_data <- list( x= 1:96, y=1:73, z= scenario_means_rcp26_orig_regrid[,,num_exp_toDSbias])
  interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
  bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))
  
  orog_cru_peltier_interp_orig_ts[which(orog_cru_peltier_interp_orig_ts<=0,arr.ind=TRUE)] = NaN
  bilinterp_newdata[is.na(orog_cru_peltier_interp_orig_ts)] = NaN # Assign grid boxes with no orography data (LT 0) as missing values

  temp_em_rcp26_interp_smooth_orig = array(NaN,c(nx_cru,ny_cru,ne_toDSbias))
  temp_em_rcp26_interp_smooth_orig[,,ne_toDSbias] = bilinterp_newdata
  
  # image.plot(model_output_tdstb_regrid[,])
  
  # image.plot(temp_tdstb_interp_smooth_orig[,])
  
  # image.plot(scenario_means_rcp26_orig_regrid[,,ne_toDSbias])
  
  # image.plot(temp_em_rcp26_interp_smooth_orig[,,ne_toDSbias])
  
  
  # Calculate downscaled regional temperature (DSbias_c)
  
  temp_em_rcp26_interp_smooth_DSbias_c = array(NaN,c(nx_cru,ny_cru,ne_toDSbias))
  temp_em_rcp26_interp_smooth_DSbias_c[,,ne_toDSbias] = temp_em_rcp26_interp_smooth_orig[,,ne_toDSbias] - temp_PI_anom_tdstb_cru_interp_smooth[,,ne_toDSbias]
  
  # image.plot(temp_em_rcp26_interp_smooth_orig[,,ne_toDSbias], zlim = c(-30,30))
  # image.plot(temp_em_rcp26_interp_smooth_DSbias_c[,,ne_toDSbias], zlim = c(-30,30))
  
  lons_cru_orig_scand = lons_cru_orig[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max)]
  lats_cru_scand = lats_cru[(ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)]
  
  temp_cru_ann_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
  temp_cru_ann_scand[,] = temp_cru_ann[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max))]
  
  temp_em_rcp26_interp_smooth_DSbias_c_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSbias))
  temp_em_rcp26_interp_smooth_DSbias_c_scand[,,ne_toDSbias] = temp_em_rcp26_interp_smooth_DSbias_c[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)),ne_toDSbias]
  
  # image.plot(lons_cru_orig_scand, lats_cru_scand, temp_em_rcp26_interp_smooth_DSbias_c_scand[,,ne_toDSbias], zlim = c(-30,30))
  
  em_rcp26_interp_smooth_scand_DSbias_c_sweden[num_exp_toDSbias,ne_toDSbias] = array(temp_em_rcp26_interp_smooth_DSbias_c_scand[Forsmark_lon_GB_CRU_scand,Forsmark_lat_GB_CRU_scand,ne_toDSbias])
  
  em_rcp26_interp_smooth_scand_DSbias_c_finland[num_exp_toDSbias,ne_toDSbias] = array(temp_em_rcp26_interp_smooth_DSbias_c_scand[Olkiluoto_lon_GB_CRU_scand,Olkiluoto_lat_GB_CRU_scand,ne_toDSbias])
  
  
  # Convert data to correct format for mapping
  
  temp_em_rcp26_interp_smooth_orig_big = temp_em_rcp26_interp_smooth_orig[,,ne_toDSbias]
  temp_em_rcp26_interp_smooth_DSbias_c_scand_big = temp_em_rcp26_interp_smooth_DSbias_c_scand[,,ne_toDSbias]
  
  # CW: At each time step, save in the larger (final) array 
  
  final_temp_rcp26_region_alltimes[,,num_exp_toDSbias] = temp_em_rcp26_interp_smooth_DSbias_c_scand_big

  message(paste(num_exp_toDSbias))
  
} # Close year loop

# Save data - SAT DOWNSCALED OVER SCANDINAVIA AT ALL TIME SLICES OVER NEXT 1 MYR - CORRESPONDS TO SIMPLE BIAS CORRECTION (ALTHOUGH INCLUDING OROGRAPHY), WRITTEN OUT
# AS TEXT FILE *Dbias_c* IN MAIN SCRIPT, E.G. em_*_interp_smooth_scand_DSbias_c_finland/sweden

kyr_num = c(0:(ne-1))

# define global attributes
nc_institution = "University of Bristol, 2018"
nc_datasource = "Climate emulator"

nc_title = "2m air temperature"
nc_file_name <- paste(dataoutfilepath,"Em_output_data_",EmulVars,"_region_bc_rcp26_0_to_1MyrAP.nc",sep="")

# define dimensions
lon_dim <- ncdim_def("longitude","degrees_east",as.double(lons_cru_orig_scand))
lat_dim <- ncdim_def("latitude","degrees_north",as.double(lats_cru_scand))
time_dim <- ncdim_def("time","",as.double(kyr_num))
surf_dim <- ncdim_def("surface","",as.double(1))

# define variables
miss_value <- 2e20
nc_var_name <- "SAT"
nc_var_unit <- "degC"
nc_var_lname <- "2m air temperature"

# create variable and ncdf file with first variable so dimensions are set
var_def <- ncvar_def(nc_var_name,nc_var_unit,list(lon_dim,lat_dim,surf_dim,time_dim),miss_value,nc_var_lname,prec="double")
nc_file <- nc_create(nc_file_name,var_def)

# define data to save
data_to_save = final_temp_rcp26_region_alltimes[,,]

# add data to ncdf file
ncvar_put(nc_file,var_def,data_to_save)

# put additional attributes into dimension and data variables
ncatt_put(nc_file,"longitude","axis","X" ,verbose=FALSE, definemode=FALSE)
ncatt_put(nc_file,"latitude","axis","Y")
ncatt_put(nc_file,"time","axis","T")

# add global attributes
ncatt_put(nc_file,0,"title",nc_title)
ncatt_put(nc_file,0,"institution",nc_institution)
ncatt_put(nc_file,0,"source",nc_datasource)

# close the file, writing data to disk
nc_close(nc_file)

#plot(times, em_rcp26_interp_smooth_scand_DSbias_c_sweden, type = 'l', ylim = c(-25, 10))
#lines(times, em_rcp26_interp_smooth_scand_DSbias_c_finland, type = 'l', ylim = c(-25, 10), col='red')
image.plot(final_temp_rcp26_region_alltimes[,,1])


# RCP4.5 ------

# Regrid emulator data to same format as CRU

scenario_means_rcp45_orig_regrid = array(0,c(nx,ny,ne))
n = ny
for (e in c(1:ne)){
  for (y in c(1:ny)){
    scenario_means_rcp45_orig_regrid[1:nx,y,e] = c(scenario_means_rcp45_orig[(nx2+1):nx,n,e],scenario_means_rcp45_orig[1:nx2,n,e])
    n = n-1
  }
  n = ny
}


ne_toDSbias = 1

em_rcp45_interp_smooth_scand_DSbias_c_sweden = array(NaN,c(ne,ne_toDSbias))
em_rcp45_interp_smooth_scand_DSbias_c_finland = array(NaN,c(ne,ne_toDSbias))

em_rcp45_interp_smooth_scand_DSbias_d_ig_only_sweden = array(NaN,c(ne,ne_toDSbias))
em_rcp45_interp_smooth_scand_DSbias_d_ig_only_finland = array(NaN,c(ne,ne_toDSbias))

num_exp_toDSbias = 1

# Create array to hold regional downscaled (simple bias correction) data at all 1001 timesteps (extract array size from regional input file i.e. first 2 values, nx and ny)
# For Korean Peninsular, this = 11,14,1001.  For Scandinavia, this = 57,41,1001

final_temp_rcp45_region_alltimes = array(0,c(location_region[1],location_region[2],1001)) 

for (num_exp_toDSbias in c(1:ne)){
  
  # Calculate orography to use
  
  gsl_index_ts = which.min(abs(model_input_rcp45_1myr_AP[num_exp_toDSbias,5] - gsl_list_all))
  gsl_value_ts = gsl_list_all[[gsl_index_ts]]
  orog_cru_peltier_interp_orig_ts = orog_all_cru_peltier_interp_orig[,,gsl_index_ts]
  
  # Interpolate emulated data
  
  bilinterp_lon <- seq( 1,96,,720)
  bilinterp_lat <- seq( 1,73,,360)
  make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid
  
  bilinterp_data <- list( x= 1:96, y=1:73, z= scenario_means_rcp45_orig_regrid[,,num_exp_toDSbias])
  interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
  bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))
  
  orog_cru_peltier_interp_orig_ts[which(orog_cru_peltier_interp_orig_ts<=0,arr.ind=TRUE)] = NaN
  bilinterp_newdata[is.na(orog_cru_peltier_interp_orig_ts)] = NaN # Assign grid boxes with no orography data (LT 0) as missing values
  
  temp_em_rcp45_interp_smooth_orig = array(NaN,c(nx_cru,ny_cru,ne_toDSbias))
  temp_em_rcp45_interp_smooth_orig[,,ne_toDSbias] = bilinterp_newdata
  
  # image.plot(model_output_tdstb_regrid[,])
  
  # image.plot(temp_tdstb_interp_smooth_orig[,])
  
  # image.plot(scenario_means_rcp45_orig_regrid[,,ne_toDSbias])
  
  # image.plot(temp_em_rcp45_interp_smooth_orig[,,ne_toDSbias])
  
  
  # Calculate downscaled regional temperature (DSbias_c)
  
  temp_em_rcp45_interp_smooth_DSbias_c = array(NaN,c(nx_cru,ny_cru,ne_toDSbias))
  temp_em_rcp45_interp_smooth_DSbias_c[,,ne_toDSbias] = temp_em_rcp45_interp_smooth_orig[,,ne_toDSbias] - temp_PI_anom_tdstb_cru_interp_smooth[,,ne_toDSbias]
  
  # image.plot(temp_em_rcp45_interp_smooth_orig[,,ne_toDSbias], zlim = c(-30,30))
  # image.plot(temp_em_rcp45_interp_smooth_DSbias_c[,,ne_toDSbias], zlim = c(-30,30))
  
  lons_cru_orig_scand = lons_cru_orig[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max)]
  lats_cru_scand = lats_cru[(ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)]
  
  temp_cru_ann_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
  temp_cru_ann_scand[,] = temp_cru_ann[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max))]
  
  temp_em_rcp45_interp_smooth_DSbias_c_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSbias))
  temp_em_rcp45_interp_smooth_DSbias_c_scand[,,ne_toDSbias] = temp_em_rcp45_interp_smooth_DSbias_c[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)),ne_toDSbias]
  
  # image.plot(lons_cru_orig_scand, lats_cru_scand, temp_em_rcp45_interp_smooth_DSbias_c_scand[,,ne_toDSbias], zlim = c(-30,30))
  
  em_rcp45_interp_smooth_scand_DSbias_c_sweden[num_exp_toDSbias,ne_toDSbias] = array(temp_em_rcp45_interp_smooth_DSbias_c_scand[Forsmark_lon_GB_CRU_scand,Forsmark_lat_GB_CRU_scand,ne_toDSbias])

  em_rcp45_interp_smooth_scand_DSbias_c_finland[num_exp_toDSbias,ne_toDSbias] = array(temp_em_rcp45_interp_smooth_DSbias_c_scand[Olkiluoto_lon_GB_CRU_scand,Olkiluoto_lat_GB_CRU_scand,ne_toDSbias])
  
  
  # Convert data to correct format for mapping
  
  temp_em_rcp45_interp_smooth_orig_big = temp_em_rcp45_interp_smooth_orig[,,ne_toDSbias]
  temp_em_rcp45_interp_smooth_DSbias_c_scand_big = temp_em_rcp45_interp_smooth_DSbias_c_scand[,,ne_toDSbias]
  
  # CW: At each time step, save in the larger (final) array 
  
  final_temp_rcp45_region_alltimes[,,num_exp_toDSbias] = temp_em_rcp45_interp_smooth_DSbias_c_scand_big

  message(paste(num_exp_toDSbias))
  
} # Close year loop

# Save data - SAT DOWNSCALED OVER SCANDINAVIA AT ALL TIME SLICES OVER NEXT 1 MYR - CORRESPONDS TO SIMPLE BIAS CORRECTION (ALTHOUGH INCLUDING OROGRAPHY), WRITTEN OUT
# AS TEXT FILE *Dbias_c* IN MAIN SCRIPT, E.G. em_*_interp_smooth_scand_DSbias_c_finland/sweden

kyr_num = c(0:(ne-1))

# define global attributes
nc_institution = "University of Bristol, 2018"
nc_datasource = "Climate emulator"

nc_title = "2m air temperature"
nc_file_name <- paste(dataoutfilepath,"Em_output_data_",EmulVars,"_region_bc_rcp45_0_to_1MyrAP.nc",sep="")

# define dimensions
lon_dim <- ncdim_def("longitude","degrees_east",as.double(lons_cru_orig_scand))
lat_dim <- ncdim_def("latitude","degrees_north",as.double(lats_cru_scand))
time_dim <- ncdim_def("time","",as.double(kyr_num))
surf_dim <- ncdim_def("surface","",as.double(1))

# define variables
miss_value <- 2e20
nc_var_name <- "SAT"
nc_var_unit <- "degC"
nc_var_lname <- "2m air temperature"

# create variable and ncdf file with first variable so dimensions are set
var_def <- ncvar_def(nc_var_name,nc_var_unit,list(lon_dim,lat_dim,surf_dim,time_dim),miss_value,nc_var_lname,prec="double")
nc_file <- nc_create(nc_file_name,var_def)

# define data to save
data_to_save = final_temp_rcp45_region_alltimes[,,]

# add data to ncdf file
ncvar_put(nc_file,var_def,data_to_save)

# put additional attributes into dimension and data variables
ncatt_put(nc_file,"longitude","axis","X" ,verbose=FALSE, definemode=FALSE)
ncatt_put(nc_file,"latitude","axis","Y")
ncatt_put(nc_file,"time","axis","T")

# add global attributes
ncatt_put(nc_file,0,"title",nc_title)
ncatt_put(nc_file,0,"institution",nc_institution)
ncatt_put(nc_file,0,"source",nc_datasource)

# close the file, writing data to disk
nc_close(nc_file)

#plot(times, em_rcp45_interp_smooth_scand_DSbias_c_sweden, type = 'l', ylim = c(-25, 10))
#lines(times, em_rcp45_interp_smooth_scand_DSbias_c_finland, type = 'l', ylim = c(-25, 10), col='red')
image.plot(final_temp_rcp45_region_alltimes[,,1])

# RCP8.5 ------

# Regrid emulator data to same format as CRU

scenario_means_rcp85_orig_regrid = array(0,c(nx,ny,ne))
n = ny
for (e in c(1:ne)){
  for (y in c(1:ny)){
    scenario_means_rcp85_orig_regrid[1:nx,y,e] = c(scenario_means_rcp85_orig[(nx2+1):nx,n,e],scenario_means_rcp85_orig[1:nx2,n,e])
    n = n-1
  }
  n = ny
}


ne_toDSbias = 1

em_rcp85_interp_smooth_scand_DSbias_c_sweden = array(NaN,c(ne,ne_toDSbias))
em_rcp85_interp_smooth_scand_DSbias_c_finland = array(NaN,c(ne,ne_toDSbias))

em_rcp85_interp_smooth_scand_DSbias_d_ig_only_sweden = array(NaN,c(ne,ne_toDSbias))
em_rcp85_interp_smooth_scand_DSbias_d_ig_only_finland = array(NaN,c(ne,ne_toDSbias))

num_exp_toDSbias = 1

# Create array to hold regional downscaled (simple bias correction) data at all 1001 timesteps (extract array size from regional input file i.e. first 2 values, nx and ny)
# For Korean Peninsular, this = 11,14,1001.  For Scandinavia, this = 57,41,1001

final_temp_rcp85_region_alltimes = array(0,c(location_region[1],location_region[2],1001)) 

for (num_exp_toDSbias in c(1:ne)){
  
  # Calculate orography to use
  
  gsl_index_ts = which.min(abs(model_input_rcp85_1myr_AP[num_exp_toDSbias,5] - gsl_list_all))
  gsl_value_ts = gsl_list_all[[gsl_index_ts]]
  orog_cru_peltier_interp_orig_ts = orog_all_cru_peltier_interp_orig[,,gsl_index_ts]
  
  # Interpolate emulated data
  
  bilinterp_lon <- seq( 1,96,,720)
  bilinterp_lat <- seq( 1,73,,360)
  make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid
  
  bilinterp_data <- list( x= 1:96, y=1:73, z= scenario_means_rcp85_orig_regrid[,,num_exp_toDSbias])
  interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
  bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))
  
  orog_cru_peltier_interp_orig_ts[which(orog_cru_peltier_interp_orig_ts<=0,arr.ind=TRUE)] = NaN
  bilinterp_newdata[is.na(orog_cru_peltier_interp_orig_ts)] = NaN # Assign grid boxes with no orography data (LT 0) as missing values
  
  temp_em_rcp85_interp_smooth_orig = array(NaN,c(nx_cru,ny_cru,ne_toDSbias))
  temp_em_rcp85_interp_smooth_orig[,,ne_toDSbias] = bilinterp_newdata
  
  # image.plot(model_output_tdstb_regrid[,])
  
  # image.plot(temp_tdstb_interp_smooth_orig[,])
  
  # image.plot(scenario_means_rcp85_orig_regrid[,,ne_toDSbias])
  
  # image.plot(temp_em_rcp85_interp_smooth_orig[,,ne_toDSbias])

  
  # Calculate downscaled regional temperature (DSbias_c)
  
  temp_em_rcp85_interp_smooth_DSbias_c = array(NaN,c(nx_cru,ny_cru,ne_toDSbias))
  temp_em_rcp85_interp_smooth_DSbias_c[,,ne_toDSbias] = temp_em_rcp85_interp_smooth_orig[,,ne_toDSbias] - temp_PI_anom_tdstb_cru_interp_smooth[,,ne_toDSbias]
  
  # image.plot(temp_em_rcp85_interp_smooth_orig[,,ne_toDSbias], zlim = c(-30,30))
  # image.plot(temp_em_rcp85_interp_smooth_DSbias_c[,,ne_toDSbias], zlim = c(-30,30))
  
  lons_cru_orig_scand = lons_cru_orig[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max)]
  lats_cru_scand = lats_cru[(ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)]
  
  temp_cru_ann_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
  temp_cru_ann_scand[,] = temp_cru_ann[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max))]
  
  temp_em_rcp85_interp_smooth_DSbias_c_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSbias))
  temp_em_rcp85_interp_smooth_DSbias_c_scand[,,ne_toDSbias] = temp_em_rcp85_interp_smooth_DSbias_c[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)),ne_toDSbias]
  
  # image.plot(lons_cru_orig_scand, lats_cru_scand, temp_em_rcp85_interp_smooth_DSbias_c_scand[,,ne_toDSbias], zlim = c(-30,30))
  
  em_rcp85_interp_smooth_scand_DSbias_c_sweden[num_exp_toDSbias,ne_toDSbias] = array(temp_em_rcp85_interp_smooth_DSbias_c_scand[Forsmark_lon_GB_CRU_scand,Forsmark_lat_GB_CRU_scand,ne_toDSbias])
  
  em_rcp85_interp_smooth_scand_DSbias_c_finland[num_exp_toDSbias,ne_toDSbias] = array(temp_em_rcp85_interp_smooth_DSbias_c_scand[Olkiluoto_lon_GB_CRU_scand,Olkiluoto_lat_GB_CRU_scand,ne_toDSbias])
  
  
  # Convert data to correct format for mapping
  
  temp_em_rcp85_interp_smooth_orig_big = temp_em_rcp85_interp_smooth_orig[,,ne_toDSbias]
  temp_em_rcp85_interp_smooth_DSbias_c_scand_big = temp_em_rcp85_interp_smooth_DSbias_c_scand[,,ne_toDSbias]
  
  # CW: At each time step, save in the larger (final) array 
  
  final_temp_rcp85_region_alltimes[,,num_exp_toDSbias] = temp_em_rcp85_interp_smooth_DSbias_c_scand_big

  message(paste(num_exp_toDSbias))
  
} # Close year loop

# Save data - SAT DOWNSCALED OVER SCANDINAVIA AT ALL TIME SLICES OVER NEXT 1 MYR - CORRESPONDS TO SIMPLE BIAS CORRECTION (ALTHOUGH INCLUDING OROGRAPHY), WRITTEN OUT
# AS TEXT FILE *Dbias_c* IN MAIN SCRIPT, E.G. em_*_interp_smooth_scand_DSbias_c_finland/sweden

kyr_num = c(0:(ne-1))

# define global attributes
nc_institution = "University of Bristol, 2018"
nc_datasource = "Climate emulator"

nc_title = "2m air temperature"
nc_file_name <- paste(dataoutfilepath,"Em_output_data_",EmulVars,"_region_bc_rcp85_0_to_1MyrAP.nc",sep="")

# define dimensions
lon_dim <- ncdim_def("longitude","degrees_east",as.double(lons_cru_orig_scand))
lat_dim <- ncdim_def("latitude","degrees_north",as.double(lats_cru_scand))
time_dim <- ncdim_def("time","",as.double(kyr_num))
surf_dim <- ncdim_def("surface","",as.double(1))

# define variables
miss_value <- 2e20
nc_var_name <- "SAT"
nc_var_unit <- "degC"
nc_var_lname <- "2m air temperature"

# create variable and ncdf file with first variable so dimensions are set
var_def <- ncvar_def(nc_var_name,nc_var_unit,list(lon_dim,lat_dim,surf_dim,time_dim),miss_value,nc_var_lname,prec="double")
nc_file <- nc_create(nc_file_name,var_def)

# define data to save
data_to_save = final_temp_rcp85_region_alltimes[,,]

# add data to ncdf file
ncvar_put(nc_file,var_def,data_to_save)

# put additional attributes into dimension and data variables
ncatt_put(nc_file,"longitude","axis","X" ,verbose=FALSE, definemode=FALSE)
ncatt_put(nc_file,"latitude","axis","Y")
ncatt_put(nc_file,"time","axis","T")

# add global attributes
ncatt_put(nc_file,0,"title",nc_title)
ncatt_put(nc_file,0,"institution",nc_institution)
ncatt_put(nc_file,0,"source",nc_datasource)

# close the file, writing data to disk
nc_close(nc_file)

#plot(times, em_rcp85_interp_smooth_scand_DSbias_c_sweden, type = 'l', ylim = c(-25, 10))
#lines(times, em_rcp85_interp_smooth_scand_DSbias_c_finland, type = 'l', ylim = c(-25, 10), col='red')
image.plot(final_temp_rcp85_region_alltimes[,,1])


# Calculate downscaled site temperature only when GSL is > lower threshold (DSbias_d_ig_only)

model_input_all_ns_1myr_AP_GSL = array(c(model_input_zero_emissions_1myr_AP[,5],model_input_rcp26_1myr_AP[,5],model_input_rcp45_1myr_AP[,5],model_input_rcp85_1myr_AP[,5]),c(ne,ns))

em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_sweden = array(NaN,c(ne,1))
em_rcp26_interp_smooth_scand_DSbias_d_ig_only_sweden = array(NaN,c(ne,1))
em_rcp45_interp_smooth_scand_DSbias_d_ig_only_sweden = array(NaN,c(ne,1))
em_rcp85_interp_smooth_scand_DSbias_d_ig_only_sweden = array(NaN,c(ne,1))
em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_finland = array(NaN,c(ne,1))
em_rcp26_interp_smooth_scand_DSbias_d_ig_only_finland = array(NaN,c(ne,1))
em_rcp45_interp_smooth_scand_DSbias_d_ig_only_finland = array(NaN,c(ne,1))
em_rcp85_interp_smooth_scand_DSbias_d_ig_only_finland = array(NaN,c(ne,1))

for (row in c(1:ne)){
  
  if (model_input_all_ns_1myr_AP_GSL[row,1] >= slr_gl_sites_threshold_sweden_lower) {
    em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_sweden[row,1] = em_zero_emissions_interp_smooth_scand_DSbias_c_sweden[row,1]
    em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_finland[row,1] = em_zero_emissions_interp_smooth_scand_DSbias_c_finland[row,1]
  } else {
    em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_sweden[row,1] = my_tim_sweden_zero_emissions_e[row]
    em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_finland[row,1] = my_tim_finland_zero_emissions_e[row]
  }
  
  if (model_input_all_ns_1myr_AP_GSL[row,2] >= slr_gl_sites_threshold_sweden_lower) {
    em_rcp26_interp_smooth_scand_DSbias_d_ig_only_sweden[row] = em_rcp26_interp_smooth_scand_DSbias_c_sweden[row]
    em_rcp26_interp_smooth_scand_DSbias_d_ig_only_finland[row] = em_rcp26_interp_smooth_scand_DSbias_c_finland[row]
  } else {
    em_rcp26_interp_smooth_scand_DSbias_d_ig_only_sweden[row] = my_tim_sweden_rcp26_e[row]
    em_rcp26_interp_smooth_scand_DSbias_d_ig_only_finland[row] = my_tim_finland_rcp26_e[row]
  }

  if (model_input_all_ns_1myr_AP_GSL[row,3] >= slr_gl_sites_threshold_sweden_lower) {
    em_rcp45_interp_smooth_scand_DSbias_d_ig_only_sweden[row] = em_rcp45_interp_smooth_scand_DSbias_c_sweden[row]
    em_rcp45_interp_smooth_scand_DSbias_d_ig_only_finland[row] = em_rcp45_interp_smooth_scand_DSbias_c_finland[row]
  } else {
    em_rcp45_interp_smooth_scand_DSbias_d_ig_only_sweden[row] = my_tim_sweden_rcp45_e[row]
    em_rcp45_interp_smooth_scand_DSbias_d_ig_only_finland[row] = my_tim_finland_rcp45_e[row]
  }

  if (model_input_all_ns_1myr_AP_GSL[row,4] >= slr_gl_sites_threshold_sweden_lower) {
    em_rcp85_interp_smooth_scand_DSbias_d_ig_only_sweden[row] = em_rcp85_interp_smooth_scand_DSbias_c_sweden[row]
    em_rcp85_interp_smooth_scand_DSbias_d_ig_only_finland[row] = em_rcp85_interp_smooth_scand_DSbias_c_finland[row]
  } else {
    em_rcp85_interp_smooth_scand_DSbias_d_ig_only_sweden[row] = my_tim_sweden_rcp85_e[row]
    em_rcp85_interp_smooth_scand_DSbias_d_ig_only_finland[row] = my_tim_finland_rcp85_e[row]
  }
  
}

plot(times, em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_sweden, type = 'l', ylim = c(-25, 10))
lines(times, em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_finland, type = 'l', ylim = c(-25, 10), col='red')



# Downscale warmest and coldest emulated timeslices to regional scale (simple bias correction compared to obs; DSbias_c) ----------------------------------------------------------
#ATKA - I think these hard coded timesteps will need to be updated
max_SAT_3rcps = c(1, 1, 2) # Maximum #ATKA - I don't think these will change, even for an updated scenario, so can remain hard coded
# min_SAT_3rcps = c(100, 132, 414) # Minimum just before ice coverage #ATKA
sl26 = modelInputs[["RCP26"]][,5]
sl45 = modelInputs[["RCP45"]][,5]
sl85 = modelInputs[["RCP85"]][,5]
min_SAT_3rcps = c(min(which(sl26< -92.59))-1,min(which(sl45< -92.59))-1,min(which(sl85< -92.59))-1)

scenario_means_3rcps_orig_regrid_warm = array(0,c(nx,ny,3))
scenario_means_3rcps_orig_regrid_cold = array(0,c(nx,ny,3))

data = scenario_means_rcp26_orig_regrid[,,max_SAT_3rcps[1]]

scenario_means_3rcps_orig_regrid_warm[,,1] = scenario_means_rcp26_orig_regrid[,,max_SAT_3rcps[1]]
scenario_means_3rcps_orig_regrid_warm[,,2] = scenario_means_rcp45_orig_regrid[,,max_SAT_3rcps[2]]
scenario_means_3rcps_orig_regrid_warm[,,3] = scenario_means_rcp85_orig_regrid[,,max_SAT_3rcps[3]]

scenario_means_3rcps_orig_regrid_cold[,,1] = scenario_means_rcp26_orig_regrid[,,min_SAT_3rcps[1]]
scenario_means_3rcps_orig_regrid_cold[,,2] = scenario_means_rcp45_orig_regrid[,,min_SAT_3rcps[2]]
scenario_means_3rcps_orig_regrid_cold[,,3] = scenario_means_rcp85_orig_regrid[,,min_SAT_3rcps[3]]

### NOT SAVED ###
# Calculate orography to use

gsl_index_ts_warm = array(NaN,c(1,3))
gsl_value_ts_warm = array(NaN,c(1,3))
gsl_index_ts_warm[,1] = which.min(abs(model_input_rcp26_1myr_AP[max_SAT_3rcps[1],5] - gsl_list_all))
gsl_index_ts_warm[,2] = which.min(abs(model_input_rcp45_1myr_AP[max_SAT_3rcps[2],5] - gsl_list_all))
gsl_index_ts_warm[,3] = which.min(abs(model_input_rcp85_1myr_AP[max_SAT_3rcps[3],5] - gsl_list_all))
gsl_value_ts_warm[,1] = gsl_list_all[[gsl_index_ts_warm[,1]]]
gsl_value_ts_warm[,2] = gsl_list_all[[gsl_index_ts_warm[,2]]]
gsl_value_ts_warm[,3] = gsl_list_all[[gsl_index_ts_warm[,3]]]
orog_cru_peltier_interp_orig_ts_all_warm = array(NaN,c(nx_cru,ny_cru,3))
orog_cru_peltier_interp_orig_ts_all_warm[,,1] = orog_all_cru_peltier_interp_orig[,,gsl_index_ts_warm[,1]]
orog_cru_peltier_interp_orig_ts_all_warm[,,2] = orog_all_cru_peltier_interp_orig[,,gsl_index_ts_warm[,2]]
orog_cru_peltier_interp_orig_ts_all_warm[,,3] = orog_all_cru_peltier_interp_orig[,,gsl_index_ts_warm[,3]]

gsl_index_ts_cold = array(NaN,c(1,3))
gsl_value_ts_cold = array(NaN,c(1,3))
gsl_index_ts_cold[,1] = which.min(abs(model_input_rcp26_1myr_AP[min_SAT_3rcps[1],5] - gsl_list_all))
gsl_index_ts_cold[,2] = which.min(abs(model_input_rcp45_1myr_AP[min_SAT_3rcps[2],5] - gsl_list_all))
gsl_index_ts_cold[,3] = which.min(abs(model_input_rcp85_1myr_AP[min_SAT_3rcps[3],5] - gsl_list_all))
gsl_value_ts_cold[,1] = gsl_list_all[[gsl_index_ts_cold[,1]]]
gsl_value_ts_cold[,2] = gsl_list_all[[gsl_index_ts_cold[,2]]]
gsl_value_ts_cold[,3] = gsl_list_all[[gsl_index_ts_cold[,3]]]
orog_cru_peltier_interp_orig_ts_all_cold = array(NaN,c(nx_cru,ny_cru,3))
orog_cru_peltier_interp_orig_ts_all_cold[,,1] = orog_all_cru_peltier_interp_orig[,,gsl_index_ts_cold[,1]]
orog_cru_peltier_interp_orig_ts_all_cold[,,2] = orog_all_cru_peltier_interp_orig[,,gsl_index_ts_cold[,2]]
orog_cru_peltier_interp_orig_ts_all_cold[,,3] = orog_all_cru_peltier_interp_orig[,,gsl_index_ts_cold[,3]]


temp_em_3rcps_interp_smooth_orig_warm = array(NaN,c(nx_cru,ny_cru,3))
temp_em_3rcps_interp_smooth_DSbias_c_warm = array(NaN,c(nx_cru,ny_cru,3))
temp_em_3rcps_interp_smooth_DSbias_c_warm_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,3))
temp_em_3rcps_interp_smooth_DSbias_c_warm_scand_big = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,3))

temp_em_3rcps_interp_smooth_orig_cold = array(NaN,c(nx_cru,ny_cru,3))
temp_em_3rcps_interp_smooth_DSbias_c_cold = array(NaN,c(nx_cru,ny_cru,3))
temp_em_3rcps_interp_smooth_DSbias_c_cold_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,3))
temp_em_3rcps_interp_smooth_DSbias_c_cold_scand_big = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,3))

temp_em_3rcps_interp_smooth_scand_DSbias_c_warm_sweden = array(NaN,c(3,1))
temp_em_3rcps_interp_smooth_scand_DSbias_c_warm_finland = array(NaN,c(3,1))

temp_em_3rcps_interp_smooth_scand_DSbias_c_cold_sweden = array(NaN,c(3,1))
temp_em_3rcps_interp_smooth_scand_DSbias_c_cold_finland = array(NaN,c(3,1))

num_exp_toDSbias = 1

for (num_exp_toDSbias in c(1:3)){
  
  # Interpolate emulated data
  
  bilinterp_lon <- seq( 1,96,,720)
  bilinterp_lat <- seq( 1,73,,360)
  make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid
  
  bilinterp_data <- list( x= 1:96, y=1:73, z= scenario_means_3rcps_orig_regrid_warm[,,num_exp_toDSbias])
  interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
  bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))

  orog_cru_peltier_interp_orig_ts_warm = orog_cru_peltier_interp_orig_ts_all_warm[,,num_exp_toDSbias]
  orog_cru_peltier_interp_orig_ts_warm[which(orog_cru_peltier_interp_orig_ts_warm<=0,arr.ind=TRUE)] = NaN
  bilinterp_newdata[which(is.na(orog_cru_peltier_interp_orig_ts_warm))] = NaN # Assign grid boxes with no orography data (LT 0) as missing values
  
  temp_em_3rcps_interp_smooth_orig_warm[,,num_exp_toDSbias] = bilinterp_newdata

  
  bilinterp_data <- list( x= 1:96, y=1:73, z= scenario_means_3rcps_orig_regrid_cold[,,num_exp_toDSbias])
  interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
  bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))
  
  orog_cru_peltier_interp_orig_ts_cold = orog_cru_peltier_interp_orig_ts_all_cold[,,num_exp_toDSbias]
  orog_cru_peltier_interp_orig_ts_cold[which(orog_cru_peltier_interp_orig_ts_cold<=0,arr.ind=TRUE)] = NaN
  bilinterp_newdata[which(is.na(orog_cru_peltier_interp_orig_ts_cold))] = NaN # Assign grid boxes with no orography data (LT 0) as missing values
  
  temp_em_3rcps_interp_smooth_orig_cold[,,num_exp_toDSbias] = bilinterp_newdata
  
  # image.plot(model_output_tdstb_regrid[,])
  
  # image.plot(temp_tdstb_interp_smooth_orig[,])
  
  # image.plot(scenario_means_3rcps_orig_regrid_warm[,,num_exp_toDSbias])
  
  # image.plot(temp_em_3rcps_interp_smooth_orig_warm[,,num_exp_toDSbias])
  
  
  # Calculate anomaly (absolute difference; bias) between observed (CRU) and modelled (tdstb) pre-industrial temperature
  
  temp_PI_anom_tdstb_cru_interp_smooth = array(NaN,c(nx_cru,ny_cru,ne_toDSbias))
  temp_PI_anom_tdstb_cru_interp_smooth[,,ne_toDSbias] = temp_tdstb_interp_smooth_orig - temp_cru_ann
  
  #image.plot(temp_tdstb_interp_smooth_orig, zlim = c(-30,30))
  # image.plot(temp_cru_ann, zlim = c(-30,30))
  # image.plot(temp_PI_anom_tdstb_cru_interp_smooth[,,ne_toDSbias], zlim = c(-10,10))
  
  
  # Calculate downscaled regional temperature (DSbias_c)
  
  temp_em_3rcps_interp_smooth_DSbias_c_warm[,,num_exp_toDSbias] = temp_em_3rcps_interp_smooth_orig_warm[,,num_exp_toDSbias] - temp_PI_anom_tdstb_cru_interp_smooth[,,ne_toDSbias]

  temp_em_3rcps_interp_smooth_DSbias_c_cold[,,num_exp_toDSbias] = temp_em_3rcps_interp_smooth_orig_cold[,,num_exp_toDSbias] - temp_PI_anom_tdstb_cru_interp_smooth[,,ne_toDSbias]
  
  # image.plot(temp_em_3rcps_interp_smooth_orig_warm[,,num_exp_toDSbias], zlim = c(-30,30))
  # image.plot(temp_em_3rcps_interp_smooth_DSbias_c_warm[,,num_exp_toDSbias], zlim = c(-30,30))
  
  lons_cru_orig_scand = lons_cru_orig[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max)]
  lats_cru_scand = lats_cru[(ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)]
  
  temp_cru_ann_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
  temp_cru_ann_scand[,] = temp_cru_ann[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max))]
  
  temp_em_3rcps_interp_smooth_DSbias_c_warm_scand[,,num_exp_toDSbias] = temp_em_3rcps_interp_smooth_DSbias_c_warm[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)),num_exp_toDSbias]

  temp_em_3rcps_interp_smooth_DSbias_c_cold_scand[,,num_exp_toDSbias] = temp_em_3rcps_interp_smooth_DSbias_c_cold[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c((ny_cru-lats_cru_scand_min):(ny_cru-lats_cru_scand_max)),num_exp_toDSbias]
  
  image.plot(lons_cru_orig_scand, lats_cru_scand, temp_em_3rcps_interp_smooth_DSbias_c_warm_scand[,,num_exp_toDSbias], zlim = c(-20,20))

  image.plot(lons_cru_orig_scand, lats_cru_scand, temp_em_3rcps_interp_smooth_DSbias_c_cold_scand[,,num_exp_toDSbias], zlim = c(-20,20))
  
  
  temp_em_3rcps_interp_smooth_scand_DSbias_c_warm_sweden[num_exp_toDSbias,ne_toDSbias] = array(temp_em_3rcps_interp_smooth_DSbias_c_warm_scand[Forsmark_lon_GB_CRU_scand,Forsmark_lat_GB_CRU_scand,num_exp_toDSbias])

  temp_em_3rcps_interp_smooth_scand_DSbias_c_cold_sweden[num_exp_toDSbias,ne_toDSbias] = array(temp_em_3rcps_interp_smooth_DSbias_c_cold_scand[Forsmark_lon_GB_CRU_scand,Forsmark_lat_GB_CRU_scand,num_exp_toDSbias])
  
  temp_em_3rcps_interp_smooth_scand_DSbias_c_warm_finland[num_exp_toDSbias,ne_toDSbias] = array(temp_em_3rcps_interp_smooth_DSbias_c_warm_scand[Olkiluoto_lon_GB_CRU_scand,Olkiluoto_lat_GB_CRU_scand,num_exp_toDSbias])

  temp_em_3rcps_interp_smooth_scand_DSbias_c_cold_finland[num_exp_toDSbias,ne_toDSbias] = array(temp_em_3rcps_interp_smooth_DSbias_c_cold_scand[Olkiluoto_lon_GB_CRU_scand,Olkiluoto_lat_GB_CRU_scand,num_exp_toDSbias])
  
  
  # Convert data to correct format for mapping
  
  temp_em_3rcps_interp_smooth_DSbias_c_warm_scand_big[,,num_exp_toDSbias] = temp_em_3rcps_interp_smooth_DSbias_c_warm_scand[,,num_exp_toDSbias]

  temp_em_3rcps_interp_smooth_DSbias_c_cold_scand_big[,,num_exp_toDSbias] = temp_em_3rcps_interp_smooth_DSbias_c_cold_scand[,,num_exp_toDSbias]
  
}
  

print('Okay so far - starting bit that was commented out at line 2818')
# Produce downscaling model to smoothed regional scale (physical-statistical model; DSstat_a + DSstat_b_ig_only) ----------------------------------------------------------

# Import CRU gridded climatology data (v2.1)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/clim.6190.lan.tmp.nc")
temp_cru_monthly = ncvar_get(nc_file,"tmp")
lats_cru = ncvar_get(nc_file,"lat")
lons_cru_orig = ncvar_get(nc_file,"lon")
nc_close(nc_file)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/cru-elevation.nc")
orog_cru_orig = ncvar_get(nc_file,"elv")
nc_close(nc_file)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice_orog.nc")
orog_modice_orig = ncvar_get(nc_file,"ht")
nc_close(nc_file)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice_orog_interp.nc")
orog_modice_interp_orig = ncvar_get(nc_file,"ht")
nc_close(nc_file)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_temp_mm_1_5m_interp.nc")
temp_tdstb_interp_orig = ncvar_get(nc_file,"temp_mm_1_5m")
nc_close(nc_file)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice_orog_interp_smooth.nc")
orog_modice_interp_smooth_orig = ncvar_get(nc_file,"ht")
nc_close(nc_file)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_temp_mm_1_5m_interp_smooth.nc")
temp_tdstb_interp_smooth_orig = ncvar_get(nc_file,"temp_mm_1_5m")
nc_close(nc_file)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_q_mm_1_5m_interp_smooth.nc")
q_tdstb_interp_smooth_orig = ncvar_get(nc_file,"q_mm_1_5m")
nc_close(nc_file)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_u_mm_p_850_interp_smooth.nc")
u_850_tdstb_interp_smooth_orig = ncvar_get(nc_file,"u_mm_p")
nc_close(nc_file)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_v_mm_p_850_interp_smooth.nc")
v_850_tdstb_interp_smooth_orig = ncvar_get(nc_file,"v_mm_p")
nc_close(nc_file)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/tdstb_cl_p_mm_msl_interp_smooth.nc")
mslp_tdstb_interp_smooth_orig = ncvar_get(nc_file,"p_mm_msl")
nc_close(nc_file)

print('Okay so far - line 2868')
# Convert CRU Data from monthly means to annual

nx_cru = 720
ny_cru = 360
nx2_cru = nx_cru/2

temp_cru_ann = array(0,c(nx_cru,ny_cru))

for (y in c(1:ny_cru)) {
  for (x in c(1:nx_cru)) {
    temp_cru_ann[x,y] = mean(temp_cru_monthly[x,y,1:12])
  }
}


# Convert SAT to Deg C

temp_tdstb_interp_orig = temp_tdstb_interp_orig - K
temp_tdstb_interp_smooth_orig = temp_tdstb_interp_smooth_orig - K


# Convert SAT to R interpolated data

bilinterp_data <- list( x= 1:96, y=1:73, z= model_output_tdst[,])
bilinterp_lon <- seq( 1,96,,720)
bilinterp_lat <- seq( 1,73,,360)
make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid
interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))
#image.plot( as.surface( loc1, bilinterp_new))
image.plot(bilinterp_newdata[,c(ny_cru:1)])


temp_tdstb_interp_smooth_orig_cdo=array(temp_tdstb_interp_smooth_orig,c(nx_cru,ny_cru))
temp_tdstb_interp_smooth_orig=array(bilinterp_newdata,c(nx_cru,ny_cru))

n = ny_cru
for (y in c(1:ny_cru)){
  for (x in c(1:nx_cru)){
    temp_tdstb_interp_smooth_orig[x,y] = bilinterp_newdata[x,n]
  }
  n = n - 1
}

image.plot(temp_tdstb_interp_smooth_orig)
image.plot(temp_tdstb_interp_smooth_orig_cdo)

print('Okay so far - line 2916')
# Reformat CRU data so it's consistent with Hadley

orog_cru = array(0,c(nx_cru,ny_cru))
temp_cru = array(0,c(nx_cru,ny_cru))

n = ny_cru
for (y in c(1:ny_cru)){
  orog_cru[1:nx_cru,y] = orog_cru_orig[(1:nx_cru),n]
  n = n-1
}

n = ny_cru
for (y in c(1:ny_cru)){
  temp_cru[1:nx_cru,y] = temp_cru_ann[(1:nx_cru),n]
  n = n-1
}

lons_cru_orig = lons_cru_orig-180


# Reformat data for downscaling

orog_modice = array(0,c(nx,ny))
temp_tdstb = array(0,c(nx,ny))
orog_modice_interp = array(0,c(nx_cru,ny_cru))
temp_tdstb_interp = array(0,c(nx_cru,ny_cru))
orog_modice_interp_smooth = array(0,c(nx_cru,ny_cru))
temp_tdstb_interp_smooth = array(0,c(nx_cru,ny_cru))
q_tdstb_interp_smooth = array(0,c(nx_cru,ny_cru))
u_850_tdstb_interp_smooth = array(0,c(nx_cru,ny_cru))
v_850_tdstb_interp_smooth = array(0,c(nx_cru,ny_cru))
mslp_tdstb_interp_smooth = array(0,c(nx_cru,ny_cru))

n = ny_cru
for (y in c(1:ny)){
  orog_modice[1:(nx),y] = c(orog_modice_orig[(nx2+1):nx,y],orog_modice_orig[1:nx2,y])
  n = n-1
}

n = ny_cru
for (y in c(1:ny)){
  temp_tdstb[1:(nx),y] = c(model_output_tdst[(nx2+1):nx,y],model_output_tdst[1:nx2,y])
  n = n-1
}

n = ny_cru
for (y in c(1:ny_cru)){
  orog_modice_interp[1:(nx_cru),n] = c(orog_modice_interp_orig[(nx2_cru+1):nx_cru,y],orog_modice_interp_orig[1:nx2_cru,y])
  n = n-1
}

n = ny_cru
for (y in c(1:ny_cru)){
  temp_tdstb_interp[1:(nx_cru),n] = c(temp_tdstb_interp_orig[(nx2_cru+1):nx_cru,y],temp_tdstb_interp_orig[1:nx2_cru,y])
  n = n-1
}

n = ny_cru
for (y in c(1:ny_cru)){
  orog_modice_interp_smooth[1:(nx_cru),n] = c(orog_modice_interp_smooth_orig[(nx2_cru+1):nx_cru,y],orog_modice_interp_smooth_orig[1:nx2_cru,y])
  n = n-1
}
orog_modice_interp_smooth_landsea = orog_modice_interp_smooth

n = ny_cru
for (y in c(1:ny_cru)){
  temp_tdstb_interp_smooth[1:(nx_cru),n] = c(temp_tdstb_interp_smooth_orig[(nx2_cru+1):nx_cru,y],temp_tdstb_interp_smooth_orig[1:nx2_cru,y])
  n = n-1
}

n = ny_cru
for (y in c(1:ny_cru)){
  q_tdstb_interp_smooth[1:(nx_cru),n] = c(q_tdstb_interp_smooth_orig[(nx2_cru+1):nx_cru,y],q_tdstb_interp_smooth_orig[1:nx2_cru,y])
  n = n-1
}

n = ny_cru
for (y in c(1:ny_cru)){
  u_850_tdstb_interp_smooth[1:(nx_cru),n] = c(u_850_tdstb_interp_smooth_orig[(nx2_cru+1):nx_cru,y],u_850_tdstb_interp_smooth_orig[1:nx2_cru,y])
  n = n-1
}

n = ny_cru
for (y in c(1:ny_cru)){
  v_850_tdstb_interp_smooth[1:(nx_cru),n] = c(v_850_tdstb_interp_smooth_orig[(nx2_cru+1):nx_cru,y],v_850_tdstb_interp_smooth_orig[1:nx2_cru,y])
  n = n-1
}

n = ny_cru
for (y in c(1:ny_cru)){
  mslp_tdstb_interp_smooth[1:(nx_cru),n] = c(mslp_tdstb_interp_smooth_orig[(nx2_cru+1):nx_cru,y],mslp_tdstb_interp_smooth_orig[1:nx2_cru,y])
  n = n-1
}

print('Okay so far - line 3011')
# Assign grid boxes with no orography data (LT 0) as missing values

orog_modice_interp_all = orog_modice_interp

temp_cru[which(orog_cru<=0,arr.ind=TRUE)] = NaN
orog_cru[which(orog_cru<=0,arr.ind=TRUE)] = NaN
temp_tdstb[which(orog_modice<=0,arr.ind=TRUE)] = NaN
orog_modice[which(orog_modice<=0,arr.ind=TRUE)] = NaN
temp_tdstb_interp[which(orog_modice_interp<=0,arr.ind=TRUE)] = NaN
orog_modice_interp[which(orog_modice_interp<=0,arr.ind=TRUE)] = NaN
temp_tdstb_interp_smooth[which(is.na(orog_cru))] = NaN
orog_modice_interp_smooth[which(is.na(orog_cru))] = NaN

temp_cru[is.na(orog_cru)] = NaN
orog_cru[is.na(orog_cru)] = NaN

temp_cru_noNaN = temp_cru
temp_cru_noNaN[is.na(orog_modice_interp_smooth)] = NaN


# Make latitude data

lats_cru_abs = abs(lats_cru)

latitude_cru = array(0,c(nx_cru,ny_cru))
for (x in 1:nx_cru) {
  latitude_cru[x,1:ny_cru] = lats_cru_abs
}


latitude_cru[which(is.na(orog_cru))] = NaN


# latitude_cru_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
# latitude_cru_scand = latitude_cru[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]


# Make temp*orog data

temptimesorog_cru = temp_cru * orog_cru


# temptimesorog_cru = array(temptimesorog_cru,c(nx_cru,ny_cru))
# temptimesorog_cru = temptimesorog_cru[(nx2_cru):(nx_cru-lons_cru_orig_max),c(lats_cru_orig_max:lats_cru_orig_min)]


# Make temp*lat data

temptimeslat_cru = temp_cru * latitude_cru


# temptimeslat_cru = array(NaN,c(nx_cru,ny_cru))
# temptimeslat_cru = temptimeslat_cru[(nx2_cru+lons_cru_orig_min):(nx_cru-lons_cru_orig_max),c(lats_cru_orig_max:lats_cru_orig_min)]


# Reformat data to a matrix

orog_cru_v = c(as.matrix(orog_cru))
temp_cru_v = c(as.matrix(temp_cru))
orog_modice_interp_smooth_v = c(as.matrix(orog_modice_interp_smooth))
temp_tdstb_interp_smooth_v = c(as.matrix(temp_tdstb_interp_smooth))
q_tdstb_interp_smooth_v = c(as.matrix(q_tdstb_interp_smooth))
u_850_tdstb_interp_smooth_v = c(as.matrix(u_850_tdstb_interp_smooth))
v_850_tdstb_interp_smooth_v = c(as.matrix(v_850_tdstb_interp_smooth))
mslp_tdstb_interp_smooth_v = c(as.matrix(mslp_tdstb_interp_smooth))
lats_cru_v = c(as.matrix(latitude_cru))
temptimesorog_cru_v = c(as.matrix(temptimesorog_cru))
temptimeslat_cru_v = c(as.matrix(temptimeslat_cru))

temp_cru_noNaN_v = c(as.matrix(temp_cru_noNaN)[!is.nan(temp_cru_noNaN)])


library(plyr)

## Fit linear model ##

num_regr = 4
#num_regr = 10

x1 = temp_cru_v
y1 = temp_tdstb_interp_smooth_v
y2 = orog_modice_interp_smooth_v
y3 = orog_cru_v
#y4 = orog_cru_v * orog_cru_v
#y4 = lats_cru_v
y4 = temptimesorog_cru_v
#y5 = temptimeslat_cru_v
#y7 = q_tdstb_interp_v
#y8 = mslp_tdstb_interp_v
#y9 = u_850_tdstb_interp_v
#y10 = v_850_tdstb_interp_v

regressors = c("y1", "y2", "y3", "y4") # Name regressors (variables)
#regressors = c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9", "y10") # Name regressors (variables)


vec = c(T, T, T, F)
#vec = c(T, T, T, F, F, F, F, F, F, F)
paste(c("x1 ~ 1", regressors[vec]), collapse=" + ")
as.formula(paste(c("x1 ~ 1", regressors[vec]), collapse=" + ")) # Create formula for model


regMat_all = expand.grid(c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE)) # Create matrix of all possible combinations of regressors
#regMat_all = expand.grid(c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE)) # Create matrix of all possible combinations of regressors

regMat_all = regMat_all[-(dim(regMat_all)[1]),] # Remove final line which contains no regressors


regMat = regMat_all

names(regMat) = paste("y", 1:num_regr, sep="") # Name columns

allModelsList_smooth = apply(regMat, 1, function(x) as.formula(paste(c("x1 ~ 1", regressors[x]),collapse=" + ")) ) # Create list of all possible models


# Fit all models

data = data.frame(x1,y1,y2,y3,y4) # Create dataframe of all data
#data = data.frame(x1,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10) # Create dataframe of all data

allModelsResults_smooth = lapply(allModelsList_smooth, function(x) lm(x, data=data))


newdata = data[2:dim(data)[2]]

allModelsPred_smooth = lapply(allModelsResults_smooth, function(x) predict(x, data=newdata))

print('Okay so far - 3154')
# Store model statistics

dfCoefNum   = ldply(allModelsResults_smooth, function(x) as.data.frame(
  t(coef(x))))
dfStdErrors = ldply(allModelsResults_smooth, function(x) as.data.frame(
  t(coef(summary(x))[, "Std. Error"])))
dftValues   = ldply(allModelsResults_smooth, function(x) as.data.frame(
  t(coef(summary(x))[, "t value"])))
dfpValues   = ldply(allModelsResults_smooth, function(x) as.data.frame(
  t(coef(summary(x))[, "Pr(>|t|)"])))


# Rename DFs so we know what the column contains

names(dfStdErrors) = paste("se", names(dfStdErrors), sep=".")
names(dftValues) = paste("t", names(dftValues), sep=".")
names(dfpValues) = paste("p", names(dfpValues), sep=".")


# Calculate p-value for overall model fit

calcPval = function(x){
  fstat = summary(x)$fstatistic
  pVal = pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
  return(pVal)
}


# Calculate other useful stats

NoOfCoef = unlist(apply(regMat, 1, sum))
R2       = unlist(lapply(allModelsResults_smooth, function(x) summary(x)$r.squared))
adjR2    = unlist(lapply(allModelsResults_smooth, function(x) summary(x)$adj.r.squared))
RMSE     = unlist(lapply(allModelsResults_smooth, function(x) summary(x)$sigma))
BIC     = unlist(lapply(allModelsResults_smooth, function(x) BIC(x)))
fstats   = unlist(lapply(allModelsResults_smooth, calcPval))

CorrCoef = array(NaN,length(allModelsList_smooth))

n = 0
for (x in 1:length(allModelsList_smooth)){
  temp_model_pred_smooth = c(as.matrix(allModelsPred_smooth[[x]]))
  if (length(temp_model_pred_smooth) == length(temp_cru_noNaN_v)){
    CorrCoef[x] = cor.test(temp_cru_noNaN_v,temp_model_pred_smooth)[4]$estimate
  }
  else{
    CorrCoef[x] = NaN
    n=n+1
  }
}

print('Okay so far - line 3206')
# Combine all results

model_results_smooth = data.frame( model = as.character(allModelsList_smooth),
                            NoOfCoef = NoOfCoef,
                            dfCoefNum,
                            dfStdErrors,
                            dftValues,
                            dfpValues,
                            R2 = R2,
                            adjR2 = adjR2,
                            RMSE = RMSE,
                            CorrCoef = CorrCoef,
                            BIC = BIC,
                            pF = fstats  )

model_results_smooth[,-c(1,2)] = round(model_results_smooth[,-c(1,2,3,9,15,21)], 3) # round the results


# Identify optimum model

opt_RMSE_smooth = which(model_results_smooth$RMSE == min(model_results_smooth$RMSE))
opt_CorrCoef_smooth = which(model_results_smooth$CorrCoef == max(model_results_smooth$CorrCoef, na.rm = TRUE))
opt_BIC_smooth = which(model_results_smooth$BIC == min(model_results_smooth$BIC, na.rm = TRUE))

if ((opt_RMSE_smooth == opt_CorrCoef_smooth)&(opt_RMSE_smooth == opt_BIC_smooth)){
  opt_model_terms_smooth = allModelsList_smooth[[opt_BIC_smooth]]
  opt_model_smooth = allModelsResults_smooth[[opt_BIC_smooth]]
  print("Match")
  print(allModelsList_smooth[[opt_BIC_smooth]])
} else {
  opt_model_terms_smooth = allModelsList_smooth[[opt_BIC_smooth]]
  opt_model_smooth = allModelsResults_smooth[[opt_BIC_smooth]]
  print("ERROR - Mismatch")
  print(allModelsList_smooth[[opt_RMSE_smooth]])
  print(allModelsList_smooth[[opt_CorrCoef_smooth[1]]])
  print(allModelsList_smooth[[opt_BIC_smooth]])
}

model_results_smooth$BIC[opt_RMSE_smooth]
model_results_smooth$BIC[opt_CorrCoef_smooth]
model_results_smooth$BIC[opt_BIC_smooth]

model_results_smooth$RMSE[opt_BIC_smooth]
model_results_smooth$CorrCoef[opt_BIC_smooth]
model_results_smooth$BIC[opt_BIC_smooth]


# Final optimised model

test_RMSE_CC_smooth = array(NaN,c(1,3))

xx1 = temp_tdstb_interp_smooth_v
xx2 = orog_modice_interp_smooth_v
xx3 = orog_cru_v
#xx4 = lats_cru_v
#xx4 = temptimesorog_cru_v
#xx5 = temptimeslat_cru_v
#xx6 = q_tdstb_interp_smooth_v
#xx7 = mslp_tdstb_interp_smooth_v
#xx8 = u_850_tdstb_interp_smooth_v
#xx9 = v_850_tdstb_interp_smooth_v

model3_smooth = lm(temp_cru_v ~ xx1 + xx2 + xx3)
#model3_smooth = lm(temp_cru_v ~ xx1 + xx2 + xx3 + xx4 + xx5 + xx6 + xx7 + xx8 + xx9)

temp_cru_v_noNaN = temp_cru_v[!is.na(temp_cru_v)]

plot(temp_cru_v_noNaN, model3_smooth$fitted.values)

xxnewdata = data.frame(xx1,xx2,xx3)
#xxnewdata = data.frame(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9)
temp_em_tdstb_interp_smooth_DSstat_a_v = predict(model3_smooth,xxnewdata)
temp_em_tdstb_interp_smooth_DSstat_a_nonDSB = array(temp_em_tdstb_interp_smooth_DSstat_a_v, c(nx_cru,ny_cru))


image.plot(lons_cru_orig, lats_cru, temp_cru[,c(ny_cru:1)], zlim=c(-30,30), xlab="Longitude", ylab="Latitude", main="")

image.plot(lons_cru_orig, lats_cru, temp_em_tdstb_interp_smooth_DSstat_a_nonDSB[,c(ny_cru:1)], zlim=c(-30,30), xlab="Longitude", ylab="Latitude", main="")


# Calculate RMSE of projected temperature compared to CRU temperature

test_RMSE_CC_smooth[1,1] = sqrt(mean(((temp_em_tdstb_interp_smooth_DSstat_a_v - temp_cru_v)^2), na.rm = TRUE))
test_RMSE_CC_smooth[1,2] = cor.test(temp_cru_v,temp_em_tdstb_interp_smooth_DSstat_a_v)[4]$estimate
test_RMSE_CC_smooth[1,3] = BIC(model3_smooth)

test_RMSE_CC_smooth


# Calculate anomaly (absolute difference; bias) between observed (CRU) and phys-stat modelled (tdstb) pre-industrial temperature

temp_PI_anom_tdstb_DSstat_a_cru_interp_smooth = array(NaN,c(nx_cru,ny_cru,ne_toDSbias))
temp_PI_anom_tdstb_DSstat_a_cru_interp_smooth[,,ne_toDSbias] = temp_em_tdstb_interp_smooth_DSstat_a_nonDSB - temp_cru

# image.plot(temp_em_tdstb_interp_smooth_DSstat_a_nonDSB, zlim = c(-30,30))
# image.plot(temp_cru, zlim = c(-30,30))
# image.plot(temp_PI_anom_tdstb_DSstat_a_cru_interp_smooth[,,ne_toDSbias], zlim = c(-10,10))


# Calculate downscaled regional temperature (DSstat_a; bias-corrected)

temp_em_tdstb_interp_smooth_DSstat_a = array(NaN,c(nx_cru,ny_cru))
temp_em_tdstb_interp_smooth_DSstat_a[,] = temp_em_tdstb_interp_smooth_DSstat_a_nonDSB[,] - temp_PI_anom_tdstb_DSstat_a_cru_interp_smooth[,,ne_toDSbias]

# image.plot(temp_em_tdstb_interp_smooth_DSstat_a_nonDSB[,], zlim = c(-30,30))
# image.plot(temp_em_tdstb_interp_smooth_DSstat_a[,], zlim = c(-30,30))

print('Okay so far - line 3314')

lons_cru_orig_scand = lons_cru_orig[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max)]
lats_cru_scand = lats_cru[(ny_cru-(lats_cru_orig_scand_min-1)):(ny_cru-(lats_cru_orig_scand_max-1))]

orog_cru_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
temp_cru_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
orog_modice_interp_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
temp_tdstb_interp_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
orog_modice_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
temp_tdstb_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
q_tdstb_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
u_850_tdstb_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
v_850_tdstb_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
mslp_tdstb_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))
temp_em_tdstb_interp_smooth_scand_DSstat_a = array(NaN,c(nx_cru_adj_scand,ny_cru_scand))

orog_cru_scand = orog_cru[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
temp_cru_scand = temp_cru[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
orog_modice_interp_scand = orog_modice_interp[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
temp_tdstb_interp_scand = temp_tdstb_interp[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
orog_modice_interp_smooth_scand = orog_modice_interp_smooth[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
temp_tdstb_interp_smooth_scand = temp_tdstb_interp_smooth[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
q_tdstb_interp_smooth_scand = q_tdstb_interp_smooth[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
u_850_tdstb_interp_smooth_scand = u_850_tdstb_interp_smooth[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
v_850_tdstb_interp_smooth_scand = v_850_tdstb_interp_smooth[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
mslp_tdstb_interp_smooth_scand = mslp_tdstb_interp_smooth[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
temp_em_tdstb_interp_smooth_scand_DSstat_a= temp_em_tdstb_interp_smooth_DSstat_a[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]

image.plot(temp_cru_scand[,c(ny_cru_scand:1)], zlim=c(-30,30))
image.plot(temp_em_tdstb_interp_smooth_scand_DSstat_a[,c(ny_cru_scand:1)], zlim=c(-30,30))


Forsmark_lon_GB_CRU_scand = Forsmark_lon_GB_CRU_scand2
Forsmark_lat_GB_CRU_scand = Forsmark_lat_GB_CRU_scand2

em_tdstb_interp_smooth_scand_DSstat_a_sweden = array(temp_em_tdstb_interp_smooth_scand_DSstat_a[Forsmark_lon_GB_CRU_scand,Forsmark_lat_GB_CRU_scand])

Olkiluoto_lon_GB_CRU_scand = Olkiluoto_lon_GB_CRU_scand2
Olkiluoto_lat_GB_CRU_scand = Olkiluoto_lat_GB_CRU_scand2

em_tdstb_interp_smooth_scand_DSstat_a_finland = array(temp_em_tdstb_interp_smooth_scand_DSstat_a[Olkiluoto_lon_GB_CRU_scand,Olkiluoto_lat_GB_CRU_scand])


# Convert data to correct format for mapping

temp_cru_scand_big = temp_cru_scand
orog_cru_scand_big = orog_cru_scand
temp_tdstb_interp_smooth_scand_big = temp_tdstb_interp_smooth_scand
orog_modice_interp_smooth_scand_big = orog_modice_interp_smooth_scand
temp_em_tdstb_interp_smooth_scand_DSstat_a_big = temp_em_tdstb_interp_smooth_scand_DSstat_a

lons_cru_scand = lons_cru_orig_scand


temp_tdstb_sweden # Pre-industrial Sweden grid box (nearest land)
temp_cru_sweden # CRU Sweden grid box
em_tdstb_interp_smooth_scand_DSstat_a_sweden # Model downscaled pre-industrial previous Sweden grid box
em_zero_emissions_interp_smooth_scand_DSbias_c_sweden[1,] # Simple downscaled ~pre-industrial Sweden grid box

temp_tdstb_finland # Pre-industrial Finland grid box (nearest land)
temp_cru_finland # CRU Finland grid box
em_tdstb_interp_smooth_scand_DSstat_a_finland # Model downscaled pre-industrial previous Finland grid box
em_zero_emissions_interp_smooth_scand_DSbias_c_finland[1,] # Simple downscaled pre-industrial Finland grid box


print('Okay so far - line 3428')
# Downscale emulated data (1 Myr) to smoothed regional scale (physical-statistical model, DSstat_a + DSstat_b_ig_only) ----------------------------------------------------------

# Import orography data (Singarayer + Valdes, and Peltier 1deg)

nc_file = nc_open("../orig/Input/2018-08-01 Final report/modice_orog_interp_smooth.nc")
orog_modice_interp_smooth_orig = ncvar_get(nc_file,"ht")
nc_close(nc_file)

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

orog_modice_orig_regrid = orog_modice_orig
for (y in c(1:ny)){
  orog_modice_orig_regrid[1:(nx),y] = c(orog_modice_orig[(nx2+1):nx,y],orog_modice_orig[1:nx2,y])
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

# image.plot(orog_peltier_interp_smooth_orig[,,20])
# image.plot(orog_peltier_interp_smooth_diff_0kyr[,,20])


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
  for (y in c(1:ny_cru)){
    orog_all_cru_peltier_interp_orig[,y,num_exp] = c(orog_peltier_HadCM3_comb_interp_smooth_orig[,y,i]) # removing first (0 m) and last (114 m) tdab
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

print("Beginning physical-statistical downscaling for all years")

# Natural ------

# for (num_time in c(1:ne)){
ne_toDSstat = 1

em_zero_emissions_interp_smooth_scand_DSstat_a_sweden = array(0,c(ne,ne_toDSstat))
em_zero_emissions_interp_smooth_scand_DSstat_a_finland = array(0,c(ne,ne_toDSstat))

num_exp_toDSstat = 75 # CW: I am fairly sure this should = 1 (like the other scenarios), so not sure why 75?  Especially as it doesn't matter, because the next line reinitialises to 1.

# Create array to hold regional downscaled (physical-statistical) data at all 1001 timesteps (extract array size from regional input file i.e. first 2 values, nx and ny)
# For Korean Peninsular, this = 11,14,1001.  For Scandinavia, this = 57,41,1001

final_temp_zero_emissions_region_alltimes_stat = array(0,c(location_region[1],location_region[2],1001)) 

for (num_exp_toDSstat in c(1:ne)){

  # Interpolated emulated data

  temp_em_zero_emissions_interp_smooth_orig = array(0,c(nx_cru,ny_cru,ne_toDSstat))

  # rm(list=ls()[(ls() %in% c('temp_em_rcp45_interp_smooth_orig','temp_em_rcp85_interp_smooth_orig'))])

  bilinterp_lon <- seq( 1,96,,720)
  bilinterp_lat <- seq( 1,73,,360)
  make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid

  bilinterp_data <- list( x= 1:96, y=1:73, z= scenario_means_zero_emissions_orig[,,num_exp_toDSstat])
  interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
  bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))

  n = ny_cru
  for (y in c(1:ny_cru)){
    for (x in c(1:nx_cru)){
      temp_em_zero_emissions_interp_smooth_orig[x,y,ne_toDSstat] = bilinterp_newdata[x,n]
    }
    n = n - 1
  }

  # image.plot(temp_em_zero_emissions_interp_smooth_orig[,,ne_toDSstat])


  # Calculate orography to use

  gsl_index_ts = which.min(abs(model_input_zero_emissions_1myr_AP[num_exp_toDSstat,5] - gsl_list_all))
  gsl_value_ts = gsl_list_all[[gsl_index_ts]]
  orog_em_zero_emissions_interp_smooth_orig_ts = orog_all_interp_smooth_orig[,,gsl_index_ts]
  orog_cru_peltier_interp_orig_ts = orog_all_cru_peltier_interp_orig[,,gsl_index_ts]


  # Reformat data for downscaling

  temp_em_zero_emissions_interp_smooth = array(0,c(nx_cru,ny_cru,ne_toDSstat))

  n = ny_cru
  for (y in c(1:ny_cru)){
    temp_em_zero_emissions_interp_smooth[1:(nx_cru),n,ne_toDSstat] = c(temp_em_zero_emissions_interp_smooth_orig[(nx2_cru+1):nx_cru,y,ne_toDSstat],temp_em_zero_emissions_interp_smooth_orig[1:nx2_cru,y,ne_toDSstat])
    n = n-1
  }


  # Assign grid boxes with no orography data (LT 0) as missing values

  orog_cru_peltier_interp_orig_ts[which(orog_cru_peltier_interp_orig_ts<=0,arr.ind=TRUE)] = NaN
  temp_em_zero_emissions_interp_smooth[is.na(orog_cru_peltier_interp_orig_ts)] = NaN
  orog_em_zero_emissions_interp_smooth_orig_ts[is.na(orog_cru_peltier_interp_orig_ts)] = NaN


  # Make temp*orog data [NOT USED]

  temptimesorog_zero_emissions = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temptimesorog_zero_emissions[,,e] = temp_em_zero_emissions_interp_smooth[,,e] * orog_cru
  }


  # Make temp*lat data [NOT USED]

  temptimeslat_zero_emissions = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temptimeslat_zero_emissions[,,e] = temp_em_zero_emissions_interp_smooth[,,e] * orog_cru
  }


  # Reformat data to a matrix

  temp_em_zero_emissions_interp_smooth_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  orog_em_zero_emissions_interp_smooth_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  orog_cru_peltier_interp_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  temptimesorog_zero_emissions_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  temptimeslat_zero_emissions_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temp_em_zero_emissions_interp_smooth_v[,e] = c(as.matrix(temp_em_zero_emissions_interp_smooth[,,e]))
    orog_em_zero_emissions_interp_smooth_v[,e] = c(as.matrix(orog_em_zero_emissions_interp_smooth_orig_ts[,]))
    orog_cru_peltier_interp_v[,e] = c(as.matrix(orog_cru_peltier_interp_orig_ts[,]))
    temptimesorog_zero_emissions_v[,e] = c(as.matrix(temptimesorog_zero_emissions[,,e]))
    temptimeslat_zero_emissions_v[,e] = c(as.matrix(temptimeslat_zero_emissions[,,e]))
  }


  # Fit final optimised linear model

  temp_em_zero_emissions_interp_smooth_DSstat_a_nonDSB = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))
  temp_em_zero_emissions_interp_smooth_DSstat_a = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))
  temp_em_zero_emissions_interp_smooth_DSstat_a_big = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    xx1 = temp_em_zero_emissions_interp_smooth_v[,e]
    xx2 = orog_em_zero_emissions_interp_smooth_v[,e]
    xx3 = orog_cru_peltier_interp_v
    #xx4 = lats_cru_v
    #xx4 = temptimesorog_zero_emissions_v[,e]
    #xx5 = temptimeslat_zero_emissions_v[,e]

    xxnewdata = data.frame(xx1,xx2,xx3)
    temp_em_zero_emissions_interp_smooth_DSstat_a_v = predict(model3_smooth,xxnewdata)
    temp_em_zero_emissions_interp_smooth_DSstat_a_nonDSB[,,e] = array(temp_em_zero_emissions_interp_smooth_DSstat_a_v, c(nx_cru,ny_cru))

    #   image.plot(lons_cru_orig, lats_cru, temp_em_zero_emissions_interp_smooth[,c(ny_cru:1),e], zlim=c(-30,30), xlab="Longitude", ylab="Latitude", main="")

    #   image.plot(lons_cru_orig, lats_cru, temp_em_zero_emissions_interp_smooth_DSstat_a_nonDSB[,c(ny_cru:1),e], zlim=c(-30,30), xlab="Longitude", ylab="Latitude", main="")
  }


  # Calculate downscaled regional temperature (DSstat_a; bias corrected)

  temp_em_zero_emissions_interp_smooth_DSstat_a[,,e] = temp_em_zero_emissions_interp_smooth_DSstat_a_nonDSB[,,e] - temp_PI_anom_tdstb_DSstat_a_cru_interp_smooth[,,ne_toDSbias]

  # image.plot(temp_em_zero_emissions_interp_smooth_DSstat_a_nonDSB[,,e], zlim = c(-30,30))
  # image.plot(temp_em_zero_emissions_interp_smooth_DSstat_a[,,e], zlim = c(-30,30))


  temp_em_zero_emissions_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  orog_em_zero_emissions_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  orog_cru_peltier_interp_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  temp_em_zero_emissions_interp_smooth_scand_DSstat_a = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))

  temp_em_zero_emissions_interp_smooth_scand[,,ne_toDSstat] = temp_em_zero_emissions_interp_smooth[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min),ne_toDSstat]
  orog_em_zero_emissions_interp_smooth_scand[,,ne_toDSstat] = orog_em_zero_emissions_interp_smooth_orig_ts[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
  orog_cru_peltier_interp_scand[,,ne_toDSstat] = orog_cru_peltier_interp_orig_ts[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
  temp_em_zero_emissions_interp_smooth_scand_DSstat_a[,,ne_toDSstat] = temp_em_zero_emissions_interp_smooth_DSstat_a[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min),ne_toDSstat]

  # image.plot(temp_cru_ann_scand[,], zlim=c(-30,30))
  # image.plot(temp_em_zero_emissions_interp_smooth_scand_DSstat_a[,ny_cru_scand:1,ne_toDSstat], zlim=c(-30,30))

  Forsmark_lon_GB_CRU_scand = Forsmark_lon_GB_CRU_scand2
  Forsmark_lat_GB_CRU_scand = Forsmark_lat_GB_CRU_scand2
  
  em_zero_emissions_interp_smooth_scand_DSstat_a_sweden[num_exp_toDSstat,ne_toDSstat] = array(temp_em_zero_emissions_interp_smooth_scand_DSstat_a[Forsmark_lon_GB_CRU_scand,Forsmark_lat_GB_CRU_scand,ne_toDSstat])

  Olkiluoto_lon_GB_CRU_scand = Olkiluoto_lon_GB_CRU_scand2
  Olkiluoto_lat_GB_CRU_scand = Olkiluoto_lat_GB_CRU_scand2

  em_zero_emissions_interp_smooth_scand_DSstat_a_finland[num_exp_toDSstat,ne_toDSstat] = array(temp_em_zero_emissions_interp_smooth_scand_DSstat_a[Olkiluoto_lon_GB_CRU_scand,Olkiluoto_lat_GB_CRU_scand,ne_toDSstat])


  # Convert data to correct format for mapping

  temp_em_zero_emissions_interp_smooth_scand_big = temp_em_zero_emissions_interp_smooth_scand
  temp_em_zero_emissions_interp_smooth_scand_DSstat_a_big = temp_em_zero_emissions_interp_smooth_scand_DSstat_a

  # CW: At each time step, save in the larger (final) array 
  
  final_temp_zero_emissions_region_alltimes_stat[,,num_exp_toDSstat] = temp_em_zero_emissions_interp_smooth_scand_DSstat_a_big
  
  message(paste(num_exp_toDSstat))
  
} # Close year loop

# Save data - SAT DOWNSCALED OVER SCANDINAVIA AT ALL TIME SLICES OVER NEXT 1 MYR - CORRESPONDS TO SIMPLE BIAS CORRECTION (ALTHOUGH INCLUDING OROGRAPHY), WRITTEN OUT
# AS TEXT FILE *DSstat_a* IN MAIN SCRIPT, E.G. em_zero_emissions_interp_smooth_scand_DSstat_a_finland/sweden

kyr_num = c(0:(ne-1))

# define global attributes
nc_institution = "University of Bristol, 2018"
nc_datasource = "Climate emulator"

nc_title = "2m air temperature"
nc_file_name <- paste(dataoutfilepath,"Em_output_data_",EmulVars,"_region_stat_natural_0_to_1MyrAP.nc",sep="")

# define dimensions
lon_dim <- ncdim_def("longitude","degrees_east",as.double(lons_cru_orig_scand))
lat_dim <- ncdim_def("latitude","degrees_north",as.double(lats_cru_scand))
time_dim <- ncdim_def("time","",as.double(kyr_num))
surf_dim <- ncdim_def("surface","",as.double(1))

# define variables
miss_value <- 2e20
nc_var_name <- "SAT"
nc_var_unit <- "degC"
nc_var_lname <- "2m air temperature"

# create variable and ncdf file with first variable so dimensions are set
var_def <- ncvar_def(nc_var_name,nc_var_unit,list(lon_dim,lat_dim,surf_dim,time_dim),miss_value,nc_var_lname,prec="double")
nc_file <- nc_create(nc_file_name,var_def)

# define data to save
data_to_save = final_temp_zero_emissions_region_alltimes_stat[,,]

# add data to ncdf file
ncvar_put(nc_file,var_def,data_to_save)

# put additional attributes into dimension and data variables
ncatt_put(nc_file,"longitude","axis","X" ,verbose=FALSE, definemode=FALSE)
ncatt_put(nc_file,"latitude","axis","Y")
ncatt_put(nc_file,"time","axis","T")

# add global attributes
ncatt_put(nc_file,0,"title",nc_title)
ncatt_put(nc_file,0,"institution",nc_institution)
ncatt_put(nc_file,0,"source",nc_datasource)

# close the file, writing data to disk
nc_close(nc_file)


plot(times, em_zero_emissions_interp_smooth_scand_DSstat_a_sweden, type = 'l', ylim = c(-25, 10))
lines(times, em_zero_emissions_interp_smooth_scand_DSstat_a_finland, type = 'l', ylim = c(-25, 10), col='red')


# RCP2.6 ------

# for (num_time in c(1:ne)){
ne_toDSstat = 1

em_rcp26_interp_smooth_scand_DSstat_a_sweden = array(0,c(ne,ne_toDSstat))
em_rcp26_interp_smooth_scand_DSstat_a_finland = array(0,c(ne,ne_toDSstat))

num_exp_toDSstat = 1

# Create array to hold regional downscaled (physical-statistical) data at all 1001 timesteps (extract array size from regional input file i.e. first 2 values, nx and ny)
# For Korean Peninsular, this = 11,14,1001.  For Scandinavia, this = 57,41,1001

final_temp_rcp26_region_alltimes_stat = array(0,c(location_region[1],location_region[2],1001)) 

for (num_exp_toDSstat in c(1:ne)){

  # Interpolated emulated data

  temp_em_rcp26_interp_smooth_orig = array(0,c(nx_cru,ny_cru,ne_toDSstat))

  # rm(list=ls()[(ls() %in% c('temp_em_rcp45_interp_smooth_orig','temp_em_rcp85_interp_smooth_orig'))])

  bilinterp_lon <- seq( 1,96,,720)
  bilinterp_lat <- seq( 1,73,,360)
  make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid

  bilinterp_data <- list( x= 1:96, y=1:73, z= scenario_means_rcp26_orig[,,num_exp_toDSstat])
  interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
  bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))

  n = ny_cru
  for (y in c(1:ny_cru)){
    for (x in c(1:nx_cru)){
      temp_em_rcp26_interp_smooth_orig[x,y,ne_toDSstat] = bilinterp_newdata[x,n]
    }
    n = n - 1
  }

  # image.plot(temp_em_rcp26_interp_smooth_orig[,,ne_toDSstat])


  # Calculate orography to use

  gsl_index_ts = which.min(abs(model_input_rcp26_1myr_AP[num_exp_toDSstat,5] - gsl_list_all))
  gsl_value_ts = gsl_list_all[[gsl_index_ts]]
  orog_em_rcp26_interp_smooth_orig_ts = orog_all_interp_smooth_orig[,,gsl_index_ts]
  orog_cru_peltier_interp_orig_ts = orog_all_cru_peltier_interp_orig[,,gsl_index_ts]


  # Reformat data for downscaling

  temp_em_rcp26_interp_smooth = array(0,c(nx_cru,ny_cru,ne_toDSstat))

  n = ny_cru
  for (y in c(1:ny_cru)){
    temp_em_rcp26_interp_smooth[1:(nx_cru),n,ne_toDSstat] = c(temp_em_rcp26_interp_smooth_orig[(nx2_cru+1):nx_cru,y,ne_toDSstat],temp_em_rcp26_interp_smooth_orig[1:nx2_cru,y,ne_toDSstat])
    n = n-1
  }


  # Assign grid boxes with no orography data (LT 0) as missing values

  orog_cru_peltier_interp_orig_ts[which(orog_cru_peltier_interp_orig_ts<=0,arr.ind=TRUE)] = NaN
  temp_em_rcp26_interp_smooth[is.na(orog_cru_peltier_interp_orig_ts)] = NaN
  orog_em_rcp26_interp_smooth_orig_ts[is.na(orog_cru_peltier_interp_orig_ts)] = NaN


  # Make temp*orog data

  temptimesorog_rcp26 = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temptimesorog_rcp26[,,e] = temp_em_rcp26_interp_smooth[,,e] * orog_cru
  }


  # Make temp*lat data [NOT USED]

  temptimeslat_rcp26 = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temptimeslat_rcp26[,,e] = temp_em_rcp26_interp_smooth[,,e] * orog_cru
  }


  # Reformat data to a matrix

  temp_em_rcp26_interp_smooth_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  orog_em_rcp26_interp_smooth_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  orog_cru_peltier_interp_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  temptimesorog_rcp26_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  temptimeslat_rcp26_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temp_em_rcp26_interp_smooth_v[,e] = c(as.matrix(temp_em_rcp26_interp_smooth[,,e]))
    orog_em_rcp26_interp_smooth_v[,e] = c(as.matrix(orog_em_rcp26_interp_smooth_orig_ts[,]))
    orog_cru_peltier_interp_v[,e] = c(as.matrix(orog_cru_peltier_interp_orig_ts[,]))
    temptimesorog_rcp26_v[,e] = c(as.matrix(temptimesorog_rcp26[,,e]))
    temptimeslat_rcp26_v[,e] = c(as.matrix(temptimeslat_rcp26[,,e]))
  }


  # Fit final optimised linear model

  temp_em_rcp26_interp_smooth_DSstat_a_nonDSB = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))
  temp_em_rcp26_interp_smooth_DSstat_a = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))
  temp_em_rcp26_interp_smooth_DSstat_a_big = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    xx1 = temp_em_rcp26_interp_smooth_v[,e]
    xx2 = orog_em_rcp26_interp_smooth_v[,e]
    xx3 = orog_cru_peltier_interp_v
    #xx4 = lats_cru_v
    #xx4 = temptimesorog_rcp26_v[,e]
    #xx5 = temptimeslat_rcp26_v[,e]

    xxnewdata = data.frame(xx1,xx2,xx3)
    temp_em_rcp26_interp_smooth_DSstat_a_v = predict(model3_smooth,xxnewdata)
    temp_em_rcp26_interp_smooth_DSstat_a_nonDSB[,,e] = array(temp_em_rcp26_interp_smooth_DSstat_a_v, c(nx_cru,ny_cru))

    #   image.plot(lons_cru_orig, lats_cru, temp_em_rcp26_interp_smooth[,c(ny_cru:1),e], zlim=c(-30,30), xlab="Longitude", ylab="Latitude", main="")

    #   image.plot(lons_cru_orig, lats_cru, temp_em_rcp26_interp_smooth_DSstat_a_nonDSB[,c(ny_cru:1),e], zlim=c(-30,30), xlab="Longitude", ylab="Latitude", main="")
  }


  # Calculate downscaled regional temperature (DSstat_a; bias corrected)

  temp_em_rcp26_interp_smooth_DSstat_a[,,e] = temp_em_rcp26_interp_smooth_DSstat_a_nonDSB[,,e] - temp_PI_anom_tdstb_DSstat_a_cru_interp_smooth[,,ne_toDSbias]

  # image.plot(temp_em_rcp26_interp_smooth_DSstat_a_nonDSB[,,e], zlim = c(-30,30))
  # image.plot(temp_em_rcp26_interp_smooth_DSstat_a[,,e], zlim = c(-30,30))

  temp_em_rcp26_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  orog_em_rcp26_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  orog_cru_peltier_interp_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  temp_em_rcp26_interp_smooth_scand_DSstat_a = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))

  temp_em_rcp26_interp_smooth_scand[,,ne_toDSstat] = temp_em_rcp26_interp_smooth[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min),ne_toDSstat]
  orog_em_rcp26_interp_smooth_scand[,,ne_toDSstat] = orog_em_rcp26_interp_smooth_orig_ts[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
  orog_cru_peltier_interp_scand[,,ne_toDSstat] = orog_cru_peltier_interp_orig_ts[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
  temp_em_rcp26_interp_smooth_scand_DSstat_a[,,ne_toDSstat] = temp_em_rcp26_interp_smooth_DSstat_a[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min),ne_toDSstat]

  # image.plot(temp_cru_ann_scand[,], zlim=c(-30,30))
  # image.plot(temp_em_rcp26_interp_smooth_scand_DSstat_a[,ny_cru_scand:1,ne_toDSstat], zlim=c(-30,30))


  Forsmark_lon_GB_CRU_scand = Forsmark_lon_GB_CRU_scand2
  Forsmark_lat_GB_CRU_scand = Forsmark_lat_GB_CRU_scand2

  em_rcp26_interp_smooth_scand_DSstat_a_sweden[num_exp_toDSstat,ne_toDSstat] = array(temp_em_rcp26_interp_smooth_scand_DSstat_a[Forsmark_lon_GB_CRU_scand,Forsmark_lat_GB_CRU_scand,ne_toDSstat])


  Olkiluoto_lon_GB_CRU_scand = Olkiluoto_lon_GB_CRU_scand2
  Olkiluoto_lat_GB_CRU_scand = Olkiluoto_lat_GB_CRU_scand2

  em_rcp26_interp_smooth_scand_DSstat_a_finland[num_exp_toDSstat,ne_toDSstat] = array(temp_em_rcp26_interp_smooth_scand_DSstat_a[Olkiluoto_lon_GB_CRU_scand,Olkiluoto_lat_GB_CRU_scand,ne_toDSstat])


  # Convert data to correct format for mapping

  temp_em_rcp26_interp_smooth_scand_big = temp_em_rcp26_interp_smooth_scand
  temp_em_rcp26_interp_smooth_scand_DSstat_a_big = temp_em_rcp26_interp_smooth_scand_DSstat_a

  # CW: At each time step, save in the larger (final) array 
  
  final_temp_rcp26_region_alltimes_stat[,,num_exp_toDSstat] = temp_em_rcp26_interp_smooth_scand_DSstat_a_big
  
  message(paste(num_exp_toDSstat))
  
} # Close year loop

# Save data - SAT DOWNSCALED OVER SCANDINAVIA AT ALL TIME SLICES OVER NEXT 1 MYR - CORRESPONDS TO PHYSICAL-STATISTICAL DOWNSCALING, WRITTEN OUT
# AS TEXT FILE *DSstat_a* IN MAIN SCRIPT, E.G. em_zero_emissions_interp_smooth_scand_DSstat_a_finland/sweden

kyr_num = c(0:(ne-1))

# define global attributes
nc_institution = "University of Bristol, 2018"
nc_datasource = "Climate emulator"

nc_title = "2m air temperature"
nc_file_name <- paste(dataoutfilepath,"Em_output_data_",EmulVars,"_region_stat_rcp26_0_to_1MyrAP.nc",sep="")

# define dimensions
lon_dim <- ncdim_def("longitude","degrees_east",as.double(lons_cru_orig_scand))
lat_dim <- ncdim_def("latitude","degrees_north",as.double(lats_cru_scand))
time_dim <- ncdim_def("time","",as.double(kyr_num))
surf_dim <- ncdim_def("surface","",as.double(1))

# define variables
miss_value <- 2e20
nc_var_name <- "SAT"
nc_var_unit <- "degC"
nc_var_lname <- "2m air temperature"

# create variable and ncdf file with first variable so dimensions are set
var_def <- ncvar_def(nc_var_name,nc_var_unit,list(lon_dim,lat_dim,surf_dim,time_dim),miss_value,nc_var_lname,prec="double")
nc_file <- nc_create(nc_file_name,var_def)

# define data to save
data_to_save = final_temp_rcp26_region_alltimes_stat[,,]

# add data to ncdf file
ncvar_put(nc_file,var_def,data_to_save)

# put additional attributes into dimension and data variables
ncatt_put(nc_file,"longitude","axis","X" ,verbose=FALSE, definemode=FALSE)
ncatt_put(nc_file,"latitude","axis","Y")
ncatt_put(nc_file,"time","axis","T")

# add global attributes
ncatt_put(nc_file,0,"title",nc_title)
ncatt_put(nc_file,0,"institution",nc_institution)
ncatt_put(nc_file,0,"source",nc_datasource)

# close the file, writing data to disk
nc_close(nc_file)


plot(times, em_rcp26_interp_smooth_scand_DSstat_a_sweden, type = 'l', ylim = c(-25, 10))
lines(times, em_rcp26_interp_smooth_scand_DSstat_a_finland, type = 'l', ylim = c(-25, 10), col='red')


# RCP4.5 ------

# for (num_time in c(1:ne)){
ne_toDSstat = 1

em_rcp45_interp_smooth_scand_DSstat_a_sweden = array(0,c(ne,ne_toDSstat))
em_rcp45_interp_smooth_scand_DSstat_a_finland = array(0,c(ne,ne_toDSstat))

num_exp_toDSstat = 1

# Create array to hold regional downscaled (physical-statistical) data at all 1001 timesteps (extract array size from regional input file i.e. first 2 values, nx and ny)
# For Korean Peninsular, this = 11,14,1001.  For Scandinavia, this = 57,41,1001

final_temp_rcp45_region_alltimes_stat = array(0,c(location_region[1],location_region[2],1001)) 

for (num_exp_toDSstat in c(1:ne)){

  # Interpolated emulated data

  temp_em_rcp45_interp_smooth_orig = array(0,c(nx_cru,ny_cru,ne_toDSstat))

  # rm(list=ls()[(ls() %in% c('temp_em_rcp45_interp_smooth_orig','temp_em_rcp85_interp_smooth_orig'))])

  bilinterp_lon <- seq( 1,96,,720)
  bilinterp_lat <- seq( 1,73,,360)
  make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid

  bilinterp_data <- list( x= 1:96, y=1:73, z= scenario_means_rcp45_orig[,,num_exp_toDSstat])
  interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
  bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))

  n = ny_cru
  for (y in c(1:ny_cru)){
    for (x in c(1:nx_cru)){
      temp_em_rcp45_interp_smooth_orig[x,y,ne_toDSstat] = bilinterp_newdata[x,n]
    }
    n = n - 1
  }

  # image.plot(temp_em_rcp45_interp_smooth_orig[,,ne_toDSstat])


  # Calculate orography to use

  gsl_index_ts = which.min(abs(model_input_rcp45_1myr_AP[num_exp_toDSstat,5] - gsl_list_all))
  gsl_value_ts = gsl_list_all[[gsl_index_ts]]
  orog_em_rcp45_interp_smooth_orig_ts = orog_all_interp_smooth_orig[,,gsl_index_ts]
  orog_cru_peltier_interp_orig_ts = orog_all_cru_peltier_interp_orig[,,gsl_index_ts]


  # Reformat data for downscaling

  temp_em_rcp45_interp_smooth = array(0,c(nx_cru,ny_cru,ne_toDSstat))

  n = ny_cru
  for (y in c(1:ny_cru)){
    temp_em_rcp45_interp_smooth[1:(nx_cru),n,ne_toDSstat] = c(temp_em_rcp45_interp_smooth_orig[(nx2_cru+1):nx_cru,y,ne_toDSstat],temp_em_rcp45_interp_smooth_orig[1:nx2_cru,y,ne_toDSstat])
    n = n-1
  }


  # Assign grid boxes with no orography data (LT 0) as missing values

  orog_cru_peltier_interp_orig_ts[which(orog_cru_peltier_interp_orig_ts<=0,arr.ind=TRUE)] = NaN
  temp_em_rcp45_interp_smooth[is.na(orog_cru_peltier_interp_orig_ts)] = NaN
  orog_em_rcp45_interp_smooth_orig_ts[is.na(orog_cru_peltier_interp_orig_ts)] = NaN


  # Make temp*orog data

  temptimesorog_rcp45 = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temptimesorog_rcp45[,,e] = temp_em_rcp45_interp_smooth[,,e] * orog_cru
  }


  # Make temp*lat data [NOT USED]

  temptimeslat_rcp45 = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temptimeslat_rcp45[,,e] = temp_em_rcp45_interp_smooth[,,e] * orog_cru
  }


  # Reformat data to a matrix

  temp_em_rcp45_interp_smooth_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  orog_em_rcp45_interp_smooth_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  orog_cru_peltier_interp_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  temptimesorog_rcp45_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  temptimeslat_rcp45_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temp_em_rcp45_interp_smooth_v[,e] = c(as.matrix(temp_em_rcp45_interp_smooth[,,e]))
    orog_em_rcp45_interp_smooth_v[,e] = c(as.matrix(orog_em_rcp45_interp_smooth_orig_ts[,]))
    orog_cru_peltier_interp_v[,e] = c(as.matrix(orog_cru_peltier_interp_orig_ts[,]))
    temptimesorog_rcp45_v[,e] = c(as.matrix(temptimesorog_rcp45[,,e]))
    temptimeslat_rcp45_v[,e] = c(as.matrix(temptimeslat_rcp45[,,e]))
  }


  # Fit final optimised linear model

  temp_em_rcp45_interp_smooth_DSstat_a_nonDSB = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))
  temp_em_rcp45_interp_smooth_DSstat_a = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))
  temp_em_rcp45_interp_smooth_DSstat_a_big = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    xx1 = temp_em_rcp45_interp_smooth_v[,e]
    xx2 = orog_em_rcp45_interp_smooth_v[,e]
    xx3 = orog_cru_peltier_interp_v
    #xx4 = lats_cru_v
    #xx4 = temptimesorog_rcp45_v[,e]
    #xx5 = temptimeslat_rcp45_v[,e]

    xxnewdata = data.frame(xx1,xx2,xx3)
    temp_em_rcp45_interp_smooth_DSstat_a_v = predict(model3_smooth,xxnewdata)
    temp_em_rcp45_interp_smooth_DSstat_a_nonDSB[,,e] = array(temp_em_rcp45_interp_smooth_DSstat_a_v, c(nx_cru,ny_cru))

    #   image.plot(lons_cru_orig, lats_cru, temp_em_rcp45_interp_smooth[,c(ny_cru:1),e], zlim=c(-30,30), xlab="Longitude", ylab="Latitude", main="")

    #   image.plot(lons_cru_orig, lats_cru, temp_em_rcp45_interp_smooth_DSstat_a_nonDSB[,c(ny_cru:1),e], zlim=c(-30,30), xlab="Longitude", ylab="Latitude", main="")
  }


  # Calculate downscaled regional temperature (DSstat_a; bias corrected)

  temp_em_rcp45_interp_smooth_DSstat_a[,,e] = temp_em_rcp45_interp_smooth_DSstat_a_nonDSB[,,e] - temp_PI_anom_tdstb_DSstat_a_cru_interp_smooth[,,ne_toDSbias]

  # image.plot(temp_em_rcp45_interp_smooth_DSstat_a_nonDSB[,,e], zlim = c(-30,30))
  # image.plot(temp_em_rcp45_interp_smooth_DSstat_a[,,e], zlim = c(-30,30))

  temp_em_rcp45_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  orog_em_rcp45_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  orog_cru_peltier_interp_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  temp_em_rcp45_interp_smooth_scand_DSstat_a = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))

  temp_em_rcp45_interp_smooth_scand[,,ne_toDSstat] = temp_em_rcp45_interp_smooth[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min),ne_toDSstat]
  orog_em_rcp45_interp_smooth_scand[,,ne_toDSstat] = orog_em_rcp45_interp_smooth_orig_ts[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
  orog_cru_peltier_interp_scand[,,ne_toDSstat] = orog_cru_peltier_interp_orig_ts[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
  temp_em_rcp45_interp_smooth_scand_DSstat_a[,,ne_toDSstat] = temp_em_rcp45_interp_smooth_DSstat_a[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min),ne_toDSstat]

  # image.plot(temp_cru_ann_scand[,], zlim=c(-30,30))
  # image.plot(temp_em_rcp45_interp_smooth_scand_DSstat_a[,ny_cru_scand:1,ne_toDSstat], zlim=c(-30,30))

  Forsmark_lon_GB_CRU_scand = Forsmark_lon_GB_CRU_scand2
  Forsmark_lat_GB_CRU_scand = Forsmark_lat_GB_CRU_scand2

  em_rcp45_interp_smooth_scand_DSstat_a_sweden[num_exp_toDSstat,ne_toDSstat] = array(temp_em_rcp45_interp_smooth_scand_DSstat_a[Forsmark_lon_GB_CRU_scand,Forsmark_lat_GB_CRU_scand,ne_toDSstat])


  Olkiluoto_lon_GB_CRU_scand = Olkiluoto_lon_GB_CRU_scand2
  Olkiluoto_lat_GB_CRU_scand = Olkiluoto_lat_GB_CRU_scand2
  

  em_rcp45_interp_smooth_scand_DSstat_a_finland[num_exp_toDSstat,ne_toDSstat] = array(temp_em_rcp45_interp_smooth_scand_DSstat_a[Olkiluoto_lon_GB_CRU_scand,Olkiluoto_lat_GB_CRU_scand,ne_toDSstat])


  # Convert data to correct format for mapping

  temp_em_rcp45_interp_smooth_scand_big = temp_em_rcp45_interp_smooth_scand
  temp_em_rcp45_interp_smooth_scand_DSstat_a_big = temp_em_rcp45_interp_smooth_scand_DSstat_a

  # CW: At each time step, save in the larger (final) array 
  
  final_temp_rcp45_region_alltimes_stat[,,num_exp_toDSstat] = temp_em_rcp45_interp_smooth_scand_DSstat_a_big
  
  message(paste(num_exp_toDSstat))
  
} # Close year loop

# Save data - SAT DOWNSCALED OVER SCANDINAVIA AT ALL TIME SLICES OVER NEXT 1 MYR - CORRESPONDS TO PHYSICAL-STATISTICAL DOWNSCALING, WRITTEN OUT
# AS TEXT FILE *DSstat_a* IN MAIN SCRIPT, E.G. em_zero_emissions_interp_smooth_scand_DSstat_a_finland/sweden

kyr_num = c(0:(ne-1))

# define global attributes
nc_institution = "University of Bristol, 2018"
nc_datasource = "Climate emulator"

nc_title = "2m air temperature"
nc_file_name <- paste(dataoutfilepath,"Em_output_data_",EmulVars,"_region_stat_rcp45_0_to_1MyrAP.nc",sep="")

# define dimensions
lon_dim <- ncdim_def("longitude","degrees_east",as.double(lons_cru_orig_scand))
lat_dim <- ncdim_def("latitude","degrees_north",as.double(lats_cru_scand))
time_dim <- ncdim_def("time","",as.double(kyr_num))
surf_dim <- ncdim_def("surface","",as.double(1))

# define variables
miss_value <- 2e20
nc_var_name <- "SAT"
nc_var_unit <- "degC"
nc_var_lname <- "2m air temperature"

# create variable and ncdf file with first variable so dimensions are set
var_def <- ncvar_def(nc_var_name,nc_var_unit,list(lon_dim,lat_dim,surf_dim,time_dim),miss_value,nc_var_lname,prec="double")
nc_file <- nc_create(nc_file_name,var_def)

# define data to save
data_to_save = final_temp_rcp45_region_alltimes_stat[,,]

# add data to ncdf file
ncvar_put(nc_file,var_def,data_to_save)

# put additional attributes into dimension and data variables
ncatt_put(nc_file,"longitude","axis","X" ,verbose=FALSE, definemode=FALSE)
ncatt_put(nc_file,"latitude","axis","Y")
ncatt_put(nc_file,"time","axis","T")

# add global attributes
ncatt_put(nc_file,0,"title",nc_title)
ncatt_put(nc_file,0,"institution",nc_institution)
ncatt_put(nc_file,0,"source",nc_datasource)

# close the file, writing data to disk
nc_close(nc_file)


plot(times, em_rcp45_interp_smooth_scand_DSstat_a_sweden, type = 'l', ylim = c(-25, 10))
lines(times, em_rcp45_interp_smooth_scand_DSstat_a_finland, type = 'l', ylim = c(-25, 10), col='red')


# RCP8.5 ------

# for (num_time in c(1:ne)){
ne_toDSstat = 1

em_rcp85_interp_smooth_scand_DSstat_a_sweden = array(0,c(ne,ne_toDSstat))
em_rcp85_interp_smooth_scand_DSstat_a_finland = array(0,c(ne,ne_toDSstat))

num_exp_toDSstat = 1

# Create array to hold regional downscaled (physical-statistical) data at all 1001 timesteps (extract array size from regional input file i.e. first 2 values, nx and ny)
# For Korean Peninsular, this = 11,14,1001.  For Scandinavia, this = 57,41,1001

final_temp_rcp85_region_alltimes_stat = array(0,c(location_region[1],location_region[2],1001)) 

for (num_exp_toDSstat in c(1:ne)){

  # Interpolated emulated data

  temp_em_rcp85_interp_smooth_orig = array(0,c(nx_cru,ny_cru,ne_toDSstat))

  # rm(list=ls()[(ls() %in% c('temp_em_rcp45_interp_smooth_orig','temp_em_rcp85_interp_smooth_orig'))])

  bilinterp_lon <- seq( 1,96,,720)
  bilinterp_lat <- seq( 1,73,,360)
  make.surface.grid( list( bilinterp_lon,bilinterp_lat)) -> bilinterp_grid

  bilinterp_data <- list( x= 1:96, y=1:73, z= scenario_means_rcp85_orig[,,num_exp_toDSstat])
  interp.surface( bilinterp_data, bilinterp_grid) -> bilinterp_new
  bilinterp_newdata = array(bilinterp_new, c(nx_cru,ny_cru))

  n = ny_cru
  for (y in c(1:ny_cru)){
    for (x in c(1:nx_cru)){
      temp_em_rcp85_interp_smooth_orig[x,y,ne_toDSstat] = bilinterp_newdata[x,n]
    }
    n = n - 1
  }

  # image.plot(temp_em_rcp85_interp_smooth_orig[,,ne_toDSstat])


  # Calculate orography to use

  gsl_index_ts = which.min(abs(model_input_rcp85_1myr_AP[num_exp_toDSstat,5] - gsl_list_all))
  gsl_value_ts = gsl_list_all[[gsl_index_ts]]
  orog_em_rcp85_interp_smooth_orig_ts = orog_all_interp_smooth_orig[,,gsl_index_ts]
  orog_cru_peltier_interp_orig_ts = orog_all_cru_peltier_interp_orig[,,gsl_index_ts]


  # Reformat data for downscaling

  temp_em_rcp85_interp_smooth = array(0,c(nx_cru,ny_cru,ne_toDSstat))

  n = ny_cru
  for (y in c(1:ny_cru)){
    temp_em_rcp85_interp_smooth[1:(nx_cru),n,ne_toDSstat] = c(temp_em_rcp85_interp_smooth_orig[(nx2_cru+1):nx_cru,y,ne_toDSstat],temp_em_rcp85_interp_smooth_orig[1:nx2_cru,y,ne_toDSstat])
    n = n-1
  }


  # Assign grid boxes with no orography data (LT 0) as missing values

  orog_cru_peltier_interp_orig_ts[which(orog_cru_peltier_interp_orig_ts<=0,arr.ind=TRUE)] = NaN
  temp_em_rcp85_interp_smooth[is.na(orog_cru_peltier_interp_orig_ts)] = NaN
  orog_em_rcp85_interp_smooth_orig_ts[is.na(orog_cru_peltier_interp_orig_ts)] = NaN


  # Make temp*orog data

  temptimesorog_rcp85 = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temptimesorog_rcp85[,,e] = temp_em_rcp85_interp_smooth[,,e] * orog_cru
  }


  # Make temp*lat data [NOT USED]

  temptimeslat_rcp85 = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temptimeslat_rcp85[,,e] = temp_em_rcp85_interp_smooth[,,e] * orog_cru
  }


  # Reformat data to a matrix

  temp_em_rcp85_interp_smooth_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  orog_em_rcp85_interp_smooth_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  orog_cru_peltier_interp_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  temptimesorog_rcp85_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))
  temptimeslat_rcp85_v = array(NaN,c(length(orog_modice_interp_smooth_v),ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    temp_em_rcp85_interp_smooth_v[,e] = c(as.matrix(temp_em_rcp85_interp_smooth[,,e]))
    orog_em_rcp85_interp_smooth_v[,e] = c(as.matrix(orog_em_rcp85_interp_smooth_orig_ts[,]))
    orog_cru_peltier_interp_v[,e] = c(as.matrix(orog_cru_peltier_interp_orig_ts[,]))
    temptimesorog_rcp85_v[,e] = c(as.matrix(temptimesorog_rcp85[,,e]))
    temptimeslat_rcp85_v[,e] = c(as.matrix(temptimeslat_rcp85[,,e]))
  }


  # Fit final optimised linear model

  temp_em_rcp85_interp_smooth_DSstat_a_nonDSB = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))
  temp_em_rcp85_interp_smooth_DSstat_a = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))
  temp_em_rcp85_interp_smooth_DSstat_a_big = array(NaN,c(nx_cru,ny_cru,ne_toDSstat))

  for (e in c(1:ne_toDSstat)){
    xx1 = temp_em_rcp85_interp_smooth_v[,e]
    xx2 = orog_em_rcp85_interp_smooth_v[,e]
    xx3 = orog_cru_peltier_interp_v
    #xx4 = lats_cru_v
    #xx4 = temptimesorog_rcp85_v[,e]
    #xx5 = temptimeslat_rcp85_v[,e]

    xxnewdata = data.frame(xx1,xx2,xx3)
    temp_em_rcp85_interp_smooth_DSstat_a_v = predict(model3_smooth,xxnewdata)
    temp_em_rcp85_interp_smooth_DSstat_a_nonDSB[,,e] = array(temp_em_rcp85_interp_smooth_DSstat_a_v, c(nx_cru,ny_cru))

    #   image.plot(lons_cru_orig, lats_cru, temp_em_rcp85_interp_smooth[,c(ny_cru:1),e], zlim=c(-30,30), xlab="Longitude", ylab="Latitude", main="")

    #   image.plot(lons_cru_orig, lats_cru, temp_em_rcp85_interp_smooth_DSstat_a_nonDSB[,c(ny_cru:1),e], zlim=c(-30,30), xlab="Longitude", ylab="Latitude", main="")
  }


  # Calculate downscaled regional temperature (DSstat_a; bias corrected)

  temp_em_rcp85_interp_smooth_DSstat_a[,,e] = temp_em_rcp85_interp_smooth_DSstat_a_nonDSB[,,e] - temp_PI_anom_tdstb_DSstat_a_cru_interp_smooth[,,ne_toDSbias]

  # image.plot(temp_em_rcp85_interp_smooth_DSstat_a_nonDSB[,,e], zlim = c(-30,30))
  # image.plot(temp_em_rcp85_interp_smooth_DSstat_a[,,e], zlim = c(-30,30))


  temp_em_rcp85_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  orog_em_rcp85_interp_smooth_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  orog_cru_peltier_interp_scand = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))
  temp_em_rcp85_interp_smooth_scand_DSstat_a = array(NaN,c(nx_cru_adj_scand,ny_cru_scand,ne_toDSstat))

  temp_em_rcp85_interp_smooth_scand[,,ne_toDSstat] = temp_em_rcp85_interp_smooth[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min),ne_toDSstat]
  orog_em_rcp85_interp_smooth_scand[,,ne_toDSstat] = orog_em_rcp85_interp_smooth_orig_ts[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
  orog_cru_peltier_interp_scand[,,ne_toDSstat] = orog_cru_peltier_interp_orig_ts[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min)]
  temp_em_rcp85_interp_smooth_scand_DSstat_a[,,ne_toDSstat] = temp_em_rcp85_interp_smooth_DSstat_a[(nx2_cru+lons_cru_orig_scand_min):(nx_cru-lons_cru_orig_scand_max),c(lats_cru_orig_scand_max:lats_cru_orig_scand_min),ne_toDSstat]

  # image.plot(temp_cru_ann_scand[,], zlim=c(-30,30))
  # image.plot(temp_em_rcp85_interp_smooth_scand_DSstat_a[,ny_cru_scand:1,ne_toDSstat], zlim=c(-30,30))


  Forsmark_lon_GB_CRU_scand = Forsmark_lon_GB_CRU_scand2
  Forsmark_lat_GB_CRU_scand = Forsmark_lat_GB_CRU_scand2
  
  em_rcp85_interp_smooth_scand_DSstat_a_sweden[num_exp_toDSstat,ne_toDSstat] = array(temp_em_rcp85_interp_smooth_scand_DSstat_a[Forsmark_lon_GB_CRU_scand,Forsmark_lat_GB_CRU_scand,ne_toDSstat])

  Olkiluoto_lon_GB_CRU_scand = Olkiluoto_lon_GB_CRU_scand2
  Olkiluoto_lat_GB_CRU_scand = Olkiluoto_lat_GB_CRU_scand2

  em_rcp85_interp_smooth_scand_DSstat_a_finland[num_exp_toDSstat,ne_toDSstat] = array(temp_em_rcp85_interp_smooth_scand_DSstat_a[Olkiluoto_lon_GB_CRU_scand,Olkiluoto_lat_GB_CRU_scand,ne_toDSstat])


  # Convert data to correct format for mapping

  temp_em_rcp85_interp_smooth_scand_big = temp_em_rcp85_interp_smooth_scand
  temp_em_rcp85_interp_smooth_scand_DSstat_a_big = temp_em_rcp85_interp_smooth_scand_DSstat_a

  # CW: At each time step, save in the larger (final) array 
  
  final_temp_rcp85_region_alltimes_stat[,,num_exp_toDSstat] = temp_em_rcp85_interp_smooth_scand_DSstat_a_big
  
  message(paste(num_exp_toDSstat))
  
} # Close year loop

# Save data - SAT DOWNSCALED OVER SCANDINAVIA AT ALL TIME SLICES OVER NEXT 1 MYR - CORRESPONDS TO PHYSICAL-STATISTICAL DOWNSCALING, WRITTEN OUT
# AS TEXT FILE *DSstat_a* IN MAIN SCRIPT, E.G. em_zero_emissions_interp_smooth_scand_DSstat_a_finland/sweden

kyr_num = c(0:(ne-1))

# define global attributes
nc_institution = "University of Bristol, 2018"
nc_datasource = "Climate emulator"

nc_title = "2m air temperature"
nc_file_name <- paste(dataoutfilepath,"Em_output_data_",EmulVars,"_region_stat_rcp85_0_to_1MyrAP.nc",sep="")

# define dimensions
lon_dim <- ncdim_def("longitude","degrees_east",as.double(lons_cru_orig_scand))
lat_dim <- ncdim_def("latitude","degrees_north",as.double(lats_cru_scand))
time_dim <- ncdim_def("time","",as.double(kyr_num))
surf_dim <- ncdim_def("surface","",as.double(1))

# define variables
miss_value <- 2e20
nc_var_name <- "SAT"
nc_var_unit <- "degC"
nc_var_lname <- "2m air temperature"

# create variable and ncdf file with first variable so dimensions are set
var_def <- ncvar_def(nc_var_name,nc_var_unit,list(lon_dim,lat_dim,surf_dim,time_dim),miss_value,nc_var_lname,prec="double")
nc_file <- nc_create(nc_file_name,var_def)

# define data to save
data_to_save = final_temp_rcp85_region_alltimes_stat[,,]

# add data to ncdf file
ncvar_put(nc_file,var_def,data_to_save)

# put additional attributes into dimension and data variables
ncatt_put(nc_file,"longitude","axis","X" ,verbose=FALSE, definemode=FALSE)
ncatt_put(nc_file,"latitude","axis","Y")
ncatt_put(nc_file,"time","axis","T")

# add global attributes
ncatt_put(nc_file,0,"title",nc_title)
ncatt_put(nc_file,0,"institution",nc_institution)
ncatt_put(nc_file,0,"source",nc_datasource)

# close the file, writing data to disk
nc_close(nc_file)

plot(times, em_rcp85_interp_smooth_scand_DSstat_a_sweden, type = 'l', ylim = c(-25, 10))
lines(times, em_rcp85_interp_smooth_scand_DSstat_a_finland, type = 'l', ylim = c(-25, 10), col='red')



# Calculate downscaled site temperature only when GSL is > lower threshold (DSstat_b_ig_only)

model_input_all_ns_1myr_AP_GSL = array(c(model_input_zero_emissions_1myr_AP[,5],model_input_rcp26_1myr_AP[,5],model_input_rcp45_1myr_AP[,5],model_input_rcp85_1myr_AP[,5]),c(ne,ns))

em_zero_emissions_interp_smooth_scand_DSstat_b_ig_only_sweden = array(NaN,c(ne,1))
em_rcp26_interp_smooth_scand_DSstat_b_ig_only_sweden = array(NaN,c(ne,1))
em_rcp45_interp_smooth_scand_DSstat_b_ig_only_sweden = array(NaN,c(ne,1))
em_rcp85_interp_smooth_scand_DSstat_b_ig_only_sweden = array(NaN,c(ne,1))
em_zero_emissions_interp_smooth_scand_DSstat_b_ig_only_finland = array(NaN,c(ne,1))
em_rcp26_interp_smooth_scand_DSstat_b_ig_only_finland = array(NaN,c(ne,1))
em_rcp45_interp_smooth_scand_DSstat_b_ig_only_finland = array(NaN,c(ne,1))
em_rcp85_interp_smooth_scand_DSstat_b_ig_only_finland = array(NaN,c(ne,1))

for (row in c(1:ne)){

  if (model_input_all_ns_1myr_AP_GSL[row,1] >= slr_gl_sites_threshold_sweden_lower) {
    em_zero_emissions_interp_smooth_scand_DSstat_b_ig_only_sweden[row,1] = em_zero_emissions_interp_smooth_scand_DSstat_a_sweden[row,1]
    em_zero_emissions_interp_smooth_scand_DSstat_b_ig_only_finland[row,1] = em_zero_emissions_interp_smooth_scand_DSstat_a_finland[row,1]
  } else {
    em_zero_emissions_interp_smooth_scand_DSstat_b_ig_only_sweden[row,1] = my_tim_sweden_zero_emissions_e[row]
    em_zero_emissions_interp_smooth_scand_DSstat_b_ig_only_finland[row,1] = my_tim_finland_zero_emissions_e[row]
  }

  if (model_input_all_ns_1myr_AP_GSL[row,2] >= slr_gl_sites_threshold_sweden_lower) {
    em_rcp26_interp_smooth_scand_DSstat_b_ig_only_sweden[row,1] = em_rcp26_interp_smooth_scand_DSstat_a_sweden[row,1]
    em_rcp26_interp_smooth_scand_DSstat_b_ig_only_finland[row] = em_rcp26_interp_smooth_scand_DSstat_a_finland[row]
  } else {
    em_rcp26_interp_smooth_scand_DSstat_b_ig_only_sweden[row,1] = my_tim_sweden_rcp26_e[row]
    em_rcp26_interp_smooth_scand_DSstat_b_ig_only_finland[row,1] = my_tim_finland_rcp26_e[row]
  }

  if (model_input_all_ns_1myr_AP_GSL[row,3] >= slr_gl_sites_threshold_sweden_lower) {
    em_rcp45_interp_smooth_scand_DSstat_b_ig_only_sweden[row,1] = em_rcp45_interp_smooth_scand_DSstat_a_sweden[row]
    em_rcp45_interp_smooth_scand_DSstat_b_ig_only_finland[row,1] = em_rcp45_interp_smooth_scand_DSstat_a_finland[row]
  } else {
    em_rcp45_interp_smooth_scand_DSstat_b_ig_only_sweden[row,1] = my_tim_sweden_rcp45_e[row]
    em_rcp45_interp_smooth_scand_DSstat_b_ig_only_finland[row,1] = my_tim_finland_rcp45_e[row]
  }

  if (model_input_all_ns_1myr_AP_GSL[row,4] >= slr_gl_sites_threshold_sweden_lower) {
    em_rcp85_interp_smooth_scand_DSstat_b_ig_only_sweden[row,1] = em_rcp85_interp_smooth_scand_DSstat_a_sweden[row]
    em_rcp85_interp_smooth_scand_DSstat_b_ig_only_finland[row,1] = em_rcp85_interp_smooth_scand_DSstat_a_finland[row]
  } else {
    em_rcp85_interp_smooth_scand_DSstat_b_ig_only_sweden[row,1] = my_tim_sweden_rcp85_e[row]
    em_rcp85_interp_smooth_scand_DSstat_b_ig_only_finland[row,1] = my_tim_finland_rcp85_e[row]
  }

}

plot(times, em_zero_emissions_interp_smooth_scand_DSstat_b_ig_only_sweden, type = 'l', ylim = c(-25, 10))
lines(times, em_zero_emissions_interp_smooth_scand_DSstat_b_ig_only_finland, type = 'l', ylim = c(-25, 10), col='red')



# png("C:\\Users\\nl6806\\OneDrive - University of Bristol\\PostDoc\\2017-02-15 Posiva + SKB\\3a. Emulate future climate\\Emulator\\Plots\\2015_Bristol_5D_v001\\2018-08-01 Final report\\temp_em_natural_1MyrAP_DSbias_a_site_only_finland_sweden.png",width=1000,height=650)
plot(times, my_tim_sweden_zero_emissions_e_DSbias_a_site_only, type = 'l', ylim = c(-25, 10))
lines(times, my_tim_finland_zero_emissions_e_DSbias_a_site_only, type = 'l', ylim = c(-25, 10), col='red')
# dev.off()

# png("C:\\Users\\nl6806\\OneDrive - University of Bristol\\PostDoc\\2017-02-15 Posiva + SKB\\3a. Emulate future climate\\Emulator\\Plots\\2015_Bristol_5D_v001\\2018-08-01 Final report\\temp_em_natural_1MyrAP_DSbias_b_site_ig_only_finland_sweden.png",width=1000,height=650)
plot(times, my_tim_sweden_zero_emissions_e_DSbias_b_site_ig_only, type = 'l', ylim = c(-25, 10))
lines(times, my_tim_finland_zero_emissions_e_DSbias_b_site_ig_only, type = 'l', ylim = c(-25, 10), col='red')
# dev.off()

# png("C:\\Users\\nl6806\\OneDrive - University of Bristol\\PostDoc\\2017-02-15 Posiva + SKB\\3a. Emulate future climate\\Emulator\\Plots\\2015_Bristol_5D_v001\\2018-08-01 Final report\\temp_em_natural_1MyrAP_DSbias_c_finland_sweden.png",width=1000,height=650)
plot(times, em_zero_emissions_interp_smooth_scand_DSbias_c_sweden, type = 'l', ylim = c(-25, 10))
lines(times, em_zero_emissions_interp_smooth_scand_DSbias_c_finland, type = 'l', ylim = c(-25, 10), col='red')
# dev.off()

# png("C:\\Users\\nl6806\\OneDrive - University of Bristol\\PostDoc\\2017-02-15 Posiva + SKB\\3a. Emulate future climate\\Emulator\\Plots\\2015_Bristol_5D_v001\\2018-08-01 Final report\\temp_em_natural_1MyrAP_DSbias_d_ig_only_finland_sweden.png",width=1000,height=650)
plot(times, em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_sweden, type = 'l', ylim = c(-25, 10))
lines(times, em_zero_emissions_interp_smooth_scand_DSbias_d_ig_only_finland, type = 'l', ylim = c(-25, 10), col='red')
# dev.off()

# png("C:\\Users\\nl6806\\OneDrive - University of Bristol\\PostDoc\\2017-02-15 Posiva + SKB\\3a. Emulate future climate\\Emulator\\Plots\\2015_Bristol_5D_v001\\2018-08-01 Final report\\temp_em_natural_1MyrAP_DSstat_a_finland_sweden.png",width=1000,height=650)
plot(times, em_zero_emissions_interp_smooth_scand_DSstat_a_sweden, type = 'l', ylim = c(-25, 10))
lines(times, em_zero_emissions_interp_smooth_scand_DSstat_a_finland, type = 'l', ylim = c(-25, 10), col='red')
# dev.off()

# png("C:\\Users\\nl6806\\OneDrive - University of Bristol\\PostDoc\\2017-02-15 Posiva + SKB\\3a. Emulate future climate\\Emulator\\Plots\\2015_Bristol_5D_v001\\2018-08-01 Final report\\temp_em_natural_1MyrAP_DSstat_b_ig_only_finland_sweden.png",width=1000,height=650)
plot(times, em_zero_emissions_interp_smooth_scand_DSstat_b_ig_only_sweden, type = 'l', ylim = c(-25, 10))
lines(times, em_zero_emissions_interp_smooth_scand_DSstat_b_ig_only_finland, type = 'l', ylim = c(-25, 10), col='red')
# dev.off()


print("Downscaling finished. Get back to work!")
check2a = Sys.time()
print(check2a-check1a)
