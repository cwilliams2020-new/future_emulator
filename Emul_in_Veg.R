###################################################################################################
## BUILD TWO EMULATORS ON TDUM + TDVO + TDAB and TDUM + TDVO + TDVP + TDVQ OUTPUT #################
## Use log CO2 ####################################################################################
## Optimised previously based on temp_mm_1_5m #####################################################
## Calibrate final using TLAI_PFT_snp_srf #########################################################
###################################################################################################

## Set up #########################################################################################

.libPaths(c("C:/Users/cw18831/OneDrive - University of Bristol/Documents/R/win-library/4.1", .libPaths()))

#setwd("C:/Users/nl6806/OneDrive - University of Bristol/PostDoc/2017-02-15 Posiva + SKB/3a. Emulate future climate/Emulator/For_Bristol/2015_Bristol_5D_v001/R")

# load data
source('../Data/Data - dTeq dT500/dTeq_dT500_veg_ann_modice_LT2000ppm_tdab.R')
source('../Data/Data - dTeq dT500/dTeq_dT500_veg_ann_modlowice_LT2000ppm.R')

print('Emulator_vegetation')

# Clear some unnecessary variables, except those listed here
source("Emulator_remove_vars1.R")

# Load PI control
source('../Data/Data - Climate NC/Tdst_veg_mm_srf.R') # CW: Pointing to newly created file

# Remove NaNs from climate data

model_output_modice_tdab[is.na(model_output_modice_tdab)] = 0
model_output_modlowice[is.na(model_output_modlowice)] = 0
#model_output_tdst[is.na(model_output_tdst)] = 0


# load resources (see superseded versions for code to install packages)
require(gsl)

require(Hmisc)

require(GP)

source('pca_emul.R')

par_orig=par()

# set CO2 to be log (natural) of CO2
model_input_modice_tdab[,1]=log(model_input_modice_tdab[,1])
model_input_modlowice[,1]=log(model_input_modlowice[,1])

model_input_all = rbind(model_input_modice_tdab,model_input_modlowice)

# redefine names and scale inputs
X_all = scale(model_input_all) # subtract column mean then divide by standard deviation for column
X_gl = array(X_all[1:length(model_input_modice_tdab[,1]),],c(length(model_input_modice_tdab[,1]),5))
X_ig = array(X_all[(length(model_input_modice_tdab[,1])+1):length(model_input_all[,1]),],c(length(model_input_modlowice[,1]),5))

Y_gl = model_output_modice_tdab
Y_ig = model_output_modlowice


## Build emulator using optimised parameters ######################################################

nkeep_gl=15 # 18
nkeep_ig=15 # 17

theta_gl=c(1.003084,6.907880,7.499054,5.460205,0.290289) # 15
theta_ig=c(0.523323,2.791735,1.310285,1.663824,10.000000) # 15

nugget_gl=0.050143 # 15
nugget_ig=0.000000000224038 # 15

var_captured_gl=0.810956 # 15
var_captured_ig=0.897191 # 15


# set hyperparameters (optimised)
hp_gl = matrix ( rep( c( theta_gl, nugget_gl ),  nkeep_gl), length(theta_gl) + 1, nkeep_gl)
hp_ig = matrix ( rep( c( theta_ig, nugget_ig ),  nkeep_ig), length(theta_ig) + 1, nkeep_ig)


# build emulator using optimised parameters (PC=optimised, N=optimised, HP=optimised)
E_gl = pe_c (X_gl, Y_gl, mypca, hp=hp_gl, nkeep=nkeep_gl)
E_ig = pe_c (X_ig, Y_ig, mypca, hp=hp_ig, nkeep=nkeep_ig)

