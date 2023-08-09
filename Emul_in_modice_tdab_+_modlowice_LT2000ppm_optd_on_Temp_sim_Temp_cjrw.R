###################################################################################################
## BUILD TWO EMULATORS ON TDUM + TDVO + TDAB and TDUM + TDVO + TDVP + TDVQ OUTPUT #################
## Use log CO2 ####################################################################################
## Optimised previously based on temp_mm_1_5m #####################################################
## Calibrate final using temp_mm_1_5m #############################################################
###################################################################################################

## Set up #########################################################################################

.libPaths(c("C:/Users/cw18831/OneDrive - University of Bristol/Documents/R/win-library/4.1", .libPaths()))

# load data
source('C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/SKB/SKB_Alan_Code_DJL/PosivaSKB/PosivaSKB/Emulator/2015_Bristol_5D_v001/Data/Data - dTeq dT500/dTeq_dT500_temp_ann_modice_LT2000ppm_tdab_cjrw.R')
source('C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/SKB/SKB_Alan_Code_DJL/PosivaSKB/PosivaSKB/Emulator/2015_Bristol_5D_v001/Data/Data - dTeq dT500/dTeq_dT500_temp_ann_modlowice_LT2000ppm_cjrw.R')


to.remove <- ls()

to.remove <- c(to.remove[!grepl("EmulVars", to.remove)])
to.remove <- c(to.remove[!grepl("Sim", to.remove)])
to.remove <- c(to.remove[!grepl("noice", to.remove)])
to.remove <- c(to.remove[!grepl("filepath", to.remove)])
to.remove <- c(to.remove[!grepl("savefigs", to.remove)])

to.remove <- c(to.remove[!grepl("^model_input_mod", to.remove)], "to.remove")
to.remove <- c(to.remove[!grepl("^model_output_mod", to.remove)], "to.remove")
to.remove <- c(to.remove[!grepl("^Exp_num", to.remove)], "to.remove")
rm(list=to.remove)

source('C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/SKB/SKB_Alan_Code_DJL/PosivaSKB/PosivaSKB/Emulator/2015_Bristol_5D_v001/Data/Data - Climate NC/Tdst_temp_mm_1_5m_cjrw.R')

# Calculate anomaly compared to PI for tdab (S+V data)

model_output_modice_tdab_orig = model_output_modice_tdab

tdab_exps = seq(Exp_num_tdum+Exp_num_tdvo+1,Exp_num_tdum+Exp_num_tdvo+Exp_num_tdab)

for(i in tdab_exps){
  model_output_modice_tdab[,,i] = model_output_modice_tdab_orig[,,i] - model_output_modice_tdab_orig[,,tdab_exps[1]]
}

# load resources (see superseded versions for code to install packages)
require(gsl)

require(Hmisc)

require(GP)

source('C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/SKB/SKB_Alan_Code_DJL/PosivaSKB/PosivaSKB/Emulator/2015_Bristol_5D_v001/R/pca_emul.R')

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

