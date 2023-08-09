# --------------------------------------------------
# LOAD LIBRARIES

library(ncdf4)
library(fields)
library(mapproj)

# ENSEMBLE:

Exp = "tdab"

Exp_list_LC_tdab = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t")

Exp_num_tdab = length(Exp_list_LC_tdab)

nlayer = 9

# --------------------------------------------------
# READ IN OUTPUT DATA

ice = list((0))

for (i in Exp_list_LC_tdab) {
	nc = nc_open(file.path('..','orig','Output','Singarayer + Valdes 2010', paste("dT500_fracPFTs_snp_srf_ann_", Exp, "28k.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	ice_all = ncvar_get(nc, var_nam)
	ice = ice_all[,,nlayer]
	nam = paste("ice_tdab", i, "_kdat", sep = "")
	assign(nam, ice)
	nc_close(nc)
	rm(nc, ice)
}


# MAKE MISSING VALUES

for (i in Exp_list_LC_tdab) {
  nam = paste("ice_tdab", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


# --------------------------------------------------
# VECTORIZE ICE MATRICES (BY COLUMN (LATITUDE))

ice_tdab_k_all = matrix(0, nrow = length(ice_tdabc_kdat), ncol = Exp_num_tdab)
j = 1


for (i in Exp_list_LC_tdab) {
  nam = paste("ice_tdab", i, "_kdat", sep = "")
  nam_new = paste("ice_tdab", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  ice_tdab_k_all[,j] = data_new
  rm(data)
  j = j + 1
}


rm(list=ls()[grep("_vec",ls())])
rm(list=ls()[grep("kdat",ls())])

# EXTRACT VARIABLES

ice_surf_tdab_19kyr = ice_tdab_k_all

# and put then on a grid format

ice_surf_tdab_19kyr <- array(ice_surf_tdab_19kyr, c(96, 73, Exp_num_tdab ))

#to.remove <- ls()
#to.remove <- c(to.remove[!grepl("^ice_surf_tdab", to.remove)], "to.remove")
#to.remove <- c(to.remove[!grepl("^Exp_num", to.remove)], "to.remove")
#rm(list=to.remove)

#rm(list= ls()[!(ls() %in% c('model_input_tdum','model_output_tdum','model_input_tdst','Exp_num_tdum','model_input_tdvo','model_output_tdvo','Exp_num_tdvo','model_input_tdab','model_output_tdab','Exp_num_tdab'))])

###################################################################################