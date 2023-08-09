# --------------------------------------------------
# LOAD LIBRARIES

library(ncdf4)
library(fields)
library(mapproj)

# ENSEMBLE:

Exp = "tdum"

Exp_list_LC_tdum = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

Exp_list_UC_tdum = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N")

Exp_num_tdum=length(Exp_list_LC_tdum)+length(Exp_list_UC_tdum)


# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','orig','Input','Samp_orbits_tdum.res'), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)

obl = cont_paramdat$obl
esinw = cont_paramdat$esinw
ecosw = cont_paramdat$ecosw
co2 = cont_paramdat$co2
ice = cont_paramdat$ice


# READ IN OUTPUT DATA

precip=list((0))

for (i in Exp_list_LC_tdum) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_precip_mm_srf_ann_", Exp, ".nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	precip = ncvar_get(nc, var_nam)
	nam = paste("precip_tdum", i, "_kdat", sep = "")
	assign(nam, precip)
	nc_close(nc)
	rm(nc, precip)
}


for (i in Exp_list_UC_tdum) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_precip_mm_srf_ann_", Exp, ".nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	precip = ncvar_get(nc, var_nam)
	nam = paste("precip_tdum", i, "_kdat", sep = "")
	assign(nam, precip)
	nc_close(nc)
	rm(nc, precip)
}



# MAKE MISSING VALUES

for (i in Exp_list_LC_tdum) {
  nam = paste("precip_tdum", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


for (i in Exp_list_UC_tdum) {
  nam = paste("precip_tdum", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}



# --------------------------------------------------
# VECTORIZE PRECIPITATION MATRICES (BY COLUMN (LATITUDE))

precip_tdum_k_all = matrix(0, nrow = length(precip_tdumc_kdat), ncol = Exp_num_tdum)
j = 1


for (i in Exp_list_LC_tdum) {
  nam = paste("precip_tdum", i, "_kdat", sep = "")
  nam_new = paste("precip_tdum", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  precip_tdum_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

for (i in Exp_list_UC_tdum) {
  nam = paste("precip_tdum", i, "_kdat", sep = "")
  nam_new = paste("precip_tdum", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  precip_tdum_k_all[,j] = data_new
  rm(data)
  j = j + 1
}


rm(list=ls()[grep("_vec",ls())])
rm(list=ls()[grep("kdat",ls())])

# EXTRACT VARIABLES FOR EMULATOR (X AND Y)

model_input_tdum = data.matrix(cont_paramdat)

model_output_tdum = precip_tdum_k_all

# and put then on a grid format

model_output_tdum <- array(model_output_tdum, c(96, 73, Exp_num_tdum ))

# Clear some unnecessary variables, except those listed here
source("Emulator_remove_vars2.R")

#rm(list= ls()[!(ls() %in% c('model_input_tdum','model_output_tdum','model_input_tdst','Exp_num_tdum'))])

###################################################################################

Exp = "tdvo"

Exp_list_LC_tdvo = c("c", "d", "g", "h", "i", "k", "n", "o", "p", "q", "s", "u", "v", "w", "y")

Exp_list_UC_tdvo = c("C", "D", "E", "H", "M")

Exp_num_tdvo=length(Exp_list_LC_tdvo)+length(Exp_list_UC_tdvo)


# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','orig','Input','Samp_orbits_tdvo_LT2000ppm.res'), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)

obl = cont_paramdat$obl
esinw = cont_paramdat$esinw
ecosw = cont_paramdat$ecosw
co2 = cont_paramdat$co2
ice = cont_paramdat$ice


# READ IN OUTPUT DATA

precip=list((0))

for (i in Exp_list_LC_tdvo) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_precip_mm_srf_ann_", Exp, "_LT2000ppm.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	precip = ncvar_get(nc, var_nam)
	nam = paste("precip_tdvo", i, "_kdat", sep = "")
	assign(nam, precip)
	nc_close(nc)
	rm(nc, precip)
}


for (i in Exp_list_UC_tdvo) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_precip_mm_srf_ann_", Exp, "_LT2000ppm.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	precip = ncvar_get(nc, var_nam)
	nam = paste("precip_tdvo", i, "_kdat", sep = "")
	assign(nam, precip)
	nc_close(nc)
	rm(nc, precip)
}



# MAKE MISSING VALUES

for (i in Exp_list_LC_tdvo) {
  nam = paste("precip_tdvo", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


for (i in Exp_list_UC_tdvo) {
  nam = paste("precip_tdvo", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}



# --------------------------------------------------
# VECTORIZE PRECIPITATION MATRICES (BY COLUMN (LATITUDE))

precip_tdvo_k_all = matrix(0, nrow = length(precip_tdvoc_kdat), ncol = Exp_num_tdvo)
j = 1


for (i in Exp_list_LC_tdvo) {
  nam = paste("precip_tdvo", i, "_kdat", sep = "")
  nam_new = paste("precip_tdvo", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  precip_tdvo_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

for (i in Exp_list_UC_tdvo) {
  nam = paste("precip_tdvo", i, "_kdat", sep = "")
  nam_new = paste("precip_tdvo", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  precip_tdvo_k_all[,j] = data_new
  rm(data)
  j = j + 1
}


rm(list=ls()[grep("_vec",ls())])
rm(list=ls()[grep("kdat",ls())])

# EXTRACT VARIABLES FOR EMULATOR (X AND Y)

model_input_tdvo = data.matrix(cont_paramdat)

model_output_tdvo = precip_tdvo_k_all

# and put then on a grid format

model_output_tdvo <- array(model_output_tdvo, c(96, 73, Exp_num_tdvo ))

# Clear some unnecessary variables, except those listed here
source("Emulator_remove_vars2.R")

#rm(list= ls()[!(ls() %in% c('model_input_tdum','model_output_tdum','model_input_tdst','Exp_num_tdum','model_input_tdvo','model_output_tdvo','Exp_num_tdvo'))])

###################################################################################

Exp = "tdab"

Exp_list_LC_tdab = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

Exp_list_UC_tdab = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")

Exp_list_NUM_tdab = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")

Exp_num_tdab=length(Exp_list_LC_tdab)+length(Exp_list_UC_tdab)+length(Exp_list_NUM_tdab)

# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','orig','Input','Singarayer + Valdes 2010','Samp_orbits_tdab.res'), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)

obl = cont_paramdat$obl
esinw = cont_paramdat$esinw
ecosw = cont_paramdat$ecosw
co2 = cont_paramdat$co2
ice = cont_paramdat$ice


# READ IN OUTPUT DATA

precip=list((0))

for (i in Exp_list_LC_tdab) {
	nc = nc_open(file.path('..','orig','Output','Singarayer + Valdes 2010', paste("dT500_precip_mm_srf_ann_", Exp, "28k.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	precip = ncvar_get(nc, var_nam)
	nam = paste("precip_tdab", i, "_kdat", sep = "")
	assign(nam, precip)
	nc_close(nc)
	rm(nc, precip)
}


for (i in Exp_list_UC_tdab) {
	nc = nc_open(file.path('..','orig','Output','Singarayer + Valdes 2010', paste("dT500_precip_mm_srf_ann_", Exp, "80k.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	precip = ncvar_get(nc, var_nam)
	nam = paste("precip_tdab", i, "_kdat", sep = "")
	assign(nam, precip)
	nc_close(nc)
	rm(nc, precip)
}


for (i in Exp_list_NUM_tdab) {
	nc = nc_open(file.path('..','orig','Output','Singarayer + Valdes 2010', paste("dT500_precip_mm_srf_ann_", Exp, "120k.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	precip = ncvar_get(nc, var_nam)
	nam = paste("precip_tdab", i, "_kdat", sep = "")
	assign(nam, precip)
	nc_close(nc)
	rm(nc, precip)
}


# MAKE MISSING VALUES

for (i in Exp_list_LC_tdab) {
  nam = paste("precip_tdab", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


for (i in Exp_list_UC_tdab) {
  nam = paste("precip_tdab", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


for (i in Exp_list_NUM_tdab) {
  nam = paste("precip_tdab", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}



# --------------------------------------------------
# VECTORIZE PRECIPITATION MATRICES (BY COLUMN (LATITUDE))

precip_tdab_k_all = matrix(0, nrow = length(precip_tdabc_kdat), ncol = Exp_num_tdab)
j = 1


for (i in Exp_list_LC_tdab) {
  nam = paste("precip_tdab", i, "_kdat", sep = "")
  nam_new = paste("precip_tdab", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  precip_tdab_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

for (i in Exp_list_UC_tdab) {
  nam = paste("precip_tdab", i, "_kdat", sep = "")
  nam_new = paste("precip_tdab", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  precip_tdab_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

for (i in Exp_list_NUM_tdab) {
  nam = paste("precip_tdab", i, "_kdat", sep = "")
  nam_new = paste("precip_tdab", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  precip_tdab_k_all[,j] = data_new
  rm(data)
  j = j + 1
}


rm(list=ls()[grep("_vec",ls())])
rm(list=ls()[grep("kdat",ls())])

# EXTRACT VARIABLES FOR EMULATOR (X AND Y)

model_input_tdab = data.matrix(cont_paramdat)

model_output_tdab = precip_tdab_k_all

# and put then on a grid format

model_output_tdab <- array(model_output_tdab, c(96, 73, Exp_num_tdab ))

# Clear some unnecessary variables, except those listed here
source("Emulator_remove_vars2.R")

#rm(list= ls()[!(ls() %in% c('model_input_tdum','model_output_tdum','model_input_tdst','Exp_num_tdum','model_input_tdvo','model_output_tdvo','Exp_num_tdvo','model_input_tdab','model_output_tdab','Exp_num_tdab'))])

###################################################################################

# MERGE VARIABLES FOR EMULATOR (X AND Y)

model_input_modice_tdab = rbind(model_input_tdum,model_input_tdvo,model_input_tdab)

model_output_modice_tdab <- array(0, c(96, 73, (Exp_num_tdum+Exp_num_tdvo+Exp_num_tdab)))

n=1

for (i in 1: Exp_num_tdum) {
	model_output_modice_tdab[,,n]=model_output_tdum[,,i]
	n=n+1
	}

for (i in 1: Exp_num_tdvo) {
	model_output_modice_tdab[,,n]=model_output_tdvo[,,i]
	n=n+1
	}
	
for (i in 1: Exp_num_tdab) {
	model_output_modice_tdab[,,n]=model_output_tdab[,,i]
	n=n+1
	}
	
#rm(list= ls()[!(ls() %in% c('model_input_modice_tdab','model_output_modice_tdab','model_input_tdst','Exp_num_tdum','Exp_num_tdvo','Exp_num_tdab'))])

