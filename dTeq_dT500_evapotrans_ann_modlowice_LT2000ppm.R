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

temp=list((0))

for (i in Exp_list_LC_tdum) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_evapotrans_mm_srf_ann_", Exp, ".nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	temp = ncvar_get(nc, var_nam)
	nam = paste("temp_tdum", i, "_kdat", sep = "")
	assign(nam, temp)
	nc_close(nc)
	rm(nc, temp)
}


for (i in Exp_list_UC_tdum) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_evapotrans_mm_srf_ann_", Exp, ".nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	temp = ncvar_get(nc, var_nam)
	nam = paste("temp_tdum", i, "_kdat", sep = "")
	assign(nam, temp)
	nc_close(nc)
	rm(nc, temp)
}



# MAKE MISSING VALUES

for (i in Exp_list_LC_tdum) {
  nam = paste("temp_tdum", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


for (i in Exp_list_UC_tdum) {
  nam = paste("temp_tdum", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}



# --------------------------------------------------
# VECTORIZE TEMPERATURE MATRICES (BY COLUMN (LATITUDE))

temp_tdum_k_all = matrix(0, nrow = length(temp_tdumc_kdat), ncol = Exp_num_tdum)
j = 1


for (i in Exp_list_LC_tdum) {
  nam = paste("temp_tdum", i, "_kdat", sep = "")
  nam_new = paste("temp_tdum", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  temp_tdum_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

for (i in Exp_list_UC_tdum) {
  nam = paste("temp_tdum", i, "_kdat", sep = "")
  nam_new = paste("temp_tdum", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  temp_tdum_k_all[,j] = data_new
  rm(data)
  j = j + 1
}


rm(list=ls()[grep("_vec",ls())])
rm(list=ls()[grep("kdat",ls())])

# EXTRACT VARIABLES FOR EMULATOR (X AND Y)

model_input_tdum = data.matrix(cont_paramdat)

model_output_tdum = temp_tdum_k_all

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

temp=list((0))

for (i in Exp_list_LC_tdvo) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_evapotrans_mm_srf_ann_", Exp, "_LT2000ppm.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	temp = ncvar_get(nc, var_nam)
	nam = paste("temp_tdvo", i, "_kdat", sep = "")
	assign(nam, temp)
	nc_close(nc)
	rm(nc, temp)
}


for (i in Exp_list_UC_tdvo) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_evapotrans_mm_srf_ann_", Exp, "_LT2000ppm.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	temp = ncvar_get(nc, var_nam)
	nam = paste("temp_tdvo", i, "_kdat", sep = "")
	assign(nam, temp)
	nc_close(nc)
	rm(nc, temp)
}



# MAKE MISSING VALUES

for (i in Exp_list_LC_tdvo) {
  nam = paste("temp_tdvo", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


for (i in Exp_list_UC_tdvo) {
  nam = paste("temp_tdvo", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}



# --------------------------------------------------
# VECTORIZE TEMPERATURE MATRICES (BY COLUMN (LATITUDE))

temp_tdvo_k_all = matrix(0, nrow = length(temp_tdvoc_kdat), ncol = Exp_num_tdvo)
j = 1


for (i in Exp_list_LC_tdvo) {
  nam = paste("temp_tdvo", i, "_kdat", sep = "")
  nam_new = paste("temp_tdvo", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  temp_tdvo_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

for (i in Exp_list_UC_tdvo) {
  nam = paste("temp_tdvo", i, "_kdat", sep = "")
  nam_new = paste("temp_tdvo", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  temp_tdvo_k_all[,j] = data_new
  rm(data)
  j = j + 1
}


rm(list=ls()[grep("_vec",ls())])
rm(list=ls()[grep("kdat",ls())])

# EXTRACT VARIABLES FOR EMULATOR (X AND Y)

model_input_tdvo = data.matrix(cont_paramdat)

model_output_tdvo = temp_tdvo_k_all

# and put then on a grid format

model_output_tdvo <- array(model_output_tdvo, c(96, 73, Exp_num_tdvo ))

# Clear some unnecessary variables, except those listed here
source("Emulator_remove_vars2.R")

#rm(list= ls()[!(ls() %in% c('model_input_tdum','model_output_tdum','model_input_tdst','Exp_num_tdum','model_input_tdvo','model_output_tdvo','Exp_num_tdvo'))])

###################################################################################

# ENSEMBLE:

Exp = "tdvp"

Exp_list_LC_tdvp = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

Exp_list_UC_tdvp = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N")

Exp_num_tdvp=length(Exp_list_LC_tdvp)+length(Exp_list_UC_tdvp)


# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','orig','Input','Samp_orbits_tdvp.res'), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)

obl = cont_paramdat$obl
esinw = cont_paramdat$esinw
ecosw = cont_paramdat$ecosw
co2 = cont_paramdat$co2
ice = cont_paramdat$ice


# READ IN OUTPUT DATA

temp=list((0))

for (i in Exp_list_LC_tdvp) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_evapotrans_mm_srf_ann_", Exp, ".nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	temp = ncvar_get(nc, var_nam)
	nam = paste("temp_tdvp", i, "_kdat", sep = "")
	assign(nam, temp)
	nc_close(nc)
	rm(nc, temp)
}


for (i in Exp_list_UC_tdvp) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_evapotrans_mm_srf_ann_", Exp, ".nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	temp = ncvar_get(nc, var_nam)
	nam = paste("temp_tdvp", i, "_kdat", sep = "")
	assign(nam, temp)
	nc_close(nc)
	rm(nc, temp)
}



# MAKE MISSING VALUES

for (i in Exp_list_LC_tdvp) {
  nam = paste("temp_tdvp", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


for (i in Exp_list_UC_tdvp) {
  nam = paste("temp_tdvp", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}



# --------------------------------------------------
# VECTORIZE TEMPERATURE MATRICES (BY COLUMN (LATITUDE))

temp_tdvp_k_all = matrix(0, nrow = length(temp_tdvpc_kdat), ncol = Exp_num_tdvp)
j = 1


for (i in Exp_list_LC_tdvp) {
  nam = paste("temp_tdvp", i, "_kdat", sep = "")
  nam_new = paste("temp_tdvp", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  temp_tdvp_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

for (i in Exp_list_UC_tdvp) {
  nam = paste("temp_tdvp", i, "_kdat", sep = "")
  nam_new = paste("temp_tdvp", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  temp_tdvp_k_all[,j] = data_new
  rm(data)
  j = j + 1
}


rm(list=ls()[grep("_vec",ls())])
rm(list=ls()[grep("kdat",ls())])

# EXTRACT VARIABLES FOR EMULATOR (X AND Y)

model_input_tdvp = data.matrix(cont_paramdat)

model_output_tdvp = temp_tdvp_k_all

# and put then on a grid format

model_output_tdvp <- array(model_output_tdvp, c(96, 73, Exp_num_tdvp ))

# Clear some unnecessary variables, except those listed here
source("Emulator_remove_vars2.R")

#rm(list= ls()[!(ls() %in% c('model_input_tdum','model_output_tdum','model_input_tdst','Exp_num_tdum','model_input_tdvo','model_output_tdvo','Exp_num_tdvo','model_input_tdvp','model_output_tdvp','Exp_num_tdvp'))])

###################################################################################

Exp = "tdvq"

Exp_list_LC_tdvq = c("c", "d", "g", "h", "i", "k", "n", "o", "p", "q", "s", "u", "v", "w", "y")

Exp_list_UC_tdvq = c("C", "D", "E", "H", "M")

Exp_num_tdvq=length(Exp_list_LC_tdvq)+length(Exp_list_UC_tdvq)


# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','orig','Input','Samp_orbits_tdvq_LT2000ppm.res'), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)

obl = cont_paramdat$obl
esinw = cont_paramdat$esinw
ecosw = cont_paramdat$ecosw
co2 = cont_paramdat$co2
ice = cont_paramdat$ice


# READ IN OUTPUT DATA

temp=list((0))

for (i in Exp_list_LC_tdvq) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_evapotrans_mm_srf_ann_", Exp, "_LT2000ppm.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	temp = ncvar_get(nc, var_nam)
	nam = paste("temp_tdvq", i, "_kdat", sep = "")
	assign(nam, temp)
	nc_close(nc)
	rm(nc, temp)
}


for (i in Exp_list_UC_tdvq) {
	nc = nc_open(file.path('..','orig','Output','dTeq','LT2000', paste("dTeq_evapotrans_mm_srf_ann_", Exp, "_LT2000ppm.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	var_nam=paste(Exp, i, sep = "")
	temp = ncvar_get(nc, var_nam)
	nam = paste("temp_tdvq", i, "_kdat", sep = "")
	assign(nam, temp)
	nc_close(nc)
	rm(nc, temp)
}



# MAKE MISSING VALUES

for (i in Exp_list_LC_tdvq) {
  nam = paste("temp_tdvq", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


for (i in Exp_list_UC_tdvq) {
  nam = paste("temp_tdvq", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}



# --------------------------------------------------
# VECTORIZE TEMPERATURE MATRICES (BY COLUMN (LATITUDE))

temp_tdvq_k_all = matrix(0, nrow = length(temp_tdvqc_kdat), ncol = Exp_num_tdvq)
j = 1


for (i in Exp_list_LC_tdvq) {
  nam = paste("temp_tdvq", i, "_kdat", sep = "")
  nam_new = paste("temp_tdvq", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  temp_tdvq_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

for (i in Exp_list_UC_tdvq) {
  nam = paste("temp_tdvq", i, "_kdat", sep = "")
  nam_new = paste("temp_tdvq", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  temp_tdvq_k_all[,j] = data_new
  rm(data)
  j = j + 1
}


rm(list=ls()[grep("_vec",ls())])
rm(list=ls()[grep("kdat",ls())])

# EXTRACT VARIABLES FOR EMULATOR (X AND Y)

model_input_tdvq = data.matrix(cont_paramdat)

model_output_tdvq = temp_tdvq_k_all

# and put then on a grid format

model_output_tdvq <- array(model_output_tdvq, c(96, 73, Exp_num_tdvq ))

# Clear some unnecessary variables, except those listed here
source("Emulator_remove_vars2.R")

#rm(list= ls()[!(ls() %in% c('model_input_tdum','model_output_tdum','model_input_tdst','Exp_num_tdum','model_input_tdvo','model_output_tdvo','Exp_num_tdvo','model_input_tdvp','model_output_tdvp','Exp_num_tdvp','model_input_tdvq','model_output_tdvq','Exp_num_tdvq'))])

###################################################################################

# MERGE VARIABLES FOR EMULATOR (X AND Y)

model_input_modlowice = rbind(model_input_tdum,model_input_tdvo,model_input_tdvp,model_input_tdvq)

model_output_modlowice <- array(0, c(96, 73, (Exp_num_tdum+Exp_num_tdvo+Exp_num_tdvp+Exp_num_tdvq)))

n=1

for (i in 1: Exp_num_tdum) {
	model_output_modlowice[,,n]=model_output_tdum[,,i]
	n=n+1
	}

for (i in 1: Exp_num_tdvo) {
	model_output_modlowice[,,n]=model_output_tdvo[,,i]
	n=n+1
	}
	
for (i in 1: Exp_num_tdvp) {
	model_output_modlowice[,,n]=model_output_tdvp[,,i]
	n=n+1
	}

for (i in 1: Exp_num_tdvq) {
	model_output_modlowice[,,n]=model_output_tdvq[,,i]
	n=n+1
	}

#rm(list= ls()[!(ls() %in% c('model_input_modlowice','model_output_modlowice','model_input_tdst','Exp_num_tdum','Exp_num_tdvo','Exp_num_tdvp','Exp_num_tdvq'))])

