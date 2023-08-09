# --------------------------------------------------
# LOAD LIBRARIES

library(ncdf)
library(fields)
library(mapproj)

# ENSEMBLE:

Exp = "tdum"

Exp_list_LC = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

Exp_list_UC = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N")


# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','orig','Input','Samp_orbits_tdum.res'), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)

obl = cont_paramdat$obl
esinw = cont_paramdat$esinw
ecosw = cont_paramdat$ecosw
co2 = cont_paramdat$co2


# READ IN OUTPUT DATA

temp=list((0))

for (i in Exp_list_LC) {
	nc = nc_open(file.path('..','orig','Output','Climate NC','LC', paste( Exp, i, "a.pdclann.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	temp = ncvar_get(nc, "temp_mm_1_5m")
	nam = paste("temp_tdum", i, "_kdat", sep = "")
	assign(nam, temp)
	nc_close(nc)
	rm(nc, temp)
}

for (i in Exp_list_UC) {
  nc = nc_open(file.path('..','orig','Output','Climate NC','UC', paste(Exp, i, "a.pdclann.nc", sep="")))
  lats_dat = ncvar_get(nc, "latitude")
  lons_dat = ncvar_get(nc, "longitude")
  
  temp = ncvar_get(nc, "temp_mm_1_5m")
  nam = paste("temp_tdum", i, "_kdat", sep = "")
  assign(nam, temp)
  nc_close(nc)
  rm(nc, temp)
}


# MAKE MISSING VALUES

for (i in Exp_list_LC) {
  nam = paste("temp_tdum", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


for (i in Exp_list_UC) {
  nam = paste("temp_tdum", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


# --------------------------------------------------
# VECTORIZE TEMPERATURE MATRICES (BY COLUMN (LATITUDE))

temp_tdum_k_all = matrix(0, nrow = length(temp_tduma_kdat), ncol = 40)
j = 1


for (i in Exp_list_LC) {
  nam = paste("temp_tdum", i, "_kdat", sep = "")
  nam_new = paste("temp_tdum", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  temp_tdum_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

for (i in Exp_list_UC) {
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

model_output_tdum <- array(model_output_tdum, c(96, 73, 40 ))

rm(list= ls()[!(ls() %in% c('model_input_tdum','model_output_tdum','model_input_tdst'))])
