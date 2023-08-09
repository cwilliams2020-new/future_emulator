# --------------------------------------------------

# ENSEMBLE:

Exp = "tdst"

#Exp_list = c("b", "c", "d", "e", "f")
Exp_list = c("b", "c") # For *a.ptclann.nc files (i.e. those containing veg), there is no d-f


# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','orig','Input','Samp_orbits_tdst.res'), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)


# READ IN OUTPUT DATA

lai=list((0))

for (i in Exp_list) {
	nc = nc_open(file.path('..','orig','Output', 'Climate NC', paste( Exp, i, "a.ptclann.nc", sep=""))) # CW: Changed this to file containing veg
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	precip = ncvar_get(nc, "LAI_mm_srf")
	nam = paste("lai_tdst", i, "_kdat", sep = "")
	assign(nam, precip)
	nc_close(nc)
	rm(nc, precip)
}


# MAKE MISSING VALUES

for (i in Exp_list) {
  nam = paste("lai_tdst", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


# --------------------------------------------------
# VECTORIZE MATRICES (BY COLUMN (LATITUDE))

lai_tdst_k_all = matrix(0, nrow = length(lai_tdstb_kdat), ncol = 5)
j = 1


for (i in Exp_list) {
  nam = paste("lai_tdst", i, "_kdat", sep = "")
  nam_new = paste("lai_tdst", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  lai_tdst_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

rm(list=ls()[grep("_vec",ls())])
rm(list=ls()[grep("kdat",ls())])


# EXTRACT VARIABLES FOR EMULATOR (X AND Y)

model_input_tdst = data.matrix(cont_paramdat)

model_output_tdst = lai_tdst_k_all

# and put then on a grid format

model_output_tdst <- array(model_output_tdst, c(96, 73, 5 ))

rm(list=ls()[(ls() %in% c('Exp','Exp_list','cont_paramdat','cont_param_dim','precip_tdst_k_all'))])
