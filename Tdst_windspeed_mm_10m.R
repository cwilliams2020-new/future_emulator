# --------------------------------------------------

# ENSEMBLE:

Exp = "tdst"

Exp_list = c("b", "c", "d", "e", "f")


# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','orig','Input','Samp_orbits_tdst.res'), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)


# READ IN OUTPUT DATA

temp1=list((0))

for (i in Exp_list) {
	nc = nc_open(file.path('..','orig','Output', 'Climate NC', paste( Exp, i, "a.pdclann.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude_1")
	lons_dat = ncvar_get(nc, "longitude_1")
	
	temp1 = ncvar_get(nc, "u_mm_10m")
	nam = paste("temp1_tdst", i, "_kdat", sep = "")
	assign(nam, temp1)
	nc_close(nc)
	rm(nc, temp1)
}


temp2=list((0))

for (i in Exp_list) {
	nc = nc_open(file.path('..','orig','Output', 'Climate NC', paste( Exp, i, "a.pdclann.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude_1")
	lons_dat = ncvar_get(nc, "longitude_1")
	
	temp2 = ncvar_get(nc, "v_mm_10m")
	nam = paste("temp2_tdst", i, "_kdat", sep = "")
	assign(nam, temp2)
	nc_close(nc)
	rm(nc, temp2)
}


# MAKE MISSING VALUES

for (i in Exp_list) {
  nam = paste("temp1_tdst", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 72)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}

for (i in Exp_list) {
  nam = paste("temp2_tdst", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 72)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


# --------------------------------------------------
# VECTORIZE TEMPERATURE MATRICES (BY COLUMN (LATITUDE))

temp1_tdst_k_all = matrix(0, nrow = length(temp1_tdstb_kdat), ncol = 5)
j = 1


for (i in Exp_list) {
  nam = paste("temp1_tdst", i, "_kdat", sep = "")
  nam_new = paste("temp1_tdst", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 72)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  temp1_tdst_k_all[,j] = data_new
  rm(data)
  j = j + 1
}


temp2_tdst_k_all = matrix(0, nrow = length(temp2_tdstb_kdat), ncol = 5)
j = 1


for (i in Exp_list) {
  nam = paste("temp2_tdst", i, "_kdat", sep = "")
  nam_new = paste("temp2_tdst", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 72)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  temp2_tdst_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

rm(list=ls()[grep("_vec",ls())])
rm(list=ls()[grep("kdat",ls())])


# CALCULATE WIND SPEED

temp_tdst_k_all = matrix(0, nrow = length(temp1_tdst_k_all), ncol = 5)

for (i in 1:5) {
	temp_tdst_k_all[,i] = sqrt((temp1_tdst_k_all[,i]^2) + (temp2_tdst_k_all[,i]^2))
}


# EXTRACT VARIABLES FOR EMULATOR (X AND Y)

model_input_tdst = data.matrix(cont_paramdat)

model_output_tdst = temp_tdst_k_all

# and put then on a grid format

model_output_tdst <- array(model_output_tdst, c(96, 72, 5 ))

rm(list=ls()[(ls() %in% c('Exp','Exp_list','cont_paramdat','cont_param_dim','temp1_tdst_k_all','temp2_tdst_k_all','temp_tdst_k_all'))])
