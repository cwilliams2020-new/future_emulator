# --------------------------------------------------

# ENSEMBLE:

Exp = "tdst"

Exp_list = c("b", "c", "d", "e", "f")


# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','orig','Input','Samp_orbits_tdst.res'), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)


convert_data_a = c(48,1,1) # Evap from soil surface and canopy and sublimation from surface (mm timestep-1 (atm ts = 30 mins) to mm day-1) (kg m-2 same as mm)
convert_data_b = c(60,60,24) # Evap from sea and transpiration rate (mm sec-1 to mm day-1) (kg m-2 same as mm)


# READ IN OUTPUT DATA

evap_mm_srf=list((0))

for (i in Exp_list) {
	nc = nc_open(file.path('..','orig','Output', 'Climate NC', paste( Exp, i, "a.pdclann.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	evap_mm_srf = ncvar_get(nc, "evap_mm_srf")
	evap_mm_srf = evap_mm_srf * convert_data_a[1] * convert_data_a[2] * convert_data_a[3]
	nam = paste("evap_mm_srf_tdst", i, "_kdat", sep = "")
	assign(nam, evap_mm_srf)
	nc_close(nc)
	rm(nc, evap_mm_srf)
}


# MAKE MISSING VALUES

for (i in Exp_list) {
  nam = paste("evap_mm_srf_tdst", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


# READ IN OUTPUT DATA

canopyevap_mm_can=list((0))

for (i in Exp_list) {
	nc = nc_open(file.path('..','orig','Output', 'Climate NC', paste( Exp, i, "a.pdclann.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	canopyevap_mm_can = ncvar_get(nc, "canopyEvap_mm_can")
	canopyevap_mm_can = canopyevap_mm_can * convert_data_a[1] * convert_data_a[2] * convert_data_a[3]
	nam = paste("canopyevap_mm_can_tdst", i, "_kdat", sep = "")
	assign(nam, canopyevap_mm_can)
	nc_close(nc)
	rm(nc, canopyevap_mm_can)
}


# MAKE MISSING VALUES

for (i in Exp_list) {
  nam = paste("canopyevap_mm_can_tdst", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


# READ IN OUTPUT DATA

sublim_mm_srf=list((0))

for (i in Exp_list) {
	nc = nc_open(file.path('..','orig','Output', 'Climate NC', paste( Exp, i, "a.pdclann.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	sublim_mm_srf = ncvar_get(nc, "sublim_mm_srf")
	sublim_mm_srf = sublim_mm_srf * convert_data_a[1] * convert_data_a[2] * convert_data_a[3]
	nam = paste("sublim_mm_srf_tdst", i, "_kdat", sep = "")
	assign(nam, sublim_mm_srf)
	nc_close(nc)
	rm(nc, sublim_mm_srf)
}


# MAKE MISSING VALUES

for (i in Exp_list) {
  nam = paste("sublim_mm_srf_tdst", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


# READ IN OUTPUT DATA

evapsea_mm_srf=list((0))

for (i in Exp_list) {
	nc = nc_open(file.path('..','orig','Output', 'Climate NC', paste( Exp, i, "a.pdclann.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	evapsea_mm_srf = ncvar_get(nc, "evapsea_mm_srf")
	evapsea_mm_srf = evapsea_mm_srf * convert_data_b[1] * convert_data_b[2] * convert_data_b[3]
	nam = paste("evapsea_mm_srf_tdst", i, "_kdat", sep = "")
	assign(nam, evapsea_mm_srf)
	nc_close(nc)
	rm(nc, evapsea_mm_srf)
}


# MAKE MISSING VALUES

for (i in Exp_list) {
  nam = paste("evapsea_mm_srf_tdst", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  assign(nam, data)
  rm(data)
}


# READ IN OUTPUT DATA

transpiration_mm_srf=list((0))

for (i in Exp_list) {
	nc = nc_open(file.path('..','orig','Output', 'Climate NC', paste( Exp, i, "a.pdclann.nc", sep="")))
	lats_dat = ncvar_get(nc, "latitude")
	lons_dat = ncvar_get(nc, "longitude")
	
	transpiration_mm_srf = ncvar_get(nc, "transpiration_mm_srf")
	transpiration_mm_srf = transpiration_mm_srf * convert_data_b[1] * convert_data_b[2] * convert_data_b[3]
	nam = paste("transpiration_mm_srf_tdst", i, "_kdat", sep = "")
	assign(nam, transpiration_mm_srf)
	nc_close(nc)
	rm(nc, transpiration_mm_srf)
}


# MAKE MISSING VALUES

for (i in Exp_list) {
  nam = paste("transpiration_mm_srf_tdst", i, "_kdat", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data = replace(data, data == 2.00000004008175e+20, NA)
  data = replace(data, data == NA, 0)
  assign(nam, data)
  rm(data)
}


# --------------------------------------------------
# CALCULATE EVAPOTRANSPIRATION

for (i in Exp_list) {
  nam1a = paste("evap_mm_srf_tdst", i, "_kdat", sep = "")
  nam1b = paste("canopyevap_mm_can_tdst", i, "_kdat", sep = "")
  nam1c = paste("sublim_mm_srf_tdst", i, "_kdat", sep = "")
  nam1d = paste("evapsea_mm_srf_tdst", i, "_kdat", sep = "")
  nam1e = paste("transpiration_mm_srf_tdst", i, "_kdat", sep = "")
  nam2 = paste("evapotrans_tdst", i, "_kdat", sep = "")
  data1a = matrix(data=eval(parse(text=nam1a)), ncol = 73)
  data1b = matrix(data=eval(parse(text=nam1b)), ncol = 73)
  data1c = matrix(data=eval(parse(text=nam1c)), ncol = 73)
  data1d = matrix(data=eval(parse(text=nam1d)), ncol = 73)
  data1e = matrix(data=eval(parse(text=nam1e)), ncol = 73)
  
  data1a[is.na(data1a) == TRUE] = 0
  data1b[is.na(data1b) == TRUE] = 0
  data1c[is.na(data1c) == TRUE] = 0
  data1d[is.na(data1d) == TRUE] = 0
  data1e[is.na(data1e) == TRUE] = 0
  
  data2 = data1a + data1b + data1c + data1d
  assign(nam2, data2)
  rm(data1a, data1b, data1c, data1d, data1e, data2)
}


# --------------------------------------------------
# VECTORIZE EVAPOTRANSPIRATION MATRICES (BY COLUMN (LATITUDE))

evapotrans_tdst_k_all = matrix(0, nrow = length(evapotrans_tdstb_kdat), ncol = 5)
j = 1


for (i in Exp_list) {
  nam = paste("evapotrans_tdst", i, "_kdat", sep = "")
  nam_new = paste("evapotrans_tdst", i, "_k_vec", sep = "")
  data = matrix(data=eval(parse(text=nam)), ncol = 73)
  data_new = as.vector(data)
  assign(nam_new, data_new)
  evapotrans_tdst_k_all[,j] = data_new
  rm(data)
  j = j + 1
}

rm(list=ls()[grep("_vec",ls())])
rm(list=ls()[grep("kdat",ls())])


# EXTRACT VARIABLES FOR EMULATOR (X AND Y)

model_input_tdst = data.matrix(cont_paramdat)

model_output_tdst = evapotrans_tdst_k_all

# and put then on a grid format

model_output_tdst <- array(model_output_tdst, c(96, 73, 5 ))

rm(list=ls()[(ls() %in% c('Exp','Exp_list','cont_paramdat','cont_param_dim','evapotrans_tdst_k_all'))])
