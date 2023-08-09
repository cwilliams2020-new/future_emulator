# --------------------------------------------------

# ENSEMBLE:

Exp = "Insol_Laskar_jul_65N_1myr_AP"

Exp_list = c(1:1000000,1000)


# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','orig','Input','2018-08-01 Final report','Insol_Laskar_jul_65N_1myr_AP.res'), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)


# EXTRACT VARIABLES FOR EMULATOR (X)

data_Insol_Laskar_jul_65N_1myr_AP = data.matrix(cont_paramdat)


rm(list=ls()[(ls() %in% c('Exp','Exp_list','cont_paramdat','cont_param_dim'))])
