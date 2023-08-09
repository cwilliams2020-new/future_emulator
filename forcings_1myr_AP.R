loadForcing <- function(x) {
    #    paste("emul_inputs_corrected_RCP26", x,"res",sep = ".")
    
# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','..','..','Results',paste("emul_inputs_natural", x,"res",sep = ".")), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)

# EXTRACT VARIABLES FOR EMULATOR (X)

model_input_zero_emissions_1myr_AP = data.matrix(cont_paramdat)



# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','..','..','Results',paste("emul_inputs_RCP26", x,"res",sep = ".")), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)


# EXTRACT VARIABLES FOR EMULATOR (X)

model_input_rcp26_1myr_AP = data.matrix(cont_paramdat)



# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','..','..','Results',paste("emul_inputs_RCP45", x,"res",sep = ".")), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)


# EXTRACT VARIABLES FOR EMULATOR (X)

model_input_rcp45_1myr_AP = data.matrix(cont_paramdat)


# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','..','..','Results',paste("emul_inputs_RCP85", x,"res",sep = ".")), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)


# EXTRACT VARIABLES FOR EMULATOR (X)

model_input_rcp85_1myr_AP = data.matrix(cont_paramdat)


modelInputs <- list("natural" = model_input_zero_emissions_1myr_AP,"RCP26" = model_input_rcp26_1myr_AP,"RCP45" = model_input_rcp45_1myr_AP,"RCP85" = model_input_rcp85_1myr_AP)
return(modelInputs)
}
