loadRnum <- function(x) {
    
# --------------------------------------------------
# READ IN INPUT DATA

cont_paramdat = read.table(file.path('..','..','..','Results',paste("Rnum_all_scens", x,"res",sep = ".")), sep=" ", header=TRUE)

cont_param_dim=dim(cont_paramdat)


# EXTRACT VARIABLES FOR EMULATOR (X)

data_input_Rnum_rcp_1myr_AP = data.matrix(cont_paramdat)
return(data_input_Rnum_rcp_1myr_AP)

}
