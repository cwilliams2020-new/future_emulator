library(gdata)

allvars=c("Temp", "Precip", "Evapotrans", "SIce", "SM", "SnDepth", "STemp", "Veg", "WSpeed")

for (vars in 1:9){
  
  print(allvars[vars])
  
  EmulVars=(allvars[vars])
  print(EmulVars)
  
  print("Running emulator")
  source("C:/Users/cw18831/OneDrive - University of Bristol/Documents/Research/KAERI/Emulator code/Emulator/2015_Bristol_5D_v001/R/Emulator_all_kaeri_vars_0_1MyrAP_cjrw_v5.R")
  
  print("Clearing environment")
  keep("allvars", "vars", "EmulVars", sure = TRUE)
  
}

print("All finished!")