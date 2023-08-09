# CW - Script to clear unnecessary variables, except those listed below, rather than placing each of these lines into Data/dTeq* files

to.remove <- ls()

# New variables
to.remove <- c(to.remove[!grepl("runemulator", to.remove)])
to.remove <- c(to.remove[!grepl("runBC", to.remove)])
to.remove <- c(to.remove[!grepl("runsaving", to.remove)])
to.remove <- c(to.remove[!grepl("runstatsplots", to.remove)])
to.remove <- c(to.remove[!grepl("simnumber", to.remove)])
to.remove <- c(to.remove[!grepl("allvars", to.remove)])
to.remove <- c(to.remove[!grepl("sens3", to.remove)])
to.remove <- c(to.remove[!grepl("savefigs", to.remove)])

# Existing (Nat's) variables
to.remove <- c(to.remove[!grepl("EmulVars", to.remove)])
to.remove <- c(to.remove[!grepl("Sim", to.remove)])
to.remove <- c(to.remove[!grepl("noice", to.remove)])
to.remove <- c(to.remove[!grepl("filepath", to.remove)])
to.remove <- c(to.remove[!grepl("^model_input", to.remove)], "to.remove")
to.remove <- c(to.remove[!grepl("^model_output", to.remove)], "to.remove")
to.remove <- c(to.remove[!grepl("^Exp_num", to.remove)], "to.remove")

rm(list=to.remove)