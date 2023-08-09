#########################################################################################
## VALIDATE EMULATOR ####################################################################
## Calibrated final using temp_mm_srf ###################################################
#########################################################################################

## Set up ###############################################################################

# Read Data supplied by Natalie

#load('../Data/.RData')


#K=273.15


# load resources (see superseded versions for code to install packages)
require(gsl)

require(Hmisc)

require(GP)

require(colorspace)

require(graphics)

library(fields)

source('pca_emul.R')

par_orig=par()

# Leave-one-out ###########################################################

# Create a design and output minus one experiment each time

state="gl"
state_num = 1 # gl
model_input = model_input_modice_tdab # gl
ne=dim(model_input_modice_tdab)[1] # gl
X = X_gl # gl
Y = Y_gl # gl
hp = hp_gl # gl
nkeep = nkeep_gl # gl
  
# state="ig" # ig
# state_num = 2 # ig
# model_input = model_input_modlowice # ig
# ne=dim(model_input_modlowice)[1] # ig
# X = X_ig # ig
# Y = Y_ig # ig
# hp = hp_ig # ig
# nkeep = nkeep_ig # ig


exp_num_L1o=c(seq(ne))

Emul_L1o=vector("list", ne)
Emulated_Y_L1o=vector("list", ne)
DF=array(0,dim=c(1,ne))
DS_mean=array(0,dim=c(1,ne))
DS=vector("list", ne)
L_man=array(0, dim=c(4,ne))
valid_score_man=array(0,1)
perc_over_2sf=array(0,1)
RMSE=array(0,dim=c(1,ne))
RMSE_average=array(0,1)
RMSE_median=array(0,1)

n=0

for (i in exp_num_L1o){
  X_L1o <- X[-i, ]
  Y_L1o <- Y[,,-i]
  
  n=n+1
  Emul_L1o[[n]] <-  pe_c (X_L1o, Y_L1o, mypca, hp=hp, nkeep=nkeep)

# Emulate the missing experiments with the relevant emulator

  Emulated_Y_L1o[[n]] <- pe_p(t(X[i, ]), Emul_L1o[[n]] )

# Calculate the standardized error and GB prediction scores
  
  DF[,n]   = sqrt ( mean  (Emulated_Y_L1o[[n]]$mean - Y[,,i] , na.rm = TRUE)^2 )
  DS_mean[,n] = mean(sqrt(Emulated_Y_L1o[[n]]$var))
  DS_mean_mean=mean(DS_mean)
  DS[[n]] = abs ( Emulated_Y_L1o[[n]]$mean - Y[,,i] ) / sqrt(Emulated_Y_L1o[[n]]$var) # Calculates number of standard deviations
  q1 <- length ( which(DS[[n]] < 1))
  q2 <- length ( which(DS[[n]] < 2)) - q1
  q3 <- length ( which(DS[[n]] < 3)) - q2 - q1
  q4 <- length ( which(DS[[n]] < 99999.)) - q3 - q2 - q1 
  L_man[,n]=array(c(q1,q2,q3,q4))
  
  RMSE[,n]=sqrt((sum((Emulated_Y_L1o[[n]]$mean-Y[,,i])^2))/7008)
  
}

valid_score_man=sum(L_man[1,]>5000)
perc_over_2sf=((sum(L_man[3,])+ sum(L_man[4,]))/sum(L_man))*100
RMSE_average=sum(RMSE)/ne
RMSE_median=median(RMSE)

# Calculate percentages of grid boxes with standard deviation bands

L_man_bands=matrix(0,nrow=6,ncol=ne+1)
rownames(L_man_bands, do.NULL = TRUE, prefix = "col")
rownames(L_man_bands)=c("<1sd","1-2sd","2-3sd",">3sd","sum","<2sd")

L_man_bands[1,1:ne]=(L_man[1,]/7008)*100
L_man_bands[2,1:ne]=(L_man[2,]/7008)*100
L_man_bands[3,1:ne]=(L_man[3,]/7008)*100
L_man_bands[4,1:ne]=(L_man[4,]/7008)*100
L_man_bands[5,1:ne]=colSums(L_man_bands[1:4,1:ne])
L_man_bands[6,1:ne]=colSums(L_man_bands[1:2,1:ne])
L_man_bands[,ne+1]=(rowSums(L_man_bands[,1:ne]))/ne

# Plot GB prediction scores
par(mar=par_orig$mar, pty="m")

layout(matrix(c(1, 2),2,2,byrow=TRUE), widths=c(4,1), heights=c(2), TRUE)
#layout.show(2)
par(mar=c(4,4,1.5,0))
barplot (L_man, names.arg=exp_num_L1o, xlab=c("Exp"),ylab=c("# of GBs"))
par(mar=c(4,0,1.5,0.5))
plot(0,0,pch=".",xlim=c(0:1),ylim=c(0:1),xaxt="n",yaxt="n",xlab="",ylab="", frame=FALSE)
text(0.3,1,sprintf("CO2"),pos=4,font=2)
text(0.0,0.8,sprintf("Leave-one-out \n analysis for \n each experiment"),pos=4,font=2)
#text(0.0,0.6,sprintf("Median RMSE= %.6f",RMSE_median),pos=4)
text(0.0,0.6,sprintf("# of PCs= %.0f",nkeep),pos=4)
text(0.0,0.4,sprintf("Mean RMSE= %.6f",RMSE_average),pos=4)
text(0.0,0.2,sprintf("Perc of total GBs \n >2 st dev= %.2f",perc_over_2sf),pos=4)


## Plot observed vs emulated SAT global means (anom compared to tdstb) for each left-out experiment

Y_mean = array(0,dim=c(1,ne))
#Emulated_Y_L1o_mean_big_mean = array(0,ne)
Emulated_Y_L1o_mean_big_mean = array(0,dim=c(1,ne))


for (i in exp_num_L1o) {
  Y_mean[i]=mean(Y[,,i])
  Emulated_Y_L1o_mean_big_mean[i]=mean(Emulated_Y_L1o[[i]]$mean)
}

layout(matrix(1,1,1,byrow=TRUE), widths=4, heights=4, TRUE)
par(mar=par_orig$mar, pty="s")
plot(Y_mean,Emulated_Y_L1o_mean_big_mean,xlim=c(-1,15),ylim=c(-1,15),xlab="Modelled mean (degC)",ylab="Emulated mean (degC)") # Can't just add error bars of variance as can't average for globe


plot(Y_mean,Emulated_Y_L1o_mean_big_mean,xlim=c(-1,15),ylim=c(-1,15),xlab="Modelled mean (degC)",ylab="Emulated mean (degC)",col="white") # Can't just add error bars of variance as can't average for globe
lines(c(-1,15),c(-1,15), col="grey")
text(Y_mean, Emulated_Y_L1o_mean_big_mean,sprintf("%.0f",seq(ne)))


## Plot observed SAT global mean (anom compared to tdstb) vs input variables for each left-out experiment

# layout(matrix(1,1,1,byrow=TRUE), widths=4, heights=4, TRUE)
# par(mar=par_orig$mar, pty="s")
# plot(model_input[1:ne_tdum,1],Y_mean[1,1:ne_tdum],xlim=c(5.5,7.6),pch=1,ylim=c(-1,15),xlab="CO2",ylab="Modelled mean (degC)") # Can't just add error bars of variance as can't average for globe
# points(model_input[ne_tdum+1:ne_tdvo,1],Y_mean[1,ne_tdum+1:ne_tdvo],pch=2) # Can't just add error bars of variance as can't average for globe
# 
# layout(matrix(1,1,1,byrow=TRUE), widths=4, heights=4, TRUE)
# par(mar=par_orig$mar, pty="s")
# plot(model_input[1:ne_tdum,2],Y_mean[1,1:ne_tdum],xlim=c(22,24.5),pch=1,ylim=c(-1,15),xlab="Obl",ylab="Modelled mean (degC)") # Can't just add error bars of variance as can't average for globe
# points(model_input[ne_tdum+1:ne_tdvo,2],Y_mean[1,ne_tdum+1:ne_tdvo],pch=2) # Can't just add error bars of variance as can't average for globe
# 
# layout(matrix(1,1,1,byrow=TRUE), widths=4, heights=4, TRUE)
# par(mar=par_orig$mar, pty="s")
# plot(model_input[1:ne_tdum,3],Y_mean[1,1:ne_tdum],xlim=c(-0.055,0.055),pch=1,ylim=c(-1,15),xlab="esinw",ylab="Modelled mean (degC)") # Can't just add error bars of variance as can't average for globe
# points(model_input[ne_tdum+1:ne_tdvo,3],Y_mean[1,ne_tdum+1:ne_tdvo],pch=2) # Can't just add error bars of variance as can't average for globe
# 
# layout(matrix(1,1,1,byrow=TRUE), widths=4, heights=4, TRUE)
# par(mar=par_orig$mar, pty="s")
# plot(model_input[1:ne_tdum,4],Y_mean[1,1:ne_tdum],xlim=c(-0.055,0.055),pch=1,ylim=c(-1,15),xlab="ecosw",ylab="Modelled mean (degC)") # Can't just add error bars of variance as can't average for globe
# points(model_input[ne_tdum+1:ne_tdvo,4],Y_mean[1,ne_tdum+1:ne_tdvo],pch=2) # Can't just add error bars of variance as can't average for globe
# 
# layout(matrix(1,1,1,byrow=TRUE), widths=4, heights=4, TRUE)
# par(mar=par_orig$mar, pty="s")
# plot(model_input[1:ne_tdum,5],Y_mean[1,1:ne_tdum],xlim=c(-0.1,1.1),pch=1,ylim=c(-1,15),xlab="ice",ylab="Modelled mean (degC)") # Can't just add error bars of variance as can't average for globe
# points(model_input[ne_tdum+1:ne_tdvo,5],Y_mean[1,ne_tdum+1:ne_tdvo],pch=2) # Can't just add error bars of variance as can't average for globe



# Use pythag. to calculate eccentricity value (hypotenuse)

model_input_ep=model_input
colnames(model_input_ep, do.NULL = TRUE, prefix = "col")
colnames(model_input_ep)=c("co2","obliquity","eccentricity","l. of perihelion","ice")

for (i in exp_num_L1o) {
  model_input_ep[i,3]=sqrt((model_input[i,3]^2) + (model_input[i,4]^2)) # Calculate Ecc
  model_input_ep[i,4]=atan2(model_input[i,3]*pi/180,model_input[i,4]*pi/180) # Calculate Per
  model_input_ep[i,4]=model_input_ep[i,4]*180/pi
}

for (i in exp_num_L1o) {
  if (model_input_ep[i,4] < 0) {
    model_input_ep[i,4] = model_input_ep[i,4] + 360; # Correct Per to be between 0 and 360 (from -180 to 180)
  }
}


# Plot input parameters vs RMSE

# par(mfrow=c(2,3), mar=c(4,5,1.5,1.5), pty="m")
# 
# plot(model_input[1:ne_tdum,1],RMSE[1,1:ne_tdum],pch=1,xlim=c(5.5,7.6),ylim=c(0.1,0.9),xlab="CO2")
# points(model_input[ne_tdum+1:ne_tdvo,1],RMSE[1,ne_tdum+1:ne_tdvo],pch=2)
# text(model_input[37,1], RMSE[,37]+0.05,sprintf("%.0f",37))
# text(model_input[43,1], RMSE[,43]+0.05,sprintf("%.0f",43))
# text(model_input[45,1], RMSE[,45]+0.05,sprintf("%.0f",45))
# text(model_input[46,1], RMSE[,46]+0.05,sprintf("%.0f",46))
# text(model_input[57,1], RMSE[,57]+0.05,sprintf("%.0f",57))
# 
# plot(model_input[1:ne_tdum,2],RMSE[1,1:ne_tdum],pch=1,xlim=c(22,24.5),ylim=c(0.1,0.9),xlab="Obliquity (deg)")
# points(model_input[ne_tdum+1:ne_tdvo,2],RMSE[1,ne_tdum+1:ne_tdvo],pch=2)
# text(model_input[37,2], RMSE[,37]+0.05,sprintf("%.0f",37))
# text(model_input[43,2], RMSE[,43]+0.05,sprintf("%.0f",43))
# text(model_input[45,2], RMSE[,45]+0.05,sprintf("%.0f",45))
# text(model_input[46,2], RMSE[,46]+0.05,sprintf("%.0f",46))
# text(model_input[57,2], RMSE[,57]+0.05,sprintf("%.0f",57))
# 
# plot(model_input[1:ne_tdum,3],RMSE[1,1:ne_tdum],pch=1,xlim=c(-0.055,0.055),ylim=c(0.1,0.9),xlab="esinw")
# points(model_input[ne_tdum+1:ne_tdvo,3],RMSE[1,ne_tdum+1:ne_tdvo],pch=2)
# text(model_input[37,3], RMSE[,37]+0.05,sprintf("%.0f",37))
# text(model_input[43,3], RMSE[,43]+0.05,sprintf("%.0f",43))
# text(model_input[45,3], RMSE[,45]+0.05,sprintf("%.0f",45))
# text(model_input[46,3], RMSE[,46]+0.05,sprintf("%.0f",46))
# text(model_input[57,3], RMSE[,57]+0.05,sprintf("%.0f",57))
# 
# plot(model_input[1:ne_tdum,4],RMSE[1,1:ne_tdum],pch=1,xlim=c(-0.055,0.055),ylim=c(0.1,0.9),xlab="ecosw")
# points(model_input[ne_tdum+1:ne_tdvo,4],RMSE[1,ne_tdum+1:ne_tdvo],pch=2)
# text(model_input[37,4], RMSE[,37]+0.05,sprintf("%.0f",37))
# text(model_input[43,4], RMSE[,43]+0.05,sprintf("%.0f",43))
# text(model_input[45,4], RMSE[,45]+0.05,sprintf("%.0f",45))
# text(model_input[46,4], RMSE[,46]+0.05,sprintf("%.0f",46))
# text(model_input[57,4], RMSE[,57]+0.05,sprintf("%.0f",57))
# 
# plot(model_input[1:ne_tdum,5],RMSE[1,1:ne_tdum],pch=1,xlim=c(-0.1,1.1),ylim=c(0.1,0.9),xlab="ecosw")
# points(model_input[ne_tdum+1:ne_tdvo,5],RMSE[1,ne_tdum+1:ne_tdvo],pch=2)
# text(model_input[37,5], RMSE[,37]+0.05,sprintf("%.0f",37))
# text(model_input[43,5], RMSE[,43]+0.05,sprintf("%.0f",43))
# text(model_input[45,5], RMSE[,45]+0.05,sprintf("%.0f",45))
# text(model_input[46,5], RMSE[,46]+0.05,sprintf("%.0f",46))
# text(model_input[57,5], RMSE[,57]+0.05,sprintf("%.0f",57))

# 
# par(mfrow=c(2,3), mar=c(4,5,1.5,1.5), pty="m")
# 
# plot(model_input_ep[1:ne_tdum,1],RMSE[1,1:ne_tdum],pch=1,xlim=c(5.5,7.6),ylim=c(0.1,0.9),xlab="CO2", ylab="RMSE")
# points(model_input_ep[ne_tdum+1:ne_tdvo,1],RMSE[1,ne_tdum+1:ne_tdvo],pch=2)
# text(model_input_ep[43,1], RMSE[,43]+0.05,sprintf("%.0f",43))
# text(model_input_ep[45,1], RMSE[,45]+0.05,sprintf("%.0f",45))
# text(model_input_ep[46,1], RMSE[,46]+0.05,sprintf("%.0f",46))
# text(model_input_ep[47,1], RMSE[,47]+0.05,sprintf("%.0f",47))
# text(model_input_ep[48,1], RMSE[,48]+0.05,sprintf("%.0f",48))
# text(model_input_ep[50,1], RMSE[,50]+0.05,sprintf("%.0f",50))
# text(model_input_ep[53,1], RMSE[,53]+0.05,sprintf("%.0f",53))
# text(model_input_ep[57,1], RMSE[,57]+0.05,sprintf("%.0f",57))
# 
# plot(model_input_ep[1:ne_tdum,2],RMSE[1,1:ne_tdum],pch=1,xlim=c(22,24.5),ylim=c(0.1,0.9),xlab="Obliquity (deg)", ylab="RMSE")
# points(model_input_ep[ne_tdum+1:ne_tdvo,2],RMSE[1,ne_tdum+1:ne_tdvo],pch=2)
# text(model_input_ep[43,2], RMSE[,43]+0.05,sprintf("%.0f",43))
# text(model_input_ep[45,2], RMSE[,45]+0.05,sprintf("%.0f",45))
# text(model_input_ep[46,2], RMSE[,46]+0.05,sprintf("%.0f",46))
# text(model_input_ep[47,2], RMSE[,47]+0.05,sprintf("%.0f",47))
# text(model_input_ep[48,2], RMSE[,48]+0.05,sprintf("%.0f",48))
# text(model_input_ep[50,2], RMSE[,50]+0.05,sprintf("%.0f",50))
# text(model_input_ep[53,2], RMSE[,53]+0.05,sprintf("%.0f",53))
# text(model_input_ep[57,2], RMSE[,57]+0.05,sprintf("%.0f",57))
# 
# plot(model_input_ep[1:ne_tdum,3],RMSE[1,1:ne_tdum],pch=1,xlim=c(0,0.055),ylim=c(0.1,0.9),xlab="eccentricity", ylab="RMSE")
# points(model_input_ep[ne_tdum+1:ne_tdvo,3],RMSE[1,ne_tdum+1:ne_tdvo],pch=2)
# text(model_input_ep[43,3], RMSE[,43]+0.05,sprintf("%.0f",43))
# text(model_input_ep[45,3], RMSE[,45]+0.05,sprintf("%.0f",45))
# text(model_input_ep[46,3], RMSE[,46]+0.05,sprintf("%.0f",46))
# text(model_input_ep[47,3], RMSE[,47]+0.05,sprintf("%.0f",47))
# text(model_input_ep[48,3], RMSE[,48]+0.05,sprintf("%.0f",48))
# text(model_input_ep[50,3], RMSE[,50]+0.05,sprintf("%.0f",50))
# text(model_input_ep[53,3], RMSE[,53]+0.05,sprintf("%.0f",53))
# text(model_input_ep[57,3], RMSE[,57]+0.05,sprintf("%.0f",57))
# 
# plot(model_input_ep[1:ne_tdum,4],RMSE[1,1:ne_tdum],pch=1,xlim=c(0,360),ylim=c(0.1,0.9),xlab="longitude of perihelion", ylab="RMSE")
# points(model_input_ep[ne_tdum+1:ne_tdvo,4],RMSE[1,ne_tdum+1:ne_tdvo],pch=2)
# text(model_input_ep[43,4], RMSE[,43]+0.05,sprintf("%.0f",43))
# text(model_input_ep[45,4], RMSE[,45]+0.05,sprintf("%.0f",45))
# text(model_input_ep[46,4], RMSE[,46]+0.05,sprintf("%.0f",46))
# text(model_input_ep[47,4], RMSE[,47]+0.05,sprintf("%.0f",47))
# text(model_input_ep[48,4], RMSE[,48]+0.05,sprintf("%.0f",48))
# text(model_input_ep[50,4], RMSE[,50]+0.05,sprintf("%.0f",50))
# text(model_input_ep[53,4], RMSE[,53]+0.05,sprintf("%.0f",53))
# text(model_input_ep[57,4], RMSE[,57]+0.05,sprintf("%.0f",57))
# 
# plot(model_input_ep[1:ne_tdum,5],RMSE[1,1:ne_tdum],pch=1,xlim=c(-0.01,1.1),ylim=c(0.1,0.9),xlab="longitude of perihelion", ylab="RMSE")
# points(model_input_ep[ne_tdum+1:ne_tdvo,5],RMSE[1,ne_tdum+1:ne_tdvo],pch=2)
# text(model_input_ep[43,5], RMSE[,43]+0.05,sprintf("%.0f",43))
# text(model_input_ep[45,5], RMSE[,45]+0.05,sprintf("%.0f",45))
# text(model_input_ep[46,5], RMSE[,46]+0.05,sprintf("%.0f",46))
# text(model_input_ep[47,5], RMSE[,47]+0.05,sprintf("%.0f",47))
# text(model_input_ep[48,5], RMSE[,48]+0.05,sprintf("%.0f",48))
# text(model_input_ep[50,5], RMSE[,50]+0.05,sprintf("%.0f",50))
# text(model_input_ep[53,5], RMSE[,53]+0.05,sprintf("%.0f",53))
# text(model_input_ep[57,5], RMSE[,57]+0.05,sprintf("%.0f",57))



#plot(model_input[,1],RMSE,xlim=c(260,560),ylim=c(0.1,0.6),xlab="CO2",col="white")
#text(model_input[,1], RMSE,sprintf("%.0f",seq(ne)))
#plot(model_input[,1],RMSE,xlim=c(5.5,7.6),ylim=c(0.1,0.6),xlab="CO2",col="white")
#plot(model_input[,2],RMSE,xlim=c(22,24.5),ylim=c(0.1,0.6),xlab="Obliquity (deg)",col="white")
#text(model_input[,2], RMSE,sprintf("%.0f",seq(ne)))
#plot(model_input[,3],RMSE,xlim=c(-0.055,0.055),ylim=c(0.1,0.6),xlab="esinw",col="white")
#text(model_input[,3], RMSE,sprintf("%.0f",seq(ne)))
#plot(model_input[,4],RMSE,xlim=c(-0.055,0.055),ylim=c(0.1,0.6),xlab="ecosw",col="white")
#text(model_input[,4], RMSE,sprintf("%.0f",seq(ne)))


# Convert data to correct format for mapping

nx = 96 # lon
ny = 73 # lat
nx2 = nx/2
ne=ne

#lons <- array(0:(96-1))*3.75
lats <- array(0:(73-1))*2.5-90

lons <- array(0:(nx-1))*3.75
lons[1:(nx+1)] <- c(lons[(nx2+1):nx]-360,lons[1:nx2],lons[nx2+1])

Emulated_Y_L1o_mean_big<-lapply(1:ne, function(x) matrix(0, nrow=(nx+1), ncol=ny))
for (i in exp_num_L1o) {
  for (x in c(1:ny)){
    Emulated_Y_L1o_mean_big[[i]][1:(nx+1),x] = c(Emulated_Y_L1o[[i]]$mean[(nx2+1):nx,x],Emulated_Y_L1o[[i]]$mean[1:nx2,x],Emulated_Y_L1o[[i]]$mean[(nx2+1),x])
  }
}

Y_big<-array(0, c((nx+1),ny,ne))
for (i in exp_num_L1o) {
  for (x in c(1:ny)){
    Y_big[1:(nx+1),x,i] = c(Y[(nx2+1):nx,x,i],Y[1:nx2,x,i],Y[(nx2+1),x,i])
  }
}

DS_big<-lapply(1:ne, function(x) matrix(0, nrow=(nx+1), ncol=ny))
for (i in exp_num_L1o) {
  for (x in c(1:ny)){
    DS_big[[i]][1:(nx+1),x] = c(DS[[i]][(nx2+1):nx,x],DS[[i]][1:nx2,x],DS[[i]][(nx2+1),x])
  }
}


par(mfrow=c(3,3), mar=c(4,5,1.5,1.5), pty="m")
par(mfrow=c(3,3), mar=c(4,5,1.5,1.5), pty="m")

# plot global emulated SAT (anom compared to tdstb)

#for (i in exp_num_L1o) {
#  image.plot(lons, lats, Emulated_Y_L1o_mean_big[[i]][,c(73:1)], zlim=c(-35,35))
#  map(add=T, interior=F, col="black")
#}  

# plot global SAT anomaly (emulated-modelled)
# 
col=palette(diverge_hsv(20))
col=palette(diverge_hsv(20))

par(mfrow=c(3,3), mar=c(4,5,1.5,1.5), pty="m")
par(mfrow=c(3,3), mar=c(4,5,1.5,1.5), pty="m")

#for (i in exp_num_L1o) {
 #image.plot(lons, lats, Emulated_Y_L1o_mean_big[[i]][,c(73:1)]-Y_big[,c(73:1),i], zlim=c(-4.5,4.5),col=col,main=exp_num_L1o[i])
 #map(add=T, interior=F, col="black")
#}

# plot global standardized error in emulated SAT (legend intervals = standard deviations)
# 
# par(mfrow=c(3,3), mar=c(4,5,1.5,1.5), pty="m")
# par(mfrow=c(3,3), mar=c(4,5,1.5,1.5), pty="m")
# 
cols=c("red","red","blue","green","blue","red","red")
#for (i in exp_num_L1o) {
#image(lons, lats, DS_big[[i]][,c(73:1)], zlim=c(-4,4), nlevel=7, breaks=c(-4,-3,-2,-1,1,2,3,4),
           #col=cols, axis.args=list(at=seq(-4, 4, 1),labels=seq(-4, 4, 1)),main=exp_num_L1o[i])
#map(add=T, interior=F, col="black")
#}
# 
# for (i in 1) {
#  image.plot(lons, lats, DS_big[[i]][,c(73:1)], zlim=c(-4,4), nlevel=7, breaks=c(-4,-3,-2,-1,1,2,3,4),
#             col=cols, axis.args=list(at=seq(-4, 4, 1),labels=seq(-4, 4, 1)),main=exp_num_L1o[i],horizontal = TRUE)
#  map(add=T, interior=F, col="black")
# }

# plot indvidual PC scores

par(mfrow=c(3,3))

for (i in exp_num_L1o) {
  with(Emulated_Y_L1o[[i]], errbar (seq(nkeep), means, means-sqrt(variances), means+sqrt(variances), 
                           xlab='PC number', ylab  = 'Score',ylim=c(-0.5,0.5))) # The earlier PCs are easier to capture, so most of the useful information is likely contained in the first 5 PCs
lines(c(0,20),c(0,0), col="grey") # But calibrating the model further may make the later PCs more useful
}

# Validate each emulator on the individual pc scores

PCA = mypca(Y, nkeep)

l1o_pc_component <- function(i)
{
  Amp <- PCA$amps[,i] # Score for eg 1st PC
  L1O_Amp <- L1O ( X, Amp, lambda = list ( theta= hp[1:4,i], nugget = hp[5,i]) )
  with(L1O_Amp, errbar(Yi, mean, mean-sd, mean+sd,xlab="Supplied output mean",ylab="GP output mean",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5)))
  lines(c(-2,2), c(-2,2), lty=2)
}

#par(mfrow=c(2,2))
#for (i in seq(4)) l1o_pc_component(i)
#for (i in seq(5,8)) l1o_pc_component(i)
#for (i in seq(9,nkeep)) l1o_pc_component(i)


{# pca_L_man=array(0, dim=c(4,40))
# pca_RMSE=array(0,dim=c(1,40))
# 
# n=40
# 
# for (i in 1:n){
#   Amp <- PCA$amps[,i] # Score for eg 1st PC
#   L1O_Amp <- L1O ( X, Amp, lambda = list ( theta= hp[1:4,13], nugget = hp[5,13]) )
#   
#   # Calculate the standardized error and GB prediction scores
#   
#   pca_q1 <- length ( which(L1O_Amp$sd < 0.05))
#   pca_q2 <- length ( which(L1O_Amp$sd < 0.1)) - pca_q1
#   pca_q3 <- length ( which(L1O_Amp$sd < 0.15)) - pca_q2 - pca_q1
#   pca_q4 <- length ( which(L1O_Amp$sd < 99999.)) - pca_q3 - pca_q2 - pca_q1 
#   pca_L_man[,i]=array(c(pca_q1,pca_q2,pca_q3,pca_q4))
#   pca_L_man[,i][pca_L_man[,i]<0]=0
#   
#   pca_RMSE[,i]=sqrt((sum((L1O_Amp$mean-L1O_Amp$Yi)^2))/ne)
#   
# }
# 
# # Calculate percentages of grid boxes with standard deviation bands
# 
# pca_L_man_bands=matrix(0,nrow=6,ncol=n+1)
# rownames(pca_L_man_bands, do.NULL = TRUE, prefix = "col")
# rownames(pca_L_man_bands)=c("<1sd","1-2sd","2-3sd",">3sd","sum","<2sd")
# 
# pca_L_man_bands[1,1:n]=(pca_L_man[1,]/60)*100
# pca_L_man_bands[2,1:n]=(pca_L_man[2,]/60)*100
# pca_L_man_bands[3,1:n]=(pca_L_man[3,]/60)*100
# pca_L_man_bands[4,1:n]=(pca_L_man[4,]/60)*100
# pca_L_man_bands[5,1:n]=colSums(pca_L_man_bands[1:4,1:ne])
# pca_L_man_bands[6,1:n]=colSums(pca_L_man_bands[1:2,1:ne])
# pca_L_man_bands[,n+1]=(rowSums(pca_L_man_bands[,1:n]))/ne
# 
# # Plot GB prediction scores
# par(mar=par_orig$mar, pty="m")
# 
# layout(matrix(c(1, 2),2,2,byrow=TRUE), widths=c(4,1), heights=c(2), TRUE)
# #layout.show(2)
# par(mar=c(4,4,1.5,0))
# barplot (L_man, names.arg=exp_num_L1o, xlab=c("Exp"),ylab=c("# of GBs"))
# par(mar=c(4,0,1.5,0.5))
# plot(0,0,pch=".",xlim=c(0:1),ylim=c(0:1),xaxt="n",yaxt="n",xlab="",ylab="", frame=FALSE)
# text(0.3,1,sprintf("CO2"),pos=4,font=2)
# text(0.0,0.8,sprintf("Leave-one-out \n analysis for \n each experiment"),pos=4,font=2)
# #text(0.0,0.6,sprintf("Median RMSE= %.6f",RMSE_median),pos=4)
# text(0.0,0.6,sprintf("# of PCs= %.0f",nkeep),pos=4)
# text(0.0,0.4,sprintf("Mean RMSE= %.6f",RMSE_average),pos=4)
# text(0.0,0.2,sprintf("Perc of total GBs \n >2 st dev= %.2f",perc_over_2sf),pos=4)
}


# # Plot variance explained by each PC
# 
# par(mfrow=c(3,3))
# 
# var_sum=array(0,ne)
# var_perc=array(0,dim=c(ne-1,ne))
# var_sum=array(0,ne)
# 
# for (i in exp_num_L1o) {
#  var_sum[i]=array(sum(Emul_L1o[[i]]$L$d))
#  var_perc[,i]=array((Emul_L1o[[i]]$L$d/var_sum[i])*100)
#  var_perc_nkeep[i]=array(sum(var_perc[1:nkeep,i]))
#  
#  barplot(var_perc[1:nkeep,i], names.arg=exp_num_L1o[1:nkeep], ylim=c(0,50), xlab=c("PC #"), ylab=c("% of variance explained"))
#  text(5,40,sprintf("nkeep=%.0f \n\n Var expl=%.3f%%",nkeep,var_perc_nkeep[i]),pos=4)
# }


## FOR POSIVA/SKB REPORT ###################################################################

# Plot GB prediction scores

fs=1.5 # Size of font
fs2=1.2 # Size of font
fs3=2 # Size of font
lns=2 # Width of lines
plns=1.5 # Width of lines
pts=1.5 # Size of points

lb=0.01 # Left border
rb=1.0 # Right border
bb=0.1 # Bottom border
ht=(1-bb)/1 # Height of panel

plot_percentile_length=c(146.5,144) # Red line length
plot_percentile_label = c(146,143.5) # Label position
plot_RMSE_dummy = c(125.5,123.25) # Dummy point position
plot_RMSE_label = c(139.5,137.25) # Label position
plot_label = c("(c)","(a)")
plot_label_position = c(-12,-11.7)
legend_position = c(33.5,33)

file_nam = paste("C:\\Users\\nl6806\\OneDrive - University of Bristol\\PostDoc\\2017-02-15 Posiva + SKB\\5. Output\\Plots\\2018-08-01 Final report\\Fig10_Bpl_performance_",state,"_5D.png", sep="")
png(file=file_nam,width=1000,height=500)
par(mar=c(5,4.5,1,5), xpd=TRUE, pty="m", mgp=c(2,0.5,0), las=1)
perc.bar=barplot (L_man_bands[1:4,1:ne], names.arg=exp_num_L1o, xlab=NA,ylab=NA, cex.axis=fs, cex.names=fs,
                  col=c("grey","blue","yellow","dark red"))
legend(legend_position[state_num], -9,  c("<1 SD", "1-2 SD","2-3 SD",">3 SD"), fill=c("grey","blue","yellow","dark red"), horiz = TRUE, bty="n", cex=fs)
lines(c(0,plot_percentile_length[state_num]),c(68,68),col="red", lwd=lns)
lines(c(0,plot_percentile_length[state_num]),c(95,95),col="red", lwd=lns)
points(perc.bar,RMSE*50,pch=16, cex=pts, lwd=plns)
points(plot_RMSE_dummy[state_num],3,pch=16, cex=pts, lwd=plns) # dummy point for legend
axis(4, at=seq(0,100,10),labels=seq(0,1,0.1), tck=0.01, cex.axis=fs)
par(las=0, xpd=TRUE,lwd=lns)
mtext("Experiment number", side=1, line=2, font=1, cex=fs, col="black")
mtext("Percentage", side=2, line=2.5, font=1, cex=fs, col="black")
mtext("RMSE", side = 4, line = 2.5, font=1, cex=fs, col="black")
#legend(61, 13, "RMSE", pch=16, cex=fs, pt.cex=pts, col="black", bty="n")
par(las=1, xpd=TRUE,lwd=lns)
text(plot_percentile_label[state_num], 93, labels="95 %", cex=fs, font=2, col="red", adj=1)
text(plot_percentile_label[state_num], 66, labels="68 %", cex=fs, font=2, col="red", adj=1)
text(plot_RMSE_label[state_num], 3, labels="RMSE", cex=fs, col="black", adj=1)
text(plot_label_position[state_num], (100*0.96), labels=plot_label[state_num], font=2, cex=fs3, col="black", adj=1)
dev.off()


# PDF

file_nam = paste("C:\\Users\\nl6806\\OneDrive - University of Bristol\\PostDoc\\2017-02-15 Posiva + SKB\\5. Output\\Plots\\2018-08-01 Final report\\Fig10_Bpl_performance_",state,"_5D.pdf", sep="")
pdf(file=file_nam,width=14,height=7.5)
par(mar=c(5,4.5,1,5), xpd=TRUE, pty="m", mgp=c(2,0.5,0), las=1)
perc.bar=barplot (L_man_bands[1:4,1:ne], names.arg=exp_num_L1o, xlab=NA,ylab=NA, cex.axis=fs, cex.names=fs,
                  col=c("grey","blue","yellow","dark red"))
legend(legend_position[state_num], -9,  c("<1 SD", "1-2 SD","2-3 SD",">3 SD"), fill=c("grey","blue","yellow","dark red"), horiz = TRUE, bty="n", cex=fs)
lines(c(0,plot_percentile_length[state_num]),c(68,68),col="red", lwd=lns)
lines(c(0,plot_percentile_length[state_num]),c(95,95),col="red", lwd=lns)
points(perc.bar,RMSE*50,pch=16, cex=pts, lwd=plns)
points(plot_RMSE_dummy[state_num],3,pch=16, cex=pts, lwd=plns) # dummy point for legend
axis(4, at=seq(0,100,10),labels=seq(0,1,0.1), tck=0.01, cex.axis=fs)
par(las=0, xpd=TRUE,lwd=lns)
mtext("Experiment number", side=1, line=2, font=1, cex=fs, col="black")
mtext("Percentage", side=2, line=2.5, font=1, cex=fs, col="black")
mtext("RMSE", side = 4, line = 2.5, font=1, cex=fs, col="black")
#legend(61, 13, "RMSE", pch=16, cex=fs, pt.cex=pts, col="black", bty="n")
par(las=1, xpd=TRUE,lwd=lns)
text(plot_percentile_label[state_num], 93, labels="95 %", cex=fs, font=2, col="red", adj=1)
text(plot_percentile_label[state_num], 66, labels="68 %", cex=fs, font=2, col="red", adj=1)
text(plot_RMSE_label[state_num], 3, labels="RMSE", cex=fs, col="black", adj=1)
text(plot_label_position[state_num], (100*0.96), labels=plot_label[state_num], font=2, cex=fs3, col="black", adj=1)
dev.off()



## Plot observed vs emulated SAT global means (anom compared to tdstb) for each left-out experiment

xlabels=c("-10", "", "-5", "", "0", "", "5",  "", "10", "", "15", "", "20")

plns=2 # Width of lines

plot_label = c("(d)","(b)")

file_nam = paste("C:\\Users\\nl6806\\OneDrive - University of Bristol\\PostDoc\\2017-02-15 Posiva + SKB\\5. Output\\Plots\\2018-08-01 Final report\\Fig10_Pl_meanSAT_mod_vs_emu_",state,"_5D.png", sep="")
png(file=file_nam,width=575,height=575)
par(mar=c(4,5,1,1), pty="s", mgp=c(2,0.5,0), lwd=lns, las=1)
plot(Y_mean[1,], Emulated_Y_L1o_mean_big_mean[1,], xlim=c(-10,20), ylim=c(-10,20), pch=16, cex=pts, lwd=plns, col="blue", xlab=NA,ylab=NA, xaxt="n", yaxt="n", xaxs="i", yaxs="i")
lines(c(-10,20),c(-10,20), col="grey49",lty=2, lwd=lns)
lines(c(-10,19),c(0,0), col="grey49",lty=3, lwd=lns)
points(Y_mean[1,], Emulated_Y_L1o_mean_big_mean[1,], pch=16, cex=pts, col="blue")
axis(1, at = seq(-10, 20, by = 2.5), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
axis(2, at = seq(-10, 20, by = 2.5), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
axis(3, at = seq(-10, 20, by = 2.5), labels=FALSE, tck=0.01, lwd=lns, cex.axis=fs)
axis(4, at = seq(-10, 20, by = 2.5), labels=FALSE, tck=0.01, lwd=lns, cex.axis=fs)
mtext("Modelled mean SAT index ("~degree*C*")", side=1, line=2.5, font=1, cex=fs, col="black")
par(las=0, xpd=TRUE,lwd=lns)
mtext("Emulated mean SAT index ("~degree*C*")", side=2, line=2.5, font=1, cex=fs, col="black")
#legend(-1, 19, legend=c(expression("highCO"[2]), expression("lowCO"[2])), text.font=c(2,2), pch=c(17,16), cex=fs, pt.cex=pts, col="blue", bty="n")
text(-12, ((20+10)*0.96)-10, labels=plot_label[state_num], font=2, cex=fs3, col="black", adj=1)
dev.off()

# PDF

file_nam = paste("C:\\Users\\nl6806\\OneDrive - University of Bristol\\PostDoc\\2017-02-15 Posiva + SKB\\5. Output\\Plots\\2018-08-01 Final report\\Fig10_Pl_meanSAT_mod_vs_emu_",state,"_5D.pdf", sep="")
pdf(file=file_nam,width=7,height=7.5)
par(mar=c(4,5,1,1), pty="s", mgp=c(2,0.5,0), lwd=lns, las=1)
plot(Y_mean[1,], Emulated_Y_L1o_mean_big_mean[1,], xlim=c(-10,20), ylim=c(-10,20), pch=16, cex=pts, lwd=plns, col="blue", xlab=NA,ylab=NA, xaxt="n", yaxt="n", xaxs="i", yaxs="i")
lines(c(-10,20),c(-10,20), col="grey49",lty=2, lwd=lns)
lines(c(-10,19),c(0,0), col="grey49",lty=3, lwd=lns)
points(Y_mean[1,], Emulated_Y_L1o_mean_big_mean[1,], pch=16, cex=pts, col="blue")
axis(1, at = seq(-10, 20, by = 2.5), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
axis(2, at = seq(-10, 20, by = 2.5), labels=xlabels, tck=0.01, lwd=lns, cex.axis=fs)
axis(3, at = seq(-10, 20, by = 2.5), labels=FALSE, tck=0.01, lwd=lns, cex.axis=fs)
axis(4, at = seq(-10, 20, by = 2.5), labels=FALSE, tck=0.01, lwd=lns, cex.axis=fs)
mtext("Modelled mean SAT index ("~degree*C*")", side=1, line=2.5, font=1, cex=fs, col="black")
par(las=0, xpd=TRUE,lwd=lns)
mtext("Emulated mean SAT index ("~degree*C*")", side=2, line=2.5, font=1, cex=fs, col="black")
#legend(-1, 19, legend=c(expression("highCO"[2]), expression("lowCO"[2])), text.font=c(2,2), pch=c(17,16), cex=fs, pt.cex=pts, col="blue", bty="n")
text(-12, ((20+10)*0.96)-10, labels=plot_label[state_num], font=2, cex=fs3, col="black", adj=1)
dev.off()


