#i 
# Copyright (c) 2014 Michel Crucifix
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is furnished to do
# so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#


pca <- function (A) 
{
  nlon = dim(A)[1]
  nlat = dim(A)[2]
  nr = dim(A)[3]
  A = matrix(A, nlon * nlat, nr)
  mean = apply(A, 1, mean)
  Am = sweep(A, 1, apply(A, 1, mean))
  o = which(!is.na(Am[, 1]))
  U = svd(Am[o, ])
  O = A
  V = A
  O[o, ] = U$u
  O = array(O, c(nlon, nlat, nr))
  mean = matrix(mean, nlon, nlat)
  d = U$d
  return(list(mean = mean, PCA = O, amps = U$v, d = d))
}


mypca <- function(var, nkeep)
{
    pca = pca(var)
  # rvm : residual variance estimated  at each grid point
  pca$rvm = sum ( pca$d[-seq(nkeep)]^2) / length(which( ! is.na (var[,,1]))) / dim(var)[3]
    pca
}


pca_reconstruct_mean <- function(L,means)
{
  # sum over basis elements scaled by L$d + mean
  sv = seq(length(means))
  nv = length(means)
  return ( apply ( sweep(L$PCA[,,sv], 3, means * L$d[sv] ,'*' ) , c(1,2), sum)  + 
     L$mean )
}

pca_sample_quantiles <- function (L, means, vars, dof, n = 10000)
{
  nv = (length(means))
  sv = seq(length(means))
  # sample variances
  #m = outer ( sqrt ( vars ) , rt(n , dof, 0)   ) 
  m = matrix( rt(n * nv , dof, 0) , nv, n)
  m = sweep ( m, 1, sqrt ( vars) , '*') 
  m = sweep ( m, 1, means, '+') 
  m = sweep ( m, 1, L$d[sv], '*' ) 

 # add means

  # provisionnaly reshapes LL to prepare the inner product
  no = dim(L$PCA)[1] * dim(L$PCA)[2]
  LL = matrix ( L$PCA[,,sv], no, nv)
  # multiply by means
  R  = LL %*% m 

  # residal variance
  rv = rnorm(n , 0, sd =  sqrt ( L$rvm ) )
  R = R + rv 

  # reshape under friendly form
  R  = array ( R, c(dim(L$PCA)[1], dim(L$PCA)[2],  n ) ) 
  R  = sweep(R, c(1,2), L$mean, '+')

 

  # quantiles                                           1    2      3    4     5     6      7    Inf
  O = apply( R, c(1,2), function (i) quantile(i, probs=c(0.002, 0.025, 0.17, 0.5, 0.83, 0.975, 0.998) ) ) 
  return(O)
}

compare_quantiles_with_ref <- function ( O, REF)
{
  I = sweep(O,  c(2,3) , REF) 
  Q = apply(I, c(2,3), function (i) min ( which ( i > 0 )) ) 
  q1 = length(which(Q == 4 | Q == 5 ))
  q2 = length(which(Q == 3 | Q == 6 ))
  q3 = length(which(Q == 2 | Q == 7 ))
  q4 = length(Q) - q1 - q2 - q3
  DF = sqrt ( mean  (REF - O[4,,] , na.rm = TRUE)^2 ) 
  return(c(q1,q2,q3,q4, DF))
}


pca_reconstruct_var <- function(L,variances)
{
  # sum of component variances provided by variances 
  # n is number of experiments used to estimate covariance
  sv = seq(length(variances))
  nv = length(variances)
  var=  apply ( sweep(L$PCA[,,sv]^2, 3, variances * (L$d[sv]^2) ,'*' ) , c(1,2), sum)  
  # spread residual variance equally
  # may not be a good idea: better to spread variance according 
  # to residual space
  if (! is.null (L$rvm)) 
  {
  var  <- var + L$rvm
  }
  return ( var ) 
}

pe_l1o <- function (X, Y, pca_function=pca, hp, nkeep=10, ...) 
{
  # X : input vector
  # Y : outputs (as matrix)
  # 

  VAR= lapply( seq(nrow(X)) , function (i) {
    PE  = pe_c (X[-i,], Y[,,-i], pca_function=pca_function, hp=hp,nkeep=nkeep)
    PRE = pe_p (X[i,, drop=FALSE], PE)
    # the distance metric is here blindly estimated assuming independent pixels
    DIFF   =  - PRE$mean + Y[,,i]
    DS  =  abs (PRE$mean - Y[,,i] ) / sqrt (  PRE$var )
    list(DIFF=DIFF, DS=DS) })
    VAR
}


pe_l1o_barplot_student <- function (X, Y, pca_function=pca, hp, nkeep=20, ...) 
{
  # X : input vector
  # Y : outputs (as matrix)
  # 

  VAR= sapply( seq(nrow(X)) , function (i) {
    PE  = pe_c (X[-i,], Y[,,-i], pca_function=pca_function, hp=hp,nkeep=nkeep)
    OUT = pe_p_student(X[i,, drop=FALSE], PE)

    compare_quantiles_with_ref(OUT, Y[,,i] )
    })

    # tyding up : set DF as an attribute
    DF = VAR[5,]
    VAR = VAR[1:4,]
    attr(VAR, 'DF') <- DF
    VAR
  
}

pe_l1o_maxerr <- function (X, Y, pca_function=pca, hp, nkeep=20, ...) 
{
 VAR= lapply( seq(nrow(X)) , function (i) {
    PE  = pe_c (X[-i,], Y[,,-i], pca_function=pca_function, hp=hp,nkeep=nkeep)
    PRE = pe_p (X[i,, drop=FALSE], PE)
    # the distance metric is here blindly estimated assuming independent pixels
    DS  =  abs (PRE$mean - Y[,,i] ) / sqrt (  PRE$var )
    })
    print('ici')
    VAR = simplify2array(VAR)
    apply(VAR, c(1,2), max)
}


pe_l1o_barplot <- function (X, Y, pca_function=pca, hp, nkeep=20, ...) 
{
  # X : input vector
  # Y : outputs (as matrix)
  # 

  VAR= sapply( seq(nrow(X)) , function (i) {
    PE  = pe_c (X[-i,], Y[,,-i], pca_function=pca_function, hp=hp,nkeep=nkeep)
    PRE = pe_p (X[i,, drop=FALSE], PE)
    # the distance metric is here blindly estimated assuming independent pixels
    DF   = sqrt ( mean  (PRE$mean - Y[,,i] , na.rm = TRUE)^2 ) 
    DS  =  abs (PRE$mean - Y[,,i] ) / sqrt (  PRE$var )
    q1 <- length ( which(DS < 1))
    q2 <- length ( which(DS < 2)) - q1
    q3 <- length ( which(DS < 3)) - q2 - q1
    q4 <- length ( which(DS < 99999.)) - q3 - q2 - q1 
    c(q1,q2,q3,q4,DF)})

    # tyding up : set DF as an attribute
    DF = VAR[5,]
    VAR = VAR[1:4,]
    attr(VAR, 'DF') <- DF
    VAR
  
}

pe_c <- function (X, Y, pca_function=pca, hp, nkeep=10, ...)
{
  # emulator calibration (for same l and nugget applied to all pcas ! )
  # otherwise one may look at an autmatic calibration (later)
  # X : design
  # Y : output under the form (nlat,nlon, nr) where nr is the
  #     number of design members
  # hp : table of hyperparameters
  # output : generate pca and list of gaussian process emulators using
  # pca funcion and table of hyperparameters

  L <- pca_function(Y, nkeep)
  n <- nrow(X)
  m <- ncol(X)

  L$scaled_amps <- sweep( L$amps, 2  , L$d, '*') 
  # attention : temporary fix : set residual variance to zero !!! 
  # do not forget to change
  GPx <- lapply(seq(nkeep), function(i) 
              GP_C(X[,], L$amps[,i], 
                   list(theta=hp[1:m,i], nugget=hp[m+1,i])) )

  return(list(L=L, GPList = GPx))

}

pe_p <- function (x, PE)
{
  # pca emulator predictor, at input x
  GPp <- lapply(PE$GPList, function(EM_Cali) GP_P(EM_Cali, x, calc_var = FALSE, extra_output = FALSE ) )
  means     <- sapply(GPp, function(x) x$yp)
  variances <- sapply(GPp, function(x) x$Sp_diag)  
  # need to consider residual variance !!! 
  # number of samples used for covariance estimation
  n = nrow(PE$GPList[[1]])
  mean_field <- pca_reconstruct_mean ( PE$L, means ) 
  var_field  <- pca_reconstruct_var ( PE$L, variances)
  return(list(mean=mean_field, var=var_field, means=means, variances=variances))
}

pe_p_student <- function (x, PE)
{
  # pca emulator predictor, at output x
  GPp <- lapply(PE$GPList, function(EM_Cali) GP_P(EM_Cali, x, calc_var = FALSE, extra_output = FALSE ) )
  # should figure out dof here
  dof = 20
  means     <- sapply(GPp, function(x) x$yp)
  variances <- sapply(GPp, function(x) x$Sp_diag)  
  out <- pca_sample_quantiles(PE$L, means, variances, dof)
  # need to consider residual variance !!! 
  return(out)
}





