br = function(n)
#BR Blue-green-orange-red color map
#   BR(M) returns an M-by-3 matrix containing a colormap. 
#   The colors begin with dark blue, range
#   through green and orange, and end with dark red.
#
#   BR returns a colormap with the same number of colors as the current
#   figure's colormap. If no figure exists, MATLAB creates one.
#
#   EXAMPLE
#
#   This example shows how to reset the colormap of the current figure.
#
#       colormap(br)
{

values = matrix((c(
  0, 0, 255,
  0, 128, 255,
  0, 255, 255,
  153, 255, 255,
  255, 255, 123,
  255, 255, 0,
  255, 128, 0,
  255, 0, 0)) / 255,
  nrow=8, ncol=3, byrow=TRUE)

P = dim(values)[1];

	  map1 = approx(values[,1], y=NULL, n=n, method = 'linear')
	  map2 = approx(values[,2], y=NULL, n=n, method = 'linear')
	  map3 = approx(values[,3], y=NULL, n=n, method = 'linear')
	  
	  map = matrix(c(map1$y,map2$y,map3$y),nrow=n, ncol=3, byrow=FALSE)
}


