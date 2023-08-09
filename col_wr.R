wr = function(n)
#WR white-orange-red color map
#   WR(M) returns an M-by-3 matrix containing a colormap. 
#   The colors begin with white range through
#   orange, and end with dark red.
#
#   WR returns a colormap with the same number of colors as the current
#   figure's colormap. If no figure exists, MATLAB creates one.
#
#   EXAMPLE
#
#   This example shows how to reset the colormap of the current figure.
#
#       colormap(wr)
{

values = matrix((c(
  255, 255, 255,
  254, 224, 139,
  253, 174, 97,
  244, 109, 67,
  213, 62, 79,
  158, 1, 66)) / 255,
  nrow=6, ncol=3, byrow=TRUE)

P = dim(values)[1];

	  map1 = approx(values[,1], y=NULL, n=n, method = 'linear')
	  map2 = approx(values[,2], y=NULL, n=n, method = 'linear')
	  map3 = approx(values[,3], y=NULL, n=n, method = 'linear')
	  
	  map = matrix(c(map1$y,map2$y,map3$y),nrow=n, ncol=3, byrow=FALSE)
}


