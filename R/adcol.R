#' Function to add a new column to the matrix
#'
#' @param x levels vector of the new factor
#' @param y number of levels of the new factor
#' @param z level vector of the initial matrix
#' @param run initial run matrix
#'
#' @return matrix with the new run order
#'
#'
#' @examples
#' x = matrix(c(-1, 1), ncol = 1)
#' y = length(x)
#' z = c(2,2,2)
#' run=matrix(c(1,-1,1,-1,1,1,-1,-1), ncol=2)
#' adcol(x,y,z,run)
#'@export
adcol <- function(x,y,z,run){
  n <- length(z)
  xr <- vector("list", prod(z[-n]))
  xr[[1]] <- x
  for(i in 2:prod(z[-n])){
    xr[[i]] <- apply(xr[[(i-1)]], 2, rev)
  }
  xr1 <- do.call(rbind, xr)
  run1 <- matrix(rep(run, each=y),ncol = ncol(run))

  out <- cbind(run1, xr1)
  colnames(out) <- NULL
  return(out)
}
