#' Calcula el mínimo y el máximo de un vector aleatorio
#'
#' @param n tamaño de la muestra
#' @param media media de la población
#' @param desvi desviación de la población
#'
#' @return un vector con mínimo y máximo
#'
#'
#' @examples
#' min_max(n=10, media=50, desvi=2)
#' min_max(n=100, desvi=3, media=180)
#'
#' @importFrom stats rnorm
#'
#' @export
min_max <- function(n, media, desvi){
  x <- rnorm(n=n, mean=media, sd=desvi)
  res <- c("minimo"=min(x), "max"=max(x))
  return(res)
}
