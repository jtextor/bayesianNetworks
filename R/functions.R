#' Simulates data for the "Sprinkler" example.
#'
#' @param N number of data points to generate.
#' @return Returns a data frame containing the simulated data.
simulate.sprinkler <- function( N = 1000 ){
	data.frame(
		Cloudy = rep(0,N),
		Sprinkler = rep(0,N),
		Rain = rep(0,N),
		WetGrass = rep(0,N)
	)
}
