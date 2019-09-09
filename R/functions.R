#' Simulates data for the "Sprinkler" example.
#'
#' @param N number of data points to generate.
#' @return Returns a data frame containing the simulated data.
#' @export
simulate.sprinkler <- function( N = 1000 ){
	p <- c(0.198, 0.002, 0.005, 0.045, 0.02, 0.18, 5e-04, 0.0495, 0.0891,  
		9e-04, 0.036, 0.324, 0.001, 0.009, 4e-04, 0.0396)
	d <- expand.grid( WetGrass=0:1, Rain=0:1, Sprinkler=0:1, Cloudy=0:1 )
	s <- sample( 1:nrow(d), N, replace=TRUE, prob=p )
	d <- d[s,]
	rownames(d) <- NULL
	d
}


