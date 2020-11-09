#' Simulates data for the "Sprinkler" example.
#'
#' @param N number of data points to generate.
#' @return returns a data frame containing the simulated data.
#' @export
simulate_sprinkler <- function( N = 1000 ){
	p <- c(0.198, 0.002, 0.005, 0.045, 0.02, 0.18, 5e-04, 0.0495, 0.0891,  
		9e-04, 0.036, 0.324, 0.001, 0.009, 4e-04, 0.0396)
	ft <- c(FALSE,TRUE)
	d <- expand.grid( WetGrass=ft, Rain=ft, Sprinkler=ft, Cloudy=ft )
	s <- sample( 1:nrow(d), N, replace=TRUE, prob=p )
	d <- d[s,]
	rownames(d) <- NULL
	d
}

#' Fits a very simple "Optimal Bayes" classifier.
#'
#' @param formula input formula with the hypothesis variable on the left-hand
#'  side and the evidence variables on the right-hand side.
#' @param data matrix or dataframe with categorical (character/factor/logical) predictors.
#' @return returns an object containing estimated probabilities for each possible combination
#'  of the values seen in the input data.
#' @export
optimal_bayes <- function( formula, data ){
	vv <- all.vars( formula )
	data <- as.data.frame( data )
	if( length(setdiff(vv,colnames(data)))>0 ){
		stop("Not all variables in the formula can be found in the input data!")
	}
	lhs <- all.vars(update(formula, .~0))
	rhs <- all.vars(update(formula, 0~.))
	if( length(lhs) != 1 ){
		stop("Need exactly one hypothesis variable!")
	}
	if( length(rhs) < 1 ){
		stop("Need at leat one evidence variable!")
	}
	data <- data[,vv]
	r <- list()
	r[['evidence']] <- rhs
	r[['hypothesis']] <- lhs
	tbl <- as.data.frame(ftable(data,row.vars=vv))
	r[['classes']] <- unique( data[,lhs] )
	tbl$Freq <- tbl$Freq / sum(tbl$Freq)
	r[['probability_table']] <- tbl
	class(r) <- "optimal_bayes"
	r
}

#' @export
predict.optimal_bayes <- function( object, newdata ){
	newdata <- as.data.frame( newdata )
	o <- object
	ev <- intersect( colnames(newdata), o[['evidence']] )
	r <- matrix( 0, ncol=length(o[['classes']]),
		nrow=nrow(newdata) )
	tbl <- o[['probability_table']]
	nd <- newdata[,intersect(colnames(newdata),o$evidence),drop=FALSE]
	nd$i.i.i.i.i <- 1:nrow(nd)

	xx <- merge( tbl, nd )

	xa <- aggregate( xx$Freq, xx[,c("i.i.i.i.i",o[['hypothesis']])], sum )

	r <- matrix( xa[,3], ncol=2 )

	colnames(r) <- unique(xa[,2])
	r <- sweep(r, 1, rowSums(r), FUN="/")
	r
}

#' Performs a Conditional Independence Test for Mixed Data.
#'
#' This tests whether X and Y are conditionally independent given Z.
#' @param x character. Name of column containing X variable.
#' @param y character. Name of column containing Y variables.
#' @param z character vector. Names of colums containing the Z variables, if any.
#' 
#' @return a list with components "effect", "p.value", and "statistic".
#' @export
#' @importFrom ranger ranger
ci.test <- function( x, y, z=NULL, data ) {
	require(ranger)

	if( !is.data.frame(data) ){
		stop("Please supply your data in a data frame!")
	}
	
	n <- nrow(data)

	if( is.ordered( data[,x] ) ){
		data[,x] <- as.integer(data[,x])
		tx <- matrix( data[,x],ncol=1 )
	} else if( is.numeric( data[,x] ) ){
		data[,x] <- rank( data[,x] )
		tx <- matrix( data[,x],ncol=1 )
	} else if( is.factor( data[,x] ) ){ 
		tx <- table( seq_len(n), data[,x] )
	} else {
		stop( "unsupported data type: x ")
	}

	if( is.ordered( data[,y] ) ){
		data[,y] <- as.integer(data[,y])
		ty <- matrix( data[,y], ncol=1 )
	} else if( is.numeric( data[,y] ) ){
		data[,y] <- rank( data[,y] )
		ty <- matrix( data[,y], ncol=1 )
	} else if( is.factor( data[,y] ) ){ 
		ty <- table( seq_len(n), data[,y] )
	} else {
		stop( "unsupported data type: y ")
	}

	# residualize
	if( is.null(z) ){
		tx <- sweep( tx, 2, colMeans(tx) )
		ty <- sweep( ty, 2, colMeans(ty) )
	} else {
		forest.x <- ranger::ranger( x=data[,z,drop=FALSE], y=data[,x],
			probability=ncol(tx)>1 )
		tx <- tx - predict( forest.x, data=data )$predictions

		forest.y <- ranger::ranger( x=data[,z,drop=FALSE], y=data[,y],
			probability=ncol(ty)>1 )
		ty <- ty - predict( forest.y, data=data )$predictions
	}


	if( is.factor(data[,x]) ){
		tx <- tx[,-ncol(tx),drop=FALSE]
	}
	if( is.factor(data[,y]) ){
		ty <- ty[,-ncol(ty),drop=FALSE]
	}

	k <- ncol(tx)
	r <- ncol(ty)	

	m <- 1
	R <- matrix( 0, nrow=n, ncol=(k)*(r) )

	# this takes long and can probably be done better
	for( i in 1:(k) ){
		for( j in 1:(r) ){
			R[,m] <- (tx[,i]*ty[,j]) / sqrt(mean(tx[,i]^2)) / sqrt(mean(ty[,j]^2))
			m <- m+1
		}
	}

	p <- ncol(R)

	x2c <- (n-p) / p / (n-1) * n * t(colMeans(R)) %*% MASS::ginv( cov(R) ) %*% colMeans(R)
	list( statistic=c(x2c), effect=c(sqrt(sum(colMeans(R)^2) / min(k,r))), 
		p.value=pchisq(c(x2c), k*r,lower.tail=FALSE) )
}


