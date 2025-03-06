rlnorm2 <- function(n, mean=1, sd=1) {
  sdlog <- sqrt(log((sd/mean)^2 + 1))
  meanlog <- log(mean) - (sdlog^2) / 2
  rlnorm(n, meanlog = meanlog, sdlog = sdlog)
}
#data <- rlnorm2(10000, mean=5, sd=2)
#hist(data)

aic_manual <- function(y_obs, y_pred, fit)
{
	n <- length(y_obs) 
	k <- length(coef(fit))
	rss <- sum((y_obs - y_pred)^2) + 1.0e-12
	sigma2_hat <- rss/n

	logL <- - (n/2) * log(2 * pi * sigma2_hat) - (rss/(2 * sigma2_hat))
	aic <- 2*k - 2*logL
	
	return(aic)
} 

WeightedErrorEvaluation <- function(obs, pred, x)
{
	w <- c(1:length(obs))/length(obs)
	w <- ifelse( x < 0.001, 0.001, x)
	w <- log(w)
	w <- ( w - min(w))/(max(w) - min(w))
	err <- sqrt(sum(((obs[1:length(obs)] - pred)*w)^2)/length(obs))

	return( err )
}

ErrorEvaluation <- function(obs, pred)
{
	err1 <- obs[1:length(obs)] - pred
	err1 <- sqrt(sum(err1^2)/length(obs))

	return( err1 )
}


fitting_initial_valuse<- function( cond , yy, a_coef, b_coef, c_coef, d_coef, noise_varience)
{
	kk = cond
	lm_pa <- runif(1,-0.001,1)
	lm_pb <- runif(1,-0.001,1)
	lm_pc <- yy[1] + runif(1,-0.0001,0.0001)
	lm_pd <- runif(1,-0.001,1)
							
	if ( length(a_coef) > 1 )
	{
		lm_pa <- rlnorm2(1, mean = mean(a_coef), sd = sd(a_coef))
		lm_pb <- rnorm(n =1,mean = mean(b_coef), sd = sd(b_coef))
		lm_pd <- rnorm(n =1,mean = 0, sd = sqrt(noise_varience)) - noise_varience/2
	}
	if ( kk > 50 )
	{
		lm_pa <- runif(1,-1,1)
		lm_pb <- runif(1,-1,1)
		lm_pc <- yy[1] + runif(1,-0.0001,0.0001)
		lm_pd <- runif(1,-1,1)
		if ( length(a_coef) > 1 && kk < 80)
		{
			lm_pa <- rlnorm2(1, mean = mean(a_coef), sd = sd(a_coef))
			lm_pb <- rnorm(n =1,mean = mean(b_coef), sd = sd(b_coef))
			lm_pd <- rnorm(n =1,mean = 0, sd = sqrt(noise_varience)) - noise_varience/2
		}
	}
	
	return(c(lm_pa,lm_pb,lm_pc,lm_pd))
}

ExponentialDegradationModel <- function(prm, xx, yy, x0, y0, exp_domain_max=30)
{
	lm_pa <- prm[1]
	lm_pb <- prm[2]
	lm_pc <- prm[3]
	lm_pd <- prm[4]
	
	lm_pc = y0 - exp(lm_pa)*exp(exp(lm_pb)*x0 + exp_domain_max*tanh(lm_pd))

	pred <- function(parS, xx) parS$c + exp(parS$a)*exp(exp(parS$b)*xx + exp_domain_max*tanh(parS$d))
	resid <- function(p, observed, xx) observed - pred(p,xx)
	parStart <- list(a=lm_pa, b=lm_pb, c=lm_pc, d=lm_pd)
	
	fit <- NULL
	
	options(show.error.messages = FALSE)
	fit <- try(nls.lm(par=parStart, fn=resid, observed=yy, xx=xx, 
				control=nls.lm.control(maxiter=1024,maxfev=1000,nprint=0)), silent = FALSE)
	if (class(fit)[1] == "try-error"||class(fit)[1] == "NULL")
	{
		fit = NULL
	}
	options(show.error.messages = T)
	
	return( fit)
}

evalExponentialDegradationModel <- function(fit, x, exp_domain_max=100)
{
	coef = coefficients(fit)
	pred <-  coef[3] + exp(coef[1])*exp(exp(coef[2])*x + exp_domain_max*tanh(coef[4]))
	
	return( pred )
}

GompertzDegradationModel <- function(prm, xx, yy, x0, y0)
{
	lm_pa <- prm[1]
	lm_pb <- prm[2]
	lm_pc <- prm[3]
	lm_pd <- prm[4]

	lm_pc = y0- exp(lm_pd)*exp(exp(lm_pa)/exp(lm_pb)*(1 - exp(-exp(lm_pb)*x0)))

	pred <- function(parS, xx) parS$c + exp(parS$d)*exp(exp(parS$a)/exp(parS$b)*( 1- exp(-exp(parS$b)*xx)))
	resid <- function(p, observed, xx) observed - pred(p,xx)
	parStart <- list(a=lm_pa, b=lm_pb, c=lm_pc, d=lm_pd)
	
	fit2 <- NULL
	options(show.error.messages = FALSE)
	fit2 <- try(nls.lm(par=parStart, fn=resid, observed=yy, xx=xx, 
				control=nls.lm.control(maxiter=1024,maxfev=1000,nprint=0)), silent = FALSE)
	if (class(fit2)[1] == "try-error"||class(fit2)[1] == "NULL")
	{
		fit2 = NULL
	}
	options(show.error.messages = T)

	return( fit2)
}

evalGompertzDegradationModel <- function(fit2, x)
{
	coef = coefficients(fit2)
	pred <-  coef[3] + exp(coef[4])*exp(exp(coef[1])/exp(coef[2])*(1 - exp(-exp(coef[2])*x)))

	return( pred )
}


