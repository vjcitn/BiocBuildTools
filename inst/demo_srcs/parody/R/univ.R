calout.detect <- function( x, alpha=0.05,
                method = c("GESD", "boxplot", "medmad", "shorth", "hybrid"), 
                             k = ((length(x) %% 2) * floor(length(x)/2) +
		                   (1 - (length(x) %% 2)) * (length(x)/2 - 1)), 
		                   scaling,
		                   ftype,
		     		location,
		     		scale,
		                     gen.region = function(x,location,scale,scaling,alpha)
		     			{
		     			g <- scaling(length(x),alpha)
		     			location(x)+c(-1,1)*g*scale(x)
		     			}
 )
	{
	if (length(method) > 1) {
		warning("GESD detection selected by default")
		method <- method[1]
		}
	if (method == "hybrid" & ( missing(scaling) | 
	                missing(location) | missing(scale) ) )
		stop("hybrid detection requires scaling, location and args")
	if (method != "GESD" & missing(scaling)) 
		{
		#warning("default scaling function assigned")
		if (method == "shorth") scaling <- shorth.scale
		else if (method == "medmad") scaling <- hamp.scale.4
		else if (method == "boxplot") scaling <- box.scale
		else if (method == "hybrid") 
			{
			scaling <- scaling
			location <- location
			scale <- scale
			}
		}
	n <- length(x)
	if ( method == "GESD" )
		ans <- gesdri( x=x, k=k, alpha=alpha )
	else if ( method == "boxplot" )
		{
	        g <- scaling( n, alpha )
		ans <- tukeyorinds( x=x, alpha=alpha, g=g )
		}
	else if ( method == "medmad" )
		{
	        g <- scaling( n, alpha )
		ans <- hampoutinds( x=x, alpha=alpha, g=g )
		}
	else if ( method == "shorth" )
		{
	        g <- scaling( n, alpha )
		ans <- rououtinds( x=x, alpha=alpha, g=g )
		}
	else if ( method == "hybrid" )
		{
		OR <- gen.region(x,location,scale,scaling,alpha)
		outinds <- c((1:length(x))[x<OR[1]],(1:length(x))[x>OR[2]])
		if (is.null(outinds)) ans <- list(ind=NA,val=NA,outlier.region=OR)
		else ans<- list(ind=outinds,val=x[outinds],outlier.region=OR)
		}
		

		ans
		
	}


#ckesd <-
#function(x, k)
#{
#	n <- as.integer(length(x))
#	x <- as.double(x)
#	k <- as.integer(k)
#	resvec <- as.double(rep(0, k))
#	q <- as.integer(rep(0, n))
#	indvec <- as.integer(rep(0, k))
#	z <- .C("ckesd",
#		x = x,
#		n = n,
#		k = k,
#		resvec = resvec,
#		q = q,
#		indvec = indvec,
#		NAOK = TRUE,
#		specialsok = TRUE,
#		pointers = NULL)
#	z
#}

lamtab <-
function(n, k, alpha = 0.05)
{
# obtain the sequence of k critical values
# to be used in checking for k outliers
	l <- 0:(k - 1)
	p <- 1 - ((alpha/2)/(n - l))
# note, this can trip a bug in Splus 3.2 qt if n is very large (>50000?)
# the invibeta routine needs to be iterated some more
	tvals <- qt(df = n - l - 2, p = p)
	lams <- ((n - l - 1) * tvals)/sqrt((n - l - 2 + tvals^2) * (n - l))
	lams
}


skesd.old <- function(x, k)
{
        bigres <- rep(NA, k)
        bigresind <- rep(NA, k)
        n <- length(x)
        curx <- x
        ind <- 1:n
        inds <- NULL
        for(i in 1:k) {
                last <- n - i + 1
                m <- mean(curx)
                ares <- abs(curx - m)
                oares <- order(ares)
                bigres[i] <- ares[oares[last]]/sqrt(var(curx))
                curx <- curx[ - oares[last]]
                inds <- c(inds, ind[oares[last]])
                ind <- ind[ - oares[last]]
        }
        list(res = bigres, ind = inds)
}

skesd <- function(x, k)
{
# thanks to Jerry Lewis of Biogen Idec
        n <- length(x)
        ind <- order(x)  # only sort once, outside the loop
        curx <- x[ind]
        inds <- bigres <- NULL
        for(last in n:(n-k+1)) {
                m <- mean(curx)
                ares <- abs(curx[c(1,last)] - m)
                if(ares[1]==ares[2]) {  # handle ties on opposite sides of m deliberately -- test the one farthest from median
                    i <- ifelse(median(curx)>m,1,last)
                } else {
                    i <- ifelse(ares[1]>ares[2],1,last)
                }
                bigres <- c(bigres, ares[ifelse(i==1,1,2)]/sqrt(var(curx)))
                curx <- curx[-i]
                inds <- c(inds, ind[i])
                ind <- ind[-i]
        }
        list(res = bigres, ind = inds)
}


gesdri <-
function(x, k = ((length(x) %% 2) * floor(length(x)/2) +
	      (1 - (length(x) %% 2)) * (length(x)/2 - 1)), alpha = 0.05)
{
#        if (is.loaded("ckesd"))
#             E <- ckesd(x, k)
#        else
        E <- skesd(x, k)
	R <- E$res
	I <- E$ind
	n <- length(x)	#if(n == 100 & k == 49)
	L <- lamtab(length(x), k, alpha = alpha)
	worst <- NA
	if (any(R>L)) worst <- max((1:k)[R > L])
	if(!is.finite(worst) || is.na(worst))
		list(ind = NA, val = NA)
	else list(ind = I[1:worst], val = x[I[1:worst]])
}

hamp.scale.3 <-
function(n, alpha)
{
# formula (15) p 790, alpha=.05
if ( n < 10 ) warning("procedure not calibrated for n < 10")
if (alpha == 0.05)
	if(n %% 2) 2.906 + 12.99 * (n - 5)^(-0.5781) else 2.906 + 11.99 * (n - 
			6)^(-0.5651)
else if (alpha == 0.01)
	if(n %% 2) 3.819 + 26.50 * (n - 7)^(-0.6089) else 3.819 + 24.09 * (n - 
			7)^(-0.5936)
else stop("procedure only calibrated for alpha=0.01, 0.05")
}

hamp.scale.4 <-
function(n, alpha)
{
if ( n < 10 ) warning("procedure not calibrated for n < 10")
# formula (16)
if (alpha == 0.05)
	if(n %% 2) 1.483 * qnorm(0.975^(1/n)) + 14.43 * ((n - 3)^(-0.7939))
		 else 1.483 * qnorm(0.975^(1/n)) + 21.61 * ((n + 1)^(-0.8655))
else if (alpha == 0.01)
	if(n %% 2) 1.483 * qnorm(0.995^(1/n)) + 24.48 * ((n - 5)^(-0.8236))
		 else 1.483 * qnorm(0.995^(1/n)) + 41.39 * ((n)^(-0.9143))
else stop("procedure only calibrated for alpha=0.01, 0.05")
}

hampor <-
function(x, g)
{
	lx <- length(x)
	Mad <- function(x)
	median(abs(x - median(x)	# Mad is the resistant scale measure
	))
	m <- median(x)
	s <- Mad(x)
	L <- m - s * g
	U <- m + s * g
	c(L, U)
}

hampoutinds <-
function(x, alpha, g=hamp.scale.4(length(x),alpha=alpha) )
{
	OR <- hampor(x, g)
	N <- length(x)
        outinds <- c((1:N)[x < OR[1]], (1:N)[x > OR[2]])
        if (sum(x<OR[1])==0 & sum(x>OR[2])==0) {
           warning("no data values in outlier region")  
           list(ind= NA, val= NA, outlier.region=OR) } else 
             list(ind= outinds, val= x[outinds], outlier.region=OR)
}


shorth.scale <- function(n, alpha=0.05)
{
# this function gives scaling values for the raw
# length of the shorth to be used in outlier detection.
        if(n < 10 | n > 2000) warning(
                        "rousseeuw calibration based on 10 <= n <= 2000")
        if(alpha == 0.05) {
                LN <- c(1, log(n)^(1:4))
                ecoef <- c(9.165430, -3.632088,
                        0.7811141, -0.07331514,
                        0.002603013)

                ocoef <- c(20.87936, -12.84945274,
                        3.50602884, -.42863233,
			0.01975694)
                if(n %% 2 == 0 | n >= 400)
                        sum(ecoef * LN)
                else sum(ocoef * LN)
        }
        else if(alpha == 0.01) {
                LN <- c(1, log(n)^(1:4))
                coe <- c(20.0988614238686, -9.7688241263516, 2.10296418210252, 
                        -0.198414242035932
                        , 0.00696260655978963)
                sum(coe * LN)
        }
        else stop("Rousseeuw scaling only defined for alpha = .05 or .01")
}


rouor <-
function(x, alpha=0.05, g=shorth.scale(length(x),alpha=alpha),frac=0.5)
{
	if (frac > .5 | frac < .5) stop("rousseeuw detection not defined for frac != .5")
	shss <- shorth(x,Alpha=frac)
# in a former incarnation, this outlier region was
# made unbiased.  but the complications are heavy
# and bias corrections are not exact.  so instead
# we now base the outlier region on lshorth which
# is basically unambiguously computable
	shss$mid - shss$len * g * c(1, -1) 
}

rououtinds <-
function(x, alpha=0.05, g=shorth.scale(length(x),alpha=alpha),frac=0.5)
{
	OR <- rouor(x, alpha, g, frac=0.5)
	N <- length(x)
        outinds <- c((1:N)[x < OR[1]], (1:N)[x > OR[2]])
        if (sum(x<OR[1])==0 & sum(x>OR[2])==0) {
           warning("no data values in outlier region")  
           list(ind= NA, val= NA, outlier.region=OR) } else list(ind= outinds, val= x[outinds], outlier.region=OR)
}

box.scale <-
function(n, alpha = 0.05)
{
        if(n > 2000)
                warning("extrapolations for n>2000 may be invalid")
        if(n <= 10)
                stop("n must be greater than 10")
        if(alpha == 0.05) {
                mod <- function(x, n)
                x %% n
                lmodff <- function(x)
                1.521267 + 0.1458931 * x
                sumodf <- function(x)
                2.668116 - 0.2723008 * x + 0.04326603 * x * x - 0.0009040389 * 
                        x * x * x
                slmodf <- function(x)
                2.308486 - 0.08364704 * x + 0.01044393 * x * x + 0.0009883642 * 
                        x * x * x
                ln <- log(n)
                if(ln > 5.7)
                        lmodff(ln)
                else if(mod(n, 4) == 0 | mod(n, 4) == 1)
                        slmodf(ln)
                else sumodf(ln)
        }
        else if(alpha == 0.01) {
                qmod <- function(n)
                7.085162 - 2.692155 * n + 0.5950019 * n * n - 0.05763641 * n * 
                        n * n + 0.002165973 * n * n * n * n
                qmod(log(n))
        }
	else stop("procedure defined only for alpha = .01 or .05")
}


tukeyorinds <-
function(x, alpha = 0.05, g = box.scale(length(x), alpha=alpha))
{
	OR <- tukeyor(x,alpha, g)
	N <- length(x)
        outinds <- c((1:N)[x < OR[1]], (1:N)[x > OR[2]])
        if (sum(x<OR[1])==0 & sum(x>OR[2])==0) {
           warning("no data values in outlier region")  
           list(ind= NA, val= NA, outlier.region=OR) } else list(ind= outinds, val= x[outinds], outlier.region=OR)
}

tukeyor <-
function(x, alpha=0.05, g = box.scale(length(x), alpha=alpha), ftype = "ideal")
{
	n <- length(x)
	if(ftype == "ideal") # see Hoaglin, Iglewicz, JASA 1987
		f <- n/4 + 5/12
	else f <- 0.5 * floor((n + 3)/2)
	xs <- sort(x)
	ff <- floor(f)
	cf <- ceiling(f)
	fracinterp <- function(x, f) # suppose 1 < f < length(x)
		# this linearly interpolates between x[floor(f)] and x[ceiling(f)]
		# typical use requires x sorted
	{
		ind <- floor(f)
		base <- x[ind]
		top <- x[ind + 1]
		frac <- f - ind
		base + (top - base) * frac
	}
	Fl <- fracinterp(xs, f)
	Fu <- fracinterp(xs, n + 1 - f)
	Fspread <- Fu - Fl
	kf <- g * Fspread
	IFl <- Fl - kf
	IFu <- Fu + kf
	c(IFl, IFu)
}

shorth <- function(x, Alpha = 0.5)
{
	x <- sort(x)
	N <- length(x)
	nsamp <- floor(N * Alpha) + 1	# rouss/ler 1988 p104
	begs <- 1:(N - nsamp + 1)
	ends <- begs + nsamp - 1
	lens <- x[ends] - x[begs]
	Nlens <- length(lens)
	leasts <- (1:Nlens)[lens <= min(lens)]
	if(length(leasts) > 1)
		warning("shorth is not unique, leftmost used")
	least <- leasts[1]
	intv <- c(x[begs][least], x[ends][least])	
	# correction factor to eliminate the parity dependence 
# of shs, Rousseeuw and Leroy
# 1988 Stat Neer p115
	te <- 3/8
	ao2 <- Alpha/2	#	if((N %% 4) == 0) {
	q <- floor((0.5 + ao2) * N)
	k <- ceiling((0.5 - ao2) * N)
	if((N %% 4) == 0) {
		q <- q - 0.5
		k <- k + 0.5
	}
	else if((N %% 4) == 3) {
		q <- q - 0.5
		k <- k + 0.5
	}
	corr <- 1/((qnorm((q - te)/(N - 2 * te + 1))) - (qnorm((k - te)/(N - 2 * 
		te + 1))))	# empirical bias correction for alpha = .5 only
	bias.corr <- 0.89 + 0.142 * log(log(log(N)))
	list(shorth = intv, length.shorth = lens[least], midpt.shorth = mean(
		intv), meanshorth = mean(x[(begs[least]):(ends[least])]), 
		correction.parity.dep = corr, bias.correction.gau.5 = bias.corr,
		alpha = Alpha)
}

        
logit <- function(x) log(x/(1-x))
al <- function(x) plogis(x)

