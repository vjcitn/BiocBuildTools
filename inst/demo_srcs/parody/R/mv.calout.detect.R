acll <- function(x) exp(-exp(x))
cll <- function(x) log(-log(x))


lams.unstr <- function(n, p, k, alpha = 0.05)
{
qbeta(alpha/(n - (0:(k - 1))), (n - (0:(k - 1)) - p - 1)/2, p/2)
}
   
mv.calout.detect <-
function(x, k = min(floor((nrow(x)-1)/2),100), Ci = C.unstr, lamfun = lams.unstr, alpha = 0.05,
  method=c("parametric", "rocke", "kosinski.raw", "kosinski.exch")[1], ...)
{
# use caroni prescott 1992 algorithm
# or the parametric detector (Ri.direct, lams.dir)
   if (method == "parametric" )
     {
        N <- nrow(x)
        p <- ncol(x)
        Ds <- rep(NA, k)
        outcands <- rep(NA, k)
        xs <- x
        outinds <- 1:N
        for(i in 1:k) {
                Ni <- nrow(xs)
                inds <- 1:Ni
                W <- Ci(xs)
                out <- inds[W <= min(W)][1]
                Ds[i] <- W[out]
                outcands[i] <- outinds[out]
                xs <- xs[ - out,  ]
                outinds <- outinds[ - out]
        }
        bad <- NULL
	Lcrit <- lamfun(n=N, k=k, p=p, alpha=alpha)
        for(j in k:1) {
                if(Ds[j] < Lcrit[j]) {
                        bad <- j:1
                        break
                }
        }
        if(length(bad) == k)
                warning("k outliers found, there may be more")
    }
  else stop("only providing parametric methods now")
        if(is.null(bad))
                return(list(inds = NA, vals = NA, k = k, alpha = alpha))
        else list(inds = outcands[bad], vals = x[outcands[bad],  ], k = k, alpha
                         = alpha)
}


CPunstrC <- function(x,k){
        N <- nrow(x)
        p <- ncol(x)
        Ds <- rep(NA, k)
        outcands <- rep(NA, k)
        outcands.vals <- rep(NA, k)
        xs <- x
        outinds <- 1:N
        for(i in 1:k) {
                Ni <- nrow(xs)
                inds <- 1:Ni
                W <- C.unstr(xs)
                out <- inds[W <= min(W)][1]
                Ds[i] <- W[out]
                outcands[i] <- outinds[out]
                xs <- xs[ - out,  ]
                outinds <- outinds[ - out]
        }
list(Ds=Ds,ind=outcands)
}

C.unstr <- function(x)
 {
 V <- var(x)
 M <- apply(x,2,mean)
 n <- nrow(x)
 1-(n/(n-1))*mahalanobis(x,M,(n-1)*V)
 }


