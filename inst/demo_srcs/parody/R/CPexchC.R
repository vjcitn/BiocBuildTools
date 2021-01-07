MLvarcov <- function(x) {
 n <- nrow(x)
 (n-1)*var(x)/n
 }

MLvarcov.di <- function(x, i) {
 MLvarcov(x[-i,])
 }

# following is nice for using R resources but quite slow
#C.exch <- function(x)
# {
## returns an N vector
# .Call("CPexchC", as.double(x), ncol(x), nrow(x), body(MLvarcov.di), 
#    body(MLvarcov), new.env())
# }

# following is much faster

#CC.exch <- function (x)
#{
#    if (!is.matrix(x))
#        stop("need mat")
#    n <- nrow(x)
#    nc <- ncol(x)
#    ans1 <- rep(0, nc)
#    ans2 <- x-x
#    ans3 <- matrix(0,nc,nc)
#    ans4 <- rep(0,n)
#    out <- .C("CCexch", as.integer(n), as.integer(nc), as.double(x),
#        as.double(ans1), demeaned=as.double(ans2),varcov=as.double(ans3),
#        ans=ans4)
#    out$ans
#}

# following is faster, no C, but proper updating

CCC.exch <- function(x) {
 N <- nrow(x)
 ans <- rep(NA,N)
 Ccalc <- ddcovGen(x)
 for (i in 1:N) ans[i] <- Ccalc(i)
 ans
}

ddcovGen <- function(x) {
#
# build up state regarding x
#
 N <- nrow(x)
 p <- ncol(x)
 fssq <- (N-1)*var(x)
 gmlv <- fssq/N
 gsv <- sum(gmlv)
 trsv <- sum(diag(gmlv))
 gr <- (gsv-trsv)/((p-1)*trsv)
 gdetr <- (1-gr)^(p-1)*(1+(p-1)*gr)
 gs2 <- trsv/p
 mx <- apply(x,2,mean)
#
# now produce closure with information
# suitable for downdating upon removing i
#
 function(i) {
   om <- matrix(x[i,],ncol=1)
   lm <- matrix((N*mx - om)/(N-1),ncol=1)
   off <- ((N-1)/N)* (om-lm)%*%t(om-lm)
   mlv <- (fssq - off)/(N-1)
   trv <- sum(diag(mlv))
   r <- (sum(mlv)-trv)/((p-1)*trv)
   s2ml <- trv/p
   detr <- (1-r)^(p-1)*(1+(p-1)*r)
   (s2ml/gs2)^p*(detr/gdetr)
 }
}

#
# following is fastest, above procedure coded in C

#DDC.exch <- function (x)
#{
#    if (!is.matrix(x))
#        stop("need mat")
#    n <- nrow(x)
#    nc <- ncol(x)
#    ans1 <- rep(0, nc)
#    ans1b <- rep(0, nc)
#    ans1c <- rep(0, nc)
#    ans2 <- x-x
#    ans3 <- matrix(0,nc,nc)
#    ans3b <-  matrix(0, nc, nc)
#    ans3c <-  matrix(0, nc, nc)
#    ans3d <- matrix(0,nc,nc)
#    ans4 <- rep(0,n)
#    out <- .C("ddDetRat", as.integer(n), as.integer(nc), as.double(x),
#        as.double(ans3), px1=as.double(ans1),nx1=as.double(ans4),
#        as.double(ans3b), as.double(ans1b), as.double(ans1c), 
#        as.double(ans3c), as.double(ans3d))
#    out$nx1
#}


# following are functions to HELP verify above

tr <- function(x) sum(diag(x))

mlvar <- function(x) (nrow(x)-1)*var(x)/nrow(x)

rhoEx <- function(x) {
 v <- mlvar(x)
 trv <- tr(v)
 p <- ncol(x)
 (sum(v) - trv)/((p-1)*trv)
 }


vcovEx <- function(x) {
 R <- matrix(rhoEx(x),ncol(x),ncol(x))
 diag(R) <- 1
 mlvar(x)* R
}



detR <- function(x) {
 r <- rhoEx(x)
 p <- ncol(x)
 (1-r)^(p-1)*(1+(p-1)*r)
 }

s2ml <- function(x)
 tr(mlvar(x))/ncol(x)


#Cdemean <- function (x,i) 
#{
#    if (!is.matrix(x)) 
#        stop("need mat")
#    if (i >= nrow(x)) stop("i must be zero-based")
#    n <- nrow(x)
#    nc <- ncol(x)
#    ans1 <- rep(0, nc)
#    ans2 <- x-x
#    out <- .C("demean", as.integer(n), as.integer(nc), as.double(x), 
#        as.double(ans1), demeaned=as.double(ans2), as.integer(i))
#    matrix(out$demeaned,ncol=ncol(x))
#}
#Cvarcov <- function (x,i) 
#{
#    if (!is.matrix(x)) 
#        stop("need mat")
#    if (i >= nrow(x)) stop("i must be zero-based")
#    n <- nrow(x)
#    nc <- ncol(x)
#    ans1 <- rep(0, nc)
#    ans2 <- x-x
#    ans3 <- matrix(0,nc,nc)
#    out <- .C("varcov", as.integer(n), as.integer(nc), as.double(x), 
#        as.double(ans1), demeaned=as.double(ans2),varcov=as.double(ans3),as.integer(i))
#    matrix(out$varcov,ncol=ncol(x))
#}
#CrhoML <- function (x,i) 
#{
#    if (!is.matrix(x)) 
#        stop("need mat")
#    n <- nrow(x)
#    nc <- ncol(x)
#    ans1 <- rep(0, nc)
#    ans2 <- x-x
#    ans3 <- matrix(0,nc,nc)
#    out <- .C("rhoEx", as.integer(n), as.integer(nc), as.double(x), 
#        as.double(ans1), demeaned=as.double(ans2),varcov=as.double(ans3),
#        rho=double(1),as.integer(i))
#    out$rho
#}
#CdetR <- function (x,i) 
#{
#    if (!is.matrix(x)) 
#        stop("need mat")
#    n <- nrow(x)
#    nc <- ncol(x)
#    ans1 <- rep(0, nc)
#    ans2 <- x-x
#    ans3 <- matrix(0,nc,nc)
#    out <- .C("detREx", as.integer(n), as.integer(nc), as.double(x), 
#        as.double(ans1), demeaned=as.double(ans2),varcov=as.double(ans3),
#        rho=double(1),det=double(1),as.integer(i))
#    out$det
#}
#
#s2ML <- function (x,i)
#{
#    if (!is.matrix(x))
#        stop("need mat")
#    n <- nrow(x)
#    nc <- ncol(x)
#    ans1 <- rep(0, nc)
#    ans2 <- x-x
#    ans3 <- matrix(0,nc,nc)
#    out <- .C("s2ML", as.integer(n), as.integer(nc), as.double(x),
#        as.double(ans1), demeaned=as.double(ans2),varcov=as.double(ans3),
#        rho=double(1), as.integer(i))
#    out$rho
#}


# */

#CPexchC <- function(x)
# {
## returns an N vector
# .Call("CPexchC", as.double(x), ncol(x), nrow(x), body(MLvarcov.di), 
#    body(MLvarcov), new.env())
# }

vtrace <- function(x) sum(diag(x))

#s2 <- function(x) vtrace(cpvar(x))/ncol(x)

d1S <- function(x,i) {n <- nrow(x); (n-2)*var(x[-i,])/(n-1) }

d1s2 <- function(x,i) { p<-ncol(x); n<-nrow(x); 
       (n-1)*vtrace(d1S(x,i))/(n*p) }

