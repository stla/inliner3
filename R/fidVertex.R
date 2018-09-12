dll <- "C:/HaskellProjects/inliner3/.stack-work/install/b7328101/lib/inliner3.dll"
#dll <- "C:/HaskellProjects/inliner3/mydll.dll"
dyn.load(dll)
.C("HsStart")
set.seed(123) # 3141592 for p=128 - not sure
p <- 8L
vt1 <- matrix(rnorm(3*p), nrow=3) -> VT1
cc1 <- matrix(sample.int(p, 3*p, replace=TRUE), nrow=3) -> CC1
vtsum <- rnorm(p) -> VTsum
U <- 2; L <- -2
k <- 1L
.C("fidVertex", list(c(vt1)), p, list(c(cc1)-1L), list(vtsum), U, L, 3L, 3L*p, result=list(0L))$result
.Call("fidVertex2", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p)
.Call("fidVertex3", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p)
.Call("fidVertex4", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p, k-1L)
.Call("fidVertex5", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p, k-1L)

f <- function(vt1, p, cc1, vtsum, U, L, Dim, n, k){
  h <- .Call("fidVertex5", vt1, p, cc1, vtsum, U, L, Dim, n, k)
  list(
    CCtemp = matrix(h[[3L]], nrow=Dim), 
    VTtemp = matrix(h[[2L]], nrow=Dim)
  )
}

ff <- function(vt1, p, cc1, vtsum, U, L, Dim, n, k){
  h <- .Call("fidVertex5", vt1, p, cc1, vtsum, U, L, Dim, n, k)
  CCtemp <- matrix(h[[4L]], nrow=3L)
  dim(CCtemp) <- c(Dim, h[[2]])
  VTtemp <- matrix(h[[3L]], nrow=3L)
  dim(VTtemp) <- c(Dim, h[[2]])
  list(
    CCtemp = CCtemp, 
    VTtemp = VTtemp
  )
}

#fidVertex _vt1 _p _cc1 _vtsum _u _v _dim _n _result = do
dyn.unload(dll)


source("~/Rstuff/FiducialSamsung/RFiducial/GFIMM_v02.R")
library(microbenchmark)
microbenchmark(
  R = fid_vertex(vt1, cc1, vtsum, U, L, 0L, 3L, k=1L, n=3L*p),
  H1 = .C("fidVertex", list(c(vt1)), p, list(c(cc1)-1L), list(vtsum), U, L, 3L, 3L*p, result=list(0L))$result,
  H2 = .Call("fidVertex2", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p),
  H3 = .Call("fidVertex3", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p),
  H4 = .Call("fidVertex4", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p, k-1L),
  H5 = .Call("fidVertex5", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p, k-1L),
  H = .Call("fidVertex5", vt1, p, cc1, vtsum, U, L, 3L, 3L*p, k),
  # f = f(vt1, p, cc1, vtsum, U, L, Dim=3L, n=3L*p, k),
  # ff = ff(vt1, p, cc1, vtsum, U, L, Dim=3L, n=3L*p, k),
  times = 500
)

#############
library(microbenchmark)
x <- rnorm(5000)
microbenchmark(
  R = 2*x,
  H1 = .Call("double1", x),
  H2 = .Call("double2", x),
  H3 = .Call("double3", x), 
  H4 = .Call("double4", x), 
  H5 = .Call("double5", x),
  H6 = .Call("double6", x),
  H7 = .Call("double7", x),
  H8 = .Call("double8", x),
  times = 1000
)

##############
x <- rnorm(300)
microbenchmark(
  matrix = matrix(x, nrow=3L),
  dim = {dim(x) <- c(3L, 100L)},
  times=500
)

##############
loop <- function(n){
  x <- 0L
  for(i in 1:n){
    x <- x+1L
  }
  return(x)
}
n <- 100000L
microbenchmark(
  R = loop(n),
  H = .Call("loop", n),
  times = 200
)