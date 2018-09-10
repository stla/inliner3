dll <- "C:/HaskellProjects/inliner3/.stack-work/install/b7328101/lib/inliner3.dll"
dyn.load(dll)
.C("HsStart")
set.seed(666)
p <- 128L
vt1 <- matrix(rnorm(3*p), nrow=3) -> VT1
cc1 <- matrix(sample.int(3*p, 3*p, replace=TRUE), nrow=3) -> CC1
vtsum <- rnorm(p) -> VTsum
U <- 2; L <- 1
k <- 1L
.C("fidVertex", list(c(vt1)), p, list(c(cc1)-1L), list(vtsum), U, L, 3L, 3L*p, result=list(0L))$result
.Call("fidVertex2", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p)
.Call("fidVertex3", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p)
.Call("fidVertex4", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p, k)

#fidVertex _vt1 _p _cc1 _vtsum _u _v _dim _n _result = do
dyn.unload(dll)


source("~/Rstuff/FiducialSamsung/RFiducial/GFIMM_v02.R")
library(microbenchmark)
microbenchmark(
  R = fid_vertex(vt1, cc1, vtsum, U, L, 0L, 3L, k=1L, n=3L*p),
  H = .C("fidVertex", list(c(vt1)), p, list(c(cc1)-1L), list(vtsum), U, L, 3L, 3L*p, result=list(0L))$result,
  H2 = .Call("fidVertex2", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p),
  H3 = .Call("fidVertex3", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p),
  H4 = .Call("fidVertex4", c(vt1), p, c(cc1)-1L, vtsum, U, L, 3L, 3L*p)
)