dyn.load("C:/HaskellProjects/inliner3/.stack-work/install/b7328101/lib/inliner3.dll")
.C("HsStart")

.Call("getDim", matrix(2, nrow=2, ncol=3))

dyn.unload("C:/HaskellProjects/inliner3/.stack-work/install/b7328101/lib/inliner3.dll")
