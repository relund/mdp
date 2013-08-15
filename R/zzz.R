
# .First.lib <- function(lib, pkg) {
# 	library.dynam("MDP", pkg, lib)
# 	.Call("MDP_Init", PACKAGE="MDP")
# }



.onLoad<-function(libname, pkgname){
  .Call("MDP_Init", PACKAGE="MDP")
  #packageStartupMessage( "so I will print these lines each time you load it")
}
