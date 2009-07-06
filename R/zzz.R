.First.lib <- function(lib, pkg) {
	library.dynam("MDP", pkg, lib)
	.Call("MDP_Init")
}
