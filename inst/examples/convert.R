library(XML)

## Set working dir
wd <- setwd(system.file("models", package = "MDP2"))

## Convert the machine example to a hmp file
prefix1 <- "machine1_"
getBinInfoStates(prefix1)
convertBinary2HMP(prefix1, duration = NULL)
# have a look at the hmp file
xmlTreeParse("machine1_converted.hmp", useInternalNodes = TRUE)

## Convert the machine example hmp file to binary files
prefix2 <- "machine_cov_"
convertHMP2Binary("machine1.hmp", prefix2)
getBinInfoStates(prefix2)
## Convert the machine example with a single dummy node to a hmp file
#convertBinary2HMP("machine2_")  # error since using scope = 3 not supported in hmp files

## Reset working dir
setwd(wd)
