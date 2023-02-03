## Set working dir
fDir <- system.file("models", package = "MDP2")
wd <- setwd(tempdir())
## Convert the machine example to a hmp file
prefix1 <- paste0(fDir,"/machine1_")
getBinInfoStates(prefix1)
convertBinary2HMP(prefix1, duration = NULL, out = "machine1_converted.hmp")
# have a look at the hmp file
cat(readr::read_file("machine1_converted.hmp"))

## Convert the machine example hmp file to binary files
convertHMP2Binary(file = paste0(fDir,"/machine1.hmp"), prefix = "machine_cov_")
getBinInfoStates(prefix = "machine_cov_")
## Convert the machine example with a single dummy node to a hmp file
#convertBinary2HMP("machine2_")  # error since using scope = 3 not supported in hmp files

## Reset working dir
setwd(wd)
