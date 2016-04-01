## Excercise 7.3 in Tijms, H.C., "A first course in stochastic models", John Wiley & Sons Ltd, 2003.
## The semi-MDP is specified using binaryMDPWriter and actions with id and pr

tau<-matrix(c(
   NA, 3, 6, 3, 2,
   4, NA, 1, 7, 5,
   5, 1, NA, 6, 8,
   3, 8, 5, NA, 2,
   2, 5, 9, 2, NA
   ), nrow=5, byrow=T)
xi<-matrix(c(
   NA, 8, 12, 6, 6,
   20, NA, 2, 14, 16,
   16, 2, NA, 18, 16,
   6, 10, 20, NA, 6,
   8, 16, 20, 8, NA
   ), nrow=5, byrow=T)


w<-binaryMDPWriter("hct73v2_", getLog = FALSE)
w$setWeights(c("Duration","Net reward"))
w$process()
   w$stage()
      for (i in 1:5) {
         w$state(label=i)
            for (a in 1:5) {
               if (a!=i) {
                  w$action(label=a, weights=c(tau[i,a], xi[i,a]), id=a-1, pr=1, end=T)
               }
            }
         w$endState()
      }
   w$endStage()
w$endProcess()
w$closeWriter()
