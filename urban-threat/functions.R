## FUNCTIONS



#-- score_run()
#---------------------------------------------------------------------#
# Assigns a log density ratio score to every observation
#
# Because we only evaluate energy levels at discrete values, the energy
# is rounded to the nearest integer and constrained to be within the 
# range of 11 through 4001. 
# Then the log density ratio score, for every source, is retrieved for 
#  the energy levels in the runs.
#---------------------------------------------------------------------#
score_run <- function(X, R){
  X %>% mutate(energy = round(energy/.5)*.5,  # round to 1/2
               energy = pmax(energy, 11),   # ensure at least 11
               energy = pmin(energy, 4001)) %>%  # ensure at most 4001
    left_join(R, by="energy") 
}


#-- smooth()
#---------------------------------------------------------------------#
# Run kernel regression on the log density ratios
#
# Main idea is to find the time point that has the largest log density
# score, which indicates the sensor is close to a specific source. 
# Inputs:
#   x: vector of times
#   Y: matrix of log density scores. One column per source.
#   bw: bandwidth for kernel smoothing. I think its sd of normal kernel, 
#       this wasn't initially apparent from the locpoly() function
#   ngrids: number of grid points at which to return estimates. This is
#       only set to reduce time for computation. The default is to calculate
#       every .05 seconds. 
# Outputs:
#   matrix with:
#     - first column the times at which estimate were made
#     - other columns for each SourceID
# Notes:
#   - requires KernSmooth function locpoly()
#   - to better identify the time the source is closest, we may want
#     to up ngrids?
#   - only returns values in [30, max(T)-4] seconds. 
#     Since no event <30 and edge effects can impact this implementation
#     restrict source to be located within 4 seconds of end of run. 
#---------------------------------------------------------------------#

smooth <- function(x, Y, bw=2, ngrids=NULL){
  xrng = c(25, max(x))  # no sources within first 30 seconds
  if(is.null(ngrids)) ngrids = 1+min(max(diff(xrng)/.025, 2500),6000) %>% ceiling()
  YHAT = matrix(0, ngrids, ncol(Y)); colnames(YHAT) = colnames(Y)
  for(j in 1:ncol(Y)){
    y = Y[,j,drop=TRUE]
    lp = KernSmooth::locpoly(x, y, bandwidth=bw, gridsize=ngrids, 
                             degree=1,
                             range.x=xrng, truncate=FALSE)  
    YHAT[,j] = lp$y
  }
  ok = (lp$x >= 30.0 & lp$x <= (max(x)-4) )
  cbind(x=lp$x[ok], YHAT[ok,])
}
