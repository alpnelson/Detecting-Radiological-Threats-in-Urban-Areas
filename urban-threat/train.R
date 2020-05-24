#---------------------------------------------------------------------#
#-- Set Directories
setwd("C:\\Users\\tetteh\\Downloads\\sir porter/")
#---------------------------------------------------------------------#


#-- The following directories need to be set

#-- Set (relative) paths to data files
dir.train =  "training/training"  # path to (runs) training data files
dir.source = "SourceInfov3"           # path to 'SourceData.csv' file
dir.train_ans = "SourceInfov3"     # path to 'trainingAnswers.csv'



#-- If run as an Rscript, then expecting two inputs:
#   <data_folder> <ground_truth_file>
args = commandArgs(trailingOnly = TRUE)

if(length(args)==0){
  print("No arguments supplied.")
} else{
  dir.train = args[1]       # <data_folder>
  dir.train_ans = args[2]   # <ground_truth_file>
}





#---------------------------------------------------------------------#
#-- Load R Packages
#---------------------------------------------------------------------#

#-- Install Packages if necessary
#packages <- c("tidyverse", "KernSmooth", "ks")
packages = c("dplyr","tibble", "tidyr", "readr", "stringr", "KernSmooth", "ks")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#-- Load Packages
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(KernSmooth)
library(ks)
library(tibble)

#-- Load functions
source("urban-threat/functions.R")

#---------------------------------------------------------------------#
#-- Source Data
#---------------------------------------------------------------------#

Source = read_csv(file.path(dir.source, "SourceData.csv")) %>% 
  mutate(PhotonEnergy = PhotonEnergy-1)  # note in competition says 13 really means [11-13]

#-- Create 6th Source, combination of 1 and 5 (HEU and 99mTc)
#   We don't know the combination, so just guess at 1/2
w = .50    # proportion 1


Source6 = Source %>% 
  filter(SourceID %in% c(1, 5)) %>%
  ## Standardize
  group_by(SourceID, Shielding) %>% 
  mutate(CountRate = CountRate/sum(CountRate)) %>% # standardize first
  group_by(Shielding, PhotonEnergy) %>% 
  summarize(CountRate = CountRate[SourceID==1]*w + CountRate[SourceID==5]*(1-w)) %>% 
  add_column(SourceType="HEU+99mTc", SourceID=6L, .before=1) %>% 
  ungroup()
  
Source = bind_rows(Source, Source6)





#-----------------------------------------------------------------------#
#-- Get density of Source at integers 11:4001
#   Estimate the density of the sources (1.0, 1.1, ..., 6.1)
#   at discrete energy values of {11, 12, ..., 4001}
#-----------------------------------------------------------------------#

## Interpolation of CountRate and standardize so results sum to 1
approx_df <- function(df, at=seq(11, 4001, by=.5)){  #at=11:4001
  tmp = approx(x=df$PhotonEnergy, y=df$CountRate, xout=at, 
               rule=2)  # constant at end points
  tmp$y = pmax(0, tmp$y)   # ensure non-negative
  as_tibble(tmp) %>% transmute(energy=x, f=y/sum(y))
}


#-- Estimated (standardized) density at discrete energy levels for all sources
#   SourceID = 'SourceID.Shielding'
S = Source %>% group_by(SourceID, Shielding) %>% do(approx_df(.)) %>% 
  ungroup() %>% unite(SourceID, SourceID, Shielding, sep=".") %>% 
  spread(SourceID, f)


#---------------------------------------------------------------------#
#-- Get the Runs Data
#---------------------------------------------------------------------#
runs = read_csv(file.path(dir.train_ans, "trainingAnswers.csv"), 
                col_types="ccd")


#---------------------------------------------------------------------#
#-- Background Density
#   Estimate the density in the baseline (no source) runs
#   Here I'm only using about 100 runs to make the estimate
#   The estimates are made at the same discrete energy levels as
#   used in to estimate the Source densities
#---------------------------------------------------------------------#

# #-- Set data dir
# data.dir = "data/training"

#-- Sample 100 null runs
set.seed(2019)
RunID = runs %>% filter(SourceID == 0) %>% pull(RunID) %>% 
  sample(size=100) %>% sort()

Y = tibble()
tt = LAM = numeric(length(RunID))
for(id in RunID){
  f = paste0(file.path(dir.train, id), ".csv")
  X = read_csv(f, col_names=FALSE, col_types="nn") %>%
    transmute(energy=X2) %>% count(energy) %>%
    add_column(RunID=id)
  Y = bind_rows(Y, X)

}

Y2 = Y %>% group_by(energy) %>% summarize(n = sum(n))

#-- Smooth with KDE
m0 = ks::kde(Y2$energy, h=1, w=Y2$n, 
             eval.points=seq(11, 4001, by=.5), 
             positive=FALSE) %>% 
             {tibble(PhotonEnergy=.$eval.points, density=.$estimate)} 


#-- Baseline (standardized) density
B = approx_df(rename(m0, CountRate=density))


#---------------------------------------------------------------------#
#-- Log Density Ratio
#   Estimate the log density ratio for comparing source to non-source
#   Each observations is either coming from the background or source, 
#   so the density ratio is either f.k(x)/f.0(x) or f.0(x)/f.0(x)=1.
#---------------------------------------------------------------------#

#-- Get density Ratios
dens_ratio <- function(f.k, f, eps=1e-20){
  logr = log(f.k + eps) - log(f + eps)  # add small eps so f is not too close to 0
  pmax(0, logr)                         # assume negative values imply non-source
}


#-- Matrix of log density ratio
R = left_join(S, B, by="energy") %>% 
  mutate_at(vars(-energy, -f), function(f.k) dens_ratio(f.k, .$f)) %>% 
  select(-f) 


#-- Output R
write_rds(R, "R.rds")


#---------------------------------------------------------------------#
#-- Estimate mu_0 and sigma_0 
#   Estimate the mean and standard deviation of the test statistics
#   under some random runs from the null model
#---------------------------------------------------------------------#


# #-- Set directory of training "runs" data
# data.dir = "data/training"


#-- Sample runs
n.sample = 900
set.seed(2019)
RunID = runs %>% filter(SourceID == 0) %>% pull(RunID) %>% 
  sample(size=n.sample) %>% sort()


#-- Set bandwidths
BW = c(.5, .75, 1, 1.25, 1.5)

#-- Evaluate Runs
OUT = tibble()

pb = progress_estimated(n.sample)
for(id in RunID){
  #-- Read in runs data
  f = paste0(file.path(dir.train, id), ".csv")
  tau = filter(runs, RunID == !!id) %>% pull(SourceTime)
  X = read_csv(f, col_names=FALSE, col_types="nn") %>% 
    transmute(time = cumsum(X1)/10^6, energy=X2)
  
  #-- Score runs 
  A = score_run(X, R)   
  for(j in 1:length(BW)){
    sm = smooth(A$time, select(A, -time, -energy), bw=BW[j])
    tau.hat = sm[apply(sm[,-1], 2, which.max),1]
    score = apply(sm[,-1], 2, max)
    out = tibble(runid=id, bw=BW[j], SourceID=colnames(sm)[-1], true.source=0,
                 tau.hat, dtau = tau - tau.hat,
                 score)
    OUT = bind_rows(OUT, out)
  }
  pb$tick()$print()
}


#-- Estimate distribution of score/Z under the null of no source
Z = OUT %>% group_by(SourceID, bw) %>% 
  summarize(mu = mean(score), sd=sd(score))


#-- Output Z
write_rds(Z, "Z.rds")


