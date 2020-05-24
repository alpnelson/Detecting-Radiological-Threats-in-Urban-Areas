#---------------------------------------------------------------------#
#-- Set Directories
#---------------------------------------------------------------------#


#-- Set (relative) paths to data files
# dir.train =  "../data/training"  # path to (runs) training data files
dir.test =   "data/testing"   # path to (runs) testing data files
output_file = "solution.csv"     # path + filename for output file
# dir.source = "mydata"            # path to 'SourceData.csv' file
# dir.train_ans = "mydata"         # path to 'trainingAnswers.csv'


#-- If run as an Rscript, then expecting two inputs:
#   <data_folder> <output_file>
args = commandArgs(trailingOnly = TRUE)

if(length(args)==0){
  print("No arguments supplied.")
} else{
  dir.test = args[1]       # <data_folder>
  output_file = args[2]    # <output_file>
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
#library(tidyverse)
library(dplyr); library(readr); library(stringr); library(tidyr)
library(KernSmooth)
library(ks)


#---------------------------------------------------------------------#
#-- Get Functions and Training Data
#---------------------------------------------------------------------#



#-- Get data and functions
#source("train.R")
source("functions.R")


#-- Get Z and R
R = read_rds("R.rds")
Z = read_rds("Z.rds")



#-- Get test runid's
RunID = sort(list.files(dir.test, pattern=".csv")) %>% 
  str_remove(".csv")

#-- Set bandwidths
BW = c(0.5, 0.75, 1, 1.25, 1.5)


#---------------------------------------------------------------------#
#-- Score Testing Data
#---------------------------------------------------------------------#

#-- Evaluate Runs
OUT = tibble()

pb = progress_estimated(length(RunID))
for(i in 1:length(RunID)){
  #-- Read in runs data
  runid = RunID[i]
  f = paste0(file.path(dir.test, runid), ".csv")
  X = read_csv(f, col_names=FALSE, col_types="nn") %>% 
    transmute(time = cumsum(X1)/10^6, energy=X2)
  
  #-- Score runs 
  A = score_run(X, R)   

  out.j = tibble()
  for(j in 1:length(BW)){
    sm = smooth(A$time, select(A, -time, -energy), bw=BW[j])
    tau.hat = sm[apply(sm[,-1], 2, which.max),1]
    score = apply(sm[,-1], 2, max)
    out = tibble(runid, bw=BW[j],    # runid=id
                 SourceID=colnames(sm)[-1],
                 tau.hat, 
                 score)
    out.j = bind_rows(out.j, out)
  }
  out = out.j %>% left_join(Z, by=c("bw", "SourceID")) %>% 
    mutate(Z = (score - mu)/sd) %>% 
    group_by(SourceID) %>% 
    ## Use bw with *max* Z 
    summarize(
      runid = runid[1],
      tau.hat = tau.hat[which.max(Z)],
      bw = bw[which.max(Z)],
      Z = max(Z),
      score = score[which.max(Z)]) %>%
    filter(Z == max(Z)) %>% 
    select(runid, bw, SourceID, tau.hat, score, Z)
    OUT = bind_rows(OUT, out)
  pb$tick()$print()
}



#-- Make submission data

thres = 2.21

solution = OUT %>% mutate(source = Z >= thres) %>% 
  # mutate(runid = RunID) %>%   
  mutate(SourceID = stringr::str_sub(SourceID, 1, 1),
         SourceID = ifelse(source, SourceID, 0L), 
         SourceTime = ifelse(source, tau.hat, 0.00)) %>% 
  select(RunID = runid, SourceID, SourceTime)


#-- save solution as output_file
write_csv(solution, output_file) 

## Or write to screen:
# format_csv(solution) %>% cat






