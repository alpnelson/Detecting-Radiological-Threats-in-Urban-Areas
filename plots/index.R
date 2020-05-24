setwd("C:\\Users\\tetteh\\Downloads\\sir porter/")

df= read.csv("SourceInfov3\\SourceData.csv")

dat = df[which(df$SourceType== "HEU" & df$Shielding==0),]

library(ggplot2)

ggplot(df, aes(PhotonEnergy, CountRate, color=factor(Shielding))) +
  
  geom_line() + facet_wrap(~paste(SourceID,SourceType, sep="-")) +
  
  scale_y_log10(limits=c(10^(-10), 10^6))

run1 = read.csv("C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_one/104901.csv", header = FALSE)  #SourceID 1
run2 = read.csv("training/training/source_two/105701.csv", header = FALSE)[(35326-1000):(35326+1000),]  #SourceID 2
run3 = read.csv("training/training/source_three/106501.csv", header = FALSE)  #SourceID 3
run4 = read.csv("training/training/source_four/108091.csv", header = FALSE)[(11941-1000):(11941+1000),]  #SourceID 4
run5 = read.csv("training/training/source_five/108101.csv", header = FALSE)[(51001-1000):(51001+1000),]  #SourceID 5
run0 = read.csv("C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_zero/100001.csv", header = FALSE)  #SourceID 0


### time at which the source was detected for each run( cummulative time in seconds)
t1= 78.3  ##75545
t2= 39.4  ##35326
t3= 44.5  ##37369
t4= 110.9 ##113941
t5= 52.4  ##51001

###cumulative time for each run
time1= round(cumsum(run1$V1)*10^(-6),digits = 1)
#time2= cumsum(run2$V1)*10^(-6)
time3= round(cumsum(run3$V1)*10^(-6),digits = 1)
time4= cumsum(run4$V1)*10^(-6)
time5= cumsum(run5$V1)*10^(-6)
time0= cumsum(run0$V1)*10^(-6)


eva_points = seq(11,4000,by=0.5)

bd0= hscv(run0$V2)
kde0= kde(run0$V2, H=bd0,eval.points = eva_points)
plot(kde0)


KL <- function(p0, p1, eps=1e-25){
  p0 = pmax(p0/sum(p0), eps)
  p1 = pmax(p1/sum(p1), eps)
  sum( p0 * (log(p0) - log(p1)) )
}



###kernel density estimation for each run
library(ks)


library(LaplacesDemon)

eva_points=B$energy
####calculate the kl divergence for source one to source zero
temp1 = list.files(path= "C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_one/",pattern="*.csv")

##loading data files, 100 for each
kl_di1=vector()
for (i in 1:100){
  pt = paste("C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_one/", temp1[i], sep = "/")
  runs = read.csv("SourceInfov3\\trainingAnswers.csv")
  
  run_id = read.csv(pt , header = FALSE)##individual runs
  time = round(cumsum(run_id$V1)*10^(-6),digits = 1)
  
  source_time1 =runs %>% filter(SourceID == 1) %>% pull(SourceTime)
  rn = match(source_time1[i],time)
  new_df = run_id[(rn-1000):(rn+1000),]$V2
  
  ##kernel density
  bd= hscv(new_df)
  kd= kde(new_df, H=bd,eval.points = eva_points)
  
  kl =KL(B$f0, kd$estimate)
  kl_di1 <- c(kl_di1,kl)
  #kg = append(kg, kl, after = length(kg))
 
}



####calculate the kl divergence for source one to source two
temp2 = list.files(path= "C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_two/",pattern="*.csv")

##loading data files, 100 for each
kl_di2=vector()
for (i in 1:100){
  pt = paste("C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_two/", temp2[i], sep = "/")
  runs = read.csv("SourceInfov3\\trainingAnswers.csv")
  
  run_id = read.csv(pt , header = FALSE)##individual runs
  time = round(cumsum(run_id$V1)*10^(-6),digits = 1)
  
  source_time1 =runs %>% filter(SourceID == 2) %>% pull(SourceTime)
  rn = match(source_time1[i],time)
  new_df = run_id[(rn-1000):(rn+1000),]$V2
  
  ##kernel density
  bd= hscv(new_df)
  kd= kde(new_df, H=bd,eval.points = eva_points)
  
  kl =KLD(B$f0, kd$estimate)$mean.sum.KLD
  kl_di2 <- c(kl_di2,kl)
  
  
}


###########################################################
####calculate the kl divergence for source one to source three
###########################################################
temp3 = list.files(path= "C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_three/",pattern="*.csv")

##loading data files, 100 for each
kl_di3=vector()
for (i in 1:100){
  pt = paste("C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_three/", temp3[i], sep = "/")
  runs = read.csv("SourceInfov3\\trainingAnswers.csv")
  
  run_id = read.csv(pt , header = FALSE)##individual runs
  time = round(cumsum(run_id$V1)*10^(-6),digits = 1)
  
  source_time1 =runs %>% filter(SourceID == 3) %>% pull(SourceTime)
  rn = match(source_time1[i],time)
  new_df = run_id[(rn-1000):(rn+1000),]$V2
  
  ##kernel density
  bd= hscv(new_df)
  kd= kde(new_df, H=bd,eval.points = eva_points)
  
  kl =KLD(B$f0, kd$estimate)$mean.sum.KLD
  kl_di3 <- c(kl_di3,kl)
  
  
}



###########################################################
####calculate the kl divergence for source one to source four
###########################################################
temp4 = list.files(path= "C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_four/",pattern="*.csv")

##loading data files, 100 for each
kl_di4=vector()
for (i in 1:100){
  pt = paste("C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_four/", temp4[i], sep = "/")
  runs = read.csv("SourceInfov3\\trainingAnswers.csv")
  
  run_id = read.csv(pt , header = FALSE)##individual runs
  time = round(cumsum(run_id$V1)*10^(-6),digits = 1)
  
  source_time1 =runs %>% filter(SourceID == 4) %>% pull(SourceTime)
  rn = match(source_time1[i],time)
  new_df = run_id[(rn-1000):(rn+1000),]$V2
  
  ##kernel density
  bd= hscv(new_df)
  kd= kde(new_df, H=bd,eval.points = eva_points)
  
  kl =KLD(B$f0, kd$estimate)$mean.sum.KLD
  kl_di4 <- c(kl_di4,kl)
  
  
}



###########################################################
####calculate the kl divergence for source one to source five
###########################################################
temp5 = list.files(path= "C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_five",pattern="*.csv")

##loading data files, 100 for each
kl_di5=vector()
for (i in 1:100){
  pt = paste("C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_five/", temp5[i], sep = "/")
  runs = read.csv("SourceInfov3\\trainingAnswers.csv")
  
  run_id = read.csv(pt , header = FALSE)##individual runs
  time = round(cumsum(run_id$V1)*10^(-6),digits = 1)
  
  source_time1 =runs %>% filter(SourceID == 5) %>% pull(SourceTime)
  rn = match(source_time1[i],time)
  new_df = run_id[(rn-1000):(rn+1000),]$V2
  
  ##kernel density
  bd= hscv(new_df)
  kd= kde(new_df, H=bd,eval.points = eva_points)
  
  kl =KLD(B$f0, kd$estimate)$mean.sum.KLD
  kl_di5 <- c(kl_di5,kl)
  
  
}


temp0 = list.files(path= "C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_zero",pattern="*.csv")

##loading data files, 100 for each
kl_di0=vector()
for (i in 1:100){
  pt = paste("C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_zero/", temp0[i], sep = "/")
  runs = read.csv("SourceInfov3\\trainingAnswers.csv")
  
  run_id = read.csv(pt , header = FALSE)##individual runs
  time = round(cumsum(run_id$V1)*10^(-6),digits = 1)
  
  source_time1 =runs %>% filter(SourceID == 0) %>% pull(SourceTime)
  rn = 2000
  new_df = run_id[(rn-1000):(rn+1000),]$V2
  
  ##kernel density
  bd= hscv(new_df)
  kd= kde(new_df, H=bd,eval.points = eva_points)
  
  kl =KLD(B$f0, kd$estimate)$sum.KLD.px.py
  kl_di0 <- c(kl_di0,kl)
  
  
}



#######kernel density estimate for Source 0
bd0= hscv(kl_di0)
kde00= kde(kl_di0, H=bd0)
plot(kde00, main= "Source Zero")
rug(kl_di0)


#######kernel density estimate for Source 1
bd1= hscv(kl_di1)
kde1= kde(kl_di1, H=bd1)
plot(kde1, main= "Source one")
#rug(kl_di1)

#######kernel density estimate for Source 2
bd2= hscv(kl_di2)
kde2= kde(kl_di2, H=bd2)
plot(kde2, main= "Source two")
#rug(kl_di2)

#######kernel density estimate for Source 3
bd3= hscv(kl_di3)
kde3= kde(kl_di3, H=bd3)
plot(kde3, main= "Source three")

#######kernel density estimate for Source 4
bd4= hscv(kl_di4)
kde4= kde(kl_di4, H=bd4)
plot(kde4, main= "Source four")


#######kernel density estimate for Source 5
bd5= hscv(kl_di5)
kde5= kde(kl_di5, H=bd5)
plot(kde5, main= "Source five")
