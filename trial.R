setwd("C:\\Users\\tetteh\\Downloads\\sir porter/")

df= read.csv("SourceInfov3\\SourceData.csv")

dat = df[which(df$SourceType== "HEU" & df$Shielding==0),]

library(ggplot2)

ggplot(df, aes(PhotonEnergy, CountRate, color=factor(Shielding))) +
  
  geom_line() + facet_wrap(~paste(SourceID,SourceType, sep="-")) +
  
  scale_y_log10(limits=c(10^(-10), 10^6))

run1 = read.csv("C:\\Users\\tetteh\\Downloads\\sir porter\\training\\training\\source_one/104901.csv", header = FALSE)  #SourceID 1
#run2 = read.csv("training/training/105701.csv", header = FALSE)[(35326-1000):(35326+1000),]  #SourceID 2
run3 = read.csv("training/training/106501.csv", header = FALSE)  #SourceID 3
run4 = read.csv("training/training/108091.csv", header = FALSE)[(11941-1000):(11941+1000),]  #SourceID 4
run5 = read.csv("training/training/108101.csv", header = FALSE)[(51001-1000):(51001+1000),]  #SourceID 5
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

round(123.456,digits=2)

###kernel density estimation for each run
library(ks)


library(LaplacesDemon)


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
  kd= kde(new_df, H=bd)
  
  kl =KLD(kde0$estimate, kd$estimate)$mean.sum.KLD
  kl_di1 <- c(kl_di1,kl)
  #kg = append(kg, kl, after = length(kg))
 
}



####calculate the kl divergence for source one to source zero
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
  kd= kde(new_df, H=bd)
  
  kl =KLD(kde0$estimate, kd$estimate)$mean.sum.KLD
  kl_di2 <- c(kl_di2,kl)
  #kg = append(kg, kl, after = length(kg))
  
}





#######kernel density estimate for Source 1
bd1= hscv(kl_di1)
kde1= kde(kl_di1, H=bd1)
plot(kde1)

#######kernel density estimate for Source 1
bd2= hscv(kl_di2)
kde2= kde(kl_di2, H=bd2)
plot(kde2)

