setwd("C:\\Users\\tetteh\\Downloads\\sir porter/")

df= read.csv("SourceInfov3\\SourceData.csv")

id1 = df[which(df$SourceType== "HEU" & df$Shielding==0),][4:5]
id1_s = df[which(df$SourceType== "HEU" & df$Shielding==1),][4:5]

id2 = df[which(df$SourceType== "WGPu" & df$Shielding==0),][4:5]
id2_s = df[which(df$SourceType== "WGPu" & df$Shielding==1),][4:5]

id3 = df[which(df$SourceType== "131I" & df$Shielding==0),][4:5]
id3_s = df[which(df$SourceType== "131I" & df$Shielding==1),][4:5]

id4 = df[which(df$SourceType== "60Co" & df$Shielding==0),][4:5]
id4_s = df[which(df$SourceType== "60Co" & df$Shielding==1),][4:5]

