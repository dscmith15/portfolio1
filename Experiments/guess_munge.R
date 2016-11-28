setwd("/var/www/html/Experiments/")

source("rources/QualDownload.R") #to extract data from qualtrics
source("rources/skewperc.R") #to estimate percentiles given skewed data
source("rources/booto.R") #simple bootstraping function

Data1 <- read.csv("dataguess/MikkiData_Genre.csv")

setwd("/var/www/html/Experiments/dataguess")
Data2 <-QualDownload("survey1.json")
Data3 <-QualDownload("survey2.json")

names(Data2)[48:102]<-c("F1_U01","F1_U02","F1_U03", "F1_U04",
                        "F1_U05", "F1_U06", "F1_U07", "F1_U08",
                        "F1_U09", "F1_U10", "F1_U11", "F2_N01",
                        "F2_N02","F2_N03", "F2_N04",
                        "F2_N05", "F2_N06", "F2_N07",
                        "F3_PI01","F3_PI02","F3_PI03", "F3_PI04",
                        "F3_PI05", "F3_PI06", "F3_PI07", "F3_PI08",
                        "F4_Enj01","F4_Enj02","R_F4_Enj03", "F4_Enj04",
                        "F4_Enj05", "F5_CF01","F5_CF02","F5_CF03", "F5_CF04",
                        "F5_CF05", "F5_CF06", "F5_CF07",
                        "F6_Aud01", "F6_Aud02", "F6_Aud03", "F6_Aud04",
                        "F7_CA01", "F7_CA02", "F7_CA03", "F7_CA04", "F7_CA05",
                        "F7_CA06", "F8_S01", "F8_S02", "F8_S03", "F8_S04",
                        "F9_V01","F9_V02","F9_V03")

names(Data3)[48:102]<-c("F1_U01","F1_U02","F1_U03", "F1_U04",
                        "F1_U05", "F1_U06", "F1_U07", "F1_U08",
                        "F1_U09", "F1_U10", "F1_U11", "F2_N01",
                        "F2_N02","F2_N03", "F2_N04",
                        "F2_N05", "F2_N06", "F2_N07",
                        "F3_PI01","F3_PI02","F3_PI03", "F3_PI04",
                        "F3_PI05", "F3_PI06", "F3_PI07", "F3_PI08",
                        "F4_Enj01","F4_Enj02","R_F4_Enj03", "F4_Enj04",
                        "F4_Enj05", "F5_CF01","F5_CF02","F5_CF03", "F5_CF04",
                        "F5_CF05", "F5_CF06", "F5_CF07",
                        "F6_Aud01", "F6_Aud02", "F6_Aud03", "F6_Aud04",
                        "F7_CA01", "F7_CA02", "F7_CA03", "F7_CA04", "F7_CA05",
                        "F7_CA06", "F8_S01", "F8_S02", "F8_S03", "F8_S04",
                        "F9_V01","F9_V02","F9_V03")

Data2 <- Data2[complete.cases(Data2$Q75),]
Data3 <- Data3[complete.cases(Data3$Q75),]

Usability_Data = rbind(Data1[,c("F1_U01","F1_U02","F1_U03", "F1_U04", "F1_U05", "F1_U06", "F1_U07", "F1_U08",
                              "F1_U09", "F1_U10", "F1_U11")],Data2[,c("F1_U01","F1_U02","F1_U03", "F1_U04", "F1_U05", "F1_U06", "F1_U07", "F1_U08",
                                                                      "F1_U09", "F1_U10", "F1_U11")],Data3[,c("F1_U01","F1_U02","F1_U03", "F1_U04", "F1_U05", "F1_U06", "F1_U07", "F1_U08",
                                                                                                              "F1_U09", "F1_U10", "F1_U11")])
Narrative_Data = rbind(Data1[,c("F2_N01", "F2_N02","F2_N03", "F2_N04", "F2_N05", "F2_N06", "F2_N07")],Data2[,c("F2_N01", "F2_N02","F2_N03", "F2_N04", "F2_N05", "F2_N06", "F2_N07")],Data3[,c("F2_N01", "F2_N02","F2_N03", "F2_N04", "F2_N05", "F2_N06", "F2_N07")])
Play_Data = rbind(Data1[,c("F3_PI01","F3_PI02","F3_PI03", "F3_PI04","F3_PI05", "F3_PI06", "F3_PI07", "F3_PI08")],Data2[,c("F3_PI01","F3_PI02","F3_PI03", "F3_PI04","F3_PI05", "F3_PI06", "F3_PI07", "F3_PI08")],Data3[,c("F3_PI01","F3_PI02","F3_PI03", "F3_PI04","F3_PI05", "F3_PI06", "F3_PI07", "F3_PI08")])
Enjoyment_Data = rbind(Data1[,c("F4_Enj01","F4_Enj02","R_F4_Enj03", "F4_Enj04", "F4_Enj05")],Data2[,c("F4_Enj01","F4_Enj02","R_F4_Enj03", "F4_Enj04", "F4_Enj05")],Data3[,c("F4_Enj01","F4_Enj02","R_F4_Enj03", "F4_Enj04", "F4_Enj05")])
Creative_Data = rbind(Data1[,c("F5_CF01","F5_CF02","F5_CF03", "F5_CF04", "F5_CF05", "F5_CF06", "F5_CF07")],Data2[,c("F5_CF01","F5_CF02","F5_CF03", "F5_CF04", "F5_CF05", "F5_CF06", "F5_CF07")],Data3[,c("F5_CF01","F5_CF02","F5_CF03", "F5_CF04", "F5_CF05", "F5_CF06", "F5_CF07")])
Audio_Data = rbind(Data1[,c("F6_Aud01", "F6_Aud02", "F6_Aud03", "F6_Aud04")],Data2[,c("F6_Aud01", "F6_Aud02", "F6_Aud03", "F6_Aud04")],Data3[,c("F6_Aud01", "F6_Aud02", "F6_Aud03", "F6_Aud04")])
Grat_Data = rbind(Data1[,c("F7_CA01", "F7_CA02", "F7_CA03", "F7_CA04", "F7_CA05", "F7_CA06")],Data2[,c("F7_CA01", "F7_CA02", "F7_CA03", "F7_CA04", "F7_CA05", "F7_CA06")],Data3[,c("F7_CA01", "F7_CA02", "F7_CA03", "F7_CA04", "F7_CA05", "F7_CA06")])
Social_Data = rbind(Data1[,c("F8_S01", "F8_S02", "F8_S03", "F8_S04")],Data2[,c("F8_S01", "F8_S02", "F8_S03", "F8_S04")],Data3[,c("F8_S01", "F8_S02", "F8_S03", "F8_S04")])
Aes_Data = rbind(Data1[,c("F9_V01","F9_V02","F9_V03")],Data2[,c("F9_V01","F9_V02","F9_V03")],Data3[,c("F9_V01","F9_V02","F9_V03")])
Title = data.frame(c(as.character(Data1$Title),as.character(Data2$GameName),as.character(Data3$GameName)))
Overall = data.frame(c(as.integer(Data1[,"Overall"]),as.integer(Data2[,"Q75"]),as.integer(Data3[,"Q75"])))
Device = data.frame(c(as.integer(Data1[,"Device"]),as.integer(Data2[,"Device4Game"]),as.integer(Data3[,"Device4Game"])))

names(Title)<-"Title"
names(Overall)<-"Overall"
names(Device)<-"Device"

#Replace 8's with NA
Usability_Data[Usability_Data==8]<-NA
Narrative_Data[Narrative_Data==8]<-NA
Play_Data[Play_Data==8]<-NA
Enjoyment_Data[Enjoyment_Data==8]<-NA
Creative_Data[Creative_Data==8]<-NA
Audio_Data[Audio_Data==8]<-NA
Social_Data[Social_Data==8]<-NA
Grat_Data[Grat_Data==8]<-NA
Aes_Data[Aes_Data==8]<-NA

#We then take that data and summarize by row mean as Mikki explains in her diss
Usability_Means = rowMeans(Usability_Data, na.rm = TRUE, dims = 1)
Narrative_Means = rowMeans(Narrative_Data, na.rm = TRUE, dims = 1)
Play_Means = rowMeans(Play_Data, na.rm = TRUE, dims = 1)
Enjoyment_Means = rowMeans(Enjoyment_Data, na.rm = TRUE, dims = 1)
Creative_Means = rowMeans(Creative_Data, na.rm = TRUE, dims = 1)
Audio_Means = rowMeans(Audio_Data, na.rm = TRUE, dims = 1)
Grat_Means = rowMeans(Grat_Data, na.rm = TRUE, dims = 1)
Social_Means = rowMeans(Social_Data, na.rm = TRUE, dims = 1)
Aes_Means = rowMeans(Aes_Data, na.rm = TRUE, dims = 1)
GUESS = Usability_Means+Narrative_Means+Play_Means+Enjoyment_Means+Creative_Means+Audio_Means+Grat_Means+Social_Means+Aes_Means

#Now merge these column vectors into one data frame that sumarizes each attribute

Guess_Frame <- data.frame(GUESS,Usability_Means,Narrative_Means,Play_Means,Enjoyment_Means,Creative_Means,
                             Audio_Means,Grat_Means,Social_Means,Aes_Means,Title, Overall, Device)

#Bootstrap paramters

bootframe <-booto(10000, 100, Guess_Frame$GUESS)

print("All done!")
