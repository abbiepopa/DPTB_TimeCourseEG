#Set-up
#Set Working directory
setwd("/Users/abbiepopa/Documents/Lab/DPTB/Time Course Eye Gaze/Data")
#Import Previous Dataset
AllDataTC25<-read.csv("AllDataTC25.csv",header=T)
#Trim the stupid rownames column
AllDataTC25<-AllDataTC25[2:24]

# import the data for the current participant
setwd("/Users/abbiepopa/Documents/Lab/DPTB/ParticipantData")
CurrPart<-read.csv("785.csv", header=TRUE)
setwd("/Users/abbiepopa/Documents/Lab/DPTB/Time Course Eye Gaze/Data")

# trim the columns to the ones you need
myCols<-c("Subject","ValidityLeftEye","ValidityRightEye","ProcCode","Display","AOI")
CurrPartCols<-CurrPart[myCols]

# trim the rows to the ones you need; here we remove all the epochs for which the facepair isn't on display and for which they have an emotional face. doing this before calculating validity means validity is a measure of how much data we are missing, not a measure of how well the participants do the task, since that validity measure could include all epochs
CurrPartColsRows<-subset(CurrPartCols, Display=="FacePair")
CurrPartColsRows<-subset(CurrPartColsRows, ProcCode!="NNL" )
CurrPartColsRows<-subset(CurrPartColsRows, ProcCode!="NNR" )
CurrPartColsRowsVal<-subset(CurrPartColsRows, ValidityLeftEye==0 & ValidityRightEye==0)

# split into angry trials and happy trials
CurrPartAngry<-subset(CurrPartColsRowsVal, ProcCode=="ANI" | ProcCode=="ANV" | ProcCode=="NAI" | ProcCode=="NAV")
CurrPartHappy<-subset(CurrPartColsRowsVal, ProcCode=="HNI" | ProcCode=="HNV" | ProcCode=="NHI" | ProcCode=="NHV")

# find numrow for each matrix
AngrySize<-nrow(CurrPartAngry)
HappySize<-nrow(CurrPartHappy)

#Split Angry into Angry Begin and Angry End -- 25%
NAngryBegin25<-AngrySize/4
NAngryEnd25<-AngrySize-NAngryBegin25

AngryBegin25<-CurrPartAngry[1:NAngryBegin25,]
AngryEnd25<-CurrPartAngry[NAngryEnd25:AngrySize,]

#Split Happy into Happy Begin and Happy End --25%
NHappyBegin25<-HappySize/4
NHappyEnd25<-HappySize-NHappyBegin25

HappyBegin25<-CurrPartHappy[1:NHappyBegin25,]
HappyEnd25<-CurrPartHappy[NHappyEnd25:HappySize,]

#Analysis of Angry Begin -- 25%
# %Angry
AngryBegin25_AngryLeft<-subset(AngryBegin25, AOI=="FaceLeft")
AngryBegin25_AngryLeft<-subset(AngryBegin25_AngryLeft, ProcCode=="ANI" | ProcCode=="ANV")
AngryBegin25_AngryRight<-subset(AngryBegin25, AOI=="FaceRight")
AngryBegin25_AngryRight<-subset(AngryBegin25_AngryRight, ProcCode=="NAI" | ProcCode=="NAV")
PercAngryBegin25_Angry<-(nrow(AngryBegin25_AngryLeft)+nrow(AngryBegin25_AngryRight))/nrow(AngryBegin25)

# %Neutral
AngryBegin25_NeutralLeft<-subset(AngryBegin25, AOI=="FaceLeft")
AngryBegin25_NeutralLeft<-subset(AngryBegin25_NeutralLeft, ProcCode=="NAI" | ProcCode=="NAV")
AngryBegin25_NeutralRight<-subset(AngryBegin25, AOI=="FaceRight")
AngryBegin25_NeutralRight<-subset(AngryBegin25_NeutralRight, ProcCode=="ANI" | ProcCode=="ANV")
PercAngryBegin25_Neutral<-(nrow(AngryBegin25_NeutralLeft)+nrow(AngryBegin25_NeutralRight))/nrow(AngryBegin25)

# %NonFace
PercAngryBegin25_NonFace<- 1-PercAngryBegin25_Angry - PercAngryBegin25_Neutral


#Analysis of Angry End -- 25%
# %Angry
AngryEnd25_AngryLeft<-subset(AngryEnd25, AOI=="FaceLeft")
AngryEnd25_AngryLeft<-subset(AngryEnd25_AngryLeft, ProcCode=="ANI" | ProcCode=="ANV")
AngryEnd25_AngryRight<-subset(AngryEnd25, AOI=="FaceRight")
AngryEnd25_AngryRight<-subset(AngryEnd25_AngryRight, ProcCode=="NAI" | ProcCode=="NAV")
PercAngryEnd25_Angry<-(nrow(AngryEnd25_AngryLeft)+nrow(AngryEnd25_AngryRight))/nrow(AngryEnd25)

# %Neutral
AngryEnd25_NeutralLeft<-subset(AngryEnd25, AOI=="FaceLeft")
AngryEnd25_NeutralLeft<-subset(AngryEnd25_NeutralLeft, ProcCode=="NAI" | ProcCode=="NAV")
AngryEnd25_NeutralRight<-subset(AngryEnd25, AOI=="FaceRight")
AngryEnd25_NeutralRight<-subset(AngryEnd25_NeutralRight, ProcCode=="ANI" | ProcCode=="ANV")
PercAngryEnd25_Neutral<-(nrow(AngryEnd25_NeutralLeft)+nrow(AngryEnd25_NeutralRight))/nrow(AngryEnd25)

# %NonFace
PercAngryEnd25_NonFace<- 1-PercAngryEnd25_Angry - PercAngryEnd25_Neutral

#Analysis of Happy Begin -- 25%
# %Happy
HappyBegin25_HappyLeft<-subset(HappyBegin25, AOI=="FaceLeft")
HappyBegin25_HappyLeft<-subset(HappyBegin25_HappyLeft, ProcCode=="HNI" | ProcCode=="HNV")
HappyBegin25_HappyRight<-subset(HappyBegin25, AOI=="FaceRight")
HappyBegin25_HappyRight<-subset(HappyBegin25_HappyRight, ProcCode=="NHI" | ProcCode=="NHV")
PercHappyBegin25_Happy<-(nrow(HappyBegin25_HappyLeft)+nrow(HappyBegin25_HappyRight))/nrow(HappyBegin25)

# %Neutral
HappyBegin25_NeutralLeft<-subset(HappyBegin25, AOI=="FaceLeft")
HappyBegin25_NeutralLeft<-subset(HappyBegin25_NeutralLeft, ProcCode=="NHI" | ProcCode=="NHV")
HappyBegin25_NeutralRight<-subset(HappyBegin25, AOI=="FaceRight")
HappyBegin25_NeutralRight<-subset(HappyBegin25_NeutralRight, ProcCode=="HNI" | ProcCode=="HNV")
PercHappyBegin25_Neutral<-(nrow(HappyBegin25_NeutralLeft)+nrow(HappyBegin25_NeutralRight))/nrow(HappyBegin25)

# %NonFace
PercHappyBegin25_NonFace<- 1-PercHappyBegin25_Happy - PercHappyBegin25_Neutral


#Analysis of Happy End -- 25%
# %Happy
HappyEnd25_HappyLeft<-subset(HappyEnd25, AOI=="FaceLeft")
HappyEnd25_HappyLeft<-subset(HappyEnd25_HappyLeft, ProcCode=="HNI" | ProcCode=="HNV")
HappyEnd25_HappyRight<-subset(HappyEnd25, AOI=="FaceRight")
HappyEnd25_HappyRight<-subset(HappyEnd25_HappyRight, ProcCode=="NHI" | ProcCode=="NHV")
PercHappyEnd25_Happy<-(nrow(HappyEnd25_HappyLeft)+nrow(HappyEnd25_HappyRight))/nrow(HappyEnd25)

# %Neutral
HappyEnd25_NeutralLeft<-subset(HappyEnd25, AOI=="FaceLeft")
HappyEnd25_NeutralLeft<-subset(HappyEnd25_NeutralLeft, ProcCode=="NHI" | ProcCode=="NHV")
HappyEnd25_NeutralRight<-subset(HappyEnd25, AOI=="FaceRight")
HappyEnd25_NeutralRight<-subset(HappyEnd25_NeutralRight, ProcCode=="HNI" | ProcCode=="HNV")
PercHappyEnd25_Neutral<-(nrow(HappyEnd25_NeutralLeft)+nrow(HappyEnd25_NeutralRight))/nrow(HappyEnd25)

# %NonFace
PercHappyEnd25_NonFace<- 1-PercHappyEnd25_Happy - PercHappyEnd25_Neutral

# For Data Cleaning at Some Point
PercAngryBegin25_nValRows<-nrow(AngryBegin25)
PercAngryEnd25_nValRows<-nrow(AngryEnd25)
PercHappyBegin25_nValRows<-nrow(HappyBegin25)
PercHappyEnd25_nValRows<-nrow(HappyEnd25)

# Calculate Change Scores (End-Begin)
PercAngryChange25_Angry<-PercAngryEnd25_Angry-PercAngryBegin25_Angry
PercAngryChange25_Neutral<-PercAngryEnd25_Neutral-PercAngryBegin25_Neutral
PercAngryChange25_NonFace<-PercAngryEnd25_NonFace-PercAngryBegin25_NonFace

PercHappyChange25_Happy<-PercHappyEnd25_Happy-PercHappyBegin25_Happy
PercHappyChange25_Neutral<-PercHappyEnd25_Neutral-PercHappyBegin25_Neutral
PercHappyChange25_NonFace<-PercHappyEnd25_NonFace-PercHappyBegin25_NonFace


#Put it all together
CurrPart25<-c(CurrPartColsRowsVal[1,1], PercAngryBegin25_Angry, PercAngryBegin25_Neutral, PercAngryBegin25_NonFace, PercAngryEnd25_Angry, PercAngryEnd25_Neutral, PercAngryEnd25_NonFace, PercHappyBegin25_Happy, PercHappyBegin25_Neutral, PercHappyBegin25_NonFace, PercHappyEnd25_Happy, PercHappyEnd25_Neutral, PercHappyEnd25_NonFace, PercAngryChange25_Angry, PercAngryChange25_Neutral, PercAngryChange25_NonFace, PercHappyChange25_Happy, PercHappyChange25_Neutral, PercHappyChange25_NonFace, PercAngryBegin25_nValRows, PercAngryEnd25_nValRows, PercHappyBegin25_nValRows, PercHappyEnd25_nValRows)  
#AllDataTC25<-t(as.data.frame(CurrPart25))
#TCColumnNames<-c("Subj", "PercAngryBegin25_Angry", "PercAngryBegin25_Neutral", "PercAngryBegin25_NonFace", "PercAngryEnd25_Angry", "PercAngryEnd25_Neutral", "PercAngryEnd25_NonFace", "PercHappyBegin25_Happy", "PercHappyBegin25_Neutral", "PercHappyBegin25_NonFace", "PercHappyEnd25_Happy", "PercHappyEnd25_Neutral", "PercHappyEnd25_NonFace", "PercAngryChange25_Angry", "PercAngryChange25_Neutral", "PercAngryChange25_NonFace", "PercHappyChange25_Happy", "PercHappyChange25_Neutral", "PercHappyChange25_NonFace", "PercAngryBegin25_nValRows", "PercAngryEnd25_nValRows", "PercHappyBegin25_nValRows", "PercHappyEnd25_nValRows")
#colnames(AllDataTC25)<-TCColumnNames
AllDataTC25<-rbind(AllDataTC25, CurrPart25)

AllDataTC25[,1]

write.csv(AllDataTC25,"AllDataTC25.csv")
setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")
DxCodes<-read.csv("DxCodes.csv",header=TRUE)
setwd("/Users/abbiepopa/Documents/Lab/DPTB/Time Course Eye Gaze/Data")
colnames(DxCodes)<-c("Subj","Dx")

AllDataTC25Dx<-merge(AllDataTC25,DxCodes)
write.csv(AllDataTC25Dx,"AllDataTC25Dx.csv")
AllDataTC25DxStats<-describeBy(AllDataTC25Dx, group=AllDataTC25Dx$Dx)
#AllDataTC25DxStatsExp<-do.call("rbind",AllDataTC25DxStats)

stats22qTC25<-AllDataTC25DxStats$'22q'[c("PercAngryChange25_Neutral","PercAngryChange25_NonFace","PercAngryChange25_Angry","PercHappyChange25_Neutral","PercHappyChange25_NonFace","PercHappyChange25_Happy"),c("mean", "sd", "se", "n")]

statsTDTC25<-AllDataTC25DxStats$TD[c("PercAngryChange25_Neutral","PercAngryChange25_NonFace","PercAngryChange25_Angry","PercHappyChange25_Neutral","PercHappyChange25_NonFace","PercHappyChange25_Happy"),c("mean", "sd", "se", "n")]

#write.csv(AllDataTC25DxStatsExp,"AllDataTC25DxStats.csv")

write.csv(stats22qTC25, "stats22qTC25.csv")
write.csv(statsTDTC25, "statsTDTC25.csv")

#Split Angry into Angry Begin and Angry End -- 33%

#Split Happy into Happy Begin and Happy End --33%

#Cluster Values
AllDataTC25<-read.csv("AllDataTC25.csv",header=TRUE)
setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")

Clus <- read.csv("ClusCodes.csv")

Clus$Cluster<-as.factor(Clus$Cluster)
#AllDataClus<-read.csv("AllDataClus.csv",header=TRUE)
AllDataTC25<-AllDataTC25[2:24]
#ClusAssign<-AllDataClus[c("Subj","Cluster")]
AllDataTC25Clus<-merge(Clus,AllDataTC25)
setwd("/Users/abbiepopa/Documents/Lab/DPTB/Time Course Eye Gaze/Data")
write.csv(AllDataTC25Clus,"AllDataTC25Clus.csv")
AllDataTC25ClusStats<-describeBy(AllDataTC25Clus,group=AllDataTC25Clus$Cluster)
AllDataTC25ClusStatsExp<-do.call("rbind",AllDataTC25ClusStats)
write.csv(AllDataTC25ClusStatsExp, "AllDataTC25ClusStats.csv")

# #look without new folks
# DxNoNew<-read.csv("AllDataTC25DxNoNew.csv", header=TRUE)
# DxNoNew<-DxNoNew[2:25]
# describeBy(DxNoNew, group=DxNoNew$Dx)

# ClusNoNew<-read.csv("AllDataTC25ClusNoNew.csv", header=TRUE)
# ClusNoNew<-ClusNoNew[2:25]
# describeBy(ClusNoNew, group=ClusNoNew$Cluster)

