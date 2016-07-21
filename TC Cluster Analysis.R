setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")

Clus <- read.csv("ClusCodes.csv")

Clus$Cluster<-as.factor(Clus$Cluster)

setwd("/Users/abbiepopa/Documents/Lab/DPTB/Time Course Eye Gaze/Data")

DataWide<-read.csv("AllDataTC25_ToUse_1.1.csv")

ClusDataWide<-merge(DataWide,Clus)

library(psych)
library(reshape)
ClusDataLong<-melt(ClusDataWide, id=c("Subj","Cluster"))

library(nlme)

### really need to check if each within group change is significant for graphing purposes

fit_angryneutral_coper<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==1 & variable == "PercAngryBegin25_Neutral") | (Cluster == 1 & variable == "PercAngryEnd25_Neutral")))

fit_angrynonface_coper<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==1 & variable == "PercAngryBegin25_NonFace") | (Cluster == 1 & variable == "PercAngryEnd25_NonFace")))
#p=0.022

fit_angry_coper<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==1 & variable == "PercAngryBegin25_Angry") | (Cluster == 1 & variable == "PercAngryEnd25_Angry")))
#p=0.047

summary(fit_angryneutral_coper)
summary(fit_angrynonface_coper)
summary(fit_angry_coper)

fit_angryneutral_struggler<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==2 & variable == "PercAngryBegin25_Neutral") | (Cluster == 2 & variable == "PercAngryEnd25_Neutral")))

fit_angrynonface_struggler<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==2 & variable == "PercAngryBegin25_NonFace") | (Cluster == 2 & variable == "PercAngryEnd25_NonFace")))

fit_angry_struggler<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==2 & variable == "PercAngryBegin25_Angry") | (Cluster == 2 & variable == "PercAngryEnd25_Angry")))

summary(fit_angryneutral_struggler)
summary(fit_angrynonface_struggler)
summary(fit_angry_struggler)

fit_happyneutral_coper<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==1 & variable == "PercHappyBegin25_Neutral") | (Cluster == 1 & variable == "PercHappyEnd25_Neutral")))
#p=0.024

fit_happynonface_coper<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==1 & variable == "PercHappyBegin25_NonFace") | (Cluster == 1 & variable == "PercHappyEnd25_NonFace")))
#p=0.014

fit_happy_coper<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==1 & variable == "PercHappyBegin25_Happy") | (Cluster == 1 & variable == "PercHappyEnd25_Happy")))
#p=0.080

summary(fit_happyneutral_coper)
summary(fit_happynonface_coper)
summary(fit_happy_coper)

fit_happyneutral_struggler<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==2 & variable == "PercHappyBegin25_Neutral") | (Cluster == 2 & variable == "PercHappyEnd25_Neutral")))

fit_happynonface_struggler<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==2 & variable == "PercHappyBegin25_NonFace") | (Cluster == 2 & variable == "PercHappyEnd25_NonFace")))

fit_happy_struggler<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong,(Cluster==2 & variable == "PercHappyBegin25_Happy") | (Cluster == 2 & variable == "PercHappyEnd25_Happy")))

summary(fit_happyneutral_struggler)
summary(fit_happynonface_struggler)
summary(fit_happy_struggler)

### at that point might as well check if each btw group (condition specific) change is significant

fit_angryneutral_change<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercAngryChange25_Neutral"),])

fit_angrynonface_change<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercAngryChange25_NonFace"),])
#p=0.076

fit_angry_change<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercAngryChange25_Angry"),])

summary(fit_angryneutral_change)
summary(fit_angrynonface_change)
summary(fit_angry_change)


fit_happyneutral_change<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercHappyChange25_Neutral"),])

fit_happynonface_change<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercHappyChange25_NonFace"),])
#p=0.014

fit_happy_change<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercHappyChange25_Happy"),])
#p=0.080

summary(fit_happyneutral_change)
summary(fit_happynonface_change)
summary(fit_happy_change)

### actually, it will probably suffice to describe these tests without visuals, but we'll see what Tony says when I send him the paper

###Omnibus Test #1###
###Is change different? And is change different between groups

fit_angry_Clus<-lme(value~Cluster*variable, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable == "PercAngryBegin25_Angry"),which(ClusDataLong$variable == "PercAngryEnd25_Angry")),])

fit_angryneutral_Clus<-lme(value~Cluster*variable, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable == "PercAngryBegin25_Neutral"),which(ClusDataLong$variable == "PercAngryEnd25_Neutral")),])

fit_angrynonface_Clus<-lme(value~Cluster*variable, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable == "PercAngryBegin25_NonFace"),which(ClusDataLong$variable == "PercAngryEnd25_NonFace")),])
#cluster p=0.07

fit_happy_Clus<-lme(value~Cluster*variable, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable == "PercHappyBegin25_Happy"),which(ClusDataLong$variable == "PercHappyEnd25_Happy")),])
#cluster p=0.08
#interaction p=0.07

fit_happyneutral_Clus<-lme(value~Cluster*variable, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable == "PercHappyBegin25_Neutral"),which(ClusDataLong$variable == "PercHappyEnd25_Neutral")),])

fit_happynonface_Clus<-lme(value~Cluster*variable, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable == "PercHappyBegin25_NonFace"),which(ClusDataLong$variable == "PercHappyEnd25_NonFace")),])
#cluster p=0.03

###Follow-Ups to Omnibus Test; Angry NonFace

fit_angrynonface_early<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercAngryBegin25_NonFace"),])
#p=0.09

fit_angrynonface_late<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercAngryEnd25_NonFace"),])

fit_angrynonface_coper<-lme(value~variable, random = ~1|Subj, data=subset(ClusDataLong, (Cluster==2 & variable=="PercAngryBegin25_NonFace") | (Cluster==2 & variable=="PercAngryEnd25_NonFace")))
#p=0.05

fit_angrynonface_struggler<-lme(value~variable, random=~1|Subj, data=subset(ClusDataLong, (Cluster==1 & variable=="PercAngryBegin25_NonFace") | (Cluster==1 & variable=="PercAngryEnd25_NonFace")))

###Follow-Ups to Omnibus Test; Happy Happy Face

fit_happy_early<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercHappyBegin25_Happy"),])
#p=0.090

fit_happy_late<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercHappyEnd25_Happy"),])

fit_happy_coper<-lme(value~variable, random=~1|Subj, data=subset(ClusDataLong, (Cluster==2 & variable == "PercHappyBegin25_Happy") | (Cluster==2 & variable == "PercHappyEnd25_Happy")))
#p=0.10

fit_happy_struggler<-lme(value~variable, random=~1|Subj, data=subset(ClusDataLong,(Cluster==1 & variable == "PercHappyBegin25_Happy") | (Cluster==1 & variable == "PercHappyEnd25_Happy")))

###Follow-Ups to Omnibus Test; Happy NonFace

fit_happynonface_early<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercHappyBegin25_NonFace"),])
#p=0.036

fit_happynonface_late<-lm(value~Cluster, data=ClusDataLong[which(ClusDataLong$variable=="PercHappyEnd25_NonFace"),])

fit_happynonface_coper<-lme(value~variable, random=~1|Subj, data=subset(ClusDataLong, (Cluster==2 & variable == "PercHappyBegin25_NonFace") | (Cluster==2 & variable == "PercHappyEnd25_NonFace")))
#p=0.04

fit_happynonface_struggler<-lme(value~variable, random=~1|Subj, data=subset(ClusDataLong,(Cluster==1 & variable == "PercHappyBegin25_NonFace") | (Cluster==1 & variable == "PercHappyEnd25_NonFace")))







###Omnibus Test #2###
###Does splitting by time give us more information about angry vs neutral?

#avn = angry vs neutral
fit_angry_avn_begin<-lme(value~variable*Cluster, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable == "PercAngryBegin25_Angry"), which(ClusDataLong$variable == "PercAngryBegin25_Neutral")),])

fit_angry_avn_end<-lme(value~variable*Cluster, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable == "PercAngryEnd25_Angry"), which(ClusDataLong$variable == "PercAngryEnd25_Neutral")),])
#interesting, angry versus neutral only significant in second half (not first half)

#hvn = happy vs neutral

fit_happy_hvn_begin<-lme(value~variable*Cluster, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable == "PercHappyBegin25_Happy"), which(ClusDataLong$variable == "PercHappyBegin25_Neutral")),])
#cluster p=0.06

fit_happy_hvn_end<-lme(value~variable*Cluster, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable == "PercHappyEnd25_Happy"), which(ClusDataLong$variable == "PercHappyEnd25_Neutral")),])
#condition p=0.088
###and happy vs neutral is only significant in the first half!

###What happens if you compare angry to happy

#avh = angry vs happy
fit_avh_begin<-lme(value~variable*Cluster, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable=="PercAngryBegin25_Angry"),which(ClusDataLong$variable=="PercHappyBegin25_Happy")),])


fit_avh_end<-lme(value~variable*Cluster, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable=="PercAngryEnd25_Angry"),which(ClusDataLong$variable=="PercHappyEnd25_Happy")),])
#angry vs happy p=0.09

fit_avh_change<-lme(value~variable*Cluster, random=~1|Subj, data=ClusDataLong[c(which(ClusDataLong$variable=="PercAngryChange25_Angry"),which(ClusDataLong$variable=="PercHappyChange25_Happy")),])




###Export Stats for DataGraph
stats<-describeBy(ClusDataWide[,c("PercAngryChange25_Neutral", "PercAngryChange25_NonFace", "PercAngryChange25_Angry","PercHappyChange25_Neutral","PercHappyChange25_NonFace","PercHappyChange25_Happy")], group=ClusDataWide$Cluster)

statsexp<-rbind(stats$'1'[,c("mean","sd","se","n")],stats$'2'[,c("mean","sd","se","n")])

rownames(statsexp)<-c("Struggler: Angry-Change in Neutral", "Struggler: Angry-Change in NonFace","Struggler: Angry-Change in Angry","Struggler: Happy-Change in Neutral","Struggler: Happy-Change in NonFace","Struggler: Happy-Change in Happy","Coper: Angry-Change in Neutral", "Coper: Angry-Change in NonFace","Coper: Angry-Change in Angry","Coper: Happy-Change in Neutral","Coper: Happy-Change in NonFace","Coper: Happy-Change in Happy")

write.csv(statsexp, "DGTCpaper1Cluster.csv")