rm(list=ls())
setwd("/Users/abbiepopa/Documents/Lab/DPTB")
e<-read.csv("everyone_demo.csv")

setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")

Dx <- read.csv("DxCodes.csv")

setwd("/Users/abbiepopa/Documents/Lab/DPTB/Time Course Eye Gaze/Data")

DataWide<-read.csv("AllDataTC25_ToUse_1.1.csv")

colnames(Dx)[1]<-"Subj"
DxDataWide<-merge(DataWide,Dx)

DxDataWide<-merge(DxDataWide, e, by.x="Subj", by.y="CABIL_ID", all.x=T)

library(psych)

library(reshape)
DxDataLong<-melt(DxDataWide, id=c("Subj","Dx", "Dx_Code","Age","Gender_Code","Cluster.Analysis.","Overall.EG","Spence_Total_T_Score_Parent1","ABAS_GAC_Composite_Parent1","WISCIV_FullScale_C"))

library(nlme)

### really need to check if each within group change is significant for graphing purposes

fit_angryneutral_TD<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Neutral") | (Dx=="TD" & variable == "PercAngryEnd25_Neutral")))

fit_angrynonface_TD<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_NonFace") | (Dx=="TD" & variable == "PercAngryEnd25_NonFace")))

fit_angry_TD<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Angry") | (Dx=="TD" & variable == "PercAngryEnd25_Angry")))

summary(fit_angryneutral_TD)
summary(fit_angrynonface_TD)
summary(fit_angry_TD)


fit_angryneutral_22q<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Neutral") | (Dx=="22q" & variable == "PercAngryEnd25_Neutral")))

fit_angrynonface_22q<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_NonFace") | (Dx=="22q" & variable == "PercAngryEnd25_NonFace")))

fit_angry_22q<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Angry") | (Dx=="22q" & variable == "PercAngryEnd25_Angry")))

summary(fit_angryneutral_22q)
summary(fit_angrynonface_22q)
summary(fit_angry_22q)

fit_happyneutral_TD<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Neutral") | (Dx=="TD" & variable == "PercHappyEnd25_Neutral")))

fit_happynonface_TD<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_NonFace") | (Dx=="TD" & variable == "PercHappyEnd25_NonFace")))

fit_happy_TD<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Happy") | (Dx=="TD" & variable == "PercHappyEnd25_Happy")))

summary(fit_happyneutral_TD)
summary(fit_happynonface_TD)
summary(fit_happy_TD)

fit_happyneutral_22q<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Neutral") | (Dx=="22q" & variable == "PercHappyEnd25_Neutral")))

fit_happynonface_22q<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_NonFace") | (Dx=="22q" & variable == "PercHappyEnd25_NonFace")))

fit_happy_22q<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Happy") | (Dx=="22q" & variable == "PercHappyEnd25_Happy")))


summary(fit_happyneutral_22q)
summary(fit_happynonface_22q)
summary(fit_happy_22q)

### at that point might as well check if each btw group (condition specific) change is significant

fit_angryneutral_change<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Neutral"),])

fit_angrynonface_change<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_NonFace"),])
p=0.07

fit_angry_change<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Angry"),])

summary(fit_angryneutral_change)
summary(fit_angrynonface_change)
summary(fit_angry_change)


fit_happyneutral_change<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Neutral"),])

fit_happynonface_change<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_NonFace"),])

fit_happy_change<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Happy"),])

summary(fit_happyneutral_change)
summary(fit_happynonface_change)
summary(fit_happy_change)


### actually, it will probably suffice to describe these tests without visuals, but we'll see what Tony says when I send him the paper

###Omnibus Test #1###
###Is change different? And is change different between groups

fit_angry_Dx<-lme(value~Dx*variable, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable == "PercAngryBegin25_Angry"), which(DxDataLong$variable == "PercAngryEnd25_Angry")),])

fit_angryneutral_Dx<-lme(value~Dx*variable, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable == "PercAngryBegin25_Neutral"), which(DxDataLong$variable == "PercAngryEnd25_Neutral")),])

fit_angrynonface_Dx<-lme(value~Dx*variable, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable == "PercAngryBegin25_NonFace"), which(DxDataLong$variable == "PercAngryEnd25_NonFace")),])

###Hint of an interaction for non-face; follow-up with stratified comparisons

fit_happy_Dx<-lme(value~Dx*variable, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable == "PercHappyBegin25_Happy"), which(DxDataLong$variable == "PercHappyEnd25_Happy")),])

fit_happyneutral_Dx<-lme(value~Dx*variable, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable == "PercHappyBegin25_Neutral"), which(DxDataLong$variable == "PercHappyEnd25_Neutral")),])

fit_happynonface_Dx<-lme(value~Dx*variable, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable == "PercHappyBegin25_NonFace"), which(DxDataLong$variable == "PercHappyEnd25_NonFace")),])

###interactions for both happy and nonface; follow-up with stratified comparisons


###Follow-ups to Omnibus Test; Angry NonFace

fit_angrynonface_early<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercAngryBegin25_NonFace"),])


fit_angrynonface_late<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercAngryEnd25_NonFace"),])

fit_angrynonface_TD<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_NonFace") | (Dx=="TD" & variable == "PercAngryEnd25_NonFace")))
### p = 0.006


fit_angrynonface_22q<-lme(value~variable, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_NonFace") | (Dx=="22q" & variable == "PercAngryEnd25_NonFace")))
###n.s.


###Follow-ups to Omnibus Test; Happy Happy Face

fit_happy_early<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercHappyBegin25_Happy"),])
#p=0.052 and everything is terrible

fit_happy_late<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercHappyEnd25_Happy"),])

fit_happy_TD<-lme(value~variable, random=~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Happy") | (Dx=="TD" & variable == "PercHappyEnd25_Happy")))
### p = 0.014

fit_happy_22q<-lme(value~variable, random=~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Happy") | (Dx=="22q" & variable == "PercHappyEnd25_Happy")))
### n.s.

###Follow-ups to Omnibus Test; Happy NonFace

fit_happynonface_early<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercHappyBegin25_NonFace"),])
###p=0.027

fit_happynonface_late<-lm(value~Dx, data=DxDataLong[which(DxDataLong$variable=="PercHappyEnd25_NonFace"),])
###n.s.

fit_happynonface_TD<-lme(value~variable, random=~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_NonFace") | (Dx=="TD" & variable == "PercHappyEnd25_NonFace")))
###p=0.0033

fit_happynonface_22q<-lme(value~variable, random=~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_NonFace") | (Dx=="22q" & variable == "PercHappyEnd25_NonFace")))
###n.s.

###Omnibus Test #2###
###Does splitting by time give us more information about angry vs neutral?

#avn = angry vs neutral
fit_angry_avn_begin<-lme(value~variable*Dx, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable == "PercAngryBegin25_Angry"),which(DxDataLong$variable == "PercAngryBegin25_Neutral")),])
###angry vs. neutral p=0.053

fit_angry_avn_end<-lme(value~variable*Dx, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable == "PercAngryEnd25_Angry"),which(DxDataLong$variable == "PercAngryEnd25_Neutral")),])
###angry vs neutral p=0.0001

#hvn = happy vs neutral
fit_happy_hvn_begin<-lme(value~variable*Dx, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable == "PercHappyBegin25_Happy"),which(DxDataLong$variable == "PercHappyBegin25_Neutral")),])
###happy vs neutral p=0.013
###TD vs 22q p=0.035

fit_happy_hvn_end<-lme(value~variable*Dx, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable == "PercHappyEnd25_Happy"),which(DxDataLong$variable == "PercHappyEnd25_Neutral")),])
###happy vs neutral p=0.077

###What happens if you compare angry to happy

#avh = angry vs happy
fit_avh_begin<-lme(value~variable*Dx, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable=="PercAngryBegin25_Angry"),which(DxDataLong$variable=="PercHappyBegin25_Happy")),])
#n.s.

fit_avh_end<-lme(value~variable*Dx, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable=="PercAngryEnd25_Angry"),which(DxDataLong$variable=="PercHappyEnd25_Happy")),])
# angry vs happy p = 0.04

fit_avh_change<-lme(value~variable*Dx, random=~1|Subj, data=DxDataLong[c(which(DxDataLong$variable=="PercAngryChange25_Angry"),which(DxDataLong$variable=="PercHappyChange25_Happy")),])
# angry vs happy p=0.08

###Export Stats for DataGraph
stats<-describeBy(DxDataWide[,c("PercAngryChange25_Neutral", "PercAngryChange25_NonFace", "PercAngryChange25_Angry","PercHappyChange25_Neutral","PercHappyChange25_NonFace","PercHappyChange25_Happy")], group=DxDataWide$Dx)

statsexp<-rbind(stats$'22q'[,c("mean","sd","se","n")],stats$TD[,c("mean","sd","se","n")])

rownames(statsexp)<-c("22q: Angry-Change in Neutral", "22q: Angry-Change in NonFace","22q: Angry-Change in Angry","22q: Happy-Change in Neutral","22q: Happy-Change in NonFace","22q: Happy-Change in Happy","TD: Angry-Change in Neutral", "TD: Angry-Change in NonFace","TD: Angry-Change in Angry","TD: Happy-Change in Neutral","TD: Happy-Change in NonFace","TD: Happy-Change in Happy")

write.csv(statsexp,"DGTCpaper1.csv")

###Age###
###only matters if there is an interaction, because otherwise we're just replicating the overall results###
#TD - Angry, within#
summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Neutral") | (Dx=="TD" & variable == "PercAngryEnd25_Neutral"))))

summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_NonFace") | (Dx=="TD" & variable == "PercAngryEnd25_NonFace"))))

summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Angry") | (Dx=="TD" & variable == "PercAngryEnd25_Angry"))))

#22q - Angry, within#
summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Neutral") | (Dx=="22q" & variable == "PercAngryEnd25_Neutral"))))

summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_NonFace") | (Dx=="22q" & variable == "PercAngryEnd25_NonFace"))))

summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Angry") | (Dx=="22q" & variable == "PercAngryEnd25_Angry"))))

###trend of an interaction with age, p=0.05, with older kids changing less

#Angry Between
summary(lm(value~Dx*Age, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Neutral"),]))

summary(lm(value~Dx*Age, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_NonFace"),]))

summary(lm(value~Dx*Age, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Angry"),]))

#TD - Happy, within
summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Neutral") | (Dx=="TD" & variable == "PercHappyEnd25_Neutral"))))

summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_NonFace") | (Dx=="TD" & variable == "PercHappyEnd25_NonFace"))))
###p=0.02, older change more

summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Happy") | (Dx=="TD" & variable == "PercHappyEnd25_Happy"))))
###p=0.03, older change less
###these are actually complimentary

#22q - Happy within

summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Neutral") | (Dx=="22q" & variable == "PercHappyEnd25_Neutral")))) 

summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_NonFace") | (Dx=="22q" & variable == "PercHappyEnd25_NonFace"))))

summary(lme(value~variable*Age, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Happy") | (Dx=="22q" & variable == "PercHappyEnd25_Happy"))))
###p = 0.04, older change less

#Happy Between

summary(lm(value~Dx*Age, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Neutral"),]))

summary(lm(value~Dx*Age, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_NonFace"),]))

summary(lm(value~Dx*Age, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Happy"),]))

###Gender###
###only matters if there is an interaction, because otherwise we're just replicating the overall results###
#TD - Angry, within#
summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Neutral") | (Dx=="TD" & variable == "PercAngryEnd25_Neutral"))))

summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_NonFace") | (Dx=="TD" & variable == "PercAngryEnd25_NonFace"))))

summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Angry") | (Dx=="TD" & variable == "PercAngryEnd25_Angry"))))
#p = 0.05, females changes less

#22q - Angry, within#
summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Neutral") | (Dx=="22q" & variable == "PercAngryEnd25_Neutral"))))

summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_NonFace") | (Dx=="22q" & variable == "PercAngryEnd25_NonFace"))))

summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Angry") | (Dx=="22q" & variable == "PercAngryEnd25_Angry"))))


#Angry Between
summary(lm(value~Dx*Gender_Code, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Neutral"),]))

summary(lm(value~Dx*Gender_Code, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_NonFace"),]))

summary(lm(value~Dx*Gender_Code, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Angry"),]))

#TD - Happy, within
summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Neutral") | (Dx=="TD" & variable == "PercHappyEnd25_Neutral"))))

summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_NonFace") | (Dx=="TD" & variable == "PercHappyEnd25_NonFace"))))

summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Happy") | (Dx=="TD" & variable == "PercHappyEnd25_Happy"))))

#22q - Happy within

summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Neutral") | (Dx=="22q" & variable == "PercHappyEnd25_Neutral")))) 

summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_NonFace") | (Dx=="22q" & variable == "PercHappyEnd25_NonFace"))))

summary(lme(value~variable*Gender_Code, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Happy") | (Dx=="22q" & variable == "PercHappyEnd25_Happy"))))

#Happy Between

summary(lm(value~Dx*Gender_Code, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Neutral"),]))

summary(lm(value~Dx*Gender_Code, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_NonFace"),]))

summary(lm(value~Dx*Gender_Code, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Happy"),]))

###FSIQ###
###only matters if there is an interaction, because otherwise we're just replicating the overall results###
#TD - Angry, within#
summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Neutral") | (Dx=="TD" & variable == "PercAngryEnd25_Neutral"))))

summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_NonFace") | (Dx=="TD" & variable == "PercAngryEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Angry") | (Dx=="TD" & variable == "PercAngryEnd25_Angry")), na.action=na.omit))

#22q - Angry, within#
summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Neutral") | (Dx=="22q" & variable == "PercAngryEnd25_Neutral")), na.action=na.omit))

summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_NonFace") | (Dx=="22q" & variable == "PercAngryEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Angry") | (Dx=="22q" & variable == "PercAngryEnd25_Angry")), na.action=na.omit))

#Angry Between
summary(lm(value~Dx*WISCIV_FullScale_C, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Neutral"),], na.action=na.omit))

summary(lm(value~Dx*WISCIV_FullScale_C, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_NonFace"),], na.action=na.omit))

summary(lm(value~Dx*WISCIV_FullScale_C, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Angry"),], na.action=na.omit))

#TD - Happy, within
summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Neutral") | (Dx=="TD" & variable == "PercHappyEnd25_Neutral")), na.action=na.omit))

summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_NonFace") | (Dx=="TD" & variable == "PercHappyEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Happy") | (Dx=="TD" & variable == "PercHappyEnd25_Happy")), na.action=na.omit))

#22q - Happy within

summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Neutral") | (Dx=="22q" & variable == "PercHappyEnd25_Neutral")), na.action=na.omit)) 

summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_NonFace") | (Dx=="22q" & variable == "PercHappyEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*WISCIV_FullScale_C, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Happy") | (Dx=="22q" & variable == "PercHappyEnd25_Happy")), na.action=na.omit))

#Happy Between

summary(lm(value~Dx*WISCIV_FullScale_C, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Neutral"),], na.action=na.omit))

summary(lm(value~Dx*WISCIV_FullScale_C, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_NonFace"),], na.action=na.omit))

summary(lm(value~Dx*WISCIV_FullScale_C, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Happy"),], na.action=na.omit))

###SCAS###
###only matters if there is an interaction, because otherwise we're just replicating the overall results###
#TD - Angry, within#
summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Neutral") | (Dx=="TD" & variable == "PercAngryEnd25_Neutral"))))

summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_NonFace") | (Dx=="TD" & variable == "PercAngryEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Angry") | (Dx=="TD" & variable == "PercAngryEnd25_Angry")), na.action=na.omit))

#22q - Angry, within#
summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Neutral") | (Dx=="22q" & variable == "PercAngryEnd25_Neutral")), na.action=na.omit))

summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_NonFace") | (Dx=="22q" & variable == "PercAngryEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Angry") | (Dx=="22q" & variable == "PercAngryEnd25_Angry")), na.action=na.omit))

#Angry Between
summary(lm(value~Dx*Spence_Total_T_Score_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Neutral"),], na.action=na.omit))

summary(lm(value~Dx*Spence_Total_T_Score_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_NonFace"),], na.action=na.omit))

summary(lm(value~Dx*Spence_Total_T_Score_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Angry"),], na.action=na.omit))

#TD - Happy, within
summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Neutral") | (Dx=="TD" & variable == "PercHappyEnd25_Neutral")), na.action=na.omit))

summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_NonFace") | (Dx=="TD" & variable == "PercHappyEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Happy") | (Dx=="TD" & variable == "PercHappyEnd25_Happy")), na.action=na.omit))

#22q - Happy within

summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Neutral") | (Dx=="22q" & variable == "PercHappyEnd25_Neutral")), na.action=na.omit)) 

summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_NonFace") | (Dx=="22q" & variable == "PercHappyEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*Spence_Total_T_Score_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Happy") | (Dx=="22q" & variable == "PercHappyEnd25_Happy")), na.action=na.omit))

#Happy Between

summary(lm(value~Dx*Spence_Total_T_Score_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Neutral"),], na.action=na.omit))

summary(lm(value~Dx*Spence_Total_T_Score_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_NonFace"),], na.action=na.omit))

summary(lm(value~Dx*Spence_Total_T_Score_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Happy"),], na.action=na.omit))

###ABAS###
###only matters if there is an interaction, because otherwise we're just replicating the overall results###
#TD - Angry, within#
summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Neutral") | (Dx=="TD" & variable == "PercAngryEnd25_Neutral"))))

summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_NonFace") | (Dx=="TD" & variable == "PercAngryEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercAngryBegin25_Angry") | (Dx=="TD" & variable == "PercAngryEnd25_Angry")), na.action=na.omit))

#22q - Angry, within#
summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Neutral") | (Dx=="22q" & variable == "PercAngryEnd25_Neutral")), na.action=na.omit))

summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_NonFace") | (Dx=="22q" & variable == "PercAngryEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercAngryBegin25_Angry") | (Dx=="22q" & variable == "PercAngryEnd25_Angry")), na.action=na.omit))

#Angry Between
summary(lm(value~Dx*ABAS_GAC_Composite_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Neutral"),], na.action=na.omit))

summary(lm(value~Dx*ABAS_GAC_Composite_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_NonFace"),], na.action=na.omit))

summary(lm(value~Dx*ABAS_GAC_Composite_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercAngryChange25_Angry"),], na.action=na.omit))

#TD - Happy, within
summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Neutral") | (Dx=="TD" & variable == "PercHappyEnd25_Neutral")), na.action=na.omit))

summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_NonFace") | (Dx=="TD" & variable == "PercHappyEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="TD" & variable=="PercHappyBegin25_Happy") | (Dx=="TD" & variable == "PercHappyEnd25_Happy")), na.action=na.omit))

#22q - Happy within

summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Neutral") | (Dx=="22q" & variable == "PercHappyEnd25_Neutral")), na.action=na.omit)) 

summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_NonFace") | (Dx=="22q" & variable == "PercHappyEnd25_NonFace")), na.action=na.omit))

summary(lme(value~variable*ABAS_GAC_Composite_Parent1, random = ~1|Subj, data=subset(DxDataLong, (Dx=="22q" & variable=="PercHappyBegin25_Happy") | (Dx=="22q" & variable == "PercHappyEnd25_Happy")), na.action=na.omit))

#Happy Between

summary(lm(value~Dx*ABAS_GAC_Composite_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Neutral"),], na.action=na.omit))

summary(lm(value~Dx*ABAS_GAC_Composite_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_NonFace"),], na.action=na.omit))

summary(lm(value~Dx*ABAS_GAC_Composite_Parent1, data=DxDataLong[which(DxDataLong$variable=="PercHappyChange25_Happy"),], na.action=na.omit))