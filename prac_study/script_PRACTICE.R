

# For this analysis, you will need to install and then open the following packages:
#install.packages("dplyr"); install.packages("ggplot2")
library("dplyr"); library("ggplot2"); library("lme4"); library("lmerTest")


## Part 1 ----------------------------------------------------------------------
## Setting the Directory -------------------------------------------------------
# Home computer
setwd("C:/Orion/Grad_Biostats/")
# On KRL Computer
setwd("C:/Users/u6015231/Box Sync/difficulty EEG study/")
list.files("./data/")

SCORES<-read.csv("./data/data_SCORES.csv", header = TRUE)
head(SCORES)

## Selecting the Practice Data ----
PRAC<-subset(SCORES, phase != "resting")
PRAC<-subset(PRAC, phase != "posttest")
PRAC<-subset(PRAC, phase != "pretest")

PRAC$phase<-factor(PRAC$phase)
summary(PRAC$phase)

## Contrast Coding the Group Variable ----
PRAC$self.c<-(as.numeric(PRAC$group)-1.5)*(-1)
head(PRAC)
PRAC$self.c
summary(PRAC$diff)
summary(PRAC$scoreQ)
PRAC$scoreQ.c<-PRAC$scoreQ-mean(PRAC$scoreQ)
summary(PRAC$block)
PRAC$block.c<-PRAC$block-mean(PRAC$block)
summary(PRAC$block.c)
PRAC$eng_st.c<-PRAC$eng_st-mean(PRAC$eng_st)
summary(PRAC$eng_st.c)
PRAC$mot_st.c<-PRAC$mot_st-mean(PRAC$mot_st)
summary(PRAC$mot_st.c)

#Descriptive Statistics of Practice --------------------------------------------
head(PRAC)
summary(PRAC$group)
SELF<-subset(PRAC, group=="self")
YOKE<-subset(PRAC, group=="yoked")

## 2A Plot of Scores over Time ------------------------------------------------
g1<-ggplot(PRAC, aes(x = jitter(block), y = score)) +
  geom_point(aes(group=subID), fill="grey", pch=21, size=2, alpha = .5) +  
  geom_line(aes(group=subID), col="grey") +
  stat_smooth(aes(lty=group), col="black", method="loess", lwd=1.5, se=FALSE)+
  facet_wrap(~group, ncol=1)
g2<-g1+scale_x_continuous(name = "Block of Practice") +
  scale_y_continuous(name = "In-Game Score")
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.position="none")

plot(g3)

## 2B Plot of Difficulty over Time ------------------------------------------------
g1<-ggplot(SELF, aes(x = jitter(block), y = jitter(diff))) +
  geom_point(aes(group=subID), fill="grey", pch=21, size=2, alpha = .5) +  
  geom_line(aes(group=subID), col="grey") +
  stat_smooth(col="black", method="loess", lwd=1.5, se=FALSE)
g2<-g1+scale_x_continuous(name = "Block of Practice") +
  scale_y_continuous(name = "Difficulty", breaks=c(1:9))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.position="none")

plot(g3)
## -----------------------------------------------------------------------------

# Descriptive statistics for the groups
head(PRAC)
BY_SUB<-summarize(group_by(PRAC, subID),
                      group = group[1],
                      meanScore = mean(score, na.rm=TRUE),
                      meanDiff = mean(diff, na.rm=TRUE),
                      meanEng = mean(eng_st, na.rm=TRUE),
                      meanMot = mean(mot_st, na.rm=TRUE))

aggregate(meanScore ~ group, BY_SUB, mean)
aggregate(meanScore ~ group, BY_SUB, sd)
t.test(meanScore~group, BY_SUB, paired=FALSE, var.equal=TRUE)

aggregate(meanDiff ~ group, BY_SUB, mean)
aggregate(meanDiff ~ group, BY_SUB, sd)
t.test(meanDiff~group, BY_SUB, paired=FALSE, var.equal=TRUE)


aggregate(meanEng ~ group, BY_SUB, mean)
aggregate(meanEng ~ group, BY_SUB, sd)
t.test(meanEng~group, BY_SUB, paired=FALSE, var.equal=TRUE)

aggregate(meanMot ~ group, BY_SUB, mean)
aggregate(meanMot ~ group, BY_SUB, sd)
t.test(meanMot~group, BY_SUB, paired=FALSE, var.equal=TRUE)



## Frontal Asymmetry -----------------------------------------------------------
## FAS as a function of block number and group ----
# Our primary dependent variable is FAS_alpha_Drest
F1 <- lmer(FAS_alpha_Drest~1+block+self.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(F1)

F2 <- lmer(FAS_alpha_Drest~1+block+self.c+diff.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(F2)

F3 <- lmer(FAS_alpha_Drest~1+block+self.c+diff.c+scoreQ.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(F3)

F4 <- lmer(FAS_alpha_Drest~1+block*self.c+diff.c*self.c+
             scoreQ.c*self.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(F4)

F5 <- lmer(FAS_alpha_Drest~1+block*self.c+diff.c*self.c
           +scoreQ.c*self.c+eng_st.c*self.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(F5)

F6 <- lmer(FAS_alpha_Drest~1+block*self.c+diff.c*self.c
           +scoreQ.c*self.c+mot_st.c*self.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(F6)

anova(F1,F2,F3,F4,F5,F6)

## Figure 3A FAS by Group, Time, and Difficulty --------------------------------
g1<-ggplot(PRAC, aes(x = jitter(diff, factor=0.5), y = FAS_alpha_Drest)) +
  geom_point(aes(fill=block), pch=21, size=3, stroke=1, alpha = .5) + 
  scale_fill_gradient(low="white", high="black")+
  facet_wrap(~group, ncol=2)
g2<-g1+scale_x_continuous(name = "Difficulty", breaks=c(1:9)) +
  scale_y_continuous(name = expression(Delta* "FAS"))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 16))

plot(g3)

group<-c("self", "yoked")
Intercepts<-c(0.001485,-0.079565)
Slopes<- c(-0.0197295, -0.0116105)
dd<-data.frame(group,Intercepts,Slopes)  
dd
g4<- g3 + geom_abline(aes(intercept=Intercepts, slope=Slopes, lty=group), lwd=2, data=dd)

plot(g4)
## -----------------------------------------------------------------------------



## Midline Frontal Theta -------------------------------------------------------
# MF Theta as a function of block number and group ----
# Our primary dependent variable is MF_theta_Drest
T1 <- lmer(MF_theta_Drest~1+block.c+self.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(T1)

T2 <- lmer(MF_theta_Drest~1+block.c+self.c+diff.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(T2)

T3 <- lmer(MF_theta_Drest~1+block.c+self.c+diff.c+scoreQ.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(T3) 

T4 <- lmer(MF_theta_Drest~1+block.c*self.c+diff.c*self.c+
             scoreQ.c*self.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(T4) 

T5 <- lmer(MF_theta_Drest~1+block.c*self.c+diff.c*self.c+
             scoreQ.c*self.c+eng_st.c*self.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(T5)

T6 <- lmer(MF_theta_Drest~1+block.c*self.c+diff.c*self.c+
             scoreQ.c*self.c+mot_st.c*self.c+(1+block|subID), data=PRAC, REML = FALSE)
summary(T6)

anova(T1,T2,T3,T4,T5,T6)

## Figure 4A MFT by Group, and Time --------------------------------------------
g1<-ggplot(PRAC, aes(x = jitter(block, factor=0.5), y = MF_theta_Drest)) +
  geom_point(aes(fill=diff), pch=21, size=3, stroke=1, alpha = .5) + 
  scale_fill_gradient(low="white", high="black")+
  facet_wrap(~group, ncol=2)
g2<-g1+scale_x_continuous(name = "Block", limits=c(0,20)) +
  scale_y_continuous(name = expression(Delta* "MFT"))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 16))

plot(g3)

# Fixed Effect of Block
group<-c("self", "yoked")
Intercepts<-c(0.0302, 0.04538)
Slopes<- c(-0.00303, -0.00394)
dd<-data.frame(group,Intercepts,Slopes)  
dd
g4<- g3 + geom_abline(aes(intercept=Intercepts, slope=Slopes, lty=group), lwd=2, data=dd)

print(g4)

## Figure 4B MFT by Group, Time, and Difficulty --------------------------------
g1<-ggplot(PRAC, aes(x = jitter(diff, factor=0.5), y = MF_theta_Drest)) +
  geom_point(aes(fill=block), pch=21, size=3, stroke=1, alpha = .5) + 
  scale_fill_gradient(low="white", high="black")+
  facet_wrap(~group, ncol=2)
g2<-g1+scale_x_continuous(name = "Difficulty", breaks=c(1:9)) +
  scale_y_continuous(name = expression(Delta* "MFT"))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 16))


plot(g3)

# Fixed Effect of Difficulty
group<-c("self", "yoked")
Intercepts<-c(0.00302, 0.04538)
Slopes<- c(0.01425, 0.01351)
dd<-data.frame(group,Intercepts,Slopes)  
dd
g4<- g3 + geom_abline(aes(intercept=Intercepts, slope=Slopes, lty=group), lwd=2, data=dd)

plot(g4)

## Figure 4C MFT by Group, Difficulty, and Score--------------------------------
g1<-ggplot(PRAC, aes(x = jitter(scoreQ, factor=0.5), y = MF_theta_Drest)) +
  geom_point(aes(fill=block), pch=21, size=3, stroke=1, alpha = .5) + 
  scale_fill_gradient(low="white", high="black")+
  facet_wrap(~group, ncol=2)
g2<-g1+scale_x_continuous(name = "Ranked Score", limits=c(0,20)) +
  scale_y_continuous(name = expression(Delta* "MFT"))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 16))


plot(g3)

# Fixed Effect of Score
group<-c("self", "yoked")
Intercepts<-c(0.0302, 0.04538)
Slopes<- c(-0.00235, -0.00447)
dd<-data.frame(group,Intercepts,Slopes)  
dd
g4<- g3 + geom_abline(aes(intercept=Intercepts, slope=Slopes, lty=group), lwd=2, data=dd)

plot(g4)
## -----------------------------------------------------------------------------


## Increase in difficulty -------------------------------------------------------
## Our primary dependent variable is will_inc
I1<- glmer(will_inc~1+block.c+self.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(I1)

I2<- glmer(will_inc~1+block.c+self.c+diff.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(I2)

I3 <- glmer(will_inc~1+block.c+self.c+diff.c+scoreQ.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(I3)

I4 <- glmer(will_inc~1+block.c+diff.c+self.c+
              scoreQ.c*self.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(I4)

I5 <- glmer(will_inc~1+block.c+diff.c+
              scoreQ.c*self.c+eng_st.c*self.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(I5)

I6 <- glmer(will_inc~1+block.c+diff.c+
              scoreQ.c*self.c+mot_st.c*self.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(I6)

anova(I1,I2,I3,I4,I5,I6)


# Post Hoc Tests
# Toggle between the YOKE and SELF subsets
I4B <- glmer(will_inc~1+block.c+diff.c+
              scoreQ.c+(1+block.c|subID), data=YOKE, family = "binomial")
summary(I4B)


## Increasing Difficulty as a function of score --------------------------------
g1<-ggplot(PRAC, aes(x = will_inc, y = scoreQ)) +
  geom_jitter(aes(fill=as.factor(will_inc)), position=position_jitterdodge(dodge.width=1), 
              pch=21, size=2, alpha = .5) + 
  geom_boxplot(aes(fill=as.factor(will_inc)), alpha = .8, notch=FALSE, 
              col="black", lwd=1, outlier.shape=NA, width=0.5)+
  scale_fill_manual(name="Increase Difficulty",
                    breaks=c("0", "1"),
                    values = c("grey20", "white"))+  
  facet_wrap(~group)+
  scale_x_continuous(name = "Increase Difficulty on Next Block", breaks=c(0,1)) +
  scale_y_continuous(name = "Ranked Score", limits=c(0,20))
g2 <- g1 + theme_bw() + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.position="none")

plot(g2)

## Increasing Difficulty as a function of Engagement ---------------------------
g1<-ggplot(PRAC, aes(x = will_inc, y = eng_st)) +
  geom_jitter(aes(fill=as.factor(will_inc)), position=position_jitterdodge(dodge.width=1), 
              pch=21, size=2, alpha = .5) + 
  geom_boxplot(aes(fill=as.factor(will_inc)), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA, width=0.5)+
  scale_fill_manual(name="Increase Difficulty",
                    breaks=c("0", "1"),
                    values = c("grey20", "white"))+  
  facet_wrap(~group)+
  scale_x_continuous(name = "Increase Difficulty on Next Block", breaks=c(0,1)) +
  scale_y_continuous(name = "Single-Item Engagement", limits=c(0,10), breaks=c(0:10))
g2 <- g1 + theme_bw() + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.position="none")

plot(g2)
## -----------------------------------------------------------------------------



## Decrease in difficulty -------------------------------------------------------
## Our primary dependent variable is will_dec
L1<- glmer(will_dec~1+block.c+self.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(L1)

L2 <- glmer(will_dec~1+block.c+self.c+diff.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(L2)

L3 <- glmer(will_dec~1+block.c+self.c+diff.c+scoreQ.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(L3)

L4 <- glmer(will_dec~1+block.c+diff.c+
              scoreQ.c*self.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(L4)

L5 <- glmer(will_dec~1+block.c+diff.c+
              scoreQ.c*self.c+eng_st.c*self.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(L5)

L6 <- glmer(will_dec~1+block.c+diff.c+
              scoreQ.c*self.c+mot_st.c*self.c+(1+block.c|subID), data=PRAC, family = "binomial")
summary(L6)

anova(L1,L2,L3,L4,L5,L6)

# Post Hoc Tests
L5P <- glmer(will_dec~1+block.c+diff.c+
              scoreQ.c+eng_st.c+(1+block.c|subID), data=YOKE, family = "binomial")
summary(L5P)


## Decreasing Difficulty as a function of score --------------------------------
g1<-ggplot(PRAC, aes(x = will_dec, y = scoreQ)) +
  geom_jitter(aes(fill=as.factor(will_dec)), position=position_jitterdodge(dodge.width=1), 
              pch=21, size=2, alpha = .5) + 
  geom_boxplot(aes(fill=as.factor(will_dec)), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA, width=0.5)+
  scale_fill_manual(name="Decrease Difficulty",
                    breaks=c("0", "1"),
                    values = c("grey20", "white"))+  
  facet_wrap(~group)+
  scale_x_continuous(name = "Decrease Difficulty on Next Block", breaks=c(0,1)) +
  scale_y_continuous(name = "Ranked Score", limits=c(0,20))
g2 <- g1 + theme_bw() + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.position="none")

plot(g2)

## Decreasing Difficulty as a function of Engagement ---------------------------
g1<-ggplot(PRAC, aes(x = will_dec, y = eng_st)) +
  geom_jitter(aes(fill=as.factor(will_dec)), position=position_jitterdodge(dodge.width=1), 
              pch=21, size=2, alpha = .5) + 
  geom_boxplot(aes(fill=as.factor(will_dec)), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA, width=0.5)+
  scale_fill_manual(name="Decrease Difficulty",
                    breaks=c("0", "1"),
                    values = c("grey20", "white"))+  
  facet_wrap(~group)+
  scale_x_continuous(name = "Decrease Difficulty on Next Block", breaks=c(0,1)) +
  scale_y_continuous(name = "Single-Item Engagement", limits=c(0,10), breaks=c(0:10))
g2 <- g1 + theme_bw() + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.position="none")

plot(g2)
## -----------------------------------------------------------------------------











# Supplemental Figures
head(PRAC)
## FAS reliability within participants -----------------------------------------
g1<-ggplot(PRAC, aes(x = subID, y = FAS_alpha)) +
  geom_boxplot(aes(fill=as.factor(subID)), alpha = .8, notch=FALSE, 
                col="black", lwd=1, outlier.shape=NA)+
  facet_wrap(~group, scales="free")+
  scale_x_discrete(name = "Subject") +
  scale_y_continuous(name = "Raw FAS")
g2 <- g1 + theme_bw() + 
  theme(axis.text=element_text(size=12, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.position="none")

plot(g2)

## Delta FAS reliabilty within participants ------------------------------------
g1<-ggplot(PRAC, aes(x = subID, y = FAS_alpha_Drest)) +
  geom_boxplot(aes(fill=as.factor(subID)), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA)+
  facet_wrap(~group, scales="free")+
  scale_x_discrete(name = "Subject") +
  scale_y_continuous(name = expression(Delta* "FAS"))
g2 <- g1 + theme_bw() + 
  theme(axis.text=element_text(size=12, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.position="none")

plot(g2)



## MFT reliability within participants -----------------------------------------
g1<-ggplot(PRAC, aes(x = subID, y = ln_Fz_theta)) +
  geom_boxplot(aes(fill=as.factor(subID)), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA)+
  facet_wrap(~group, scales="free")+
  scale_x_discrete(name = "Subject") +
  scale_y_continuous(name = "Raw MFT")
g2 <- g1 + theme_bw() + 
  theme(axis.text=element_text(size=12, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.position="none")

plot(g2)


## Delta MFT reliability within participants -----------------------------------------
g1<-ggplot(PRAC, aes(x = subID, y = MF_theta_Drest)) +
  geom_boxplot(aes(fill=as.factor(subID)), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA)+
  facet_wrap(~group, scales="free")+
  scale_x_discrete(name = "Subject") +
  scale_y_continuous(name = expression(Delta* "MFT"))
g2 <- g1 + theme_bw() + 
  theme(axis.text=element_text(size=12, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=16,face="bold", hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 16))+
  theme(legend.position="none")

plot(g2)
