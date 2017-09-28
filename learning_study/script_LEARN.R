# Analysis of Gaming Data.
# By Keith Lohse, Rehabilitation Informatics Lab, 2017-09-25

# For this analysis, you will need to install and then open the following packages:
# install.packages("metafor"); install.packages("dplyr"); install.packages("ggplot2")
library("plyr"); library("ggplot2"); library("lme4"); library("lmerTest");
library("ez"); library("car"); library("dplyr"); library("RCurl")

##----------------------- Data Cleaning and QA ---------------------------------
## Setting the Directory -------------------------------------------------------
getwd()
# You can either download the data files from GitHub and save them in your 
# working directory or read the files into your working directory directly from 
# GitHub. The code for reading in the files from GitHub is provided below.


## Importing Data and QA -------------------------------------------------------
# The SCORES data file contains all of the scores and single item engagement
# scores from practice and retention testing. 
SCORES <- read.csv(text = getURL("https://raw.githubusercontent.com/keithlohse/difficulty_EEG/master/learning_study/data/data_SCORES.csv"), 
              header = TRUE)
# If you saved the files into your working directory, then run:
# SCORES<-read.csv("./data_SCORES.csv", header = TRUE)
head(SCORES) # Should be 1440 observations of 105 variables

# The SURVEY data file contains the post-test data integrated with the composite 
# scores from all of the participant surveys. Responses for single questions are 
# not included in this file, but are included in the SURVEY_MASTER Excel file.
SURVEY <- read.csv(text = getURL("https://raw.githubusercontent.com/keithlohse/difficulty_EEG/master/learning_study/data/data_SURVEY.csv"), 
                   header = TRUE)
# If you saved the files into your working directory, then run:
# SURVEY<-read.csv("./data_SURVEY.csv", header = TRUE)
head(SURVEY) # Should be 60 observations of 79 variables.

# The COMB data file combines the score data from the current experiment (Exp 2)
# and from Leiker, Bruzi, et al, HMS, 2016. These data allow us to meta-
# analytically test the effect of group controlling for pretest. 
COMB <- read.csv(text = getURL("https://raw.githubusercontent.com/keithlohse/difficulty_EEG/master/learning_study/data/data_SCORES_COMB_LONG.csv"), 
                   header = TRUE)
# If you saved the files into your working directory, then run:
# COMB<-read.csv("./data/data_SCORES_COMB_LONG.csv", header = TRUE)
head(COMB) # Should be 480 observations of 18 variables.
# For use later, we will create a mean centered version of the pre-test variable
COMB$pretest.c<-COMB$pretest-mean(COMB$pretest)




##-------------------- AsPredicted.org Analyses --------------------------------
## Effect of Choice on Learning ------------------------------------------------
## RM ANCOVA of post-test controlling for pre-test -----------------------------
# Subsetting the data into just the post-test scores.
POST<-subset(SCORES,phase=="posttest")
head(POST)
POST$difficulty<-factor(POST$difficulty) #refactoring to remove missing levels
POST$group<-factor(POST$group)
summary(POST$difficulty)
summary(POST$group)

# Creating a mean centered pre-test variable:
POST$pre_score.c<-POST$pre_score-mean(POST$pre_score)

# RM ANCOVA for primary outcome
summary(aov(score~difficulty*pre_score.c+difficulty*group+Error(subID/difficulty),
            data=POST))

## Table 1. --------------------------------------------------------------------
# Desriptive statistics for the different difficulties and groups. 
# Pre-Test Scores
mean(POST$pre_score);sd(POST$pre_score)
SC<-subset(POST, group=="self")
YK<-subset(POST, group=="yoked")
mean(SC$pre_score);sd(SC$pre_score)
mean(YK$pre_score);sd(YK$pre_score)
# Post-Test N2 scores
mean(POST$score[POST$difficulty=="N2"]);sd(POST$score[POST$difficulty=="N2"])
mean(SC$score[SC$difficulty=="N2"]);sd(SC$score[SC$difficulty=="N2"])
mean(YK$score[YK$difficulty=="N2"]);sd(YK$score[YK$difficulty=="N2"])
# Post-Test P2 scores
mean(POST$score[POST$difficulty=="P2"]);sd(POST$score[POST$difficulty=="P2"])
mean(SC$score[SC$difficulty=="P2"]);sd(SC$score[SC$difficulty=="P2"])
mean(YK$score[YK$difficulty=="P2"]);sd(YK$score[YK$difficulty=="P2"])
# Post-Test E2 scores
mean(POST$score[POST$difficulty=="E2"]);sd(POST$score[POST$difficulty=="E2"])
mean(SC$score[SC$difficulty=="E2"]);sd(SC$score[SC$difficulty=="E2"])
mean(YK$score[YK$difficulty=="E2"]);sd(YK$score[YK$difficulty=="E2"])

head(SURVEY)
aggregate(totalENG ~ group, SURVEY, mean)
aggregate(totalENG ~ group, SURVEY, sd)

aggregate(Focused.Attention ~ group, SURVEY, mean)
aggregate(Focused.Attention ~ group, SURVEY, sd)

aggregate(Usability ~ group, SURVEY, mean)
aggregate(Usability ~ group, SURVEY, sd)

aggregate(Aesthetics ~ group, SURVEY, mean)
aggregate(Aesthetics ~ group, SURVEY, sd)

aggregate(Endurability ~ group, SURVEY, mean)
aggregate(Endurability ~ group, SURVEY, sd)

aggregate(Novelty ~ group, SURVEY, mean)
aggregate(Novelty ~ group, SURVEY, sd)

aggregate(Involvement ~ group, SURVEY, mean)
aggregate(Involvement ~ group, SURVEY, sd)

aggregate(interest.enjoy ~ group, SURVEY, mean)
aggregate(interest.enjoy ~ group, SURVEY, sd)

aggregate(competence ~ group, SURVEY, mean)
aggregate(competence ~ group, SURVEY, sd)

aggregate(effort ~ group, SURVEY, mean)
aggregate(effort ~ group, SURVEY, sd)

aggregate(pressure.tension ~ group, SURVEY, mean)
aggregate(pressure.tension ~ group, SURVEY, sd)

aggregate(totalBDI ~ group, SURVEY, mean)
aggregate(totalBDI ~ group, SURVEY, sd)

aggregate(eng_st_ave ~ group, SURVEY, mean)
aggregate(eng_st_ave ~ group, SURVEY, sd)

aggregate(mot_st_ave ~ group, SURVEY, mean)
aggregate(mot_st_ave ~ group, SURVEY, sd)

aggregate(Pre_sEBR ~ group, SURVEY, mean)
aggregate(Pre_sEBR ~ group, SURVEY, sd)

aggregate(rest_FAS ~ group, SURVEY, mean)
aggregate(rest_FAS ~ group, SURVEY, sd)

aggregate(prac_FAS_AVE ~ group, SURVEY, mean)
aggregate(prac_FAS_AVE ~ group, SURVEY, sd)

aggregate(FAS_diff ~ group, SURVEY, mean)
aggregate(FAS_diff ~ group, SURVEY, sd)
## -----------------------------------------------------------------------------

## Post-Hoc test for the pre-test by difficulty interaction. -------------------
# First, subset the data into the lower and upper 25% of pre-test scores. 
summary(POST$pre_score)
LOW<-subset(POST, pre_score <= 1049)
HI<-subset(POST, pre_score >= 1384)
# Effect of Difficulty in the lowest scoring participants.
mean(LOW$score[LOW$difficulty=="N2"])
mean(LOW$score[LOW$difficulty=="P2"])
mean(LOW$score[LOW$difficulty=="E2"])
# Effect of Difficulty in the highest scoring participants.
mean(HI$score[HI$difficulty=="N2"])
mean(HI$score[HI$difficulty=="P2"])
mean(HI$score[HI$difficulty=="E2"])





## Meta-analytic RM ANCOVA, combing data from Exp 1 and Exp 2 ------------------
# First, we will exclude the post-test average scores from the combined data
# leaving three scores per person (N2, P2, and E2 difficulties).
summary(COMB$diff)
POST2<-subset(COMB,diff!="ave")
COMB_ave<-subset(COMB,diff=="ave")
head(COMB_ave)

head(POST2)
summary(aov(post_score~diff*pretest.c+diff*group*experiment+
                Error(subID/diff), data=POST2))
# Across the two experiments, the effect of Group is statistically significant.
# Furthermore, there is no Group by Experiment interaction, so the effect of 
# group was not statistically different between the two experiments. This 
# pooled effect gives us the best estimate of the "true" effect of group. 

# Averaging across difficulty, we can see the group effect in a linear model in 
# the different studies:
comb<-lm(post_score~1+pretest.c+self.c, data=COMB_ave)
summary(comb)
summary(lm(post_score~1+pretest.c+self.c, data=COMB_ave[COMB_ave$experiment=="Leiker et al. (2016)",]))
summary(lm(post_score~1+pretest.c+self.c, data=COMB_ave[COMB_ave$experiment=="Replication",]))

plot(comb) # We can check the statistical assumptions of our regression

## Figure 2 --------------------------------------------------------------------
COMB_ave$experiment<-revalue(COMB_ave$experiment, c("Leiker et al. (2016)"="Leiker et al. (2016)", 
                               "Replication"="Current Study"))

myX<-scale_x_continuous(name = "Pre-Test Score", limits=c(0,2000))
myY<-scale_y_continuous(name = "Post-Test Score", limits=c(0,1500))
g1<-ggplot(data=COMB_ave, aes(x=pretest, y = post_score))+
  geom_point(aes(fill=group), pch=21, size=2.5, stroke=1)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  facet_wrap(~experiment)+myX+myY

g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 16))
  
print(g2)

## Combined analysis of engagement and motivation -------------------------------
tail(COMB_ave)
COMB_ave$INT_ENJ.c<-COMB_ave$INT_ENJ-mean(COMB_ave$INT_ENJ)
COMB_ave$totalENG.c<-COMB_ave$totalENG-mean(COMB_ave$totalENG)
# Intrinsic motivation as a predictor
comb_a<-lm(post_score~1+self.c*pretest.c+INT_ENJ.c, data=COMB_ave)
summary(comb_a)
# Total Engagement as a predictor
comb_b<-lm(post_score~1+self.c*pretest.c+totalENG.c, data=COMB_ave)
summary(comb_b)


## Effects of Explanatory Variables on Learning --------------------------------
head(SURVEY)
# As there was no group by difficulty interaction in the RMA ANCOVA, we are 
# averaging across all levels of difficulty on the post-test for our regression 
# models. 
mod0<-lm(post_ave~1+pretest.c, data=SURVEY)
summary(mod0)


mod1<-lm(post_ave~1+pretest.c+self.c, data=SURVEY)
summary(mod1)
# We can visually check the statistical assumptions of our regression:
plot(mod1)
shapiro.test(resid(mod1))


## Adding Engagement (Long Form) as a predictor --------------------------------
SURVEY$eng.c<-SURVEY$totalENG-mean(SURVEY$totalENG)
mod2<-lm(post_ave~1+pretest.c+self.c+eng.c, data=SURVEY)
summary(mod2)
# Single Item engagement is not predictive either
mod2b<-lm(post_ave~1+pretest.c+self.c+eng_st_ave, data=SURVEY)
summary(mod2b)

# We can visually check the statistical assumptions of our regression:
plot(mod2)
shapiro.test(resid(mod2))
vif(mod2)

# Figure 3A. -------------------------------------------------------------------
SURVEY$post_res<-resid(lm(post_ave~1+pretest.c+self.c, data=SURVEY))
SURVEY$eng_res<-resid(lm(totalENG~1+pretest.c+self.c, data=SURVEY))

#Note that residuals result from actual minus fitted values
myX<-scale_x_continuous(name = "Engagement Residuals")
myY<-scale_y_continuous(name = "Post-Test Residuals", limits=c(-500,500))
g1<-ggplot(data=SURVEY, aes(x=eng_res, y = post_res))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
    theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)


## Adding Intrinsic Motivation (Long Form) as a predictor ----------------------
SURVEY$mot.c<-SURVEY$interest.enjoy-mean(SURVEY$interest.enjoy)
mod3<-lm(post_ave~1+pretest.c+self.c+mot.c, data=SURVEY)
summary(mod3)
# Single Item motivation is not predictive either
mod2b<-lm(post_ave~1+pretest.c+self.c+mot_st_ave, data=SURVEY)
summary(mod2b)

# We can visually check the statistical assumptions of our regression:
plot(mod3)
shapiro.test(resid(mod3))
vif(mod3)

## Figure 3B. ------------------------------------------------------------------
SURVEY$mot_res<-resid(lm(interest.enjoy~1+pretest.c+self.c, data=SURVEY))
#Note that residuals are actual - fitted values
myX<-scale_x_continuous(name = "Intrinsic Motivation Residuals")
myY<-scale_y_continuous(name = "Post-Test Residuals", limits=c(-500,500))
g1<-ggplot(data=SURVEY, aes(x=mot_res, y = post_res))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
    theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)

## Adding Frontal Asymmetry as a predictor -------------------------------------
head(SURVEY)

mod4<-lm(post_ave~1+pretest.c+self.c+FAS_diff, data=SURVEY)
summary(mod4)

# We can visually check the statistical assumptions of our regression:
plot(mod4)
shapiro.test(resid(mod4))
vif(mod4)

## Figure 3C. ------------------------------------------------------------------
SURVEY$fas_res<-resid(lm(FAS_diff~1+pretest.c+self.c, data=SURVEY))
#Note that residuals are actual - fitted values
myX<-scale_x_continuous(name = "Frontal Asymmetry Residuals")
myY<-scale_y_continuous(name = "Post-Test Residuals", limits=c(-500,500))
g1<-ggplot(data=SURVEY, aes(x=fas_res, y = post_res))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
    theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)


## Adding sEBR as a predictor --------------------------------------------------
head(SURVEY)
SURVEY$Pre_sEBR.c<-SURVEY$Pre_sEBR-mean(SURVEY$Pre_sEBR, na.rm=TRUE)
mod5<-lm(post_ave~1+pretest.c+self.c+Pre_sEBR.c, data=SURVEY)
summary(mod5)

# We can visually check the statistical assumptions of our regression:
plot(mod5)
shapiro.test(resid(mod5))
vif(mod5)

## Figure 3D. ------------------------------------------------------------------
# Because one partcipant is missing eye-blink data, we need to remove that 
# participant prior to plotting.
sEBR<-subset(SURVEY, Pre_sEBR != "na")
sEBR$sebr_res<-resid(lm(Pre_sEBR~1+pretest.c+self.c, data=sEBR))
#Note that residuals are actual minus fitted values
myX<-scale_x_continuous(name = "Eye Blink Rate Residuals")
myY<-scale_y_continuous(name = "Post-Test Residuals", limits=c(-500,500))
g1<-ggplot(data=sEBR, aes(x=sebr_res, y = post_res))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
    theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)

## Effect of Choice During Acquisiton ------------------------------------------
head(SURVEY)

summary(lm(prac_ave~self.c, data=SURVEY))
summary(lm(totalENG~pretest.c+self.c, data=SURVEY))
summary(lm(interest.enjoy~pretest.c+self.c, data=SURVEY))
summary(lm(eng_st_ave~pretest.c+self.c, data=SURVEY)) # statistically significant
summary(lm(mot_st_ave~pretest.c+self.c, data=SURVEY)) # statistically significant
summary(lm(Pre_sEBR~pretest.c+self.c, data=SURVEY))
summary(lm(FAS_diff~pretest.c+self.c, data=SURVEY))


# Long-Form User Engagement to the Average of Single Item Engagement Scores
summary(lm(totalENG~1+self.c+eng_st_ave, data=SURVEY))
# Partial correlation controlling for Group
x<-resid(lm(SURVEY$totalENG~SURVEY$self.c))
y<-resid(lm(SURVEY$eng_st_ave~SURVEY$self.c))
cor.test(x,y)

# Long-Form Intrinsic Motivation to the Average of Single Item Motivation Scores
summary(lm(interest.enjoy~1+self.c+mot_st_ave, data=SURVEY))
# Partial correlation controlling for Group
x<-resid(lm(SURVEY$interest.enjoy~SURVEY$self.c))
y<-resid(lm(SURVEY$mot_st_ave~SURVEY$self.c))
cor.test(x,y)


## Engagement and Intinsic Motivation as a Function of Group -------------------
# Total LF Engagement 
summary(lm(totalENG~1+pretest.c+self.c, data=SURVEY))
# Average of ST Enagement
summary(lm(eng_st_ave~1+pretest.c+self.c, data=SURVEY))


# Total LF Intrinsic Motivation
summary(lm(interest.enjoy~1+pretest.c+self.c, data=SURVEY))
# Average of ST Intrinsic Motivation
summary(lm(mot_st_ave~1+pretest.c+self.c, data=SURVEY))

## Figure 4 --------------------------------------------------------------------
# Long form Engagement by Group
g1<-ggplot(SURVEY, aes(x = group, y = totalENG, fill=group)) +
  geom_jitter(pch=19, fill="black", size=2, width=0.1) + 
  geom_boxplot(alpha = .75, outlier.shape=NA, notch=TRUE, width=0.3)+
  scale_fill_manual(values=c("grey40","white"))

g2<-g1+scale_x_discrete(name = "Group") +
  scale_y_continuous(name = "Long-Form Engagement", limits = c(0,10))
g3 <- g2 + theme_bw() + theme(axis.text=element_text(size=16, colour="black"), 
                              axis.title=element_text(size=16,face="bold")) + 
  theme(legend.position="none")
plot(g3) 

# Single Trial Engagement by Group
g1<-ggplot(SURVEY, aes(x = group, y = eng_st_ave, fill=group)) +
  geom_jitter(pch=19, fill="black", size=2, width=0.1) + 
  geom_boxplot(alpha = .75, outlier.shape=NA, notch=TRUE, width=0.3)+
  scale_fill_manual(values=c("grey40","white"))
g2<-g1+scale_x_discrete(name = "Group") +
  scale_y_continuous(name = "Single-Trial Engagement", limits = c(0,10))
g3 <- g2 + theme_bw() + theme(axis.text=element_text(size=16, colour="black"), 
                              axis.title=element_text(size=16,face="bold")) +
  theme(legend.position="none")
plot(g3) 


# Long form Intrinsic Motivation by Group
g1<-ggplot(SURVEY, aes(x = group, y = interest.enjoy, fill=group)) +
  geom_jitter(pch=19, fill="black", size=2, width=0.1) + 
  geom_boxplot(alpha = .75, outlier.shape=NA, notch=TRUE, width=0.3)+
  scale_fill_manual(values=c("grey40","white"))
g2<-g1+scale_x_discrete(name = "Group") +
  scale_y_continuous(name = "Long-Form Intrinsic Motivation", limits = c(0,10))
g3 <- g2 + theme_bw() + theme(axis.text=element_text(size=16, colour="black"), 
                              axis.title=element_text(size=16,face="bold")) + 
  theme(legend.position="none")
plot(g3) 

# Single Trial Intrinsic Motivation by Group
g1<-ggplot(SURVEY, aes(x = group, y = mot_st_ave, fill=group)) +
  geom_jitter(pch=19, fill="black", size=2, width=0.1) + 
  geom_boxplot(alpha = .75, outlier.shape=NA, notch=TRUE, width=0.3)+
  scale_fill_manual(values=c("grey40","white"))
g2<-g1+scale_x_discrete(name = "Group") +
  scale_y_continuous(name = "Single-Trial Intrinsic Motivation", limits = c(0,10))
g3 <- g2 + theme_bw() + theme(axis.text=element_text(size=16, colour="black"), 
                              axis.title=element_text(size=16,face="bold")) +
  theme(legend.position="none")
plot(g3) 

## Relationships between survey variables and FAS ------------------------------
# FAS and User Engagement Scale
summary(lm(FAS_diff~1+self.c+Focused.Attention, data=SURVEY))
summary(lm(FAS_diff~1+self.c+Usability, data=SURVEY)) # Statistically significant
summary(lm(FAS_diff~1+self.c+Aesthetics, data=SURVEY))
summary(lm(FAS_diff~1+self.c+Endurability, data=SURVEY))
summary(lm(FAS_diff~1+self.c+Novelty, data=SURVEY))
summary(lm(FAS_diff~1+self.c+Involvement, data=SURVEY))
summary(lm(FAS_diff~1+self.c+totalENG, data=SURVEY))

# FAS and Intrinsic Motivation Inventory
summary(lm(FAS_diff~1+self.c+interest.enjoy, data=SURVEY))
summary(lm(FAS_diff~1+self.c+competence, data=SURVEY))
summary(lm(FAS_diff~1+self.c+effort, data=SURVEY))
summary(lm(FAS_diff~1+self.c+pressure.tension, data=SURVEY))


## Relationship between Survey Variables and sEBR  -----------------------------
# sEBR and User Engagement Scale
summary(lm(Pre_sEBR.c~1+self.c+Focused.Attention, data=SURVEY))
summary(lm(Pre_sEBR.c~1+self.c+Usability, data=SURVEY))
summary(lm(Pre_sEBR.c~1+self.c+Aesthetics, data=SURVEY))
summary(lm(Pre_sEBR.c~1+self.c+Endurability, data=SURVEY))
summary(lm(Pre_sEBR.c~1+self.c+Novelty, data=SURVEY))
summary(lm(Pre_sEBR.c~1+self.c+Involvement, data=SURVEY))
summary(lm(Pre_sEBR.c~1+self.c+totalENG, data=SURVEY))

# sEBR and Intrinsic Motivation Inventory
summary(lm(Pre_sEBR.c~1+self.c+interest.enjoy, data=SURVEY))
summary(lm(Pre_sEBR.c~1+self.c+competence, data=SURVEY))
summary(lm(Pre_sEBR.c~1+self.c+effort, data=SURVEY))
summary(lm(Pre_sEBR.c~1+self.c+pressure.tension, data=SURVEY))

## Relationship between FAS and sEBR -------------------------------------------
summary(lm(FAS_diff~1+self.c+Pre_sEBR.c, data=SURVEY))
summary(lm(rest_FAS~1+self.c+Pre_sEBR.c, data=SURVEY))


## Relationships between IMI and Engagment -------------------------------------
## Relationship between IMI and Engagement
summary(lm(totalENG~1+self.c+interest.enjoy, data=SURVEY))
x<-resid(lm(SURVEY$totalENG~SURVEY$self.c))
y<-resid(lm(SURVEY$interest.enjoy~SURVEY$self.c))
cor.test(x,y)



## Figure 4 --------------------------------------------------------------------
## Plots Showing FAS_prac_ave against difficulty and changes in difficulty
res1<-resid(lm(FAS_diff~1+self.c, data=SURVEY))
res2<-resid(lm(Usability~1+self.c, data=SURVEY))
res3<-resid(lm(competence~1+self.c, data=SURVEY))
res4<-resid(lm(effort~1+self.c, data=SURVEY))

## Figure 5 -------------------------------------------------------------------
# FAS residuals versus Usability Residuals
myX<-scale_x_continuous(name = "Usability Residuals", limits=c(-4,4))
myY<-scale_y_continuous(name = "Frontal Asymmetry Residuals", limits=c(-2,2))
g1<-ggplot(data=SURVEY, aes(x=res2, y = res1))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)

# FAS residuals versus Competence Residuals
myX<-scale_x_continuous(name = "Competence Residuals", limits=c(-4,4))
myY<-scale_y_continuous(name = "Frontal Asymmetry Residuals", limits=c(-2,2))
g1<-ggplot(data=SURVEY, aes(x=res3, y = res1))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)

# FAS residuals versus Effort Residuals
myX<-scale_x_continuous(name = "Effort Residuals", limits=c(-4,4))
myY<-scale_y_continuous(name = "Frontal Asymmetry Residuals", limits=c(-2,2))
g1<-ggplot(data=SURVEY, aes(x=res4, y = res1))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)

# FAS residuals versus sEBR Residuals
res5<-resid(lm(FAS_diff~1+self.c, data=sEBR))
res6<-resid(lm(Pre_sEBR~1+self.c, data=sEBR))

myX<-scale_x_continuous(name = "sEBR Residuals", limits = c(-50,50))
myY<-scale_y_continuous(name = "Frontal Asymmetry Residuals", limits = c(-2,2))
g1<-ggplot(data=sEBR, aes(x=res6, y = res5))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)
## -----------------------------------------------------------------------------




## Supplemental Analyses for Appendix III --------------------------------------
# Correlations between resting, practice, and pre-test FAS ---------------------
head(SURVEY)
cor.test(SURVEY$rest_FAS, SURVEY$pre_FAS)
cor.test(SURVEY$rest_FAS, SURVEY$prac_FAS_AVE)
cor.test(SURVEY$rest_FAS, SURVEY$FAS_diff)


myX<-scale_x_continuous(name = "Resting FAS")
myY<-scale_y_continuous(name = "Pre-Test FAS")
g1<-ggplot(data=SURVEY, aes(x=rest_FAS, y = pre_FAS))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)


myX<-scale_x_continuous(name = "Resting FAS")
myY<-scale_y_continuous(name = "Average Practice FAS")
g1<-ggplot(data=SURVEY, aes(x=rest_FAS, y = prac_FAS_AVE))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)

myX<-scale_x_continuous(name = "Resting FAS")
myY<-scale_y_continuous(name = "Delta FAS (Practice - Resting)")
g1<-ggplot(data=SURVEY, aes(x=rest_FAS, y = FAS_diff))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)


# Correlations between Beck Depression Inventory and Physio Measures -----------
cor.test(SURVEY$totalBDI, SURVEY$pre_FAS)
cor.test(SURVEY$totalBDI, SURVEY$prac_FAS_AVE)
cor.test(SURVEY$totalBDI, SURVEY$FAS_diff)
cor.test(SURVEY$totalBDI, SURVEY$Pre_sEBR)

myX<-scale_x_continuous(name = "Beck Depression Inventory Total")
myY<-scale_y_continuous(name = "Delta FAS (Practice - Resting)")
g1<-ggplot(data=SURVEY, aes(x=totalBDI, y = FAS_diff))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)

myX<-scale_x_continuous(name = "Beck Depression Inventory Total")
myY<-scale_y_continuous(name = "Resting FAS")
g1<-ggplot(data=SURVEY, aes(x=totalBDI, y = rest_FAS))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)




## Relationship between Engagement/IMI and Gameplay ----------------------------
# Engagement and the average score/difficulty during practice
SURVEY$prac_ave.c<-SURVEY$prac_ave-mean(SURVEY$prac_ave)
SURVEY$prac_diff.c<-SURVEY$prac_diff_ave-mean(SURVEY$prac_diff_ave)
SURVEY$prac_diff_sd.c<-SURVEY$prac_diff_sd-mean(SURVEY$prac_diff_sd)
SURVEY$prac_diff_switch.c<-SURVEY$prac_diff_switch-mean(SURVEY$prac_diff_switch)

summary(lm(totalENG~1+pretest+self.c*prac_ave.c, data=SURVEY))
summary(lm(totalENG~1+pretest+self.c*prac_diff.c, data=SURVEY))
summary(lm(totalENG~1+pretest+self.c*prac_diff_switch.c, data=SURVEY))

# Engagement and the average score/difficulty during practice
summary(lm(interest.enjoy~1+pretest+self.c*prac_ave.c, data=SURVEY))
summary(lm(interest.enjoy~1+pretest+self.c*prac_diff.c, data=SURVEY))
summary(lm(interest.enjoy~1+pretest+self.c*prac_diff_switch.c, data=SURVEY))

# Relationship between Score and Total Engagement
myX<-scale_x_continuous(name = "Average Score During Practice")
myY<-scale_y_continuous(name = "Total Engagment Score")
g1<-ggplot(data=SURVEY, aes(x=prac_ave, y = totalENG))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)


# Relationship between Score and Intrinsic Motivation
myX<-scale_x_continuous(name = "Average Score During Practice")
myY<-scale_y_continuous(name = "Interest/Enjoyment Score")
g1<-ggplot(data=SURVEY, aes(x=prac_ave, y = interest.enjoy))+
  geom_point(aes(fill=group), pch=21, stroke = 1, size=3)+
  scale_fill_manual(values=c("grey40","white"))+
  stat_smooth(aes(lty=group), col="black", method=lm, se=FALSE, size=1.5)+
  myX+myY
g2<-g1+theme_bw()+theme(axis.text=element_text(size=14, colour="black"), 
                        axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
print(g2)



# Correlations between variables in the SC Group Only:
# Engagement
SELF<-subset(SURVEY, group=="self")
cor.test(SELF$totalENG, SELF$prac_ave)
cor.test(SELF$totalENG, SELF$prac_diff_ave)
cor.test(SELF$totalENG, SELF$prac_diff_switch)

# Intrinsic Motivation
cor.test(SELF$interest.enjoy, SELF$prac_ave)
cor.test(SELF$interest.enjoy, SELF$prac_diff_ave)
cor.test(SELF$interest.enjoy, SELF$prac_diff_switch)

