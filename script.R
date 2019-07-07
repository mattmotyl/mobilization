library(Hmisc)
library(rockchalk)
library(stargazer)
library(apaTables)
library(psych)
library(effects)
library(ggplot2)
library(lavaan)
library(relaimpo)
library(plyr)
library(gridExtra)
library(pscl)
library(psychometric)

# Set working directory (wherever the "anes_timeseries_2016_new_voteval_osf.csv" file is located)
setwd("data/")

# Read in dataset
dat1 <- read.csv("anes_timeseries_2016_new_voteval_osf.csv", header = T)


### DESIGNATE MISSING, RESCALE, AND RESTRUCTURE VARIABLES OF INTEREST ###
#####
## SAMPLE WEIGHT ##
# Recoding sample weights for full sample pre and post items
dat1$weight <- dat1$V160102


## DEMOGRAPHIC CONTROLS ##
# AGE #
# Designate missing values
is.na(dat1$V161267) <- dat1$V161267 == -9 | dat1$V161267 == -8
dat1$age <- dat1$V161267

# GENDER #
# Designate missing values and effects code (to average across gender as control variable)
# 1 = male, 0 = female
is.na(dat1$V161342) <- dat1$V161342 == -9 | dat1$V161342 == -8 | dat1$V161342 == 3
dat1$gender <- ifelse(dat1$V161342 == 2, 0, 1)

# EDUCATION #
# Designate missing values
# Dichotomize to just two categories, 1 = college degree, 0 = no college degree
is.na(dat1$V161270) <- dat1$V161270 == -9 | dat1$V161270 == -8 | dat1$V161270 == 90 | dat1$V161270 == 95
dat1$edu <- ifelse(dat1$V161270 < 13, 0, 1)

# INCOME #
# Designate missing values
is.na(dat1$V161361x) <- dat1$V161361x == -9 | dat1$V161361x == -5
dat1$income <- dat1$V161361x

# RACE #
# Designate missing values
# Dichotomize to just two categories, 1 = White, 0 = non-White
is.na(dat1$V161310x) <- dat1$V161310x == -9
dat1$race <- ifelse(dat1$V161310x == 1, 1, 0)


## CONGRESSIONAL DISTRICT PERCENT VOTE FOR TRUMP ##
# Center congressional district ideology at 50/50 split betwen Trump and Clinton
# Higher scores indicate greater percentage vote for Trump
dat1$Trump2016.c <- dat1$Trump2016 - 50
dat1$Obama2012.c <- dat1$Obama2012 - 50


## PARTISAN VOTER INDEX FOR 2016 BY CONGRESSIONAL DISTRICT ##
# Already centered at 0 (even split between Democrat and Republican)
# Higher scores indicate more Republican district
dat1$PVI2016 <- dat1$raw16PVIuse


## POLITICAL ORIENTATION ##
# Designate missing values and center political orientation at the midpoint
# Higher scores indicate greater conservatism
is.na(dat1$V161126) <- dat1$V161126 == -9 | dat1$V161126 == -8 | dat1$V161126 == 99
dat1$po <- dat1$V161126
dat1$po.c <- dat1$po - 4


## PARTY IDENTIFICATION ##
# Designate missing values and center party identification at the midpoint
# Higher scores indicate stronger Republican
is.na(dat1$V161158x) <- dat1$V161158x == -9 | dat1$V161158x == -8 | dat1$V161158x == -2
dat1$partyid <- dat1$V161158x
dat1$partyid.c <- dat1$partyid - 4


## SELF-REPORT POST-ELECTION VOTE ##
# Designate missing values
# 1 = voted, 0 = did not vote
is.na(dat1$V162031x) <- dat1$V162031x == -9 | dat1$V162031x == -8 | dat1$V162031x == -7 | 
                        dat1$V162031x == -6 | dat1$V162031x == -1 | dat1$V161026 == 2 | dat1$V162034 == 2
dat1$didvote <- dat1$V162031x


## VALIDATED VOTE ##
# Turnout in the 2016 General Election weighted by the clerical review. It is equal to the product between clerical review and vote2016
# 1 = voted, 0 = did not vote
# vote2016_clerical (already coded)


## DOES WHO VOTE FOR MAKE A DIFFERENCE ##
# Designate missing values
# Higher scores indicate greater perceived efficacy
is.na(dat1$V162282) <- dat1$V162282 == -9 | dat1$V162282 == -8 | dat1$V162282 == -7 | dat1$V162282 == -6
dat1$post.votemakediff <- dat1$V162282


## DOES WHO IS IN POWER MAKE A DIFFERENCE ##
# Designate missing values
# Higher scores indicate greater perceived efficacy
is.na(dat1$V162281) <- dat1$V162281 == -9 | dat1$V162281 == -8 | dat1$V162281 == -7 | dat1$V162281 == -6
dat1$post.powermakediff <- dat1$V162281


## DEMOCRATIC EFFICACY ##
# Average of Vote and Power Efficacy variables
# Higher scores indicate greater perceived democratic efficacy
dat1$post.efficacymakediff <- rowMeans(cbind(dat1$post.powermakediff, dat1$post.votemakediff), na.rm = T)


## FEELING THERMOMETER TOWARD DEMOCRATIC CANDIDATE ##
is.na(dat1$V161086) <- dat1$V161086 == -99 | dat1$V161086 == -89 | dat1$V161086 == -88
dat1$pre.feeling.demprescand <- dat1$V161086


## FEELING THERMOMETER TOWARD REPUBLICAN CANDIDATE ##
is.na(dat1$V161087) <- dat1$V161087 == -99 | dat1$V161087 == -89 | dat1$V161087 == -88
dat1$pre.feeling.repprescand <- dat1$V161087


## AMBIVALENCE TOWARDS CANDIDATES ##
dat1$pre.cand.ambivalence <- ((dat1$pre.feeling.demprescand + dat1$pre.feeling.repprescand) / 2) - 
                             abs(dat1$pre.feeling.demprescand - dat1$pre.feeling.repprescand)


## POLITICAL INTEREST ##
is.na(dat1$V161003) <- dat1$V161003 == -9 | dat1$V161003 == -8
dat1$pre.polinterest <- 6 - dat1$V161003


## HOW LONG RESIDE IN COMMUNITY ##
is.na(dat1$V161331a) <- dat1$V161331a == -9
dat1$pre.reside <- dat1$V161331a

#####


### ANALYSES ###

## SELF-REPORT POST-ELECTION VOTE ##
# Getting non-missing cases for variables used in analysis
dat1sub1 <- na.omit(dat1[,c("PVI2016","partyid","partyid.c","didvote","weight","age","gender","edu","income","race","pre.polinterest","pre.reside")])

# Creating scaled (0 to 1) person and place ideology and age and income
dat1sub1$PVI2016.scale <- (dat1sub1$PVI2016 - min(dat1sub1$PVI2016)) / 
                          (max(dat1sub1$PVI2016) - min(dat1sub1$PVI2016))
dat1sub1$partyid.scale <- (dat1sub1$partyid - min(dat1sub1$partyid)) / 
                          (max(dat1sub1$partyid) - min(dat1sub1$partyid))
dat1sub1$age.scale <- (dat1sub1$age - min(dat1sub1$age)) / 
                      (max(dat1sub1$age) - min(dat1sub1$age))
dat1sub1$income.scale <- (dat1sub1$income - min(dat1sub1$income)) / 
                         (max(dat1sub1$income) - min(dat1sub1$income))
dat1sub1$pre.polinterest.scale <- (dat1sub1$pre.polinterest - min(dat1sub1$pre.polinterest)) / 
                                  (max(dat1sub1$pre.polinterest) - min(dat1sub1$pre.polinterest))
dat1sub1$pre.reside.scale <- (dat1sub1$pre.reside - min(dat1sub1$pre.reside)) / 
                             (max(dat1sub1$pre.reside) - min(dat1sub1$pre.reside))

# Centering scaled versions around midpoint (.5) for person and place ideology
dat1sub1$PVI2016.scale.c <- dat1sub1$PVI2016.scale - .5
dat1sub1$partyid.scale.c <- dat1sub1$partyid.scale - .5

# Test using scaled variables
# With controls
test1swc <- glm(didvote ~ PVI2016.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub1, family = binomial(link = "logit"), weights = weight)
summary(test1swc)
exp(summary(test1swc)$coefficients)
round(pR2(test1swc),3)
exp(confint(test1swc))

# Simple slope follow ups
dat1sub1$partyid.scale.dem <- dat1sub1$partyid.scale.c + .5
dat1sub1$partyid.scale.rep <- dat1sub1$partyid.scale.c - .5

test1swcdem <- glm(didvote ~ PVI2016.scale.c * partyid.scale.dem + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub1, family = binomial(link = "logit"), weights = weight)
summary(test1swcdem)
exp(summary(test1swcdem)$coefficients)
exp(confint(test1swcdem))
test1swcrep <- glm(didvote ~ PVI2016.scale.c * partyid.scale.rep + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub1, family = binomial(link = "logit"), weights = weight)
summary(test1swcrep)
exp(summary(test1swcrep)$coefficients)
exp(confint(test1swcrep))

# GG Plot
#####
# Getting the effects for the partyid by PVI interaction at most Democratic and most Republican id
# Specifying the full range of PVI
didvotescale.PVIAtPO.fitted<-effect('PVI2016.scale.c*partyid.scale.c', test1swc,
                                     xlevels=list(partyid.scale.c=c(-.5,.5),
                                     PVI2016.scale.c = seq(from = -.5, to = .5, by = .1)), 
                                     se=TRUE, confidence.level=.95)
# Saving the effects as a data frame
didvotescale.PVIAtPO.Eff<-as.data.frame(didvotescale.PVIAtPO.fitted)
# Recoding the moderator variable (PO) as a factor with the names of the groups
didvotescale.PVIAtPO.Eff$pomod<-factor(didvotescale.PVIAtPO.Eff$partyid.scale.c,
                                    levels=c(-.5,.5),
                                    labels=c("Democrats","Republicans"))
# Plotting the figure
didvotescale.PVIAtPO.Plot2016 <-ggplot(data = didvotescale.PVIAtPO.Eff, aes(x = PVI2016.scale.c, y =fit, group=pomod))+
  coord_cartesian(xlim=c(-.5, .5),ylim = c(0,1))+ 
  scale_x_continuous(breaks = c(-.5,-.4,.4,.5), labels = c("","Most Democratic","Most Republican",""))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .25), labels = c("0%","25%","50%","75%","100%"))+
  geom_line(aes(color=pomod, linetype = pomod),size=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=pomod),alpha=.2)+
  xlab("2016 Congressional District PVI")+
  ylab("Vote Probability")+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  theme_bw()+
  theme(text=element_text(family="", face="bold", size=14),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(size = .5, colour = "grey0"),
        legend.title=element_blank(),
        legend.position = c(.25, .27))
#####
didvotescale.PVIAtPO.Plot2016
#####

#####
### Panel plots ###
grid.arrange(didvotescale.PVIAtPO.Plot2016, verifiedvotescale.PVIAtPO.Plot, nrow = 1, ncol = 2)
#####


# Testing if political interest or years living in community changes as a function of person and place politics
test1interest <- lm(pre.polinterest.scale ~ PVI2016.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race, data = dat1sub1, weights = weight)
summary(test1interest)

test1reside <- lm(pre.reside.scale ~ PVI2016.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race, data = dat1sub1, weights = weight)
summary(test1reside)


## VALIDATED POST-ELECTION VOTE ##
# Getting non-missing cases for variables used in analysis
dat1sub2 <- na.omit(dat1[,c("PVI2016","partyid","partyid.c","vote2016_clerical","weight","age","gender","edu","income","race","pre.polinterest","pre.reside")])

# Creating scaled (0 to 1) person and place ideology and age and income
dat1sub2$PVI2016.scale <- (dat1sub2$PVI2016 - min(dat1sub2$PVI2016)) / 
                          (max(dat1sub2$PVI2016) - min(dat1sub2$PVI2016))
dat1sub2$partyid.scale <- (dat1sub2$partyid - min(dat1sub2$partyid)) / 
                          (max(dat1sub2$partyid) - min(dat1sub2$partyid))
dat1sub2$age.scale <- (dat1sub2$age - min(dat1sub2$age)) / 
                      (max(dat1sub2$age) - min(dat1sub2$age))
dat1sub2$income.scale <- (dat1sub2$income - min(dat1sub2$income)) / 
                         (max(dat1sub2$income) - min(dat1sub2$income))
dat1sub2$pre.polinterest.scale <- (dat1sub2$pre.polinterest - min(dat1sub2$pre.polinterest)) / 
                                  (max(dat1sub2$pre.polinterest) - min(dat1sub2$pre.polinterest))
dat1sub2$pre.reside.scale <- (dat1sub2$pre.reside - min(dat1sub2$pre.reside)) / 
                             (max(dat1sub2$pre.reside) - min(dat1sub2$pre.reside))

# Centering scaled versions around midpoint (.5) for person and place ideology
dat1sub2$PVI2016.scale.c <- dat1sub2$PVI2016.scale - .5
dat1sub2$partyid.scale.c <- dat1sub2$partyid.scale - .5

# Test using scaled variables
# With controls
test2swc <- glm(vote2016_clerical ~ PVI2016.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub2, family = binomial(link = "logit"), weights = weight)
summary(test2swc)
exp(summary(test2swc)$coefficients)
round(pR2(test2swc),3)
exp(confint(test2swc))

# Simple slope follow ups
dat1sub2$partyid.scale.dem <- dat1sub2$partyid.scale.c + .5
dat1sub2$partyid.scale.rep <- dat1sub2$partyid.scale.c - .5

test2swcdem <- glm(vote2016_clerical ~ PVI2016.scale.c * partyid.scale.dem + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub2, family = binomial(link = "logit"), weights = weight)
summary(test2swcdem)
exp(summary(test2swcdem)$coefficients)
exp(confint(test2swcdem))
test2swcrep <- glm(vote2016_clerical ~ PVI2016.scale.c * partyid.scale.rep + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub2, family = binomial(link = "logit"), weights = weight)
summary(test2swcrep)
exp(summary(test2swcrep)$coefficients)
exp(confint(test2swcrep))

# GG Plot
#####
# Getting the effects for the partyid by PVI interaction at most Democratic and most Republican id
# Specifying the full range of PVI
votevalscale.PVIAtPO.fitted<-effect('PVI2016.scale.c*partyid.scale.c', test2swc,
                                    xlevels=list(partyid.scale.c=c(-.5,.5),
                                                 PVI2016.scale.c = seq(from = -.5, to = .5, by = .1)), 
                                    se=TRUE, confidence.level=.95)
# Saving the effects as a data frame
votevalscale.PVIAtPO.Eff<-as.data.frame(votevalscale.PVIAtPO.fitted)
# Recoding the moderator variable (PO) as a factor with the names of the groups
votevalscale.PVIAtPO.Eff$pomod<-factor(votevalscale.PVIAtPO.Eff$partyid.scale.c,
                                       levels=c(-.5,.5),
                                       labels=c("Democrats","Republicans"))
# Plotting the figure
votevalscale.PVIAtPO.Plot2016 <-ggplot(data = votevalscale.PVIAtPO.Eff, aes(x = PVI2016.scale.c, y =fit, group=pomod))+
  coord_cartesian(xlim=c(-.5, .5),ylim = c(0,1))+ 
  scale_x_continuous(breaks = c(-.5,-.4,.4,.5), labels = c("","Most Democratic","Most Republican",""))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .25), labels = c("0%","25%","50%","75%","100%"))+
  geom_line(aes(color=pomod, linetype = pomod),size=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=pomod),alpha=.2)+
  xlab("2016 Congressional District PVI")+
  ylab("Validated Vote Probability")+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  theme_bw()+
  theme(text=element_text(family="", face="bold", size=14),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(size = .5, colour = "grey0"),
        legend.title=element_blank(),
        legend.position = c(.25, .27))
#####
votevalscale.PVIAtPO.Plot2016
#####

#####
### Panel plots ###
grid.arrange(votevalscale.PVIAtPO.Plot2016, validatedvotescale.PVIAtPO.Plot, nrow = 1, ncol = 2)
#####


# Testing if political interest or years living in community changes as a function of person and place politics
test2interest <- lm(pre.polinterest.scale ~ PVI2016.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race, data = dat1sub2, weights = weight)
summary(test2interest)


test2reside <- lm(pre.reside.scale ~ PVI2016.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race, data = dat1sub2, weights = weight)
summary(test2reside)


## AMBIVALENCE ##
# Getting non-missing cases for variables used in analysis
dat1sub3 <- na.omit(dat1[,c("PVI2016","partyid","partyid.c","pre.cand.ambivalence","weight","age","gender","edu","income","race","pre.polinterest","pre.reside")])

# Creating scaled (0 to 1) person and place ideology and age and income and life satisfaction
dat1sub3$PVI2016.scale <- (dat1sub3$PVI2016 - min(dat1sub3$PVI2016)) / 
                          (max(dat1sub3$PVI2016) - min(dat1sub3$PVI2016))
dat1sub3$partyid.scale <- (dat1sub3$partyid - min(dat1sub3$partyid)) / 
                          (max(dat1sub3$partyid) - min(dat1sub3$partyid))
dat1sub3$age.scale <- (dat1sub3$age - min(dat1sub3$age)) / 
                      (max(dat1sub3$age) - min(dat1sub3$age))
dat1sub3$income.scale <- (dat1sub3$income - min(dat1sub3$income)) / 
                         (max(dat1sub3$income) - min(dat1sub3$income))
dat1sub3$pre.cand.ambivalence.scale <- (dat1sub3$pre.cand.ambivalence - min(dat1sub3$pre.cand.ambivalence)) / 
                                       (max(dat1sub3$pre.cand.ambivalence) - min(dat1sub3$pre.cand.ambivalence))
dat1sub3$pre.polinterest.scale <- (dat1sub3$pre.polinterest - min(dat1sub3$pre.polinterest)) / 
                                  (max(dat1sub3$pre.polinterest) - min(dat1sub3$pre.polinterest))
dat1sub3$pre.reside.scale <- (dat1sub3$pre.reside - min(dat1sub3$pre.reside)) / 
                             (max(dat1sub3$pre.reside) - min(dat1sub3$pre.reside))

# Centering scaled versions around midpoint (.5) for person and place ideology
dat1sub3$PVI2016.scale.c <- dat1sub3$PVI2016.scale - .5
dat1sub3$partyid.scale.c <- dat1sub3$partyid.scale - .5

# Test using scaled variables
# With controls
test3swc <- lm(pre.cand.ambivalence.scale ~ PVI2016.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub3, weights = weight)
summary(test3swc)
calc.relimp(test3swc)
CI.Rsq(.0189, 3335, 10)

# Simple slope follow ups
dat1sub3$partyid.scale.dem <- dat1sub3$partyid.scale.c + .5
dat1sub3$partyid.scale.rep <- dat1sub3$partyid.scale.c - .5

test3swcdem <- lm(pre.cand.ambivalence.scale ~ PVI2016.scale.c * partyid.scale.dem + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub3, weights = weight)
summary(test3swcdem)
test3swcrep <- lm(pre.cand.ambivalence.scale ~ PVI2016.scale.c * partyid.scale.rep + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub3, weights = weight)
summary(test3swcrep)
#####


## DEMOCRATIC EFFICACY ##
# Getting non-missing cases for variables used in analysis
dat1sub4 <- na.omit(dat1[,c("PVI2016","partyid","partyid.c","post.efficacymakediff","weight","age","gender","edu","income","race","pre.polinterest","pre.reside")])

# Creating scaled (0 to 1) person and place ideology and age and income and democratic efficacy
dat1sub4$PVI2016.scale <- (dat1sub4$PVI2016 - min(dat1sub4$PVI2016)) / 
                          (max(dat1sub4$PVI2016) - min(dat1sub4$PVI2016))
dat1sub4$partyid.scale <- (dat1sub4$partyid - min(dat1sub4$partyid)) / 
                          (max(dat1sub4$partyid) - min(dat1sub4$partyid))
dat1sub4$age.scale <- (dat1sub4$age - min(dat1sub4$age)) / 
                      (max(dat1sub4$age) - min(dat1sub4$age))
dat1sub4$income.scale <- (dat1sub4$income - min(dat1sub4$income)) / 
                         (max(dat1sub4$income) - min(dat1sub4$income))
dat1sub4$post.efficacymakediff.scale <- (dat1sub4$post.efficacymakediff - min(dat1sub4$post.efficacymakediff)) / 
                                        (max(dat1sub4$post.efficacymakediff) - min(dat1sub4$post.efficacymakediff))
dat1sub4$pre.polinterest.scale <- (dat1sub4$pre.polinterest - min(dat1sub4$pre.polinterest)) / 
                                  (max(dat1sub4$pre.polinterest) - min(dat1sub4$pre.polinterest))
dat1sub4$pre.reside.scale <- (dat1sub4$pre.reside - min(dat1sub4$pre.reside)) / 
                             (max(dat1sub4$pre.reside) - min(dat1sub4$pre.reside))

# Centering scaled versions around midpoint (.5) for person and place ideology
dat1sub4$PVI2016.scale.c <- dat1sub4$PVI2016.scale - .5
dat1sub4$partyid.scale.c <- dat1sub4$partyid.scale - .5

# Test using scaled variables
# With controls
test4swc <- lm(post.efficacymakediff.scale ~ PVI2016.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub4, weights = weight)
summary(test4swc)
calc.relimp(test4swc)
CI.Rsq(.0082, 3365, 10)

# Simple slope follow ups
dat1sub4$partyid.scale.dem <- dat1sub4$partyid.scale.c + .5
dat1sub4$partyid.scale.rep <- dat1sub4$partyid.scale.c - .5

test4swcdem <- lm(post.efficacymakediff.scale ~ PVI2016.scale.c * partyid.scale.dem + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub4, weights = weight)
summary(test4swcdem)
test4swcrep <- lm(post.efficacymakediff.scale ~ PVI2016.scale.c * partyid.scale.rep + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub4, weights = weight)
summary(test4swcrep)
#####

# For regression table in supplemental materials
stargazer(test2swc,test3swc,test4swc,type="text",
          column.labels = c("Validated Vote", "Ambivalence", "Democratic Efficacy"),
          intercept.bottom = FALSE, star.cutoffs = c(.05,.01,.001),
          single.row=FALSE, 
          notes.append = FALSE,
          header=FALSE)
#####


### MODERATED MEDIATION TESTING MULTIPLE MEDIATION WITH EFFICACY AND AMBIVALENCE ###
#####
dat1sub5 <- na.omit(dat1[,c("PVI2016","partyid","partyid.c","vote2016_clerical","post.efficacymakediff","pre.cand.ambivalence","weight","age","gender","edu","income","race","pre.polinterest","pre.reside")])

# Creating scaled (0 to 1) person and place ideology and age and income and democratic efficacy
dat1sub5$PVI2016.scale <- (dat1sub5$PVI2016 - min(dat1sub5$PVI2016)) / 
                          (max(dat1sub5$PVI2016) - min(dat1sub5$PVI2016))
dat1sub5$partyid.scale <- (dat1sub5$partyid - min(dat1sub5$partyid)) / 
                          (max(dat1sub5$partyid) - min(dat1sub5$partyid))
dat1sub5$age.scale <- (dat1sub5$age - min(dat1sub5$age)) / 
                      (max(dat1sub5$age) - min(dat1sub5$age))
dat1sub5$income.scale <- (dat1sub5$income - min(dat1sub5$income)) / 
                        (max(dat1sub5$income) - min(dat1sub5$income))
dat1sub5$pre.polinterest.scale <- (dat1sub5$pre.polinterest - min(dat1sub5$pre.polinterest)) / 
                                  (max(dat1sub5$pre.polinterest) - min(dat1sub5$pre.polinterest))
dat1sub5$pre.reside.scale <- (dat1sub5$pre.reside - min(dat1sub5$pre.reside)) / 
                             (max(dat1sub5$pre.reside) - min(dat1sub5$pre.reside))
dat1sub5$post.efficacymakediff.scale <- (dat1sub5$post.efficacymakediff - min(dat1sub5$post.efficacymakediff)) / 
                                        (max(dat1sub5$post.efficacymakediff) - min(dat1sub5$post.efficacymakediff))
dat1sub5$pre.cand.ambivalence.scale <- (dat1sub5$pre.cand.ambivalence - min(dat1sub5$pre.cand.ambivalence)) / 
                                       (max(dat1sub5$pre.cand.ambivalence) - min(dat1sub5$pre.cand.ambivalence))

# Centering scaled versions around midpoint (.5) for person and place ideology
dat1sub5$PVI2016.scale.c <- dat1sub5$PVI2016.scale - .5
dat1sub5$partyid.scale.c <- dat1sub5$partyid.scale - .5

# Compute interaction term for use in lavaan (it's weird with on the fly interaction terms)
dat1sub5$PVI2016.scale.cXpartyid.scale.c <- dat1sub5$PVI2016.scale.c * dat1sub5$partyid.scale.c

# Scaled analysis with controls using lavaan
efficacyambivalence.model <- '
#Regressions
post.efficacymakediff.scale ~ a11*PVI2016.scale.c + a12*partyid.scale.c + a13*PVI2016.scale.cXpartyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale
pre.cand.ambivalence.scale ~ a21*PVI2016.scale.c + a22*partyid.scale.c + a23*PVI2016.scale.cXpartyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale
vote2016_clerical ~ cdash1*PVI2016.scale.c + cdash2*partyid.scale.c + cdash3*PVI2016.scale.cXpartyid.scale.c + b1*post.efficacymakediff.scale + b2*pre.cand.ambivalence.scale + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale

#Indirect effects thru efficacy conditional on moderator (a1 + a3*ModValue)*b1
indirect.eff.dems := (a11 + a13*(-.5))*b1
indirect.eff.repubs := (a11 + a13*(.5))*b1

#Indirect effects thru ambivalence conditional on moderator (a1 + a3*ModValue)*b1
indirect.amb.dems := (a21 + a23*(-.5))*b2
indirect.amb.repubs := (a21 + a23*(.5))*b2

#Direct effects conditional on moderator (cdash1 + cdash3*ModValue)
direct.dems := cdash1 + cdash3*(-.5) 
direct.repubs := cdash1 + cdash3*(.5)

#Total effects conditional on moderator
total.dems := direct.dems + indirect.eff.dems + indirect.amb.dems
total.repubs := direct.repubs + indirect.eff.repubs + indirect.amb.repubs

#Proportion mediated conditional on moderator
prop.mediated.eff.dems := indirect.eff.dems / total.dems
prop.mediated.eff.repubs := indirect.eff.repubs / total.repubs
prop.mediated.amb.dems := indirect.amb.dems / total.dems
prop.mediated.amb.repubs := indirect.amb.repubs / total.repubs

#Correlated residual variances for mediators
post.efficacymakediff.scale ~~ pre.cand.ambivalence.scale

#Index of moderated mediation
#An alternative way of testing if conditional indirect effects are significantly different from each other
index.mod.med.eff := a13*b1
index.mod.med.amb := a23*b2
'
efficacyambivalence.fit <- sem(efficacyambivalence.model, data = dat1sub5, ordered = "vote2016_clerical", estimator = "WLSMV", link = "probit")

summary(efficacyambivalence.fit, fit.measures = F, rsquare = T, standardize = T)

parameterEstimates(efficacyambivalence.fit, boot.ci.type = "bca.simple", level = .95, ci = T)[c(107:120),c(4:10)]
#####

# To get total effect estimate for medition model
totaleffect <- glm(vote2016_clerical ~ PVI2016.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub5, family = binomial(link = "probit"), weights = weight)
summary(totaleffect)

# To get R squared estimate for full mediation model
totalmodel <- glm(vote2016_clerical ~ PVI2016.scale.c * partyid.scale.c + pre.cand.ambivalence.scale + post.efficacymakediff.scale + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub5, family = binomial(link = "probit"), weights = weight)
summary(totalmodel)
pR2(totalmodel)


### ESTIMATING PRACTICAL EFFECT SIZE ###
## The map generation scripts use the two datasets created in the following code (datpractical.csv and datpracticalstate.csv) ##
testpractical <- glm(vote2016_clerical ~ PVI2016 * partyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub2, family = binomial(link = "logit"), weights = weight)
summary(testpractical)

# Getting predicted probabilities (turnout) for each level and average of all levels of democrats and republicans
# for the PVI in each congressional district
datpractical <- read.csv("115th Congress Vote_OSF.csv", header = T)
datpractical$PVI2016 <- datpractical$raw16PVIuse
datpractical$District<-datpractical[,1]
datpractical <- datpractical[order(datpractical$District),]
voteprob.fitted <- effect('PVI2016*partyid.scale.c', testpractical,
                          xlevels=list(partyid.scale.c=c(-.5,-.4,-.3,-.2,-.1,.1,.2,.3,.4,.5),
                                       PVI2016=datpractical$PVI2016), se=TRUE, confidence.level=.95)
voteprob.fitted.data <- as.data.frame(voteprob.fitted)
datpractical$dem5.cd.turnout <- subset(voteprob.fitted.data, partyid.scale.c == -.5)[,"fit"]
datpractical$dem4.cd.turnout <- subset(voteprob.fitted.data, partyid.scale.c == -.4)[,"fit"]
datpractical$dem3.cd.turnout <- subset(voteprob.fitted.data, partyid.scale.c == -.3)[,"fit"]
datpractical$dem2.cd.turnout <- subset(voteprob.fitted.data, partyid.scale.c == -.2)[,"fit"]
datpractical$dem1.cd.turnout <- subset(voteprob.fitted.data, partyid.scale.c == -.1)[,"fit"]
datpractical$rep5.cd.turnout <- subset(voteprob.fitted.data, partyid.scale.c == .5)[,"fit"]
datpractical$rep4.cd.turnout <- subset(voteprob.fitted.data, partyid.scale.c == .4)[,"fit"]
datpractical$rep3.cd.turnout <- subset(voteprob.fitted.data, partyid.scale.c == .3)[,"fit"]
datpractical$rep2.cd.turnout <- subset(voteprob.fitted.data, partyid.scale.c == .2)[,"fit"]
datpractical$rep1.cd.turnout <- subset(voteprob.fitted.data, partyid.scale.c == .1)[,"fit"]
datpractical$demavg.cd.turnout <- rowMeans(datpractical[,c("dem5.cd.turnout","dem4.cd.turnout","dem3.cd.turnout",
                                                           "dem2.cd.turnout","dem1.cd.turnout")], na.rm = T)
datpractical$repavg.cd.turnout <- rowMeans(datpractical[,c("rep5.cd.turnout","rep4.cd.turnout","rep3.cd.turnout",
                                                           "rep2.cd.turnout","rep1.cd.turnout")], na.rm = T)

# Getting predicted probabilities (turnout) of democrats and republicans for
# 0 PVI, average dem and rep PVI, and extreme dem and rep PVI (for comparisons)
avgdempvi <- mean(subset(datpractical, PVI2016 < 0)[,"PVI2016"])
avgreppvi <- mean(subset(datpractical, PVI2016 > 0)[,"PVI2016"])
extdempvi <- min(datpractical$PVI2016)
extreppvi <- max(datpractical$PVI2016)
voteprobcomp.fitted <- effect('PVI2016*partyid.scale.c', testpractical,
                              xlevels=list(partyid.scale.c=c(-.5,-.4,-.3,-.2,-.1,.1,.2,.3,.4,.5),
                                           PVI2016=c(0,avgdempvi,avgreppvi,extdempvi,extreppvi)), se=TRUE, confidence.level=.95)
voteprobcomp.fitted.data <- as.data.frame(voteprobcomp.fitted)
dem5.cd0.turnout <- voteprobcomp.fitted.data[1,"fit"]
dem4.cd0.turnout <- voteprobcomp.fitted.data[6,"fit"]
dem3.cd0.turnout <- voteprobcomp.fitted.data[11,"fit"]
dem2.cd0.turnout <- voteprobcomp.fitted.data[16,"fit"]
dem1.cd0.turnout <- voteprobcomp.fitted.data[21,"fit"]
demavg.cd0.turnout <- mean(voteprobcomp.fitted.data[c(1,6,11,16,21),"fit"])
rep1.cd0.turnout <- voteprobcomp.fitted.data[26,"fit"]
rep2.cd0.turnout <- voteprobcomp.fitted.data[31,"fit"]
rep3.cd0.turnout <- voteprobcomp.fitted.data[36,"fit"]
rep4.cd0.turnout <- voteprobcomp.fitted.data[41,"fit"]
rep5.cd0.turnout <- voteprobcomp.fitted.data[46,"fit"]
repavg.cd0.turnout <- mean(voteprobcomp.fitted.data[c(26,31,36,41,46),"fit"])
dem5.cdavgdem.turnout <- voteprobcomp.fitted.data[2,"fit"]
dem4.cdavgdem.turnout <- voteprobcomp.fitted.data[7,"fit"]
dem3.cdavgdem.turnout <- voteprobcomp.fitted.data[12,"fit"]
dem2.cdavgdem.turnout <- voteprobcomp.fitted.data[17,"fit"]
dem1.cdavgdem.turnout <- voteprobcomp.fitted.data[22,"fit"]
demavg.cdavgdem.turnout <- mean(voteprobcomp.fitted.data[c(2,7,12,17,22),"fit"])
rep1.cdavgrep.turnout <- voteprobcomp.fitted.data[28,"fit"]
rep2.cdavgrep.turnout <- voteprobcomp.fitted.data[33,"fit"]
rep3.cdavgrep.turnout <- voteprobcomp.fitted.data[38,"fit"]
rep4.cdavgrep.turnout <- voteprobcomp.fitted.data[43,"fit"]
rep5.cdavgrep.turnout <- voteprobcomp.fitted.data[48,"fit"]
repavg.cdavgrep.turnout <- mean(voteprobcomp.fitted.data[c(28,33,38,43,48),"fit"])
dem5.cdavgrep.turnout <- voteprobcomp.fitted.data[3,"fit"]
dem4.cdavgrep.turnout <- voteprobcomp.fitted.data[8,"fit"]
dem3.cdavgrep.turnout <- voteprobcomp.fitted.data[13,"fit"]
dem2.cdavgrep.turnout <- voteprobcomp.fitted.data[18,"fit"]
dem1.cdavgrep.turnout <- voteprobcomp.fitted.data[23,"fit"]
demavg.cdavgrep.turnout <- mean(voteprobcomp.fitted.data[c(3,8,13,18,23),"fit"])
rep1.cdavgdem.turnout <- voteprobcomp.fitted.data[27,"fit"]
rep2.cdavgdem.turnout <- voteprobcomp.fitted.data[32,"fit"]
rep3.cdavgdem.turnout <- voteprobcomp.fitted.data[37,"fit"]
rep4.cdavgdem.turnout <- voteprobcomp.fitted.data[42,"fit"]
rep5.cdavgdem.turnout <- voteprobcomp.fitted.data[47,"fit"]
repavg.cdavgdem.turnout <- mean(voteprobcomp.fitted.data[c(27,32,37,42,47),"fit"])
dem5.cdextdem.turnout <- voteprobcomp.fitted.data[4,"fit"]
dem4.cdextdem.turnout <- voteprobcomp.fitted.data[9,"fit"]
dem3.cdextdem.turnout <- voteprobcomp.fitted.data[14,"fit"]
dem2.cdextdem.turnout <- voteprobcomp.fitted.data[19,"fit"]
dem1.cdextdem.turnout <- voteprobcomp.fitted.data[24,"fit"]
demavg.cdextdem.turnout <- mean(voteprobcomp.fitted.data[c(4,9,14,19,24),"fit"])
rep1.cdextrep.turnout <- voteprobcomp.fitted.data[30,"fit"]
rep2.cdextrep.turnout <- voteprobcomp.fitted.data[35,"fit"]
rep3.cdextrep.turnout <- voteprobcomp.fitted.data[40,"fit"]
rep4.cdextrep.turnout <- voteprobcomp.fitted.data[45,"fit"]
rep5.cdextrep.turnout <- voteprobcomp.fitted.data[50,"fit"]
repavg.cdextrep.turnout <- mean(voteprobcomp.fitted.data[c(30,35,40,45,50),"fit"])
dem5.cdextrep.turnout <- voteprobcomp.fitted.data[5,"fit"]
dem4.cdextrep.turnout <- voteprobcomp.fitted.data[10,"fit"]
dem3.cdextrep.turnout <- voteprobcomp.fitted.data[15,"fit"]
dem2.cdextrep.turnout <- voteprobcomp.fitted.data[20,"fit"]
dem1.cdextrep.turnout <- voteprobcomp.fitted.data[25,"fit"]
demavg.cdextrep.turnout <- mean(voteprobcomp.fitted.data[c(5,10,15,20,25),"fit"])
rep1.cdextdem.turnout <- voteprobcomp.fitted.data[29,"fit"]
rep2.cdextdem.turnout <- voteprobcomp.fitted.data[34,"fit"]
rep3.cdextdem.turnout <- voteprobcomp.fitted.data[39,"fit"]
rep4.cdextdem.turnout <- voteprobcomp.fitted.data[44,"fit"]
rep5.cdextdem.turnout <- voteprobcomp.fitted.data[49,"fit"]
repavg.cdextdem.turnout <- mean(voteprobcomp.fitted.data[c(29,34,39,44,49),"fit"])

### Estimating Clinton versus Trump vote gains from fit effect comparison relative to vote losses from misfit effect comparisons
## Average partisan district comparison
# Fit comparison for Clinton equals democratic turnout for the average democratic district (PVI -16.8)
# Misfit comparison for Clinton equals democratic turnout for the average republican district (PVI 12.9)
# Fit comparison for Trump equals republican turnout for the average republican district (PVI 12.9)
# Misfit comparison for Trump equals republican turnout for the average democratic district (PVI -16.8)

# For strong democrats and strong republicans (i.e., -.5 and .5 on our scaled and centered partyid variable)
datpractical$ClintonPredictedGainAvg5 <- round((datpractical$Clinton2016vote*dem5.cdavgdem.turnout /
                                            datpractical$dem5.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$ClintonPredictedLossAvg5 <- round((datpractical$Clinton2016vote*dem5.cdavgrep.turnout /
                                            datpractical$dem5.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$TrumpPredictedGainAvg5 <- round((datpractical$Trump2016vote*rep5.cdavgrep.turnout /
                                          datpractical$rep5.cd.turnout) - datpractical$Trump2016vote, 0)
datpractical$TrumpPredictedLossAvg5 <- round((datpractical$Trump2016vote*rep5.cdavgdem.turnout /
                                            datpractical$rep5.cd.turnout) - datpractical$Trump2016vote, 0)

datpractical$ClintonGainVLossAvg5 <- datpractical$ClintonPredictedGainAvg5 + datpractical$ClintonPredictedLossAvg5
datpractical$TrumpGainVLossAvg5 <- datpractical$TrumpPredictedGainAvg5 + datpractical$TrumpPredictedLossAvg5
datpractical$RelativeClintonGainVLossAvg5 <- datpractical$ClintonGainVLossAvg5 - datpractical$TrumpGainVLossAvg5

# For moderate democrats and moderate republicans (i.e., -.3 and .3 on our scaled and centered partyid variable)
datpractical$ClintonPredictedGainAvg3 <- round((datpractical$Clinton2016vote*dem3.cdavgdem.turnout /
                                                  datpractical$dem3.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$ClintonPredictedLossAvg3 <- round((datpractical$Clinton2016vote*dem3.cdavgrep.turnout /
                                                  datpractical$dem3.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$TrumpPredictedGainAvg3 <- round((datpractical$Trump2016vote*rep3.cdavgrep.turnout /
                                                datpractical$rep3.cd.turnout) - datpractical$Trump2016vote, 0)
datpractical$TrumpPredictedLossAvg3 <- round((datpractical$Trump2016vote*rep3.cdavgdem.turnout /
                                                datpractical$rep3.cd.turnout) - datpractical$Trump2016vote, 0)

datpractical$ClintonGainVLossAvg3 <- datpractical$ClintonPredictedGainAvg3 + datpractical$ClintonPredictedLossAvg3
datpractical$TrumpGainVLossAvg3 <- datpractical$TrumpPredictedGainAvg3 + datpractical$TrumpPredictedLossAvg3
datpractical$RelativeClintonGainVLossAvg3 <- datpractical$ClintonGainVLossAvg3 - datpractical$TrumpGainVLossAvg3

# For weak democrats and weak republicans (i.e., -.1 and .1 on our scaled and centered partyid variable)
datpractical$ClintonPredictedGainAvg1 <- round((datpractical$Clinton2016vote*dem1.cdavgdem.turnout /
                                                  datpractical$dem1.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$ClintonPredictedLossAvg1 <- round((datpractical$Clinton2016vote*dem1.cdavgrep.turnout /
                                                  datpractical$dem1.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$TrumpPredictedGainAvg1 <- round((datpractical$Trump2016vote*rep1.cdavgrep.turnout /
                                                datpractical$rep1.cd.turnout) - datpractical$Trump2016vote, 0)
datpractical$TrumpPredictedLossAvg1 <- round((datpractical$Trump2016vote*rep1.cdavgdem.turnout /
                                                datpractical$rep1.cd.turnout) - datpractical$Trump2016vote, 0)

datpractical$ClintonGainVLossAvg1 <- datpractical$ClintonPredictedGainAvg1 + datpractical$ClintonPredictedLossAvg1
datpractical$TrumpGainVLossAvg1 <- datpractical$TrumpPredictedGainAvg1 + datpractical$TrumpPredictedLossAvg1
datpractical$RelativeClintonGainVLossAvg1 <- datpractical$ClintonGainVLossAvg1 - datpractical$TrumpGainVLossAvg1

# For average of democrats and average of republicans (i.e., averaged turnout across all levels of partisanship)
datpractical$ClintonPredictedGainAvgAvg <- round((datpractical$Clinton2016vote*demavg.cdavgdem.turnout /
                                                  datpractical$demavg.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$ClintonPredictedLossAvgAvg <- round((datpractical$Clinton2016vote*demavg.cdavgrep.turnout /
                                                  datpractical$demavg.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$TrumpPredictedGainAvgAvg <- round((datpractical$Trump2016vote*repavg.cdavgrep.turnout /
                                                datpractical$repavg.cd.turnout) - datpractical$Trump2016vote, 0)
datpractical$TrumpPredictedLossAvgAvg <- round((datpractical$Trump2016vote*repavg.cdavgdem.turnout /
                                                datpractical$repavg.cd.turnout) - datpractical$Trump2016vote, 0)

datpractical$ClintonGainVLossAvgAvg <- datpractical$ClintonPredictedGainAvgAvg + datpractical$ClintonPredictedLossAvgAvg
datpractical$TrumpGainVLossAvgAvg <- datpractical$TrumpPredictedGainAvgAvg + datpractical$TrumpPredictedLossAvgAvg
datpractical$RelativeClintonGainVLossAvgAvg <- datpractical$ClintonGainVLossAvgAvg - datpractical$TrumpGainVLossAvgAvg

## Extreme partisan district comparison
# Fit comparison for Clinton equals democratic turnout for the extreme democratic district (PVI -43.9)
# Misfit comparison for Clinton equals democratic turnout for the average republican district (PVI 33.7)
# Fit comparison for Trump equals republican turnout for the average republican district (PVI 33.7)
# Misfit comparison for Trump equals republican turnout for the average democratic district (PVI -43.9)

# For strong democrats and strong republicans (i.e., -.5 and .5 on our scaled and centered partyid variable)
datpractical$ClintonPredictedGainExt5 <- round((datpractical$Clinton2016vote*dem5.cdextdem.turnout /
                                                  datpractical$dem5.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$ClintonPredictedLossExt5 <- round((datpractical$Clinton2016vote*dem5.cdextrep.turnout /
                                                  datpractical$dem5.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$TrumpPredictedGainExt5 <- round((datpractical$Trump2016vote*rep5.cdextrep.turnout /
                                                datpractical$rep5.cd.turnout) - datpractical$Trump2016vote, 0)
datpractical$TrumpPredictedLossExt5 <- round((datpractical$Trump2016vote*rep5.cdextdem.turnout /
                                                datpractical$rep5.cd.turnout) - datpractical$Trump2016vote, 0)

datpractical$ClintonGainVLossExt5 <- datpractical$ClintonPredictedGainExt5 + datpractical$ClintonPredictedLossExt5
datpractical$TrumpGainVLossExt5 <- datpractical$TrumpPredictedGainExt5 + datpractical$TrumpPredictedLossExt5
datpractical$RelativeClintonGainVLossExt5 <- datpractical$ClintonGainVLossExt5 - datpractical$TrumpGainVLossExt5

# For moderate democrats and moderate republicans (i.e., -.3 and .3 on our scaled and centered partyid variable)
datpractical$ClintonPredictedGainExt3 <- round((datpractical$Clinton2016vote*dem3.cdextdem.turnout /
                                                  datpractical$dem3.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$ClintonPredictedLossExt3 <- round((datpractical$Clinton2016vote*dem3.cdextrep.turnout /
                                                  datpractical$dem3.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$TrumpPredictedGainExt3 <- round((datpractical$Trump2016vote*rep3.cdextrep.turnout /
                                                datpractical$rep3.cd.turnout) - datpractical$Trump2016vote, 0)
datpractical$TrumpPredictedLossExt3 <- round((datpractical$Trump2016vote*rep3.cdextdem.turnout /
                                                datpractical$rep3.cd.turnout) - datpractical$Trump2016vote, 0)

datpractical$ClintonGainVLossExt3 <- datpractical$ClintonPredictedGainExt3 + datpractical$ClintonPredictedLossExt3
datpractical$TrumpGainVLossExt3 <- datpractical$TrumpPredictedGainExt3 + datpractical$TrumpPredictedLossExt3
datpractical$RelativeClintonGainVLossExt3 <- datpractical$ClintonGainVLossExt3 - datpractical$TrumpGainVLossExt3

# For weak democrats and weak republicans (i.e., -.1 and .1 on our scaled and centered partyid variable)
datpractical$ClintonPredictedGainExt1 <- round((datpractical$Clinton2016vote*dem1.cdextdem.turnout /
                                                  datpractical$dem1.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$ClintonPredictedLossExt1 <- round((datpractical$Clinton2016vote*dem1.cdextrep.turnout /
                                                  datpractical$dem1.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$TrumpPredictedGainExt1 <- round((datpractical$Trump2016vote*rep1.cdextrep.turnout /
                                                datpractical$rep1.cd.turnout) - datpractical$Trump2016vote, 0)
datpractical$TrumpPredictedLossExt1 <- round((datpractical$Trump2016vote*rep1.cdextdem.turnout /
                                                datpractical$rep1.cd.turnout) - datpractical$Trump2016vote, 0)

datpractical$ClintonGainVLossExt1 <- datpractical$ClintonPredictedGainExt1 + datpractical$ClintonPredictedLossExt1
datpractical$TrumpGainVLossExt1 <- datpractical$TrumpPredictedGainExt1 + datpractical$TrumpPredictedLossExt1
datpractical$RelativeClintonGainVLossExt1 <- datpractical$ClintonGainVLossExt1 - datpractical$TrumpGainVLossExt1

# For average of democrats and average of republicans (i.e., averaged turnout across all levels of partisanship)
datpractical$ClintonPredictedGainExtAvg <- round((datpractical$Clinton2016vote*demavg.cdextdem.turnout /
                                                  datpractical$demavg.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$ClintonPredictedLossExtAvg <- round((datpractical$Clinton2016vote*demavg.cdextrep.turnout /
                                                  datpractical$demavg.cd.turnout) - datpractical$Clinton2016vote, 0)
datpractical$TrumpPredictedGainExtAvg <- round((datpractical$Trump2016vote*repavg.cdextrep.turnout /
                                                datpractical$repavg.cd.turnout) - datpractical$Trump2016vote, 0)
datpractical$TrumpPredictedLossExtAvg <- round((datpractical$Trump2016vote*repavg.cdextdem.turnout /
                                                datpractical$repavg.cd.turnout) - datpractical$Trump2016vote, 0)

datpractical$ClintonGainVLossExtAvg <- datpractical$ClintonPredictedGainExtAvg + datpractical$ClintonPredictedLossExtAvg
datpractical$TrumpGainVLossExtAvg <- datpractical$TrumpPredictedGainExtAvg + datpractical$TrumpPredictedLossExtAvg
datpractical$RelativeClintonGainVLossExtAvg <- datpractical$ClintonGainVLossExtAvg - datpractical$TrumpGainVLossExtAvg

# Adding actual vote difference
datpractical$ActualClintonLostVotes <- datpractical$Trump2016vote - datpractical$Clinton2016vote

# Creating indicator of state flip based on aggregrated CD votes lost or gained for Clinton per state
datpracticalstate <- aggregate(cbind(RelativeClintonGainVLossExt5,
                                     RelativeClintonGainVLossExt3,
                                     RelativeClintonGainVLossExt1,
                                     RelativeClintonGainVLossAvg5,
                                     RelativeClintonGainVLossAvg3,
                                     RelativeClintonGainVLossAvg1,
                                     RelativeClintonGainVLossExtAvg,
                                     RelativeClintonGainVLossAvgAvg,
                                     ActualClintonLostVotes) ~ State, datpractical, sum)
datpracticalstate$StateFlipExt5 <- ifelse(abs(datpracticalstate$RelativeClintonGainVLossExt5) >
                                            abs(datpracticalstate$ActualClintonLostVotes) &
                                            datpracticalstate$ActualClintonLostVotes > 0, "Yes", "No")
datpracticalstate$StateFlipExt3 <- ifelse(abs(datpracticalstate$RelativeClintonGainVLossExt3) >
                                            abs(datpracticalstate$ActualClintonLostVotes) &
                                            datpracticalstate$ActualClintonLostVotes > 0, "Yes", "No")
datpracticalstate$StateFlipExt1 <- ifelse(abs(datpracticalstate$RelativeClintonGainVLossExt1) >
                                            abs(datpracticalstate$ActualClintonLostVotes) &
                                            datpracticalstate$ActualClintonLostVotes > 0, "Yes", "No")
datpracticalstate$StateFlipAvg5 <- ifelse(abs(datpracticalstate$RelativeClintonGainVLossAvg5) >
                                           abs(datpracticalstate$ActualClintonLostVotes) &
                                           datpracticalstate$ActualClintonLostVotes > 0, "Yes", "No")
datpracticalstate$StateFlipAvg3 <- ifelse(abs(datpracticalstate$RelativeClintonGainVLossAvg3) >
                                            abs(datpracticalstate$ActualClintonLostVotes) &
                                            datpracticalstate$ActualClintonLostVotes > 0, "Yes", "No")
datpracticalstate$StateFlipAvg1 <- ifelse(abs(datpracticalstate$RelativeClintonGainVLossAvg1) >
                                            abs(datpracticalstate$ActualClintonLostVotes) &
                                            datpracticalstate$ActualClintonLostVotes > 0, "Yes", "No")
datpracticalstate$StateFlipExtAvg <- ifelse(abs(datpracticalstate$RelativeClintonGainVLossExtAvg) >
                                            abs(datpracticalstate$ActualClintonLostVotes) &
                                            datpracticalstate$ActualClintonLostVotes > 0, "Yes", "No")
datpracticalstate$StateFlipAvgAvg <- ifelse(abs(datpracticalstate$RelativeClintonGainVLossAvgAvg) >
                                            abs(datpracticalstate$ActualClintonLostVotes) &
                                            datpracticalstate$ActualClintonLostVotes > 0, "Yes", "No")

#########################################################################################################
## To use with the map building scripts. These datasets are already available on OSF but if interested ##
## researchers want to tweak any of the practical effect analyses, these files should be rewritten     ##
## before running the map scripts.                                                                     ##
##                                                                                                     ##
## write.csv(datpractical, file = "datpractical.csv")                                                  ##
## write.csv(datpracticalstate, file = "datpracticalstate.csv")                                        ##
#########################################################################################################


