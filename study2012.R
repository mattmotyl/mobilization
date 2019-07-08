library(Hmisc)
library(rockchalk)
library(stargazer)
library(apaTables)
library(psych)
library(effects)
library(ggplot2)
library(pscl)
library(lavaan)
library(relaimpo)
library(plyr)
library(gridExtra)
library(pscl)

# Set working directory (wherever the "anes_timeseries_2012voteval_osf.csv" file is located)
setwd("C:/GitHub/mobilization")

# Read in dataset
dat1 <- read.csv("data/anes_timeseries_2012voteval_osf.csv", header = T)


### DESIGNATE MISSING, RESCALE, AND RESTRUCTURE VARIABLES OF INTEREST ###
#####
## SAMPLE WEIGHT ##
# Recoding sample weights for full sample pre and post items
dat1$weight <- dat1$weight_full


## DEMOGRAPHIC CONTROLS ##
# AGE #
# Designate missing values
is.na(dat1$dem_age_r_x) <- dat1$dem_age_r_x == -3 | dat1$dem_age_r_x == -2
dat1$age <- dat1$dem_age_r_x

# GENDER #
# 1 = male, 0 = female
dat1$gender <- ifelse(dat1$gender_respondent_x == 2, 0, 1)

# EDUCATION #
# Designate missing values
# Dichotomize to just two categories, 1 = college degree, 0 = no college degree
is.na(dat1$dem_edu) <- dat1$dem_edu == -9 | dat1$dem_edu == -8 | dat1$dem_edu == 95
dat1$edu <- ifelse(dat1$dem_edu < 13, 0, 1)

# INCOME #
# Designate missing values
is.na(dat1$incgroup_prepost_x) <- dat1$incgroup_prepost_x == -9 | dat1$incgroup_prepost_x == -8 | dat1$incgroup_prepost_x == -7 | dat1$incgroup_prepost_x == -6
dat1$income <- dat1$incgroup_prepost_x

# RACE #
# Designate missing values
# Dichotomize to just two categories, 1 = White, 0 = non-White
is.na(dat1$dem_raceeth_x) <- dat1$dem_raceeth_x == -9
dat1$race <- ifelse(dat1$dem_raceeth_x == 1, 1, 0)


## CONGRESSIONAL DISTRICT PERCENT VOTE FOR OBAMA ##
# Center congressional district ideology at 50/50 split betwen Obama and Romney
# Higher scores indicate greater percentage vote for Obama
dat1$Obama2012.c <- dat1$Obama2012 - 50


## PARTISAN VOTER INDEX FOR 2012 BY CONGRESSIONAL DISTRICT ##
# Already centered at 0 (even split between Democrat and Republican)
# Higher scores indicate more Republican district
dat1$PVI2012 <- dat1$raw12PVIuse


## POLITICAL ORIENTATION ##
# Designate missing values and center political orientation at the midpoint
# Higher scores indicate greater conservatism
is.na(dat1$libcpre_self) <- dat1$libcpre_self == -9 | dat1$libcpre_self == -8 | dat1$libcpre_self == -2
dat1$po <- dat1$libcpre_self
dat1$po.c <- dat1$po - 4


## PARTY IDENTIFICATION ##
# Designate missing values and center party identification at the midpoint
# Higher scores indicate stronger Republican
is.na(dat1$pid_x) <- dat1$pid_x == -2
dat1$partyid <- dat1$pid_x
dat1$partyid.c <- dat1$partyid - 4


## validated POST-ELECTION VOTE ##
# Designate missing values
# 1 = voted, 0 = did not vote
is.na(dat1$votevalid_2012g) <- dat1$votevalid_2012g == -4


## DOES WHO VOTE FOR MAKE A DIFFERENCE ##
# Designate missing values
# Higher scores indicate greater perceived efficacy
is.na(dat1$cses_diffvote) <- dat1$cses_diffvote == -9 | dat1$cses_diffvote == -8 | dat1$cses_diffvote == -7 | dat1$cses_diffvote == -6
dat1$post.votemakediff <- dat1$cses_diffvote


## DOES WHO IS IN POWER MAKE A DIFFERENCE ##
# Designate missing values
# Higher scores indicate greater perceived efficacy
is.na(dat1$cses_diffpower) <- dat1$cses_diffpower == -9 | dat1$cses_diffpower == -8 | dat1$cses_diffpower == -7 | dat1$cses_diffpower == -6
dat1$post.powermakediff <- dat1$cses_diffpower


## DEMOCRATIC EFFICACY ##
# Average of Vote and Power Efficacy variables
# Higher scores indicate greater perceived democratic efficacy
dat1$post.efficacymakediff <- rowMeans(cbind(dat1$post.powermakediff, dat1$post.votemakediff), na.rm = T)


## FEELING THERMOMETER TOWARD DEMOCRATIC CANDIDATE ##
is.na(dat1$ft_dpc) <- dat1$ft_dpc == -9 | dat1$ft_dpc == -8 | dat1$ft_dpc == -2
dat1$pre.feeling.demprescand <- dat1$ft_dpc


## FEELING THERMOMETER TOWARD REPUBLICAN CANDIDATE ##
is.na(dat1$ft_rpc) <- dat1$ft_rpc == -9 | dat1$ft_rpc == -8 | dat1$ft_rpc == -2
dat1$pre.feeling.repprescand <- dat1$ft_rpc


## AMBIVALENCE TOWARDS CANDIDATES ##
dat1$pre.cand.ambivalence <- ((dat1$pre.feeling.demprescand + dat1$pre.feeling.repprescand) / 2) - 
                             abs(dat1$pre.feeling.demprescand - dat1$pre.feeling.repprescand)


## POLITICAL INTEREST ##
is.na(dat1$interest_attention) <- dat1$interest_attention == -9 | dat1$interest_attention == -8
dat1$pre.polinterest <- 6 - dat1$interest_attention


## HOW LONG RESIDE IN COMMUNITY ##
is.na(dat1$dem3_yearscomm) <- dat1$dem3_yearscomm == -9
dat1$pre.reside <- dat1$dem3_yearscomm

#####


### ANALYSES ###

## validated POST-ELECTION VOTE ##
# Getting non-missing cases for variables used in analysis
dat1sub3 <- na.omit(dat1[,c("PVI2012","partyid","votevalid_2012g","weight","age","gender","edu","income","race","pre.polinterest","pre.reside")])

# Creating scaled (0 to 1) person and place ideology and age and income
dat1sub3$PVI2012.scale <- (dat1sub3$PVI2012 - min(dat1sub3$PVI2012)) / 
                          (max(dat1sub3$PVI2012) - min(dat1sub3$PVI2012))
dat1sub3$partyid.scale <- (dat1sub3$partyid - min(dat1sub3$partyid)) / 
                          (max(dat1sub3$partyid) - min(dat1sub3$partyid))
dat1sub3$age.scale <- (dat1sub3$age - min(dat1sub3$age)) / 
                      (max(dat1sub3$age) - min(dat1sub3$age))
dat1sub3$income.scale <- (dat1sub3$income - min(dat1sub3$income)) / 
                         (max(dat1sub3$income) - min(dat1sub3$income))
dat1sub3$pre.polinterest.scale <- (dat1sub3$pre.polinterest - min(dat1sub3$pre.polinterest)) / 
                                  (max(dat1sub3$pre.polinterest) - min(dat1sub3$pre.polinterest))
dat1sub3$pre.reside.scale <- (dat1sub3$pre.reside - min(dat1sub3$pre.reside)) / 
                             (max(dat1sub3$pre.reside) - min(dat1sub3$pre.reside))

# Centering scaled versions around midpoint (.5) for person and place ideology
dat1sub3$PVI2012.scale.c <- dat1sub3$PVI2012.scale - .5
dat1sub3$partyid.scale.c <- dat1sub3$partyid.scale - .5

# Test using scaled variables
# With controls
test3swc <- glm(votevalid_2012g ~ PVI2012.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub3, family = binomial(link = "logit"), weights = weight)
summary(test3swc)
exp(summary(test3swc)$coefficients)
round(pR2(test3swc),3)
exp(confint(test3swc))

# Simple slope follow ups
dat1sub3$partyid.scale.dem <- dat1sub3$partyid.scale.c + .5
dat1sub3$partyid.scale.rep <- dat1sub3$partyid.scale.c - .5

test3swcdem <- glm(votevalid_2012g ~ PVI2012.scale.c * partyid.scale.dem + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub3, family = binomial(link = "logit"), weights = weight)
summary(test3swcdem)
exp(summary(test3swcdem)$coefficients)
exp(confint(test3swcdem))
test3swcrep <- glm(votevalid_2012g ~ PVI2012.scale.c * partyid.scale.rep + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub3, family = binomial(link = "logit"), weights = weight)
summary(test3swcrep)
exp(summary(test3swcrep)$coefficients)
exp(confint(test3swcrep))

# GG Plot
#####
# Getting the effects for the PO by PVI interaction at most Democratic and most Republican id
# Specifying the full range of PVI
validatedvotescale.PVIAtPO.fitted<-effect('PVI2012.scale.c*partyid.scale.c', test3swc,
                                      xlevels=list(partyid.scale.c=c(-.5,.5),
                                      PVI2012.scale.c = seq(from = -.5, to = .5, by = .1)), 
                                      se=TRUE, confidence.level=.95)
# Saving the effects as a data frame
validatedvotescale.PVIAtPO.Eff<-as.data.frame(validatedvotescale.PVIAtPO.fitted)
# Recoding the moderator variable (PO) as a factor with the names of the groups
validatedvotescale.PVIAtPO.Eff$pomod<-factor(validatedvotescale.PVIAtPO.Eff$partyid.scale.c,
                                         levels=c(-.5,.5),
                                         labels=c("Democrats","Republicans"))
# Plotting the figure
validatedvotescale.PVIAtPO.Plot <-ggplot(data = validatedvotescale.PVIAtPO.Eff, aes(x = PVI2012.scale.c, y =fit, group=pomod))+
  coord_cartesian(xlim=c(-.5, .5),ylim = c(0,1))+ 
  scale_x_continuous(breaks = c(-.5,-.33,.33,.5), labels = c("","Most Democratic","Most Republican",""))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .25), labels = c("0%","25%","50%","75%","100%"))+
  geom_line(aes(color=pomod, linetype = pomod),size=2)+
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=pomod),alpha=.2)+
  xlab("2012 Congressional District PVI")+
  ylab("Validated Vote Probability")+
  scale_color_manual(values=c("#3b5998", "#994c3c"))+
  scale_fill_manual(values=c("#3b5998", "#994c3c"))+
  theme_bw()+
  theme(text=element_text(family="Segoe", face="bold", size=38),
        axis.ticks.x = element_blank(),
        axis.title.x=element_text(vjust=-1),
        axis.text.x = element_text(vjust=-.2),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(size = .5, colour = "grey0"),
        legend.title=element_blank(),
        legend.position = c(.83, .25),
        plot.margin=unit(c(1.1,1,1,1),"cm"))
#####
validatedvotescale.PVIAtPO.Plot
#####
ggsave(file = "plots/validvotePlot2012.png", plot=validatedvotescale.PVIAtPO.Plot,width = 11.95,height =12.58,dpi=300,units="in")


## AMBIVALENCE ##
# Getting non-missing cases for variables used in analysis
dat1sub9 <- na.omit(dat1[,c("PVI2012","partyid","partyid.c","pre.cand.ambivalence","weight","age","gender","edu","income","race","pre.polinterest","pre.reside")])

# Creating scaled (0 to 1) person and place ideology and age and income and life satisfaction
dat1sub9$PVI2012.scale <- (dat1sub9$PVI2012 - min(dat1sub9$PVI2012)) / 
                          (max(dat1sub9$PVI2012) - min(dat1sub9$PVI2012))
dat1sub9$partyid.scale <- (dat1sub9$partyid - min(dat1sub9$partyid)) / 
                          (max(dat1sub9$partyid) - min(dat1sub9$partyid))
dat1sub9$age.scale <- (dat1sub9$age - min(dat1sub9$age)) / 
                      (max(dat1sub9$age) - min(dat1sub9$age))
dat1sub9$income.scale <- (dat1sub9$income - min(dat1sub9$income)) / 
                         (max(dat1sub9$income) - min(dat1sub9$income))
dat1sub9$pre.cand.ambivalence.scale <- (dat1sub9$pre.cand.ambivalence - min(dat1sub9$pre.cand.ambivalence)) / 
                                       (max(dat1sub9$pre.cand.ambivalence) - min(dat1sub9$pre.cand.ambivalence))
dat1sub9$pre.polinterest.scale <- (dat1sub9$pre.polinterest - min(dat1sub9$pre.polinterest)) / 
                                  (max(dat1sub9$pre.polinterest) - min(dat1sub9$pre.polinterest))
dat1sub9$pre.reside.scale <- (dat1sub9$pre.reside - min(dat1sub9$pre.reside)) / 
                             (max(dat1sub9$pre.reside) - min(dat1sub9$pre.reside))

# Centering scaled versions around midpoint (.5) for person and place ideology
dat1sub9$PVI2012.scale.c <- dat1sub9$PVI2012.scale - .5
dat1sub9$partyid.scale.c <- dat1sub9$partyid.scale - .5

# Test using scaled variables
# With controls
test13swc <- lm(pre.cand.ambivalence.scale ~ PVI2012.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub9, weights = weight)
summary(test13swc)
calc.relimp(test13swc)
CI.Rsq(.0146, 1827, 10)

# Simple slope follow ups
dat1sub9$partyid.scale.dem <- dat1sub9$partyid.scale.c + .5
dat1sub9$partyid.scale.rep <- dat1sub9$partyid.scale.c - .5

test13swcdem <- lm(pre.cand.ambivalence.scale ~ PVI2012.scale.c * partyid.scale.dem + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub9, weights = weight)
summary(test13swcdem)
test13swcrep <- lm(pre.cand.ambivalence.scale ~ PVI2012.scale.c * partyid.scale.rep + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub9, weights = weight)
summary(test13swcrep)
#####


## DEMOCRATIC EFFICACY ##
# Getting non-missing cases for variables used in analysis
dat1sub2 <- na.omit(dat1[,c("PVI2012","partyid","post.efficacymakediff","weight","age","gender","edu","income","race","pre.polinterest","pre.reside")])

# Creating scaled (0 to 1) person and place ideology and age and income and democratic efficacy
dat1sub2$PVI2012.scale <- (dat1sub2$PVI2012 - min(dat1sub2$PVI2012)) / 
                          (max(dat1sub2$PVI2012) - min(dat1sub2$PVI2012))
dat1sub2$partyid.scale <- (dat1sub2$partyid - min(dat1sub2$partyid)) / 
                          (max(dat1sub2$partyid) - min(dat1sub2$partyid))
dat1sub2$age.scale <- (dat1sub2$age - min(dat1sub2$age)) / 
                      (max(dat1sub2$age) - min(dat1sub2$age))
dat1sub2$income.scale <- (dat1sub2$income - min(dat1sub2$income)) / 
                         (max(dat1sub2$income) - min(dat1sub2$income))
dat1sub2$post.efficacymakediff.scale <- (dat1sub2$post.efficacymakediff - min(dat1sub2$post.efficacymakediff)) / 
                                        (max(dat1sub2$post.efficacymakediff) - min(dat1sub2$post.efficacymakediff))
dat1sub2$pre.polinterest.scale <- (dat1sub2$pre.polinterest - min(dat1sub2$pre.polinterest)) / 
                                  (max(dat1sub2$pre.polinterest) - min(dat1sub2$pre.polinterest))
dat1sub2$pre.reside.scale <- (dat1sub2$pre.reside - min(dat1sub2$pre.reside)) / 
                             (max(dat1sub2$pre.reside) - min(dat1sub2$pre.reside))

# Centering scaled versions around midpoint (.5) for person and place ideology
dat1sub2$PVI2012.scale.c <- dat1sub2$PVI2012.scale - .5
dat1sub2$partyid.scale.c <- dat1sub2$partyid.scale - .5

# Test using scaled variables
# With controls
test2swc <- lm(post.efficacymakediff.scale ~ PVI2012.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub2, weights = weight)
summary(test2swc)
calc.relimp(test2swc)
CI.Rsq(.0069, 1749, 10)

# Simple slope follow ups
dat1sub2$partyid.scale.dem <- dat1sub2$partyid.scale.c + .5
dat1sub2$partyid.scale.rep <- dat1sub2$partyid.scale.c - .5

test2swcdem <- lm(post.efficacymakediff.scale ~ PVI2012.scale.c * partyid.scale.dem + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub2, weights = weight)
summary(test2swcdem)
test2swcrep <- lm(post.efficacymakediff.scale ~ PVI2012.scale.c * partyid.scale.rep + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub2, weights = weight)
summary(test2swcrep)
#####

#####
# For regression table in supplemental materials
stargazer(test3swc,test13swc,test2swc,type="text",
          column.labels = c("Vote", "Ambivalence", "Democratic Efficacy"),
          intercept.bottom = FALSE, star.cutoffs = c(.05,.01,.001),
          single.row=FALSE, 
          notes.append = FALSE,
          header=FALSE)
#####


### MODERATED MEDIATION TESTING MULTIPLE MEDIATION WITH EFFICACY AND AMBIVALENCE ###
#####
dat1sub11 <- na.omit(dat1[,c("PVI2012","partyid","partyid.c","votevalid_2012g","post.efficacymakediff","pre.cand.ambivalence","weight","age","gender","edu","income","race","pre.polinterest","pre.reside")])

# Creating scaled (0 to 1) person and place ideology and age and income and democratic efficacy
dat1sub11$PVI2012.scale <- (dat1sub11$PVI2012 - min(dat1sub11$PVI2012)) / 
                           (max(dat1sub11$PVI2012) - min(dat1sub11$PVI2012))
dat1sub11$partyid.scale <- (dat1sub11$partyid - min(dat1sub11$partyid)) / 
                           (max(dat1sub11$partyid) - min(dat1sub11$partyid))
dat1sub11$age.scale <- (dat1sub11$age - min(dat1sub11$age)) / 
                       (max(dat1sub11$age) - min(dat1sub11$age))
dat1sub11$income.scale <- (dat1sub11$income - min(dat1sub11$income)) / 
                          (max(dat1sub11$income) - min(dat1sub11$income))
dat1sub11$pre.polinterest.scale <- (dat1sub11$pre.polinterest - min(dat1sub11$pre.polinterest)) / 
                                   (max(dat1sub11$pre.polinterest) - min(dat1sub11$pre.polinterest))
dat1sub11$pre.reside.scale <- (dat1sub11$pre.reside - min(dat1sub11$pre.reside)) / 
                              (max(dat1sub11$pre.reside) - min(dat1sub11$pre.reside))
dat1sub11$post.efficacymakediff.scale <- (dat1sub11$post.efficacymakediff - min(dat1sub11$post.efficacymakediff)) / 
                                         (max(dat1sub11$post.efficacymakediff) - min(dat1sub11$post.efficacymakediff))
dat1sub11$pre.cand.ambivalence.scale <- (dat1sub11$pre.cand.ambivalence - min(dat1sub11$pre.cand.ambivalence)) / 
                                        (max(dat1sub11$pre.cand.ambivalence) - min(dat1sub11$pre.cand.ambivalence))

# Centering scaled versions around midpoint (.5) for person and place ideology
dat1sub11$PVI2012.scale.c <- dat1sub11$PVI2012.scale - .5
dat1sub11$partyid.scale.c <- dat1sub11$partyid.scale - .5

# Compute interaction term for use in lavaan (it's weird with on the fly interaction terms)
dat1sub11$PVI2012.scale.cXpartyid.scale.c <- dat1sub11$PVI2012.scale.c * dat1sub11$partyid.scale.c

# Scaled analysis with controls using lavaan
efficacyambivalence.model <- '
#Regressions
post.efficacymakediff.scale ~ a11*PVI2012.scale.c + a12*partyid.scale.c + a13*PVI2012.scale.cXpartyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale
pre.cand.ambivalence.scale ~ a21*PVI2012.scale.c + a22*partyid.scale.c + a23*PVI2012.scale.cXpartyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale
votevalid_2012g ~ cdash1*PVI2012.scale.c + cdash2*partyid.scale.c + cdash3*PVI2012.scale.cXpartyid.scale.c + b1*post.efficacymakediff.scale + b2*pre.cand.ambivalence.scale + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale

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
efficacyambivalence.fit <- sem(efficacyambivalence.model, data = dat1sub11, ordered = "votevalid_2012g", estimator = "WLSMV", link = "probit")

summary(efficacyambivalence.fit, fit.measures = F, rsquare = T, standardize = T)

parameterEstimates(efficacyambivalence.fit, boot.ci.type = "bca.simple", level = .95, ci = T)[c(107:120),c(4:10)]
#####

# To get total effect estimate for mediation model
totaleffect <- glm(votevalid_2012g ~ PVI2012.scale.c * partyid.scale.c + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub11, family = binomial(link = "probit"), weights = weight)
summary(totaleffect)

# To get R squared estimate for full mediation model
totalmodel <- glm(votevalid_2012g ~ PVI2012.scale.c * partyid.scale.c + pre.cand.ambivalence.scale + post.efficacymakediff.scale + age.scale + gender + edu + income.scale + race + pre.polinterest.scale + pre.reside.scale, data = dat1sub11, family = binomial(link = "probit"), weights = weight)
summary(totalmodel)
pR2(totalmodel)
