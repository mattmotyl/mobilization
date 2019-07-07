library(effsize)
library(effects)
library(ggplot2)
library(relaimpo)
library(psychometric)

# Study 1 Archival Analyses

setwd("")

# Read in data
padata <- read.csv("pa_archival_data_osf.csv", header = T)

# Compute overall turnout of registered voters in PA
padata$Turnout <- padata$Total.Vote / rowSums(padata[,c("Reg.Democrats","Reg.Republicans","Reg.Other")])

# Compute estimates of Democratic and Repulican turnout
# I.e., taking into account party registrants who voted for other party, other party registrants who voted for candidate,
# and independent/non-party votes (estimates for percentage of votes for each of these categories comes from National Election
# Pool exit polls [https://www.nytimes.com/interactive/2016/11/08/us/politics/election-exit-polls.html?_r=0])
padata$Dem.Turnout <- (padata$Clinton.Vote - .42*padata$Reg.Other*padata$Turnout - .07*padata$Reg.Republicans*padata$Turnout
                           + .09*padata$Reg.Democrats*padata$Turnout) / padata$Reg.Democrats
padata$Rep.Turnout <- (padata$Trump.Vote - .48*padata$Reg.Other*padata$Turnout - .09*padata$Reg.Democrats*padata$Turnout
                           + .07*padata$Reg.Republicans*padata$Turnout) / padata$Reg.Republicans

## Correct Republican turnout so that turnout cannot be over 100%
padata$Rep.Turnout.C <- ifelse(padata$Rep.Turnout > 1, 1, padata$Rep.Turnout)

# Compute correlation between County % Vote for Trump and Democratic turnout and % Vote for Trump and Republican turnout
Dem.Corr <- cor.test(padata$Trump.Perc, padata$Dem.Turnout, method = "pearson")
Dem.Corr

Rep.Corr <- cor.test(padata$Trump.Perc, padata$Rep.Turnout.C, method = "pearson")
Rep.Corr

# Compute regression equations predicting turnout from % vote for trump controlling for county level age, gender, education, income, and race
Dem.reg <- lm(Dem.Turnout ~ Trump.Perc + Age + Gender + Edu + Income + Race, data = padata)
summary(Dem.reg)
calc.relimp(Dem.reg)
CI.Rsq(.2454, 67, 6)
Dem.reg.sum <- summary(Dem.reg)
round(Dem.reg.sum$coefficients,3)
round(confint(Dem.reg),3)
plot(effect("Trump.Perc", Dem.reg))

Rep.reg <- lm(Rep.Turnout.C ~ Trump.Perc + I(Trump.Perc^2) + Age + Gender + Edu + Income + Race, data = padata)
summary(Rep.reg)
calc.relimp(Rep.reg)
CI.Rsq(.1623, 67, 7)
CI.Rsq(.1298, 67, 7)
Rep.reg.sum <- summary(Rep.reg)
round(Rep.reg.sum$coefficients,3)
round(confint(Rep.reg),3)
plot(effect("Trump.Perc", Rep.reg))

