## Created by: Eric William Shannon, PhD
## Date modified: 20190804

## Setting Up Environments

require(ggthemes)
require(janitor)
require(lme4)
require(openxlsx)
require(psych)
require(tidyverse)

`%!in%` <- negate(`%in%`)

## Bringing in and Cleaning Up Data

data2 <- read.xlsx("../data/20190407.xlsx", colNames = TRUE, startRow = 2)
data2 <- data2[, -1]
conf <- read.csv("../data/teams.csv", header = FALSE)
rownames(data2) <- data2[, 1]
data2 <- data2[, -1]
rownames(data2) <- gsub("NCAA", "", rownames(data2))
data2 <- data2[(complete.cases(data2)), ]
data2 <- clean_names(data2)

## Principal Components Analysis

offense_data <- data2 %>% 
  select(tm, pace, o_rtg, f_tr, x3p_ar, ts_percent, trb_percent, ast_percent, e_fg_percent, ft_fga)

pr_offense <- principal(offense_data, nfactors = 1, rotate = "varimax",
                        scores = TRUE, oblique.scores = FALSE, covar = TRUE)
pr_offense <- as.data.frame(pr_offense$scores)
names(pr_offense)[names(pr_offense) == "PC1"] <- "OFFENSE"

defense_data <- data2 %>% select(opp, stl_percent, blk_percent, tov_percent, orb_percent)

pr_defense <- principal(defense_data, nfactors = 1, rotate = "varimax",
                        scores = TRUE, oblique.scores = FALSE, covar = TRUE)
pr_defense <- as.data.frame(pr_defense$scores)
names(pr_defense)[names(pr_defense) == "PC1"] <- "DEFENSE"

## Setting Up Final Data Set

scores2 <- as.data.frame(cbind(data2, pr_offense, pr_defense)) %>% 
  select(srs, w_l_percent, g, w, l, sos, OFFENSE, DEFENSE) %>% rownames_to_column('V1') %>% 
  mutate(V1 = sub("^\\s+", "", V1)) %>% mutate(V1 = gsub("(^\\s+)|(\\s+$)", "", V1))

conf <- conf %>% mutate(V1 = as.character(V1)) %>% mutate(V1 = sub("^\\s+", "", V1)) %>%
  mutate(V1 = gsub("(^\\s+)|(\\s+$)", "", V1))

scores2 <- scores2 %>% left_join(conf)

## Mixed Effects Model

fit <- glmer(w/g ~ OFFENSE + DEFENSE + sos + (1 | V2),
             weights = g, nAGQ = 0, verbose = TRUE, data = scores2,
             family = binomial(link = probit))

stargazer::stargazer(fit, title = "GLMM Results", align = TRUE, dep.var.labels = c("Winning Probability"), covariate.labels = c("Offensive Capability", "Defensive Capability", "Strength of Schedule", "Simple Rating System"), type = "html", style = "aer", out = "../writeup/results.html")

## Predicted Probabilities, Lower and Upper 95% CIs

scores2$estimate <- merTools::predictInterval(merMod = fit,
                                                  newdata = scores2, level = 0.95, n.sims = 1000,
                                                  stat = "mean", type = "probability", 
                                                  include.resid.var = FALSE, fix.intercept.variance = TRUE)$fit
scores2$lower <- merTools::predictInterval(merMod = fit,
                                               newdata = scores2, level = 0.95, n.sims = 1000,
                                               stat = "mean", type = "probability", 
                                               include.resid.var = FALSE, fix.intercept.variance = TRUE)$lwr
scores2$upper <- merTools::predictInterval(merMod = fit,
                                               newdata = scores2, level = 0.95, n.sims = 1000,
                                               stat = "mean", type = "probability", 
                                               include.resid.var = FALSE, fix.intercept.variance = TRUE)$upr

## Dataset for the Shiny App

scores2 <- scores2 %>% select(V1, V2, OFFENSE, DEFENSE, estimate, lower, upper) %>%
  column_to_rownames('V1') %>%
  rownames_to_column('team') %>%
  mutate_at(vars(OFFENSE, DEFENSE, estimate, lower, upper), funs(round(. , 3)))

setwd("..")
openxlsx::write.xlsx(scores2, file = "ncaa_bracket/teams.xlsx")

rsconnect::deployApp("ncaa_bracket")
