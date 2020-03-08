## Author: Dr. Eric William Shannon
## Date Edited: 20200223
## Purpose: NCAA Winning Probabilities

## Cleaning the environment os I can load all of the packages
## that I need for this specific project. Creating the negative
## %in% function which is convenient for filtering purposes.

lapply(paste('package:',names(sessionInfo()$otherPkgs), sep = ""),
       detach,character.only = TRUE,unload = TRUE)

packages <- c("ggthemes", "lavaan", "lme4", "psych", "RCurl", "rlist", "tidyverse", "XML")
lapply(packages, require, character.only = TRUE)

rm(list = ls())
`%!in%` <- negate(`%in%`)   

## Loading in the basic school statistics from sports-reference
## and then cleaning up the data into a data frame so that it
## is able to be used in the analysis.

schoolStats <- getURL("https://www.sports-reference.com/cbb/seasons/2020-school-stats.html") 
schoolStats <- schoolStats %>% readHTMLTable(.) %>% 
  list.clean(., fun = is.null, recursive = FALSE) %>% do.call(rbind.data.frame, .) %>% 
  janitor::clean_names() %>% remove_rownames() %>% filter(school %!in% c("Overall", "School")) %>%
  column_to_rownames(., "school")
schoolStats[] <- lapply(schoolStats, function(x) as.numeric(as.character(x))) 

## Loading in conference information as well as AP ratings for the
## multilevel model portions of the analysis.

conferenceInfo <- getURL("https://www.sports-reference.com/cbb/seasons/2020-ratings.html")
conferenceInfo <- conferenceInfo %>% readHTMLTable(.) %>% 
  list.clean(., fun = is.null, recursive = FALSE) %>% do.call(rbind.data.frame, .) %>%
  janitor::clean_names() %>% remove_rownames() %>% filter(school %!in% c("School", "")) %>%
  column_to_rownames(., "school") %>% select(conf, ap_rank)

## Combining the basic school statistics with the conference information/
## AP poll ratings in order to have an analytics-ready data frame.

combinedTables <- schoolStats %>% rownames_to_column(., "school") %>%
  left_join(., rownames_to_column(conferenceInfo, "school"), by = "school") %>%
  select(school, conf, games = g, wins = w, loss = l, w_l_percent, srs, sos, conf_wins = w_2,
         conf_loss = l_2, home_wins = w_3, home_loss = l_3, away_wins = w_4,
         away_loss = l_4, tm, opp, mp, fg, fga, fg_percent, three_fg = x3p, three_fga = x3pa,
         three_fg_percent = x3p_percent, ft, fta, ft_percent, orb, trb, ast, stl, 
         blk, tov, pf, ap_rank) %>% mutate(ap_rank = if_else(ap_rank == "", 0, 1))

## Exploratory factor analysis in order to know which factors load and how they load
## on the CFA model

fa.random(data = combinedTables[,15:33], nfactors = 3, fix = TRUE, rotate = "varimax",
          oblique.scores = FALSE, scores = "regression", fm = "ML")

## Fitting the CFA model based off the loadings from the EFA model above, using 
## a bifactor model where all of the factors set on one general factor. And then
## two additional factors which are Factors One and Three from the model above.

myLavaan <- '

            generalFactor =~ tm + opp + fga + fg_percent + three_fg + three_fg_percent + ft_percent + ast + stl + blk + fg + orb + tov + pf
            factorOne =~ tm + opp + fga + fg_percent + three_fg + three_fg_percent + ft_percent + ast + stl + blk + fg
            factorThree =~ tm + fg + orb + tov + pf


            generalFactor =~ 0*factorOne
            generalFactor =~ 0*factorThree
            factorOne =~ 0*factorThree

            three_fg ~~ three_fg_percent

'

fit <- cfa(model = myLavaan, data = combinedTables, cluster = "conf", 
           std.lv = TRUE, std.ov = TRUE, auto.efa = TRUE, orthogonal = TRUE,
           information = "expected", verbose = TRUE, mimic = "Mplus") 

### Checking parameter estimates and model fit.
summary(fit, standardized = TRUE, fit.measures = TRUE)
cov_table <- residuals(fit)$cov
cov_table[upper.tri(cov_table)] <- NA
print(cov_table)

modindices(fit, sort. = TRUE)

semPlot::semPaths(fit, what = c("paths", "est"), whatLabels = "est", layout = "spring", intercepts = FALSE, residuals = TRUE)

## Extracting the factor scores and then binding them to individual
## schools in order to analyze the multilevel model.

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit, type = "lv")
for (fs in colnames(fscores)) {
  combinedTables[idx, fs] <- fscores[, fs]
}

## Cleaning up the tables after binding the factor scores

finalTables <- combinedTables %>% select(school, conf, games, w_l_percent, srs, sos, ap_rank,
                                         factorOne, factorThree, generalFactor)         

## Estimating a generalized linear mixed model with a binomial
## family and a probit link weighing the outcomes by the number
## of games that a team played.

predictiveModel <- glmer(w_l_percent ~ srs + sos + ap_rank + factorOne + 
                           factorThree + generalFactor + (1 | conf),
      family = binomial(link = "probit"), weights = games, data = combinedTables)

## Binding predictive/confidence intervals onto the data frame
## in order to get an idea of the uncertainty of a team's performance.
## Need to look into the residual variance vs. intercept variance.

finalTables$estimate <- merTools::predictInterval(merMod = predictiveModel,
                                                  newdata = finalTables, level = 0.95, n.sims = 1000,
                                                  stat = "mean", type = "probability", 
                                                  include.resid.var = FALSE, fix.intercept.variance = TRUE)$fit

finalTables$lower <- merTools::predictInterval(merMod = predictiveModel,
                                               newdata = finalTables, level = 0.95, n.sims = 1000,
                                               stat = "mean", type = "probability", 
                                               include.resid.var = FALSE, 
                                               fix.intercept.variance = TRUE)$lwr

finalTables$upper <- merTools::predictInterval(merMod = predictiveModel,
                                               newdata = finalTables, level = 0.95, n.sims = 1000,
                                               stat = "mean", type = "probability", 
                                               include.resid.var = FALSE, 
                                               fix.intercept.variance = TRUE)$upr

finalTables <- finalTables %>% select(school, conf, estimate, upper, lower) %>%
  mutate_if(is.numeric, round, digits = 3)

setwd("../ncaa_bracket")
openxlsx::write.xlsx(finalTables, file = "teams.xlsx")  
