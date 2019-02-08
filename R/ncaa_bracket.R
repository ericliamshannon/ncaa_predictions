## Created by: Eric William Shannon, PhD
## Date modified: 20190202

require(tidyverse)
require(ggplot2)
require(ggthemes)
require(psych)


data2 <- openxlsx::read.xlsx("../data/20190207.xlsx", colNames = TRUE, startRow = 2)
data2 <- data2[, -1]
conf <- read.csv("../data/teams.csv", header = FALSE)
rownames(data2) <- data2[, 1]
data2 <- data2[, -1]
rownames(data2) <- gsub("NCAA", "", rownames(data2))
data2 <- data2[(complete.cases(data2)), ]
data2 <- janitor::clean_names(data2)

offense_data <- data2 %>% select(tm, pace, o_rtg, f_tr, x3p_ar, ts_percent, trb_percent, ast_percent, e_fg_percent, ft_fga)

pr_offense <- principal(offense_data, nfactors = 1, rotate = "varimax",
                        scores = TRUE, oblique.scores = FALSE, covar = TRUE)
pr_offense <- as.data.frame(pr_offense$scores)
names(pr_offense)[names(pr_offense) == "PC1"] <- "OFFENSE"

defense_data <- data2 %>% select(stl_percent, blk_percent, tov_percent, orb_percent)

pr_defense <- principal(defense_data, nfactors = 1, rotate = "varimax",
                        scores = TRUE, oblique.scores = FALSE, covar = TRUE)
pr_defense <- as.data.frame(pr_defense$scores)
names(pr_defense)[names(pr_defense) == "PC1"] <- "DEFENSE"

scores2 <- as.data.frame(cbind(data2, pr_offense, pr_defense))
scores2 <- scores2 %>% select(srs, w_l_percent, g, sos, OFFENSE, DEFENSE)

scores2 <- scores2 %>% rownames_to_column('V1')
scores2$V1 <- sub("^\\s+", "", scores2$V1)
scores2$V1 <- gsub("(^\\s+)|(\\s+$)", "", scores2$V1)

conf$V1 <- as.character(conf$V1)
conf$V1 <- sub("^\\s+", "", conf$V1)
conf$V1 <- gsub("(^\\s+)|(\\s+$)", "", conf$V1)

scores2 <- scores2 %>% left_join(conf)

conference_effect <- lme4::lmer(srs ~ sos + w_l_percent + DEFENSE + OFFENSE + (1 | V2), data = scores2)
scores2$simulated <- simulate(conference_effect, seed = 1, newdata = scores2, allow.new.levels = TRUE, re.form = NA, cond.sim = FALSE)$sim_1

scores2 <- scores2 %>% select(V1, V2, OFFENSE, DEFENSE, simulated)

scores2 <- scores2 %>%
  mutate(simulatedR = dense_rank(desc(simulated))) %>%
  column_to_rownames('V1')

scores2 <- scores2 %>%
  rownames_to_column('team')

scores2$simulated <- scale(scores2$simulated)

scores2 <-
  scores2 %>% mutate_at(vars(OFFENSE, DEFENSE, simulated), funs(round(. , 4)))

setwd("..")
openxlsx::write.xlsx(scores2, file = "ncaa_bracket/teams.xlsx")

rsconnect::deployApp("ncaa_bracket")

theme_set(theme_tufte())

ggplot(scores2, aes(V2, simulated)) + 
  geom_tufteboxplot() + theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
  labs(title = "NCAA Conferences",
       caption = "Source: EWS",
       x = "",
       y = "Simulated Metric")

scores2 %>% filter(simulatedR <= 68) %>% ggplot(aes(OFFENSE, DEFENSE)) + geom_text(aes(label = team), check_overlap = TRUE)
