## Created by: Eric William Shannon, PhD
## Date modified: 20190314

require(tidyverse)
require(ggplot2)
require(ggthemes)
require(psych)

`%!in%` <- negate(`%in%`)

data2 <- openxlsx::read.xlsx("../data/20190317.xlsx", colNames = TRUE, startRow = 2)
data2 <- data2[, -1]
conf <- read.csv("../data/teams.csv", header = FALSE)
rownames(data2) <- data2[, 1]
data2 <- data2[, -1]
rownames(data2) <- gsub("NCAA", "", rownames(data2))
data2 <- data2[(complete.cases(data2)), ]
data2 <- janitor::clean_names(data2)

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

scores2 <- as.data.frame(cbind(data2, pr_offense, pr_defense))
scores2 <- scores2 %>% select(srs, w_l_percent, g, sos, OFFENSE, DEFENSE)

scores2 <- scores2 %>% rownames_to_column('V1')
scores2$V1 <- sub("^\\s+", "", scores2$V1)
scores2$V1 <- gsub("(^\\s+)|(\\s+$)", "", scores2$V1)

conf$V1 <- as.character(conf$V1)
conf$V1 <- sub("^\\s+", "", conf$V1)
conf$V1 <- gsub("(^\\s+)|(\\s+$)", "", conf$V1)

scores2 <- scores2 %>% left_join(conf)

conference_effect <- lme4::lmer(srs ~ sos + w_l_percent + DEFENSE + OFFENSE + (1 | V2), 
                                data = scores2)
scores2$simulated <- simulate(conference_effect, seed = 1, newdata = scores2, 
                              allow.new.levels = TRUE, re.form = NA, cond.sim = FALSE)$sim_1

scores2 <- scores2 %>% select(V1, V2, OFFENSE, DEFENSE, simulated)

scores2 <- scores2 %>%
  mutate(simulatedR = dense_rank(desc(simulated))) %>%
  column_to_rownames('V1')

scores2 <- scores2 %>%
  rownames_to_column('team')

scores2$simulated <- scale(scores2$simulated)

scores2 <-
  scores2 %>% mutate_at(vars(OFFENSE, DEFENSE, simulated), funs(round(. , 3)))

setwd("..")
openxlsx::write.xlsx(scores2, file = "ncaa_bracket/teams.xlsx")

rsconnect::deployApp("ncaa_bracket")

ggplot(scores2, aes(V2, simulated)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
  labs(title = "NCAA Conferences",
       caption = "Source: EWS",
       x = "",
       y = "Simulated Metric") + scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

champs <- scores2 %>%
  group_by(V2) %>% top_n(1, simulated)
at_large <- scores2 %>% filter(team %!in% champs$team) %>%
                               top_n(35, simulated)
ncaa_predicts <- bind_rows(champs, at_large)
ncaa_predicts %>% group_by(V2) %>% count(V2, sort = TRUE)

ggplot(ncaa_predicts, aes(team, simulated)) +
  geom_bar(stat = "identity", aes(fill = team)) +
  scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

scores2 %>% filter(V2 == "SEC") %>% 
  ggplot(., aes(simulated)) + geom_density() +
  geom_vline(xintercept = c(2.337, 2.152), linetype = "dotted")
