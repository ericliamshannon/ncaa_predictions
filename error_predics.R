## Created by: Eric William Shannon, PhD
## Date modified: 20190122

errors <- openxlsx::read.xlsx("error_predics.xlsx", sheet = 3)
errors$Home <- sub("^\\s+", "", errors$Home)
errors$Home <- gsub("(^\\s+)|(\\s+$)", "", errors$Home)
errors$Away <- sub("^\\s+", "", errors$Away)
errors$Away <- gsub("(^\\s+)|(\\s+$)", "", errors$Away)

errors2 <- merge(x = errors, y = scores2, by.x = "Away", by.y = "team", all.x = TRUE, sort = FALSE)
errors3 <- merge(x = errors2, y = scores2, by.x = "Home", by.y = "team", all.x = TRUE, 
                 sort = FALSE)

errors3 <- subset(errors3, select = -c(simulatedR.x, simulatedR.y, V2.y, V2.x))
errors3 <- janitor::clean_names(errors3)
names(errors3) <- gsub(x = names(errors3), pattern = "x", replacement = "away")
names(errors3) <- gsub(x = names(errors3), pattern = "_y", replacement = "_home")

errors3$winner <- ifelse(errors3$away_score > errors3$home_score, 
                         as.character(errors3$away), as.character(errors3$home))
errors3$predicted <- ifelse(errors3$simulated_away > errors3$simulated_home,
                            as.character(errors3$away), as.character(errors3$home))
