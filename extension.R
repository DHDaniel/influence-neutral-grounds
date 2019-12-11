
library(Matching)

load("data_matching_not_away.Rda")
load("data_matching_not_home.Rda")

df_not_away <- data_matching_not_away
df_not_away$match_place_num <- rep(0, nrow(data_matching_not_away))
df_not_away[which(data_matching_not_away$match_place_num == 3),]$match_place_num <- 1
df_not_away$sport <- as.numeric(df_not_away$sport)
df_not_away$competition_dummy_factor <- as.numeric(df_not_away$competition_dummy_factor)

Tr_away <- df_not_away$match_place_num
X_away <- df_not_away[, -c(1, 3)]

genout_away <- GenMatch(Tr=Tr_away, X=X_away, estimand="ATT", pop.size = 30, max.generations = 25)
mout_away <- Match(Tr=Tr_away, X=X_away, Weight.matrix = genout_away)

mb_data_away <- data.frame(Tr=Tr_away, X_away)

mb_away <- MatchBalance(Tr ~ sport + competition_dummy_factor + elo_diff_adjusted_log, data = mb_data_away, match.out = mout_away, nboots = 1000)
