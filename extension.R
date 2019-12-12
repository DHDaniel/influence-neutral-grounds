
library(Matching)

load("data_matching_not_away.Rda")
load("data_matching_not_home.Rda")

# analysis for away

# pre-process data for genmatch
df_not_away <- data_matching_not_away
df_not_away$match_place_num <- rep(0, nrow(data_matching_not_away))
df_not_away[which(data_matching_not_away$match_place_num == 3),]$match_place_num <- 1
df_not_away$sport <- as.numeric(df_not_away$sport)
df_not_away$competition_dummy_factor <- as.numeric(df_not_away$competition_dummy_factor)

# define treatment and the x matrix for genmatch
Tr_away <- df_not_away$match_place_num
X_away <- df_not_away[, -c(1, 3)]
# make balance matrix for genmatch using interaction terms
attach(df_not_away)
BalanceMat_away <- cbind(sport, competition_dummy_factor, elo_diff_adjusted_log, I(elo_diff_adjusted_log^2), 
                         I(competition_dummy_factor*elo_diff_adjusted_log), I(sport*competition_dummy_factor))
detach(df_not_away)
# run genmatch disallowing ties
genout_away <- GenMatch(Tr=Tr_away, 
                        X=X_away, 
                        BalanceMatrix = BalanceMat_away, 
                        estimand="ATT", 
                        pop.size = 100, 
                        max.generations = 30, 
                        caliper=0.2,
                        ties = FALSE
                        )
# match data using genmatch weights
mout_away <- Match(Tr=Tr_away, X=X_away, Weight.matrix = genout_away, caliper=0.2, ties = FALSE)
# evaluate balance
mb_data_away <- data.frame(Tr=Tr_away, X_away)
mb_away <- MatchBalance(Tr ~ sport + competition_dummy_factor + elo_diff_adjusted_log + I(elo_diff_adjusted_log^2) + 
                            I(competition_dummy_factor*elo_diff_adjusted_log) + I(sport*competition_dummy_factor)
                        , data = mb_data_away, match.out = mout_away, nboots = 1000)
# evaluate treatment effect using a logistic regression, just like in the original
# paper.
df_results <- df_not_away[c(mout_away$index.control, mout_away$index.treated), ]
model_away <- glm(match_place_num ~ result_dummy_num, data = df_results, family = "binomial")
# results
summary(model_away)
confint(model_away)

# Analysis for home

df_not_home <- data_matching_not_home
df_not_home$match_place_num <- rep(0, nrow(data_matching_not_home))
df_not_home[which(data_matching_not_home$match_place_num == 2),]$match_place_num <- 1
df_not_home$sport <- as.numeric(df_not_home$sport)
df_not_home$competition_dummy_factor <- as.numeric(df_not_home$competition_dummy_factor)

Tr_home <- df_not_home$match_place_num
X_home <- df_not_home[, -c(1, 3)]

genout_home <- GenMatch(Tr=Tr_home, X=X_home, estimand="ATT", pop.size = 30, max.generations = 25)
mout_home <- Match(Tr=Tr_home, X=X_home, Weight.matrix = genout_home)

mb_data_home <- data.frame(Tr=Tr_home, X_home)

mb_home <- MatchBalance(Tr ~ sport + competition_dummy_factor + elo_diff_adjusted_log, data = mb_data_home, match.out = mout_home, nboots = 1000)

