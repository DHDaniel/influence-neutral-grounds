
library(Matching)
library(ggplot2)
library(ggthemes)
library(rbounds)

load("data_matching_not_away.Rda")
load("data_matching_not_home.Rda")

# for replication purposes
set.seed(123)

# better ggplot theme
theme_set(ggthemes::theme_few())

# analysis for home

# pre-process data for genmatch
df_home <- data_matching_not_away
df_home$match_place_num <- rep(0, nrow(data_matching_not_away))
df_home[which(data_matching_not_away$match_place_num == 3),]$match_place_num <- 1
df_home$sport <- as.numeric(df_home$sport)
df_home$competition_dummy_factor <- as.numeric(df_home$competition_dummy_factor)

# define treatment and the x matrix for genmatch
Tr_home <- df_home$match_place_num
X_home <- df_home[, -c(1, 3)]
# make balance matrix for genmatch using interaction terms
attach(df_home)
BalanceMat_home <- cbind(sport, competition_dummy_factor, elo_diff_adjusted_log, I(elo_diff_adjusted_log^2), 
                         I(competition_dummy_factor*elo_diff_adjusted_log), I(sport*competition_dummy_factor))
detach(df_home)
# run genmatch disallowing ties
genout_home <- GenMatch(Tr=Tr_home, 
                        X=X_home, 
                        BalanceMatrix = BalanceMat_home, 
                        estimand="ATT", 
                        pop.size = 100, 
                        max.generations = 30, 
                        caliper=0.2,
                        ties = FALSE
                        )
# match data using genmatch weights
mout_home <- Match(Tr=Tr_home, X=X_home, Weight.matrix = genout_home, caliper=0.2, ties = FALSE)
# evaluate balance
mb_data_home <- data.frame(Tr=Tr_home, X_home)
mb_home <- MatchBalance(Tr ~ sport + competition_dummy_factor + elo_diff_adjusted_log + I(elo_diff_adjusted_log^2) + 
                            I(competition_dummy_factor*elo_diff_adjusted_log) + I(sport*competition_dummy_factor)
                        , data = mb_data_home, match.out = mout_home, nboots = 1000)
# evaluate treatment effect using a logistic regression, just like in the original
# paper.
df_results_home <- df_home[c(mout_home$index.control, mout_home$index.treated), ]
model_home <- glm(match_place_num ~ result_dummy_num, data = df_results_home, family = "binomial")
model_home_cov <- glm(match_place_num ~ result_dummy_num + sport + elo_diff_adjusted_log + competition_dummy_factor, data = df_results_home, family = "binomial")

# Analysis for away

df_away <- data_matching_not_home
df_away$match_place_num <- rep(0, nrow(data_matching_not_home))
df_away[which(data_matching_not_home$match_place_num == 2),]$match_place_num <- 1
df_away$sport <- as.numeric(df_away$sport)
df_away$competition_dummy_factor <- as.numeric(df_away$competition_dummy_factor)

Tr_away <- df_away$match_place_num
X_away <- df_away[, -c(1, 3)]

# make balance matrix for genmatch using interaction terms
attach(df_away)
BalanceMat_away <- cbind(sport, competition_dummy_factor, elo_diff_adjusted_log, I(elo_diff_adjusted_log^2), 
                         I(competition_dummy_factor*elo_diff_adjusted_log), I(sport*competition_dummy_factor))
detach(df_away)

genout_away <- GenMatch(Tr=Tr_away, 
                        X=X_away, 
                        BalanceMatrix = BalanceMat_away,
                        estimand="ATT", 
                        caliper=0.2,
                        pop.size = 100, 
                        max.generations = 25, 
                        ties = FALSE)
mout_away <- Match(Tr=Tr_away, X=X_away, Weight.matrix = genout_away, ties = FALSE, caliper = 0.2)

mb_data_away <- data.frame(Tr=Tr_away, X_away)

mb_away <- MatchBalance(Tr ~ sport + competition_dummy_factor + elo_diff_adjusted_log + I(elo_diff_adjusted_log^2) + 
                            I(competition_dummy_factor*elo_diff_adjusted_log) + I(sport*competition_dummy_factor),
                        data = mb_data_away, match.out = mout_away, nboots = 1000)
# evaluate treatment effect using a logistic regression, just like in the original
# paper.
df_results_away <- df_away[c(mout_away$index.control, mout_away$index.treated), ]
model_away <- glm(match_place_num ~ result_dummy_num, data = df_results_away, family = "binomial")
model_away_cov <- glm(match_place_num ~ result_dummy_num + sport + elo_diff_adjusted_log + competition_dummy_factor, data = df_results_away, family = "binomial")


#########################
# Plotting our results
#########################

cem_data_plot <- readRDS("figure_3_data_plot.Rda")
cem_data_plot <- data.frame(
    "model_type"=cem_data_plot$model_type,
    "SATT"=cem_data_plot$SATT, 
    "ci_low"=cem_data_plot$ci_low, 
    "ci_high"=cem_data_plot$ci_high, 
    "labels_axis"=cem_data_plot$labels_axis
)
genmatch_data_home <- data.frame(
    "model_type"=rep(c("From Home to Neutral"), 2),
    "SATT"=c(model_home$coefficients[2], model_home_cov$coefficients[2]),
    "ci_low"=c(confint(model_home)[2, 1], confint(model_home_cov)[2, 1]),
    "ci_high"=c(confint(model_home)[2, 2],  confint(model_home_cov)[2, 2]),
    "labels_axis"=c("GenMatch (99.6%)", "GenMatch, with covariates (99.6%)")
)
genmatch_data_away <- data.frame(
    "model_type"=rep(c("From Away to Neutral"), 2),
    "SATT"=c(model_away$coefficients[2], model_away_cov$coefficients[2]),
    "ci_low"=c(confint(model_away)[2, 1], confint(model_away_cov)[2, 1]),
    "ci_high"=c(confint(model_away)[2, 2],  confint(model_away_cov)[2, 2]),
    "labels_axis"=c("GenMatch (97.8%)", "GenMatch, with covariates (97.8%)")
)
plot_data <- rbind(cem_data_plot, genmatch_data_home, genmatch_data_away)

ggplot(plot_data, aes(y = SATT, x = labels_axis)) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "red") +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high)) +
    coord_flip() + 
    facet_wrap(~model_type, nrow = 2, scales = "free_y") +
    labs(y = "Sample average treatment effect on the treated (SATT) \n and 95% CI of log-odds",
         x = NULL) +
    theme(axis.text = element_text(colour = "black"),
          strip.text = element_text(size = 11, face = "bold"))

##########################
# Sensitivity analysis
##########################

# Rosenbaum sensitivity analysis for home
mout_home_est <- Match(Y = df_home$result_dummy_num, Tr=Tr_home, X=X_home, Weight.matrix = genout_home, caliper=0.2, ties = FALSE)
sens_home <- psens(mout_home_est, Gamma = 3, GammaInc = 0.1)
summary(sens_home)

# Rosenbaum sensitivity analysis for away
mout_away_est <- Match(Y = df_away$result_dummy_num, Tr=Tr_away, X=X_away, Weight.matrix = genout_away, ties = FALSE, caliper = 0.2)
sens_away <- psens(mout_away_est, Gamma = 3, GammaInc = 0.1)
summary(sens_away)
