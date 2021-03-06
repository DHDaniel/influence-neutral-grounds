
##############################
####
#### Authors: Lieam Kneafsey, and Stefan Müller
#### Title: "Assessing the Influence of Neutral Grounds on Match Outcomes."
#### Journal: International Journal of Performance Analysis in Sport
#### 
#### Replication Material for all plots and tables reported 
#### in the paper and in the Supplementary Material
#### 
##############################

## load required packages
## note: you can install packages from CRAN
## using install.packages("name_of_package")

library(rio)
library(tidyverse)
library(ggthemes)
library(cem)
library(lubridate)
library(rms)
library(lme4)
library(texreg)
library(effects)
library(ggeffects)
library(nnet)

## This script was run successfully with the following versions 
## of the packages above:

# attached base packages:
#     [1] tcltk     stats     graphics  grDevices utils     datasets  methods  
# [8] base     
# 
# other attached packages:
#     [1] nnet_7.3-12     ggeffects_0.3.2 effects_4.0-2   carData_3.0-1  
# [5] texreg_1.36.23  lme4_1.1-18-1   Matrix_1.2-14   rms_5.1-2      
# [9] SparseM_1.77    Hmisc_4.1-1     Formula_1.2-2   survival_2.41-3
# [13] lubridate_1.7.4 cem_1.1.19      lattice_0.20-35 ggthemes_3.4.2 
# [17] forcats_0.3.0   stringr_1.3.1   dplyr_0.7.6     purrr_0.2.5    
# [21] readr_1.1.1     tidyr_0.8.1     tibble_1.4.2    ggplot2_3.0.0  
# [25] tidyverse_1.2.1 rio_0.5.10     

## select plotting scheme

theme_set(ggthemes::theme_few())

## load raw data and combine to one data frame

data_raw_football <- rio::import("gaa_results_football.xlsx") %>% 
    mutate(sport = "Gaelic Football")

data_raw_hurling <- rio::import("gaa_results_hurling.xlsx") %>% 
    mutate(sport = "Hurling")

data_raw <- bind_rows(data_raw_football, data_raw_hurling) 

data_select <- data_raw %>% 
    select(Date:Margin, sport) %>% 
    mutate(game_id = paste(Date, Team_1, Team_2, sep = "_")) %>% 
    mutate(result = if_else(Sc_1 > Sc_2, "Team 1 Won",
                            if_else(Sc_1 == Sc_2, "Draw", 
                                    "Team 2 Won")))

## transform data to long format

data_long <- data_select %>% 
    select(-c(P_1, P_2, G_1, G_2)) %>% 
    gather(key = score_type, value = score_team, -c(game_id, sport, Date, result, Grade, Team_1, Team_2, Elo_1, Elo_2, Home, Margin)) %>% 
    arrange(game_id)

## create dummy for competition and recode ground 
data_long_modified <- data_long %>% 
    mutate(competition_dummy = if_else(Grade %in% c("All-Ireland", "Provincial", "Qualifier"), "Championship", "Other")) %>% 
    mutate(match_place = if_else(Home == "Y", "Home/Away", "Neutral")) %>% 
    mutate(team = if_else(score_type == "Sc_1", Team_1, Team_2)) %>% 
    mutate(elo_team = if_else(score_type == "Sc_1", Elo_1, Elo_2)) %>% 
    mutate(elo_diff_team_1 = Elo_1 - Elo_2) %>% 
    mutate(elo_diff_team_2 = Elo_2 - Elo_1) %>% 
    mutate(elo_diff = if_else(score_type == "Sc_1", elo_diff_team_1, elo_diff_team_2)) %>% 
    mutate(result = if_else(result == "Team 1 Won" & score_type == "Sc_1", "winner",
                            if_else(result == "Team 2 Won" & score_type == "Sc_2", "winner",
                                    if_else(result == "Team 1 Won" & score_type == "Sc_2", "loser",
                                            if_else(result == "Team 2 Won" & score_type == "Sc_1", "loser", "draw"))))) %>% 
    mutate(match_place = if_else(score_type == "Sc_1" & Home == "Y", "Home",
                                 if_else(score_type == "Sc_2" & Home == "Y", "Away", "Neutral Ground"))) %>% 
    mutate(Elo_1_no_home_adv = if_else(Home == "Y", Elo_1 - 100, Elo_1)) %>% 
    mutate(elo_adjusted_diff_team_1 = Elo_1_no_home_adv - Elo_2) %>% 
    mutate(elo_adjusted_diff_team_2 = Elo_2 - Elo_1_no_home_adv) %>% 
    mutate(elo_diff_adjusted = if_else(score_type == "Sc_1", elo_adjusted_diff_team_1, elo_adjusted_diff_team_2)) %>% 
    mutate(competition_dummy_factor = as.factor(competition_dummy))

## recode result to numberic and transform some variables to factors

data_long_dummy <- data_long_modified %>% 
    mutate(result_dummy = if_else(result == "winner", 1, 0)) %>% 
    mutate(result_num = if_else(result == "winner", 1, 
                                if_else(result == "loser", 0, 0.5))) %>% 
    mutate(result = as.factor(result)) %>% 
    mutate(game_id = as.factor(game_id),
           team = as.factor(team),
           match_place = as.factor(match_place), 
           sport = as.factor(sport),
           year = as.factor(lubridate::year(Date))) %>% 
    mutate(Grade = as.factor(Grade)) %>% 
    mutate(elo_diff_log = log(elo_diff + 1)) %>% 
    mutate(elo_diff_adjusted_log = log(elo_diff_adjusted + 1))  

## only keep the favourite team for each match in the dataset
data_long_one_team <- data_long_dummy %>% 
    group_by(game_id) %>% 
    arrange(-elo_diff_adjusted) %>% 
    filter(row_number() == 1) %>% 
    mutate(team_sport = as.factor(paste(team, sport, sep = "_")))


data_long_one_team$team_sport <- as.factor(data_long_one_team$team_sport)

## Calculate home advantage (with bootstrapped confidence intervals) ----

cases_home_advantage <- data_long_dummy %>%
    filter(match_place != "Neutral Ground") %>% 
    filter(match_place == "Home") %>% 
    group_by(sport, competition_dummy) %>% 
    count() 

boot_home_advantage <- data_long_dummy %>%
    filter(match_place != "Neutral Ground") %>% 
    ungroup() %>% 
    group_by(sport, competition_dummy, match_place) %>%
    do(data.frame(rbind(smean.cl.boot(.$result_num, na.rm = TRUE)))) %>% 
    filter(match_place == "Home") %>% 
    mutate(label_axis = paste(sport, competition_dummy, sep = ": "))

boot_home_advantage <- boot_home_advantage %>% 
    bind_cols(cases_home_advantage)

## Run regression models for paper

data_long_one_team_dummy <- data_long_one_team %>% 
    mutate(team_sport = paste(team, sport, sep = "_")) %>% 
    filter(result %in% c("winner", "loser")) %>% 
    mutate(team_sport = as.factor(team_sport))


## Model 1 (Table 1) ----
glmer_total <- glmer(result_dummy ~  competition_dummy_factor * match_place *
                         sport + 
                         elo_diff_adjusted_log +year + (1 | team_sport),
                     family = binomial(logit),  
                     data = data_long_one_team_dummy)

## Model 2 (Table 1) ----
glmer_football <- glmer(result_dummy ~ 
                            elo_diff_adjusted_log + 
                            competition_dummy_factor * match_place +  
                            year + (1 | team),
                        family = binomial(logit),  
                        data = filter(data_long_one_team_dummy,
                                      sport == "Gaelic Football"))

## Model 3 (Table 1) ----
glmer_hurling <- glmer(result_dummy ~ elo_diff_adjusted_log + 
                           competition_dummy_factor * match_place + 
                           year + (1 | team),
                       family = binomial(logit),  
                       data = filter(data_long_one_team_dummy, 
                                     sport == "Hurling"))

## Figure 2 ----

data_effect_place <- Effect(c("match_place", "sport"), xlevels = 100, 
                            glmer_total) %>% 
    as.data.frame()

effect_match_place <- ggplot(data_effect_place, 
                             aes(x = match_place, 
                                 y = fit, ymin = lower, ymax = upper)) + 
    geom_pointrange() + 
    labs(x = NULL, y = "Predicted probability of winning") +
    geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red") +
    facet_wrap(~sport) + 
    scale_y_continuous(limits = c(0.4, 1), breaks = c(seq(0.4, 1, 0.1))) +
    theme(axis.text = element_text(colour = "black"),
          strip.text = element_text(size = 11, face = "bold"))

effect_match_place




## create data frame for matching (excluding draws)

data_matching <- data_long_one_team %>% 
    filter(result != "draw") %>% 
    ungroup() %>% 
    mutate(match_place_num = as.numeric(match_place)) %>% 
    mutate(result_dummy = as.factor(result_dummy),
           result_dummy_num = as.numeric(result_dummy),
           result_dummy_num = car::recode(result_dummy_num, "1=0;2=1"),
           team_sport = as.numeric(team_sport)) %>% 
    select(match_place, 
           match_place_num, 
           sport, 
           result_dummy_num, 
           competition_dummy_factor, elo_diff_adjusted_log) %>% 
    as.data.frame()


## models excluding away (neutral -> home)    

data_matching_not_away <- data_matching %>% 
    filter(match_place != "Away") %>% 
    select(-match_place) %>% 
    as.data.frame()

data_matching_not_away$sport

q1.grp <- list(c("Hurling"), c("Gaelic Football"))

mat_home_neutral_weight <- cem(treatment = "match_place_num", 
                               data = data_matching_not_away, 
                               grouping = list(sport = q1.grp),
                               drop = "result_dummy_num")


est_home_neutral_weight <- att(mat_home_neutral_weight, result_dummy_num ~ match_place_num, data = data_matching_not_away, model = "logit")

est_home_neutral_weight_ivs <- att(mat_home_neutral_weight, 
                                   result_dummy_num ~ match_place_num +  
                                       sport  + 
                                       competition_dummy_factor + 
                                       elo_diff_adjusted_log, 
                                   data = data_matching_not_away,
                                   model = "logit")

mat_home_neutral_exact <- cem(treatment = "match_place_num", 
                              data = data_matching_not_away, 
                              drop = "result_dummy_num", keep.all = TRUE)

mat_home_neutral_exact <- k2k(mat_home_neutral_exact, 
                              data_matching_not_away, "euclidean", 1)

est_home_neutral_exact <- att(mat_home_neutral_exact, 
                              result_dummy_num ~ match_place_num, 
                              data = data_matching_not_away, model = "logit")

est_home_neutral_exact_ivs <- att(mat_home_neutral_exact, 
                                  result_dummy_num ~ match_place_num +  
                                      sport + 
                                      competition_dummy_factor + 
                                      elo_diff_adjusted_log, 
                                  data = data_matching_not_away,
                                  model = "logit")

data_matching_not_home <- data_matching %>% 
    filter(match_place != "Home") %>% 
    select(-match_place) %>% 
    mutate(match_place_num = car::recode(match_place_num, "3=2")) %>% 
    as.data.frame()

mat_away_neutral_weighted <- cem(treatment = "match_place_num", data = data_matching_not_home, drop = "result_dummy_num")

est_away_neutral_weight <- att(mat_away_neutral_weighted, 
                               result_dummy_num ~ match_place_num, 
                               data = data_matching_not_home, model = "logit")


est_away_neutral_weight_ivs <- att(mat_away_neutral_weighted, 
                                   result_dummy_num ~ match_place_num + 
                                       sport + 
                                       competition_dummy_factor +
                                       elo_diff_adjusted_log, 
                                   data = data_matching_not_home,
                                   model = "logit")


mat_away_neutral_exact <- cem(treatment = "match_place_num", 
                              data = data_matching_not_home, 
                              drop = "result_dummy_num", keep.all = TRUE)

mat_away_neutral_exact <- k2k(mat_away_neutral_exact, 
                              data_matching_not_home, "euclidean", 1)

est_away_neutral_exact <- att(mat_away_neutral_exact, 
                              result_dummy_num ~ match_place_num, 
                              data = data_matching_not_home, model = "logit")


est_away_neutral_exact_ivs <- att(mat_away_neutral_exact, 
                                  result_dummy_num ~ match_place_num + 
                                      sport +
                                      competition_dummy_factor + 
                                      elo_diff_adjusted_log, 
                                  data = data_matching_not_home,
                                  model = "logit")


## write function to extract model coefficients and add note on model
extract_cem <- function(model, note) {
    est <- model
    
    treatment_df <- data.frame(est$tab)
    treatment_df <- rowSums(treatment_df)
    percentage_matched <- paste0(round(treatment_df[2] / treatment_df[1] * 100, 1), "%")
    
    coefs <- data.frame(est$att.model)
    satt <- coefs$match_place_num[1]
    satt_se <- coefs$match_place_num[2]
    satt_pval <-  sprintf("%.4f", round(coefs$match_place_num[4], 4))
    
    df <- data.frame(
        `model` = as.character(note),
        `matchted` = percentage_matched,
        satt = round(satt, 3),
        `se` = round(satt_se, 3),
        `p-value` = satt_pval
        
    )
    rownames(df) <- c() 
    print(df)
}



model1a <- extract_cem(est_home_neutral_weight, note = "Home -> Neutral (weighted)")
model1b <- extract_cem(est_home_neutral_exact, note = "Home -> Neutral (balanced)")
model1c <- extract_cem(est_home_neutral_weight_ivs, note = "Home -> Neutral (weighted) with covariates")
model1d <- extract_cem(est_home_neutral_exact_ivs, note = "Home -> Neutral (balanced) with covariates")
model2a <- extract_cem(est_away_neutral_weight, note = "Away -> Neutral (weighted)")
model2b <- extract_cem(est_away_neutral_exact, note = "Away -> Neutral (balanced)")
model2c <- extract_cem(est_away_neutral_weight_ivs, note = "Away -> Neutral (weighted) with covariates")
model2d <- extract_cem(est_away_neutral_exact_ivs, note = "Away -> Neutral (balanced) with covariates")


output_cem <- bind_rows(model1a, model1b, model1c, model1d,  
                        model2a, model2b, model2c, model2d) %>% 
    rownames_to_column(var = "Model") %>% 
    mutate(Model = paste("Model", Model))
colnames(output_cem) <- c("Model", "Specification", "Matched observations", "SATT", "SE (SATT)", "p-value")


## prepare data frame for plot
output_cem_plot <- output_cem %>% 
    mutate(ci_low = SATT - 1.96 * `SE (SATT)`,
           ci_high = SATT + 1.96 * `SE (SATT)`) %>% 
    mutate(SATT = as.numeric(SATT)) %>% 
    mutate(model_type = ifelse(str_detect(Specification, "Home"), "From Home to Neutral", "From Away to Neutral")) %>% 
    mutate(specification_short = car::recode(Specification, 
                                             "'Home -> Neutral (weighted)'='Weighted';'Away -> Neutral (weighted)'='Weighted'; 'Home -> Neutral (balanced)'='Balanced'; 'Home -> Neutral (weighted) with covariates'='Weighted, with covariates'; 'Home -> Neutral (balanced) with covariates'='Balanced, with covariates';'Away -> Neutral (balanced)'='Balanced';
                                             'Away -> Neutral (weighted) with covariates'='Weighted, with covariates';'Neutral (balanced) with covariates'='Balanced, with covariates';
                                             'Away -> Neutral (balanced) with covariates'='Balanced, with covariates'")) %>% 
    mutate(labels_axis = paste0(specification_short, " (", `Matched observations`, ")"))


## Figure 3 ----
ggplot(output_cem_plot, aes(y = SATT, x = labels_axis)) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "red") +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high)) +
    coord_flip() + 
    facet_wrap(~model_type, nrow = 2, scales = "free_y") +
    labs(y = "Sample average treatment effect on the treated (SATT) \n and 95% CI of log-odds",
         x = NULL) +
    theme(axis.text = element_text(colour = "black"),
          strip.text = element_text(size = 11, face = "bold"))

