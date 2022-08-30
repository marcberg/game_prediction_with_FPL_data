
games <- get_game_list() %>%
          #filter(finished == TRUE)%>% 
          filter(!is.na(team_h_score))%>% 
          select(id, team_h, team_a, team_h_score, team_a_score, kickoff) %>%
          mutate(result = case_when(team_h_score > team_a_score ~ "1",
                                    team_h_score < team_a_score ~ "2",
                                    TRUE ~ "X"),
                 match_day = as.Date(substr(as.character(kickoff), 1, 10))) %>%
          select(-kickoff)


# team_h_strength <- get_fdr() %>%
#                     select(id, strength_overall_home, strength_attack_home, strength_defence_home) %>%
#                     rename(team_h = id)
# team_a_strength <- get_fdr() %>%
#                     select(id, strength_overall_away, strength_attack_away, strength_defence_away) %>%
#                     rename(team_a = id)


# add team to get_player_details(name="Virgil van Dijk")
# använd " elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements " sen till score-data för att se om spelare startar eller ej
player_data <- get_player_details() 

player <- player_data %>%
  #filter(minutes > 0) %>%
  mutate(match_day = as.Date(substr(as.character(kickoff_time), 1, 10))) %>%
  rename(player_id = element) %>%
  select(-kickoff_time) %>%
  left_join(get_game_list() %>% select(id, team_h, team_a), by = c("fixture" = "id")) %>%
  mutate(team = case_when(was_home ~ team_h,
                          TRUE ~ team_a)) %>%
  select(-c("team_h", "team_a")) %>%
  
  group_by(player_id) %>%
  arrange(fixture) %>%
  mutate(next.fixture = lead(fixture, order_by = player_id),
         goals_per_hour = (goals_scored / minutes)*60,
         assists_per_hour = (assists / minutes)*60,
         goals_conceded_per_hour = (goals_conceded / minutes)*60,
         temp = ifelse(minutes < 45, 0, 1),
         win = case_when(minutes > 45 & was_home & team_h_score > team_a_score ~ 1,
                         minutes > 45 & !was_home & team_h_score < team_a_score ~ 1,
                         minutes > 45 ~ 0),
         draw = case_when(minutes > 45 & team_h_score == team_a_score ~ 1,
                          minutes > 45 ~ 0),
         loss = case_when(minutes > 45 & was_home & team_h_score < team_a_score ~ 1,
                          minutes > 45 & !was_home & team_h_score > team_a_score ~ 1,
                          minutes > 45 ~ 0)
         ) %>%
  
  mutate(mean_goal_5 = rollapply(goals_scored, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         mean_goal_10 = rollapply(goals_scored, 10, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         mean_goal_per_hour_5 = rollapply(goals_per_hour, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         mean_goal_per_hour_10 = rollapply(goals_per_hour, 10, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         
         mean_assists_5 = rollapply(assists, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         mean_assists_10 = rollapply(assists, 10, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         mean_assists_per_hour_5 = rollapply(assists_per_hour, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         mean_assists_per_hour_10 = rollapply(assists_per_hour, 10, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         
         mean_goals_conceded_5 = rollapply(goals_conceded, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         mean_goals_conceded_10 = rollapply(goals_conceded, 10, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         mean_goals_conceded_per_hour_5 = rollapply(goals_conceded_per_hour, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         mean_goals_conceded_per_hour_10 = rollapply(goals_conceded_per_hour, 10, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         
         n_games = rollapply(ifelse(minutes > 45, player_id/player_id, player_id/player_id-1), 38, sum, align = 'right', fill = NA, partial = TRUE),
         minutes_5 = rollapply(minutes, 5, sum, align = 'right', fill = NA, partial = TRUE),
         
         win_rate = rollapply(win, 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         draw_rate = rollapply(draw, 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
         loss_rate = rollapply(loss, 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE)
         ) %>%
  left_join(get_player_info() %>% select(id, element_type), by = c("player_id" = "id")) %>%
  select(playername, player_id, element_type,
         team, fixture, next.fixture, 
         value, # vart fan är "position"???
         n_games, 
         minutes, 
         minutes_5,
         win, 
         win_rate, 
         draw, 
         draw_rate, 
         loss, 
         loss_rate,
         goals_scored, 
         mean_goal_5, mean_goal_10, 
         goals_per_hour, 
         mean_goal_per_hour_5, mean_goal_per_hour_10, 
         assists, 
         mean_assists_5, mean_assists_10, 
         assists_per_hour, 
         mean_assists_per_hour_5, mean_assists_per_hour_10, 
         goals_conceded, 
         mean_goals_conceded_5, mean_goals_conceded_10, goals_conceded_per_hour, mean_goals_conceded_per_hour_5, mean_goals_conceded_per_hour_10) %>%
  arrange(player_id, fixture)

player %>% View

# den här är för att säga vilka spelare som spelar en match från start. När jag score:ar måste jag skatta vilka som ska spela
player_selected <- player %>% 
                    mutate(element_type2 = ifelse(element_type == 1, 1, 2)) %>%
                    group_by(team, fixture, element_type2) %>%
      
                      mutate(hierarchy = row_number(desc(minutes))) %>% ungroup() %>%
  
                    filter((element_type2 == 1 & hierarchy == 1) | 
                             (element_type2 == 2 & hierarchy <= 10)) %>%
                    select(team, fixture, player_id) %>% 
                    arrange(team, fixture, player_id)

# beräknar lag-stats för att senare kolla hur mkt spelarna tillför till det
team_performance <- player %>%
  
  group_by(team, next.fixture) %>%
  # det här blir egentligen fel, för vi kommer räkna samma sak för många gånger. Men nu hinner jag inte ändra det
  summarise(team_no_goals_10 = sum(mean_goal_10), 
            team_no_assists_10 = sum(mean_assists_10), 
            team_no_goals_conceded_10 = sum(mean_goals_conceded_10)) %>%
  select(team, next.fixture, team_no_goals_10, team_no_assists_10, team_no_goals_conceded_10)
           
# beräknar hur viktig en spelare är för sitt lag inom mål, assists, insläppta mål,
player_performance <- player %>% left_join(team_performance, by = c("team", "next.fixture")) %>%
                        mutate(player_share_goals_10 = coalesce(mean_goal_10 / team_no_goals_10, 0), 
                               player_share_assists_10 = coalesce(mean_assists_10 / team_no_assists_10, 0), 
                               # spelare som spelar sällan gynnas av nedan
                               player_share_goals_conceded_10 = coalesce(mean_goals_conceded_10 / team_no_goals_conceded_10, 0)) %>%
  
                        group_by(team, next.fixture) %>% mutate(player_goals_hierarchy = row_number(desc(player_share_goals_10)),
                                                                player_assists_hierarchy = row_number(desc(player_share_assists_10)),
                                                                player_goals_conceded_hierarchy = row_number(desc(player_share_goals_conceded_10))) %>% ungroup() %>%

                        inner_join(player_selected, by = c("player_id", "team", "next.fixture" = "fixture")) %>%
                        select(player_id, element_type, team, next.fixture, value, player_share_goals_10, player_goals_hierarchy, player_share_assists_10, player_assists_hierarchy, player_share_goals_conceded_10, player_goals_conceded_hierarchy)
 
player_performance_group <- player_performance %>%
                              group_by(team, next.fixture) %>%
                              summarise(goalkeeper_value = sum(ifelse(element_type == 1, value, 0)),
                                        highest_defender_value = sum(ifelse(element_type == 2, value, 0)),
                                        highest_midfielder_value = sum(ifelse(element_type == 3, value, 0)),
                                        highest_striker_value = sum(ifelse(element_type == 4, value, 0)),
                                        team_value = sum(value),
                                        
                                        sum_player_share_goals_10 = sum(player_share_goals_10),
                                        best_player_share_goals_10 = max(player_share_goals_10),
                                        
                                        sum_player_share_assists_10 = sum(player_share_assists_10),
                                        best_player_share_assists_10 = max(player_share_assists_10),
                                        
                                        sum_player_share_conceded_goals_10 = sum(player_share_goals_conceded_10),
                                        best_player_share_conceded_goals_10 = max(player_share_goals_conceded_10),
                                        
                                        no_defenders = sum(ifelse(element_type == 2, 1, 0)),
                                        no_midfielders = sum(ifelse(element_type == 3, 1, 0)),
                                        no_strikers = sum(ifelse(element_type == 4, 1, 0)))
                         

###############

# lagets form
teams_matches <- list()
for(i in 1:20){
  teams_matches[[i]] <- get_game_list() %>%
                          select(id, team_h, team_a, team_h_score, team_a_score, kickoff) %>%
                          mutate(result = case_when(team_h_score > team_a_score ~ "1",
                                                    team_h_score < team_a_score ~ "2",
                                                    TRUE ~ "X"),
                                 match_day = as.Date(substr(as.character(kickoff), 1, 10))) %>%
                          select(-kickoff) %>% filter(team_a == i | team_h == i) %>%
                          mutate(team = i,
                                 home = ifelse(team_h == i, TRUE, FALSE),
                                 win = case_when(home & team_h_score > team_a_score ~ 1,
                                                 !home & team_h_score < team_a_score ~ 1,
                                                 TRUE ~ 0),
                                 draw = case_when(team_h_score == team_a_score ~ 1,
                                                  TRUE ~ 0),
                                 loss = case_when(home & team_h_score < team_a_score ~ 1,
                                                 !home & team_h_score > team_a_score ~ 1,
                                                 TRUE ~ 0),
                                 goals_scored = case_when(home ~ team_h_score,
                                                          !home ~ team_a_score),
                                 goals_conceded = case_when(home ~ team_a_score,
                                                            !home ~ team_h_score)) %>%
                            select(team, id, home, win, draw, loss, goals_scored, goals_conceded)
}

teams_matches <- do.call("rbind", teams_matches)
teams_matches %>% View #select(team, win, draw, loss, fixture, next.fixture) %>% select(-player_id) %>% View

teams_matches_form <- teams_matches %>%
                        mutate(goals_scored_avg_4 = coalesce(rollapply(goals_scored, 4, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE), 0),
                               goals_conceded_avg_4 = coalesce(rollapply(goals_conceded, 4, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE), 0),
                               win_avg_4 = coalesce(rollapply(win, 4, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE), 0),
                               draw_avg_4 = coalesce(rollapply(draw, 4, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE), 0),
                               loss_avg_4 = coalesce(rollapply(loss, 4, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE), 0)) %>%
                        group_by(team) %>%
                        mutate(next.fixture = lead(id, order_by = id)) %>%
                        select(team, next.fixture, goals_scored_avg_4, goals_conceded_avg_4, win_avg_4, draw_avg_4, loss_avg_4)

##############




train <-  games %>%
  
              left_join(player_performance_group %>%
                          rename(h_goalkeeper_value = goalkeeper_value,
                                 h_highest_defender_value = highest_defender_value,
                                 h_highest_midfielder_value = highest_midfielder_value,
                                 h_highest_striker_value = highest_striker_value,
                                 h_team_value = team_value,
                                 
                                 h_sum_player_share_goals_10 = sum_player_share_goals_10,
                                 h_best_player_share_goals_10 = best_player_share_goals_10,
                                 
                                 h_sum_player_share_assists_10 = sum_player_share_assists_10,
                                 h_best_player_share_assists_10 = best_player_share_assists_10,
                                 
                                 h_sum_player_share_conceded_goals_10 = sum_player_share_conceded_goals_10,
                                 h_best_player_share_conceded_goals_10 = best_player_share_conceded_goals_10,
                                 
                                 h_no_defenders = no_defenders,
                                 h_no_midfielders = no_midfielders,
                                 h_no_strikers = no_strikers), by = c("team_h" = "team", "id" = "next.fixture")) %>%
  
              left_join(player_performance_group %>%
                          rename(a_goalkeeper_value = goalkeeper_value,
                                 a_highest_defender_value = highest_defender_value,
                                 a_highest_midfielder_value = highest_midfielder_value,
                                 a_highest_striker_value = highest_striker_value,
                                 a_team_value = team_value,
                                 
                                 a_sum_player_share_goals_10 = sum_player_share_goals_10,
                                 a_best_player_share_goals_10 = best_player_share_goals_10,
                                 
                                 a_sum_player_share_assists_10 = sum_player_share_assists_10,
                                 a_best_player_share_assists_10 = best_player_share_assists_10,
                                 
                                 a_sum_player_share_conceded_goals_10 = sum_player_share_conceded_goals_10,
                                 a_best_player_share_conceded_goals_10 = best_player_share_conceded_goals_10,
                                 
                                 a_no_defenders = no_defenders,
                                 a_no_midfielders = no_midfielders,
                                 a_no_strikers = no_strikers), by = c("team_a" = "team", "id" = "next.fixture")) %>%
  
              left_join(teams_matches_form %>%
                          rename(h_goals_scored_avg_4 = goals_scored_avg_4,
                                 h_goals_conceded_avg_4 = goals_conceded_avg_4,
                                 h_win_avg_4 = win_avg_4,
                                 h_draw_avg_4 = draw_avg_4,
                                 h_loss_avg_4 = loss_avg_4), by = c("team_h" = "team", "id" = "next.fixture")) %>%
  
              left_join(teams_matches_form %>%
                          rename(a_goals_scored_avg_4 = goals_scored_avg_4,
                                 a_goals_conceded_avg_4 = goals_conceded_avg_4,
                                 a_win_avg_4 = win_avg_4,
                                 a_draw_avg_4 = draw_avg_4,
                                 a_loss_avg_4 = loss_avg_4), by = c("team_a" = "team", "id" = "next.fixture")) #%>%
  
#              select(-team_a, -team_h, -team_h_score, -team_a_score, -match_day, -id)
          

train %>% View 
