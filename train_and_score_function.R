

create_train_and_score_df <- function(seasons = c(20, 21, 22), score_seasons = c(FALSE, FALSE, TRUE)){
  
  n_seasons <- length(seasons)
  list_df <- list()
  
  for(i in 1:n_seasons){
    season <- seasons[i]
    score_season <- score_seasons[i]
    
    game_list <- if(!score_season){get_game_list(season = season)}else{get_game_list()}
    game_list <- game_list %>% select(-one_of("X"))
    
    team_id <- game_list %>% select(team_h, home) %>% rename(team = team_h, team_name = home) %>%
      union(game_list %>% select(team_a, away) %>% rename(team = team_a, team_name = away)) %>% distinct
    
    train_games <- game_list %>%
      #filter(finished == TRUE)%>% 
      filter(!is.na(team_h_score)) %>% 
      select(id, team_h, team_a, team_h_score, team_a_score, kickoff) %>%
      mutate(result = case_when(team_h_score > team_a_score ~ "1",
                                team_h_score < team_a_score ~ "2",
                                TRUE ~ "X"),
             match_day = as.Date(substr(as.character(kickoff), 1, 10))) %>%
      select(-kickoff, -team_h_score, -team_a_score)
    print("train_games")
    
    if(score_season){
      
      score_games <- game_list %>%
        #filter(finished == TRUE)%>% 
        filter(is.na(team_h_score)) %>% 
        select(id, team_h, team_a, kickoff) %>%
        mutate(result = NA, 
               match_day = as.Date(substr(as.character(kickoff), 1, 10))) %>%
        select(-kickoff)
      print("score_games")
      
      
      teams_next_game <- score_games %>% select(team_h, id, match_day) %>% rename(team = team_h) %>% 
        union(score_games %>% select(team_a, id, match_day) %>% rename(team = team_a)) %>%
        group_by(team) %>% mutate(hierarchy = row_number(match_day)) %>% filter(hierarchy == 1) %>% select(-hierarchy)
      print("teams_next_game")
      
      
      next_fixture <- score_games %>% 
        select(team_h, id, match_day) %>% 
        rename(team = team_h) %>% 
        union(score_games %>% 
                select(team_a, id, match_day) %>% 
                rename(team = team_a)
        ) %>% 
        union(
          train_games %>% 
            select(team_h, id, match_day) %>% 
            rename(team = team_h) %>% 
            union(train_games %>% 
                    select(team_a, id, match_day) %>% 
                    rename(team = team_a)
            )
        ) %>% 
        arrange(team, match_day) %>%
        group_by(team) %>%
        mutate(next_fixture = lead(id)) %>%
        select(-match_day) %>%
        as.data.frame
      
    }else{
      
      next_fixture <- train_games %>% 
        select(team_h, id, match_day) %>% 
        rename(team = team_h) %>% 
        union(train_games %>% 
                select(team_a, id, match_day) %>% 
                rename(team = team_a)
        ) %>% 
        arrange(team, match_day) %>%
        group_by(team) %>%
        mutate(next_fixture = lead(id)) %>%
        select(-match_day) %>%
        as.data.frame
      
    }
    print("next_fixture")
    
    player_data <- if(!score_season){get_player_details(season = season)}else{get_player_details()} 
    print("player_data")
    player_info <- if(!score_season){get_player_info(season = season)}else{get_player_info()}
    print("player_info")
    
    player <- player_data %>%
      mutate(match_day = as.Date(substr(as.character(kickoff_time), 1, 10))) %>%
      rename(player_id = element) %>%
      select(-kickoff_time) %>%
      left_join(game_list %>% select(id, team_h, team_a), by = c("fixture" = "id")) %>% # add which team the player plays for
      mutate(team = case_when(was_home ~ team_h,
                              TRUE ~ team_a)) %>%
      select(-c("team_h", "team_a")) %>%
      group_by(player_id) %>%
      arrange(player_id, fixture) %>%
      mutate(
        goals_per_hour = (goals_scored / minutes)*60,
        assists_per_hour = (assists / minutes)*60,
        goals_conceded_per_hour = (goals_conceded / minutes)*60,
        temp = ifelse(minutes < 45, 0, 1),
      ) %>%
      
      mutate(total_goals = rollapply(goals_scored, 38, sum, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
             total_assists = rollapply(assists, 38, sum, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
             
             mean_goal_5 = rollapply(goals_scored, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
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
             minutes_5 = rollapply(minutes, 5, sum, align = 'right', fill = NA, partial = TRUE)
      ) %>%
      left_join(player_info %>% select(id, element_type), by = c("player_id" = "id")) %>% # adds position. Snapshot with no history
      select(playername, 
             player_id, 
             element_type,# 1 goalie, 2 defender, 3 midfielder, 4 forward
             team, fixture, 
             value, 
             n_games, 
             minutes, 
             minutes_5,
             goals_scored, 
             total_goals, total_assists,
             mean_goal_5, mean_goal_10, 
             goals_per_hour, 
             mean_goal_per_hour_5, mean_goal_per_hour_10, 
             assists, 
             mean_assists_5, mean_assists_10, 
             assists_per_hour, 
             mean_assists_per_hour_5, mean_assists_per_hour_10, 
             goals_conceded, 
             mean_goals_conceded_5, mean_goals_conceded_10, goals_conceded_per_hour, mean_goals_conceded_per_hour_5, mean_goals_conceded_per_hour_10) %>%
      mutate(element_type = case_when(element_type == 1 ~ "G",
                                      element_type == 2 ~ "D",
                                      element_type == 3 ~ "M",
                                      element_type == 4 ~ "F")
      ) %>%
      rename(position = element_type) %>%
      # add next fixture to the last row so we can use the data in score-df
      left_join(next_fixture, # problem when player change team, then next-fixture will be the new team next fixture
                by = c("team" = "team", "fixture" = "id")) %>%
      arrange(player_id, fixture)
    print("player")
    
    
      player_didnt_play <- player %>%
        filter(minutes == 0 | is.na(minutes)) %>%
        select(team, fixture, player_id, position) %>% 
        arrange(team, fixture, player_id)
      print("player_didnt_play")
    
      if(score_season){
        player_will_not_play <- player_info %>% 
          filter(chance_of_playing_this_round <= 25) %>%
          mutate(element_type2 = ifelse(element_type == 1, 1, 2)) %>%
          mutate(position = case_when(element_type == 1 ~ "G",
                                      element_type == 2 ~ "D",
                                      element_type == 3 ~ "M",
                                      element_type == 4 ~ "F")
          ) %>%
          left_join(teams_next_game %>% select(-match_day) %>% rename(fixture = id), by = "team") %>%
          rename(player_id = id) %>%
          select(team, fixture, player_id, position) %>%
          anti_join(train_games %>% distinct(id), by = c("fixture" = "id")) %>%
          group_by(team, player_id) %>% mutate(min_fixture = rollapply(fixture, 38, min, align = 'right', fill = NA, partial = TRUE)) %>% ungroup %>% filter(fixture == min_fixture) %>% select(-min_fixture) 
          print("player_will_not_play")
        
          }
      
      team_top <- player %>%
        filter(!is.na(team)) %>%
        group_by(team, fixture, next_fixture) %>%
        summarise(
          n = n(),
          max_value = max(ifelse(is.finite(value), value, NA), na.rm = TRUE),
          max_value_position_g = max(ifelse(position == "G", value, NA), na.rm = TRUE),
          max_value_position_d = max(ifelse(position == "D", value, NA), na.rm = TRUE),
          max_value_position_m = max(ifelse(position == "M", value, NA), na.rm = TRUE),
          max_value_position_f = max(ifelse(position == "F", value, NA), na.rm = TRUE),
          
          max_goal = max(total_goals, na.rm = TRUE),
          max_goal_position_m = max(ifelse(position == "M", total_goals, NA), na.rm = TRUE),
          max_goal_position_f = max(ifelse(position == "F", total_goals, NA), na.rm = TRUE),
          
          max_assists = max(total_assists, na.rm = TRUE),
          max_assists_position_d = max(ifelse(position == "D", total_assists, NA), na.rm = TRUE),
          max_assists_position_m = max(ifelse(position == "M", total_assists, NA), na.rm = TRUE),
          max_assists_position_f = max(ifelse(position == "F", total_assists, NA), na.rm = TRUE)
        ) %>% ungroup() %>%
        # filter out player that switch teams between rounds
        group_by(fixture) %>%  mutate(hierarchy = row_number(desc(n))) %>% ungroup() %>% filter(hierarchy <= 2) %>% select(-hierarchy) %>%
        arrange(team, fixture, next_fixture) 
      print("team_top")
      
      
      if(score_season){
        
        
        player_team_top <- player %>%
          left_join(team_top, by = c("team" = "team", "fixture" = "fixture", "next_fixture" = "next_fixture")) %>%
          mutate(
            most_valuable = case_when(value == max_value ~ 1, TRUE ~ 0),
            most_valuable_position_g = case_when(position == "G" & value > 0 & value == max_value_position_g ~ 1, TRUE ~ 0),
            most_valuable_position_d = case_when(position == "D" & value > 0 & value == max_value_position_d ~ 1, TRUE ~ 0),
            most_valuable_position_m = case_when(position == "M" & value > 0 & value == max_value_position_m ~ 1, TRUE ~ 0),
            most_valuable_position_f = case_when(position == "F" & value > 0 & value == max_value_position_f ~ 1, TRUE ~ 0),
            
            best_goal_scorer = case_when(total_goals == max_goal & total_goals > 0 ~ 1, TRUE ~ 0),
            best_goal_scorer_position_m = case_when(position == "M" & total_goals > 0 & total_goals == max_goal_position_m ~ 1, TRUE ~ 0),
            best_goal_scorer_position_f = case_when(position == "F" & total_goals > 0 & total_goals == max_goal_position_f ~ 1, TRUE ~ 0),
            
            most_assists = case_when(total_assists == max_assists & total_assists > 0 ~ 1, TRUE ~ 0),
            most_assists_position_d = case_when(position == "D" & total_assists > 0 & total_assists == max_assists_position_d ~ 1, TRUE ~ 0),
            most_assists_position_m = case_when(position == "M" & total_assists > 0 & total_assists == max_assists_position_m ~ 1, TRUE ~ 0),
            most_assists_position_f = case_when(position == "F" & total_assists > 0 & total_assists == max_assists_position_f ~ 1, TRUE ~ 0)
          )  %>%
          left_join(player_didnt_play %>% union(player_will_not_play) %>% select(-position) %>% mutate(no_play_temp = 1), by = c("team" = "team", "fixture" = "fixture", "player_id" = "player_id")) %>% 
          mutate(no_play = case_when(is.na(no_play_temp) ~ 0, TRUE ~ 1)) %>% select(-no_play_temp)
        
        
      }else{
          
        
        player_team_top <- player %>%
          left_join(team_top, by = c("team" = "team", "fixture" = "fixture", "next_fixture" = "next_fixture")) %>%
          mutate(
            most_valuable = case_when(value == max_value ~ 1, TRUE ~ 0),
            most_valuable_position_g = case_when(position == "G" & value > 0 & value == max_value_position_g ~ 1, TRUE ~ 0),
            most_valuable_position_d = case_when(position == "D" & value > 0 & value == max_value_position_d ~ 1, TRUE ~ 0),
            most_valuable_position_m = case_when(position == "M" & value > 0 & value == max_value_position_m ~ 1, TRUE ~ 0),
            most_valuable_position_f = case_when(position == "F" & value > 0 & value == max_value_position_f ~ 1, TRUE ~ 0),
            
            best_goal_scorer = case_when(total_goals == max_goal & total_goals > 0 ~ 1, TRUE ~ 0),
            best_goal_scorer_position_m = case_when(position == "M" & total_goals > 0 & total_goals == max_goal_position_m ~ 1, TRUE ~ 0),
            best_goal_scorer_position_f = case_when(position == "F" & total_goals > 0 & total_goals == max_goal_position_f ~ 1, TRUE ~ 0),
            
            most_assists = case_when(total_assists == max_assists & total_assists > 0 ~ 1, TRUE ~ 0),
            most_assists_position_d = case_when(position == "D" & total_assists > 0 & total_assists == max_assists_position_d ~ 1, TRUE ~ 0),
            most_assists_position_m = case_when(position == "M" & total_assists > 0 & total_assists == max_assists_position_m ~ 1, TRUE ~ 0),
            most_assists_position_f = case_when(position == "F" & total_assists > 0 & total_assists == max_assists_position_f ~ 1, TRUE ~ 0)
          )  %>%
          left_join(player_didnt_play %>% select(-position) %>% mutate(no_play_temp = 1), by = c("team" = "team", "fixture" = "fixture", "player_id" = "player_id")) %>% 
          mutate(no_play = case_when(is.na(no_play_temp) ~ 0, TRUE ~ 1)) %>% select(-no_play_temp)
        
        
      }
      print("player_team_top")
      
      info_available_player <- player %>% select(team, next_fixture, player_id, position) %>%
        inner_join(player_team_top %>% select(-team, -position, -fixture), by = c("player_id" = "player_id", "next_fixture" = "next_fixture")) %>%
        group_by(team, next_fixture) %>%
        summarise(
          max_value = max(ifelse(no_play == 0, value, 0), na.rm = TRUE),
          max_value_position_g = max(ifelse(position == "G" & no_play == 0, value, 0), na.rm = TRUE),
          max_value_position_d = max(ifelse(position == "D" & no_play == 0, value, 0), na.rm = TRUE),
          max_value_position_m = max(ifelse(position == "M" & no_play == 0, value, 0), na.rm = TRUE),
          max_value_position_f = max(ifelse(position == "F" & no_play == 0, value, 0), na.rm = TRUE),
          
          avg_goals_per_hour_5 = mean(ifelse(no_play == 0, mean_goal_per_hour_5, NA), na.rm = TRUE),
          avg_goals_per_hour_10 = mean(ifelse(no_play == 0, mean_goal_per_hour_10, NA), na.rm = TRUE),
          avg_assists_per_hour_5 = mean(ifelse(no_play == 0, mean_assists_per_hour_5, NA), na.rm = TRUE),
          avg_assists_per_hour_10 = mean(ifelse(no_play == 0, mean_assists_per_hour_10, NA), na.rm = TRUE),
          avg_goals_conceded_per_hour_5 = mean(ifelse(no_play == 0, mean_goals_conceded_per_hour_5, NA), na.rm = TRUE),
          avg_goals_conceded_per_hour_10 = mean(ifelse(no_play == 0, mean_goals_conceded_per_hour_10, NA), na.rm = TRUE),
          
          most_valuable_player_playing = max(ifelse(no_play == 0, most_valuable, NA), na.rm = TRUE),
          most_valuable_g_player_playing = max(ifelse(no_play == 0, most_valuable_position_g, 0), na.rm = TRUE),
          most_valuable_f_player_playing = max(ifelse(no_play == 0, most_valuable_position_d, 0), na.rm = TRUE),
          most_valuable_m_player_playing = max(ifelse(no_play == 0, most_valuable_position_m, 0), na.rm = TRUE),
          most_valuable_f_player_playing = max(ifelse(no_play == 0, most_valuable_position_f, 0), na.rm = TRUE),
          
          best_goal_scorer_playing = max(ifelse(no_play == 0, best_goal_scorer, NA), na.rm = TRUE),
          best_m_goal_scorer_playing = max(ifelse(no_play == 0, best_goal_scorer_position_m, 0), na.rm = TRUE),
          best_f_goal_scorer_playing = max(ifelse(no_play == 0, best_goal_scorer_position_f, 0), na.rm = TRUE),
          
          best_assists_player_playing = max(ifelse(no_play == 0, best_goal_scorer, NA), na.rm = TRUE),
          best_assists_d_player_playing = max(ifelse(no_play == 0, most_assists_position_d, 0), na.rm = TRUE),
          best_assists_m_player_playing = max(ifelse(no_play == 0, most_assists_position_m, 0), na.rm = TRUE),
          best_assists_f_player_playing = max(ifelse(no_play == 0, most_assists_position_f, 0), na.rm = TRUE)
          
        ) %>% 
        arrange(next_fixture, team)
      print("info_available_player")
      
      
      finished_games <- game_list %>%
        filter(!is.na(team_h_score))
      print("finished_games")
      
      
      home <- finished_games %>%
        mutate(
          win = ifelse(team_h_score > team_a_score, 1, 0),
          draw = ifelse(team_h_score == team_a_score, 1, 0),
          loss = ifelse(team_h_score < team_a_score, 1, 0),
          played_at_home = 1
        ) %>%
        rename(team = team_h) %>%
        rename(scored_goals = team_h_score) %>%
        rename(conceded_goals = team_a_score) %>%
        group_by(team) %>% mutate(field_hierarchy = row_number(kickoff)) %>%
        select(-GW, -home, -away, -team_a, -finished)
      print("home")
      
      
      away <- finished_games %>%
        mutate(
          win = ifelse(team_h_score < team_a_score, 1, 0),
          draw = ifelse(team_h_score == team_a_score, 1, 0),
          loss = ifelse(team_h_score > team_a_score, 1, 0),
          played_at_home = 0
        ) %>%
        rename(team = team_a) %>%
        rename(scored_goals = team_a_score) %>%
        rename(conceded_goals = team_h_score) %>%
        group_by(team) %>% mutate(field_hierarchy = row_number(kickoff)) %>%
        select(-GW, -home, -away, -team_h, -finished)
      print("away")
      
      
      union_a_h <- home %>% union(away) %>%
        group_by(team) %>% mutate(hierarchy = row_number(kickoff)) %>% select(-kickoff) %>%
        left_join(next_fixture, by = c("team" = "team", "id" = "id"))
      print("union_a_h")
      
      
      team_form <- union_a_h %>%
        arrange(team, hierarchy) %>%
        group_by(team) %>%
        mutate(
          # loss
          win_rate = rollapply(win, 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          win_rate_last_5 = rollapply(win, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          win_rate_h = rollapply(ifelse(played_at_home == 1, win, NA), 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          win_rate_h_last_5 = rollapply(ifelse(played_at_home == 1, win, NA), 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          win_rate_a = rollapply(ifelse(played_at_home != 1, win, NA), 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          win_rate_a_last_5 = rollapply(ifelse(played_at_home != 1, win, NA), 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          # draw
          draw_rate = rollapply(draw, 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          draw_rate_last_5 = rollapply(draw, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          draw_rate_h = rollapply(ifelse(played_at_home == 1, draw, NA), 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          draw_rate_h_last_5 = rollapply(ifelse(played_at_home == 1, draw, NA), 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          draw_rate_a = rollapply(ifelse(played_at_home != 1, draw, NA), 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          draw_rate_a_last_5 = rollapply(ifelse(played_at_home != 1, draw, NA), 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          # loss
          loss_rate = rollapply(loss, 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          loss_rate_last_5 = rollapply(loss, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          loss_rate_h = rollapply(ifelse(played_at_home == 1, loss, NA), 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          loss_rate_h_last_5 = rollapply(ifelse(played_at_home == 1, loss, NA), 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          loss_rate_a = rollapply(ifelse(played_at_home != 1, loss, NA), 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          loss_rate_a_last_5 = rollapply(ifelse(played_at_home != 1, loss, NA), 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          # scored goals
          avg_scored_goals = rollapply(scored_goals, 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          avg_scored_goals_last_5 = rollapply(scored_goals, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          avg_scored_goals_h = rollapply(ifelse(played_at_home == 1, scored_goals, NA), 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          avg_scored_goals_h_last_5 = rollapply(ifelse(played_at_home == 1, scored_goals, NA), 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          avg_scored_goals_a = rollapply(ifelse(played_at_home != 1, scored_goals, NA), 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          avg_scored_goals_a_last_5 = rollapply(ifelse(played_at_home != 1, scored_goals, NA), 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          # scored goals
          avg_conceded_goals = rollapply(conceded_goals, 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          avg_conceded_goals_last_5 = rollapply(conceded_goals, 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          avg_conceded_goals_h = rollapply(ifelse(played_at_home == 1, conceded_goals, NA), 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          avg_conceded_goals_h_last_5 = rollapply(ifelse(played_at_home == 1, conceded_goals, NA), 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          
          avg_conceded_goals_a = rollapply(ifelse(played_at_home != 1, conceded_goals, NA), 38, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE),
          avg_conceded_goals_a_last_5 = rollapply(ifelse(played_at_home != 1, conceded_goals, NA), 5, mean, na.rm = TRUE, align = 'right', fill = NA, partial = TRUE)
        ) %>%
        arrange(team, hierarchy) 
      print("team_form")
      
      
      if(score_season){
        df <- train_games %>% 
          filter(id > 10) %>%
          # add next fixture so I don't need to do the same thing for both train- and score-df
          union(score_games %>% inner_join(teams_next_game %>% as.data.frame %>% select(id) %>% distinct, by = c("id" = "id"))) %>% # can be more than 10 games since they donät finish the rounds at the same time, but it doesn't matter here.
          left_join(team_form %>%
                      select(
                        -played_at_home,
                        -id,
                        -field_hierarchy,
                        -hierarchy,
                        -win_rate_a,
                        -win_rate_a_last_5,
                        -loss_rate_a,
                        -loss_rate_a_last_5,
                        -draw_rate_a,
                        -draw_rate_a_last_5,
                        -draw_rate_a,
                        -draw_rate_a_last_5,
                        -avg_scored_goals_a,
                        -avg_scored_goals_a_last_5,
                        -avg_conceded_goals_a,
                        -avg_conceded_goals_a_last_5
                      ) %>% rename_with(.fn = ~ paste0(.x, "_team_h"))
                    , by = c("team_h" = "team_team_h", "id" = "next_fixture_team_h")) %>%
          
          left_join(team_form %>%
                      select(
                        -played_at_home,
                        -id,
                        -field_hierarchy,
                        -hierarchy,
                        -win_rate_h,
                        -win_rate_h_last_5,
                        -loss_rate_h,
                        -loss_rate_h_last_5,
                        -draw_rate_h,
                        -draw_rate_h_last_5,
                        -draw_rate_h,
                        -draw_rate_h_last_5,
                        -avg_scored_goals_h,
                        -avg_scored_goals_h_last_5,
                        -avg_conceded_goals_h,
                        -avg_conceded_goals_h_last_5
                      ) %>% rename_with(.fn = ~ paste0(.x, "_team_a"))
                    , by = c("team_a" = "team_team_a", "id" = "next_fixture_team_a")) %>%
          inner_join(info_available_player %>% rename_with(.fn = ~ paste0(.x, "_player_h")), by = c("team_h" = "team_player_h", "id" = "next_fixture_player_h")) %>%
          inner_join(info_available_player %>% rename_with(.fn = ~ paste0(.x, "_player_a")), by = c("team_a" = "team_player_a", "id" = "next_fixture_player_a")) %>%
        left_join(team_id %>% rename_with(.fn = ~ paste0(.x, "_home")), by = c("team_h" = "team_home")) %>%
          left_join(team_id %>% rename_with(.fn = ~ paste0(.x, "_away")), by = c("team_a" = "team_away")) 
        print("df")
        
        list_df[[i]] <- df %>% mutate(season = paste("20", season, "/20", season+1, sep = ""))
        
      }else{
        
        df <- train_games %>% 
          filter(id > 10) %>%
          # add next fixture so I don't need to do the same thing for both train- and score-df
          #union(score_games %>% inner_join(teams_next_game %>% as.data.frame %>% select(id) %>% distinct, by = c("id" = "id"))) %>% # can be more than 10 games since they donät finish the rounds at the same time, but it doesn't matter here.
          left_join(team_form %>%
                      select(
                        -played_at_home,
                        -id,
                        -field_hierarchy,
                        -hierarchy,
                        -win_rate_a,
                        -win_rate_a_last_5,
                        -loss_rate_a,
                        -loss_rate_a_last_5,
                        -draw_rate_a,
                        -draw_rate_a_last_5,
                        -draw_rate_a,
                        -draw_rate_a_last_5,
                        -avg_scored_goals_a,
                        -avg_scored_goals_a_last_5,
                        -avg_conceded_goals_a,
                        -avg_conceded_goals_a_last_5
                      ) %>% rename_with(.fn = ~ paste0(.x, "_team_h"))
                    , by = c("team_h" = "team_team_h", "id" = "next_fixture_team_h")) %>%
          
          left_join(team_form %>%
                      select(
                        -played_at_home,
                        -id,
                        -field_hierarchy,
                        -hierarchy,
                        -win_rate_h,
                        -win_rate_h_last_5,
                        -loss_rate_h,
                        -loss_rate_h_last_5,
                        -draw_rate_h,
                        -draw_rate_h_last_5,
                        -draw_rate_h,
                        -draw_rate_h_last_5,
                        -avg_scored_goals_h,
                        -avg_scored_goals_h_last_5,
                        -avg_conceded_goals_h,
                        -avg_conceded_goals_h_last_5
                      ) %>% rename_with(.fn = ~ paste0(.x, "_team_a"))
                    , by = c("team_a" = "team_team_a", "id" = "next_fixture_team_a")) %>%
          inner_join(info_available_player %>% rename_with(.fn = ~ paste0(.x, "_player_h")), by = c("team_h" = "team_player_h", "id" = "next_fixture_player_h")) %>%
          inner_join(info_available_player %>% rename_with(.fn = ~ paste0(.x, "_player_a")), by = c("team_a" = "team_player_a", "id" = "next_fixture_player_a")) %>%
          left_join(team_id %>% rename_with(.fn = ~ paste0(.x, "_home")), by = c("team_h" = "team_home")) %>%
          left_join(team_id %>% rename_with(.fn = ~ paste0(.x, "_away")), by = c("team_a" = "team_away")) 
        print("df")
        
        list_df[[i]] <- df %>% mutate(season = paste("20", season, "/20", season+1, sep = ""))
        
      }
      print("list_df")
      
  }
  
  return(list_df)
}

df_list <- create_train_and_score_df()

df <- do.call("rbind", df_list)

write.csv2(df, file = "train_score.csv")


