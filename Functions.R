library(dplyr)

load("players_database.RData")

calculate_teams = function(players_data) {
  # Calculates one possible team split from players data
  # Returns teams and statistics about team split
  n = nrow(players_data)/2
  
  ind = sample(1:(2*n), n)
  team_1 = players_data[ind,]
  team_2 = players_data[-ind,]
  
  stat = data.frame(
    team_1_skill = sum(team_1$skill),
    team_2_skill = sum(team_2$skill),
    skill_diff = sum(team_1$skill) - sum(team_2$skill),
    team_1_var = var(team_1$skill),
    team_2_var = var(team_2$skill),
    var_diff = var(team_1$skill) - var(team_2$skill)
  )
  return(list(team_1 = team_1, team_2 = team_2, stat = stat))
}



simulate_teams = function(players_data, n_sim = 1000, printaj = FALSE) {
  # Simulates teams n_sim times and based on statistics return best team split
  out_stat = data.frame()
  out_team_1 = list()
  out_team_2 = list()
  i = 1
  
  for(i in 1:n_sim) {
    out = calculate_teams(players_data)
    
    out_team_1[[i]] = out$team_1
    out_team_2[[i]] = out$team_2
    
    out_stat = bind_rows(out_stat, out$stat)
  }
  
  out_stat$skill_diff = abs(out_stat$skill_diff)
  out_stat$var_diff = abs(out_stat$var_diff)
  out_stat$index = 1:nrow(out_stat)
  
  out_stat = out_stat %>% 
    arrange(skill_diff, var_diff)
  
  best_team_index = out_stat$index[[1]]
  
  if(printaj) {
    print(out_team_1[[best_team_index]] %>% 
            arrange(desc(skill)))
    
    print(out_team_2[[best_team_index]] %>% 
            arrange(desc(skill)))
    print(out_stat[1,])  
  }
  
  return(list(team_1 = out_team_1[[best_team_index]] %>% 
                arrange(desc(skill)),
              team_2 = out_team_2[[best_team_index]] %>% 
                arrange(desc(skill)),
              stat = out_stat[1,]))
}



players_names_to_list = function(players_database) {
  # For check box input
  out = list()
  for(i in 1:nrow(players_database))
    out[[players_database$name[[i]]]] = players_database$name[[i]]
  
  return(out)
}




