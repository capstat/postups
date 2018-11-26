#historical pyt thm win pct

all = read.csv(paste0("https://raw.githubusercontent.com/",
                      "capstat/postups/master/nba538/data/",
                      "nba_1983to2017.csv"), stringsAsFactors = F)

get_season = function(yr){
  nba = all[all$Season==yr,]
  nba_pyt = nba %>%
    select("G", "Date", "Home", "Team", "Opponent", 
           "Result", "Team_Points", "Opp_Points") %>%
    group_by(Team, Home) %>%
    mutate(Home_Away_G = cumsum(Result==1 | Result==0),
           Home_Away_Points = cumsum(Team_Points),
           Home_Away_Opp_Points = cumsum(Opp_Points),
           Home_Away_Pyt = Home_Away_Points^16.5/
             (Home_Away_Points^16.5 + Home_Away_Opp_Points^16.5))
  home = filter(nba_pyt, Home==1)
  home$Home_Away_Pyt = lag(home$Home_Away_Pyt)
  home$Home_Away_Pyt[home$Home_Away_G == 1] = NA
  away = filter(nba_pyt, Home==0)
  away$Home_Away_Pyt = lag(away$Home_Away_Pyt)
  away$Home_Away_Pyt[away$Home_Away_G == 1] = NA
  final1617 = home %>% 
    left_join(away[,c("Team", "Date", "G", "Home_Away_G", "Home_Away_Pyt")], 
              by = c("Opponent"="Team", "Date"),
              suffix=c("_HOMETEAM", "_OPP")) %>%
    mutate(Pyt_Diff = Home_Away_Pyt_HOMETEAM - Home_Away_Pyt_OPP)
  
  check = data.frame()
  diffs = seq(-1, 1, 0.001)
  for(i in c(1:length(diffs))){
    temp = final1617 %>%
      mutate(
        pred=ifelse(Pyt_Diff > diffs[i], "WIN", "LOSE"),
        right=ifelse((pred=="WIN" & Result==1) | (pred=="LOSE" & Result==0),
                     1, 0)) %>%
      filter(!is.na(Home_Away_Pyt_HOMETEAM),
             !is.na(Home_Away_Pyt_OPP)) %>%
      group_by() %>%
      summarize(thr=diffs[i], n=n(), n_right=sum(right), prop=n_right/n)
    check = rbind(check, temp)
  }
  check$Season = yr
  return(check)
}

seasons = unique(all$Season)
all_check = data.frame()
for(each in seasons){
  if(each=="2017-2018") next
  temp = get_season(each)
  all_check = rbind(all_check, temp)
  write.csv(all_check, "data/pyt_thm_hist.csv", row.names=F)
  print(each)
}

