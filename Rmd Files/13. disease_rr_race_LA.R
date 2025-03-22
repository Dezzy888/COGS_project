df <- read.csv('bgc_cDiag123_cities_std.csv')

add_nr <- function(races1, races2, df_to_edit, disease) {
  not_represented <- c()
  
  for (race in races2) {
    if (!is.element(race, races1))
      not_represented <- c(not_represented, race)
    else
      next
  }
  
  for (race in not_represented) {
    df_to_edit <- rbind(df_to_edit, data.frame(cRace = race, 
                                               cDiagnosis3 = disease,
                                               total_count = 0))
  }
  
  df_to_edit <- df_to_edit[order(df_to_edit$cRace),]
  return (df_to_edit)
}

# custom relative risk function
rr_fn <- function(vec) {
  rel_risk <- NA
  if (min(vec) != 0) {
    rel_risk <- vec / min(vec)
  }
  else {
    v <- sort(vec)
    rel_risk <- vec / v[min(which(v != 0))]
  }
  return (rel_risk)
}

df_baltimore <- df[df$cLocationCity == 'Baltimore', ]
# NOTE: baltimore did not have all the races or diseases
# Proceeded using Los Angeles instead

df_LA <- df[df$cLocationCity == 'Los Angeles', ]

by_race_diagnosis2 <- df_LA %>% 
  dplyr::group_by(cRace, cDiagnosis2) %>%  
  dplyr::summarise(total_count=n(), .groups = 'drop')

by_race_diagnosis3 <- df_LA %>% 
  dplyr::group_by(cRace, cDiagnosis3) %>%  
  dplyr::summarise(total_count=n(), .groups = 'drop')


# Exclude other or unknown
by_race_diagnosis3 <- by_race_diagnosis3[by_race_diagnosis3$cRace != 'OT' &
                                         by_race_diagnosis3$cRace != 'OT/UNK' &
                                         by_race_diagnosis3$cRace != 'UNK' &
                                         by_race_diagnosis3$cDiagnosis3 != '', ]

# Relative Risks for all conditions

# Total Race Counts
race_df <- as.data.frame(plyr::count(df_LA, 'cRace'))
race_df <- race_df[1:6,] #exclude unknown or other

# CS
cs <- by_race_diagnosis3[by_race_diagnosis3$cDiagnosis3 == 'CS', ]
cs$incidence <- cs$total_count / race_df$freq
cs$rr <- rr_fn(cs$incidence)
View(cs)

# MDD
mdd <- by_race_diagnosis3[by_race_diagnosis3$cDiagnosis3 == 'MDD', ]
mdd <- add_nr(mdd$cRace, cs$cRace, mdd, 'MDD')
mdd$incidence <- mdd$total_count / race_df$freq
mdd$rr <- rr_fn(mdd$incidence)
View(mdd)

# SZSAFD
szsafd <- by_race_diagnosis3[by_race_diagnosis3$cDiagnosis3 == 'SZSAFD', ]
szsafd <- add_nr(szsafd$cRace, cs$cRace, szsafd, 'SZSAFD')
szsafd$incidence <- szsafd$total_count / race_df$freq
szsafd$rr <- rr_fn(szsafd$incidence)
View(szsafd)

# BAD1
bad1 <- by_race_diagnosis3[by_race_diagnosis3$cDiagnosis3 == 'BAD1', ]
bad1 <- add_nr(bad1$cRace, cs$cRace, bad1, 'BAD1')
bad1$incidence <- bad1$total_count / race_df$freq
bad1$rr <- rr_fn(bad1$incidence)
View(bad1)

# BAD2
bad2 <- by_race_diagnosis3[by_race_diagnosis3$cDiagnosis3 == 'BAD2', ]
bad2 <- add_nr(bad2$cRace, cs$cRace, bad2, 'BAD2')
bad2$incidence <- bad2$total_count / race_df$freq
bad2$rr <- rr_fn(bad2$incidence)
View(bad2)

# Collection of Results

rr_race_total <- rbind(cs, mdd, szsafd, bad1, bad2)
View(rr_race_total)
write.csv(rr_race_total, file = 'rr_racescdiag3_LA.csv')


