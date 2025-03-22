# plotting total race proportions
study_colors <- c('#F8766D', '#529EFF', 'darkgreen', 'gold1')
study_colors_t <- c('#F8766D', '#529EFF', 'darkgreen', 'gold1', '#808080')

race <- ggplot(data = props_race, aes(x = cRace, y = Proportion, fill = cRace)) +
  geom_bar(stat = "identity") +
  geom_col(position = position_dodge(0.7)) +
  ggtitle('Total Race Proportions') + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#race

# Race within Studies

by_study_race <- merg_us %>% 
  dplyr::group_by(cStudy, cRace) %>%  
  dplyr::summarise(total_count=n(), .groups = 'drop')

rel_freqs_race <- rep(0, nrow(by_study_race))

# generating relative frequencies from counts
for (i in 1:nrow(study_counts)) {
  ind_to_divide <- which(by_study_race[, 1] == study_counts[i, 1])
  replacement_vec <- unname(unlist(by_study_race[ind_to_divide, 3] / study_counts[i,2]))
  for (idx in 1:length(ind_to_divide)) {
    rel_freqs_race[ind_to_divide[idx]] <- replacement_vec[idx]
  }
}

# adding new column
by_study_race$Proportion<- rel_freqs_race

race_within <- ggplot(data = by_study_race, aes(x = cRace, y = Proportion, fill = cStudy)) +
  geom_bar(position = position_dodge(), 
           stat = 'identity') +
  scale_fill_manual(values = study_colors) +
  ggtitle('Proportions of Race within Studies') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#race_within

# Total race
race_df_ext <- cbind(props_race$cStudy, race_df, props_race$Proportion)

# making all column names the same before binding
colnames(race_df_ext) <- colnames(by_study_race)

bsr_tot <- rbind(by_study_race, race_df_ext)

race_tots <- ggplot(data = bsr_tot, aes(x = cRace, y = Proportion, fill = cStudy)) +
  geom_bar(position = position_dodge(), 
           stat = 'identity') +
  scale_fill_manual(values = study_colors_t) +
  ggtitle('Proportions of Race') + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#race_tots

# Plot Collection
race
race_within
race_tots

# Data Frame Collection
# View(props_race)
# View(by_study_race)
# View(bsr_tot)

