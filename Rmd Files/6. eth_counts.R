# plotting total ethnicity proportions
eth_colors <- c('gray', '#00ABFD', '#F8766D', '#00BC59')
eth <- ggplot(data = props_eth, aes(x = cHispanicorLatino, y = Proportion, fill = cHispanicorLatino)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = eth_colors) + 
  geom_text(aes(label= round(Proportion, 4), vjust=-0.3)) + 
  ggtitle('Total Ethnicity Proportions') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
eth

# Race within Studies

by_study_eth <- merg_us %>% 
  dplyr::group_by(cStudy, cHispanicorLatino) %>%  
  dplyr::summarise(total_count=n(), .groups = 'drop')

rel_freqs_eth <- rep(0,nrow(by_study_eth))

# generating relative frequencies from counts
for (i in 1:nrow(study_counts)) {
  ind_to_divide <- which(by_study_eth[, 1] == study_counts[i, 1])
  replacement_vec <- unname(unlist(by_study_eth[ind_to_divide, 3] / study_counts[i,2]))
  for (idx in 1:length(ind_to_divide)) {
    rel_freqs_eth[ind_to_divide[idx]] <- replacement_vec[idx]
  }
}

# adding new column
by_study_eth$Proportion<- rel_freqs_eth

eth_within <- ggplot(data = by_study_eth, aes(x = cHispanicorLatino, y = Proportion, fill = cStudy)) +
  geom_bar(position = position_dodge(), 
           stat = 'identity') +
  scale_fill_manual(values = study_colors) +
  ggtitle('Proportions of Ethnicity within Studies') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

eth_within

# Total genders
eth_df_ext <- cbind(props_eth$cStudy, eth_df, props_eth$Proportion)

# making all column names the same before binding
colnames(eth_df_ext) <- colnames(by_study_eth)

bse_tot <- rbind(by_study_eth, eth_df_ext)

eth_tots <- ggplot(data = bse_tot, aes(x = cHispanicorLatino, y = Proportion, fill = cStudy)) +
  geom_bar(position = position_dodge(), 
           stat = 'identity') +
  scale_fill_manual(values = study_colors_t) +
  ggtitle('Proportions of Ethnicity') + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#eth_tots

# Plot Collection
eth
eth_within
eth_tots

# Data Frame Collection
View(props_eth)
View(by_study_eth)
View(bse_tot)


