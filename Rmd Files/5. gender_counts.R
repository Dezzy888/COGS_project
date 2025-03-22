# plotting total gender proportions
#View(props_gen)
gen_colors <- c('red', 'pink', 'green', 'blue', 'orange')
gender <- ggplot(data = props_gen, aes(x = cGender, y = Proportion, fill = cGender)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = gen_colors) +
  geom_text(aes(label= round(Proportion, 4), vjust=-0.3)) +
  ggtitle('Total Gender Proportions') + 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
#gender

# Group by study and gender to get total counts
by_study_gender <- merg_us %>% 
  dplyr::group_by(cStudy, cGender) %>%  
  dplyr::summarise(total_count=n(), .groups = 'drop')

rel_freqs <- rep(0, nrow(by_study_gender))
rel_freqs_gen <- rel_freqs

# generating relative frequencies from counts
for (i in 1:nrow(study_counts)) {
  ind_to_divide <- which(by_study_gender[, 1] == study_counts[i, 1])
  replacement_vec <- unname(unlist(by_study_gender[ind_to_divide, 3] / study_counts[i,2]))
  for (idx in 1:length(ind_to_divide)) {
    rel_freqs_gen[ind_to_divide[idx]] <- replacement_vec[idx]
  }
}

# adding new column
by_study_gender$Proportion<- rel_freqs_gen
#View(by_study_gender)

# Genders within Studies

gen_within <- ggplot(data = by_study_gender, aes(x = cGender, y = Proportion, fill = cStudy)) +
  geom_bar(position = position_dodge(), 
           stat = 'identity') +
  scale_fill_manual(values = study_colors) +
  ggtitle('Proportions of Genders within Studies') + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#gen_within

# Total genders
gen_df_ext <- cbind(props_gen$cStudy, gender_df, props_gen$Proportion)

# making all column names the same before binding
colnames(gen_df_ext) <- colnames(by_study_gender)

bsg_tot <- rbind(by_study_gender, gen_df_ext)

gen_tots <- ggplot(data = bsg_tot, aes(x = cGender, y = Proportion, fill = cStudy)) +
  geom_bar(position = position_dodge(), 
           stat = 'identity') +
  scale_fill_manual(values = study_colors_t) +
  ggtitle('Proportions of Genders') + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#gen_tots

# Plot Collection
gender
gen_within
gen_tots

# Data Frame Collection
# View(props_gen)
# View(by_study_gender)
# View(bsg_tot)
