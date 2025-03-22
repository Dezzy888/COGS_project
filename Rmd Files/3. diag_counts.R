# Begin Diagnosis counts
library(dplyr)
library(plyr)

merg_us <- read.csv('ds_merg_us_only.csv')

study_counts <- as.data.frame(merg_us %>% 
                                dplyr::group_by(cStudy) %>%  
                                dplyr::summarise(total_count=n(), .groups = 'drop'))

diag_df <- as.data.frame(plyr::count(merg_us, 'cDiagnosis'))

# Group by study and diagnosis to get total counts
by_study_diagnosis <- merg_us %>% 
  dplyr::group_by(cStudy, cDiagnosis) %>%  
  dplyr::summarise(total_count=n(), .groups = 'drop')

rel_freqs <- rep(0, nrow(by_study_diagnosis))
rel_freqs_diag <- rel_freqs


# generating relative frequencies from counts
for (i in 1:nrow(study_counts)) {
  ind_to_divide <- which(by_study_diagnosis[, 1] == study_counts[i, 1])
  replacement_vec <- unname(unlist(by_study_diagnosis[ind_to_divide, 3] / study_counts[i,2]))
  for (idx in 1:length(ind_to_divide)) {
    rel_freqs_diag[ind_to_divide[idx]] <- replacement_vec[idx]
  }
}

# adding a new column
by_study_diagnosis$Proportion<- rel_freqs_diag
View(by_study_diagnosis)

# total diagnosis
diag_df_ext <- cbind(props_diag$cStudy, diag_df, props_diag$Proportion)

# making all column names the same before binding
colnames(diag_df_ext) <- colnames(by_study_diagnosis)

bsd_tot <- rbind(by_study_diagnosis, diag_df_ext)

# Data Frame Collection
View(diag_df)
View(props_diag)
View(by_study_diagnosis)
View(bsd_tot)

write.csv(bsd_tot, file = 'total_diagnosis_counts_2.csv')

# SAVE PLOTTING FOR LATER

# disease <- ggplot(data = props_disease, aes(x = cDiagnosis, y = Proportion, fill = cDiagnosis)) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values = gen_colors) +
#   geom_text(aes(label= round(Proportion, 4), vjust=-0.3)) +
#   ggtitle('Total Diagnosis Proportions') + 
#   theme_minimal()+
#   theme(plot.title = element_text(hjust = 0.5))




