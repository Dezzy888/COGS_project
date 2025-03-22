setwd('C:\\Users\\danie\\Documents\\Joshi Lab Materials\\3 Studies Dataset\\Dataset Merge')

merg_df <- read.csv('bgc_only_060723.csv')

# US only
us_only <- which(merg_df$cLocationInstitution != 'Mexico' &
                 merg_df$cLocationInstitution != 'Sobell' )
merg_us <- merg_df[us_only, ]

levels(factor(merg_us$cLocationCity))
levels(factor(merg_us$cLocationCounty))
levels(factor(merg_us$cLocationInstitution))

# Begin Totals

# Finding raw counts for Gender, Race, Ethnicity compiled among both DS
gender_df <- as.data.frame(plyr::count(merg_us, 'cGender'))
race_df <- as.data.frame(plyr::count(merg_us, 'cRace'))
eth_df <- as.data.frame(plyr::count(merg_us, 'cHispanicorLatino'))
diag_df <- as.data.frame(plyr::count(merg_us, 'cDiagnosis'))

# putting raw counts into a list
total_cts_ls <- list(gender_df, race_df, eth_df, diag_df)

# turning counts into proportions across the whole 
total_cts_props <- lapply(total_cts_ls, function(df) {
  df[,2] <- df[,2] / nrow(merg_us)
  colnames(df)[2] <- 'Proportion'
  df$cStudy <- rep('Total', nrow(df))
  return (df)
})

# storage into variables for easy access
props_gen <- total_cts_props[[1]]
props_race <- total_cts_props[[2]]
props_eth <- total_cts_props[[3]]
props_diag <- total_cts_props[[4]]

study_counts <- as.data.frame(merg_us %>% 
  dplyr::group_by(cStudy) %>%  
  dplyr::summarise(total_count=n(), .groups = 'drop'))
#View(study_counts)

# write.csv(merg_us, file = 'ds_merg_us_only.csv', row.names = FALSE)
write.csv(merg_us, file = 'ds_merg_us_only_060723.csv', row.names = FALSE)
       