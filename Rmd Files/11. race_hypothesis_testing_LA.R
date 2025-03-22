setwd('C:\\Users\\danie\\Documents\\Joshi Lab Materials\\3 Studies Dataset\\Dataset Merge')
library(readxl)

df <- read.csv('bgc_merge_cDiag123_new.csv')
View(head(df))

df_LA <- df[df$cLocationCity == 'Los Angeles', ]

race_df <- as.data.frame(plyr::count(df_LA, 'cRace'))

# Omit unknown or other
race_df <- race_df[1:6, ]

df_check_2010 <- read_xlsx('reproj_30_2010.xlsx')
df_check_2020 <- read_xlsx('reproj_30_2020.xlsx')

LA_2010 <- df_check_2010[which(df_check_2010$City == 'Los Angeles, CA'),]
LA_2020 <- df_check_2020[which(df_check_2020$City == 'Los Angeles, CA'),]
LA <- rbind(LA_2010, LA_2020)
LA <- LA[,-1]

LA_avg <- as.data.frame(sapply(LA, mean, margin = 2))
LA_avg$cRace <- rownames(LA_avg)
rownames(LA_avg) <- NULL
freq <- LA_avg[,1]
LA_avg[,1] <- LA_avg[,2]
LA_avg[,2] <- freq

colnames(LA_avg) <- c('cRace', 'freq')

LA_avg_races <- LA_avg[1:7,]

# Standardizing race acronyms
LA_avg_races$cRace[LA_avg_races$cRace == 'NA'] <- 'AE'
LA_avg_races$cRace[LA_avg_races$cRace == 'PI'] <- 'NH'
LA_avg_races <- LA_avg_races[order(LA_avg_races$cRace), ]
rownames(LA_avg_races) <- NULL
LA_avg_races <- LA_avg_races[1:6, ]

View(LA_avg_races)
View(race_df)

LA_races <- levels(factor(LA_avg_races$cRace))

not_represented <- c()

for (race in LA_races) {
  if (!is.element(race, race_df$cRace))
    not_represented <- c(not_represented, race)
  else
    next
}

for (race in not_represented) {
  race_df <- rbind(race_df, data.frame(cRace = race, freq = 0))
}

race_df <- race_df[order(race_df$cRace),]

exp_props <- LA_avg_races$freq / sum(LA_avg_races$freq)
exp_cts <- round(sum(race_df$freq) * exp_props, 0)

race_df$exp_freq <- exp_cts

View(race_df)

res <- chisq.test(race_df$freq, p = exp_props)
res$expected # checking if expecting values are large enough to use chi
res
# Result: significant difference between expected and observed

# Alternate approach: resampling
# nrow(race_df): Bonferroni correction: 6 tests, one for each race
# use p-value of 0.05/6

null_dist <- rep(LA_avg_races$cRace, round(LA_avg_races$freq,0))
results <- matrix(0,6,1000)



for (i in 1:1000) {
  sam <- sample(null_dist, sum(race_df$freq), replace = F)
  race_sam <- as.data.frame(plyr::count(sam))
  
  not_represented <- c()
  
  for (race in LA_races) {
    if (!is.element(race, race_sam$x))
      not_represented <- c(not_represented, race)
    else
      next
  }
  
  for (race in not_represented) {
    race_sam <- rbind(race_sam, data.frame(x = race, freq = 0))
  }
  
  race_sam <- race_sam[order(race_sam$x),]
  rownames(race_sam) <- race_sam$x
  race_sam$x <- NULL
  
  results[,i] <- race_sam$freq
}

sigs <- rep(NA, 6)
# two-tailed test with p-value = 0.05/6 = 0.008
# generate a 99.2% CI
# see if the observed value lies outside of this interval
lower_bounds <- c()
upper_bounds <- c()

for (i in 1:nrow(race_df)) {
  row_res <- sort(results[i, ])
  lower_bound <- row_res[4]
  lower_bounds <- c(lower_bounds, lower_bound)
  upper_bound <- row_res[996]
  upper_bounds <- c(upper_bounds, upper_bound)
  
  
  if (race_df$freq[i] > upper_bound || race_df$freq[i] < lower_bound) {
    sigs[i] <- T
  }
  else {
    sigs[i] <- F
  }
  
}

race_df$lower <- lower_bounds
race_df$upper <- upper_bounds
race_df$Significance <- sigs

# displays whether or not the test found a significant difference
# between the observed value and the null distribution
View(race_df)
# write.csv(race_df, file = 'LA_race_resam_hypothesis_test.csv')

# RESULT: All tests returned significantly different



