setwd('C:\\Users\\danie\\Documents\\Joshi Lab Materials\\3 Studies Dataset\\Dataset Merge')
library(readxl)

df <- read.csv('bgc_merge_cDiag123_new.csv')
View(head(df))

df_baltimore <- df[df$cLocationCity == 'Baltimore', ]

race_df <- as.data.frame(plyr::count(df_baltimore, 'cRace'))

df_check_2010 <- read_xlsx('reproj_30_2010.xlsx')
df_check_2020 <- read_xlsx('reproj_30_2020.xlsx')

baltimore_2010 <- df_check_2010[4,]
baltimore_2020 <- df_check_2020[4,]
baltimore <- rbind(baltimore_2010, baltimore_2020)
baltimore <- baltimore[,-1]

baltimore_avg <- as.data.frame(sapply(baltimore, mean, margin = 2))
baltimore_avg$cRace <- rownames(baltimore_avg)
rownames(baltimore_avg) <- NULL
freq <- baltimore_avg[,1]
baltimore_avg[,1] <- baltimore_avg[,2]
baltimore_avg[,2] <- freq

colnames(baltimore_avg) <- c('cRace', 'freq')

baltimore_avg_races <- baltimore_avg[1:7,]
baltimore_avg_races <- baltimore_avg_races[order(baltimore_avg_races$cRace), ]
View(baltimore_avg_races)
View(race_df)

baltimore_races <- levels(factor(baltimore_avg_races$cRace))

not_represented <- c()

for (race in baltimore_races) {
  if (!is.element(race, race_df$cRace))
    not_represented <- c(not_represented, race)
  else
    next
}

for (race in not_represented) {
  race_df <- rbind(race_df, data.frame(cRace = race, freq = 0))
}

race_df <- race_df[order(race_df$cRace),]

exp_props <- baltimore_avg_races$freq / sum(baltimore_avg_races$freq)
exp_cts <- round(sum(race_df$freq) * exp_props, 0)

race_df$exp_freq <- exp_cts

View(race_df)

res <- chisq.test(race_df$freq, p = exp_props)
res$expected
# Result: expected values are too small to use chi-square; requires n > 5 for all categories

# Alternate approach: resampling
# nrow(race_df): Bonferroni correction: 7 tests, one for each race
# use p-value of 0.05/7 = 0.007142857

null_dist <- rep(baltimore_avg_races$cRace, round(baltimore_avg_races$freq,0))
results <- matrix(0,7,1000)

for (i in 1:1000) {
  sam <- sample(null_dist, sum(race_df$freq), replace = F)
  race_sam <- as.data.frame(plyr::count(sam))
  
  not_represented <- c()
  
  for (race in baltimore_races) {
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

sigs <- rep(NA, 7)
lower_bounds <- c()
upper_bounds <- c()
# two-tailed test with p-value = 0.05/7 = 0.007
# generate a 99.2% CI
# see if the observed value lies outside of this interval
for (i in 1:nrow(race_df)) {
  row_res <- sort(results[i, ])
  lower_bound <- row_res[3]
  lower_bounds <- c(lower_bounds, lower_bound)
  upper_bound <- row_res[997]
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
# write.csv(race_df, file = 'balt_race_resam_hypothesis_test.csv')

# RESULT: undersampled AA and oversampled CA
## everything else was NS