# Genders
by_gender_diagnosis3 <- df_LA %>% 
  dplyr::group_by(cGender, cDiagnosis3) %>%  
  dplyr::summarise(total_count=n(), .groups = 'drop')

View(by_gender_diagnosis3)

# Exclude blank diagnoses
by_gender_diagnosis3 <- by_gender_diagnosis3[by_gender_diagnosis3$cDiagnosis3 != '', ]

# Relative Risks male vs female

gender_df <- as.data.frame(plyr::count(df_LA, 'cGender'))

# CS
cs <- by_gender_diagnosis3[by_gender_diagnosis3$cDiagnosis3 == 'CS', ]
cs$incidence <- cs$total_count / gender_df$freq
cs$rr <- rr_fn(cs$incidence)
View(cs)

# MDD
mdd <- by_gender_diagnosis3[by_gender_diagnosis3$cDiagnosis3 == 'MDD', ]
mdd$incidence <- mdd$total_count / gender_df$freq
mdd$rr <- rr_fn(mdd$incidence)
View(mdd)

# SZSAFD
szsafd <- by_gender_diagnosis3[by_gender_diagnosis3$cDiagnosis3 == 'SZSAFD', ]
szsafd$incidence <- szsafd$total_count / gender_df$freq
szsafd$rr <- rr_fn(szsafd$incidence)
View(szsafd)

# BAD1
bad1 <- by_gender_diagnosis3[by_gender_diagnosis3$cDiagnosis3 == 'BAD1', ]
bad1$incidence <- bad1$total_count / gender_df$freq
bad1$rr <- rr_fn(bad1$incidence)
View(bad1)

# BAD2
bad2 <- by_gender_diagnosis3[by_gender_diagnosis3$cDiagnosis3 == 'BAD2', ]
bad2$incidence <- bad2$total_count / gender_df$freq
bad2$rr <- rr_fn(bad2$incidence)
View(bad2)

# Collection of Results

rr_gender_total <- rbind(cs, mdd, szsafd, bad1, bad2)
View(rr_gender_total)
write.csv(rr_gender_total, file = 'rr_genderscdiag3_LA.csv')

