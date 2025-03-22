
setwd('C:/Users/danie/Documents/Joshi Lab Materials/3 Studies Dataset/Dataset Merge')


# helper function for transforming by a dictionary
dict_transform <- function(keys, values, vec) {
  for (i in 1:length(keys)) {
    vec[vec == keys[i]] <- values[i]
  }
  return (vec)
}

rlvnt_diag <- read.csv("C:/Users/danie/Documents/Joshi Lab Materials/3 Studies Dataset/Dataset Merge/total_diagnosis_counts2_021023.csv")

diag_keysv2 <- as.character(factor(rlvnt_diag$cDiagnosis[rlvnt_diag$cStudy == 'Total']))
diag_valsv2 <- as.character(factor(rlvnt_diag$cDiagnosis2[rlvnt_diag$cStudy == 'Total']))
diag_valsv3 <- as.character(factor(rlvnt_diag$cDiagnosis3[rlvnt_diag$cStudy == 'Total']))
diag_valsv4 <- as.character(factor(rlvnt_diag$cDiagnosis4[rlvnt_diag$cStudy == 'Total']))

# cDiagnosis to cDiagnosis2 transformation dictionary
c1_c2 <- data.frame(cDiagnosis1 = diag_keysv2,
                    cDiagnosis2 = diag_valsv2)
# View(c1_c2[1:20,])
# View(c1_c2[21:nrow(c1_c2),])

# cDiagnosis2 to cDiagnosis3
c2_c3 <- data.frame(cDiagnosis2 = diag_valsv2,
                    cDiagnosis3 = diag_valsv3)
# View(c2_c3[1:20,])
# View(c2_c3[21:nrow(c2_c3),])

# cDiagnosis3 to cDiagnosis4
c3_c4 <- data.frame(cDiagnosis3 = diag_valsv3,
                    cDiagnosis4 = diag_valsv4)
# View(c3_c4[1:20,])
# View(c3_c4[21:nrow(c3_c4),])

levels(factor(diag_valsv2))
levels(factor(diag_valsv3))
levels(factor(diag_valsv4))

# cleaning on US subjects
#us_copy <- read.csv('ds_merg_us_only.csv')
us_copy <- read.csv('ds_merg_us_only_060723.csv')

# Standardizing cities
us_copy$cLocationCity[us_copy$cLocationCity == 'La Jolla'] <- 'San Diego'
us_copy$cLocationCity[us_copy$cLocationCity == 'Brooklyn'] <- 'New York'
us_copy$cLocationCity[us_copy$cLocationCity == 'Stony Brook'] <- 'Stonybrook'

us_copy <- us_copy[us_copy$cLocationCity != 'USC/MASS', ]


# adding cDiagnosis2 and cDiagnosis3 columns
us_copy$cDiagnosis2 <- dict_transform(diag_keysv2, diag_valsv2, us_copy$cDiagnosis)
us_copy$cDiagnosis3 <- dict_transform(diag_keysv2, diag_valsv3, us_copy$cDiagnosis)
us_copy$cDiagnosis4 <- dict_transform(diag_keysv2, diag_valsv4, us_copy$cDiagnosis)

# counting relevant diagnoses for diag2
cdiag2_cts <- us_copy %>% 
  dplyr::group_by(cStudy, cDiagnosis2) %>%  
  dplyr::summarise(total_count=n(), .groups = 'drop')

# removing blank diagnoses
cdiag2_cts <- cdiag2_cts[cdiag2_cts$cDiagnosis2 != '', ]
cdiag2_cts

# counting relevant diagnoses for diag3
cdiag3_cts <- us_copy %>% 
  dplyr::group_by(cStudy, cDiagnosis3) %>%  
  dplyr::summarise(total_count=n(), .groups = 'drop')

# removing blank diagnoses
cdiag3_cts <- cdiag3_cts[cdiag3_cts$cDiagnosis3 != '', ]
cdiag3_cts

# Verifying format is as expected and no additional data points were omitted
# View(head(us_copy))
# nrow(us_copy) == nrow(merg_us_og) # Returns TRUE

# exporting edited us_only as a csv
#write.csv(us_copy, file = 'bgc_merge_cDiag123_new.csv', row.names = FALSE)
write.csv(us_copy, file = 'bgc_merge_cDiag123_060723.csv', row.names = FALSE)
