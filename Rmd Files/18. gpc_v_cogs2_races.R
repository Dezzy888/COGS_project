library(plyr)
library(dplyr)
library(ggplot2)
library(binom)

df <- read.csv("C:/Users/danie/Documents/Joshi Lab Materials/3 Studies Dataset/Dataset Merge/012623/bgc_merge_cDiag123_new.csv")

# limit  to only gpc or cogs2
df_gpc <- df[df$cStudy == 'GPC', ]
df_cogs2 <- df[df$cStudy == 'COGS2', ]

# Differences in Proportions of Controls vs Patients

# Approach: look at cDiag3 
## Do not ignore blanks (diags we don't care about)
## CS = healthy control, not CS = patient
## Wald CI test for difference between proportions

CS_gpc <- sum(df_gpc$cDiagnosis3 == 'CS')
pt_gpc <- sum(df_gpc$cDiagnosis3 != 'CS')

CS_cogs2 <- sum(df_cogs2$cDiagnosis3 == 'CS')
pt_cogs2<- sum(df_cogs2$cDiagnosis3 != 'CS')

cs_v_pt <- array(c(CS_gpc, CS_cogs2, pt_gpc, pt_cogs2), dim = c(2,2))
dimnames(cs_v_pt) <- list(Study = c('GPC', 'COGS2'),
                          cDiag3 = c('CS', 'PT'))
cs_v_pt

prop.test(cs_v_pt, 1, correct = FALSE)

# FINDING: There is a significant difference
## 0.06 (95% CI: 0.04, 0.08, p = 3.57e-08)
## proportion of controls in GPC was 6% higher than
## in COGS2


# Race differences in CS characteristics
csdf_cogs2 <- df_cogs2[df_cogs2$cDiagnosis3 == 'CS', ]
csdf_gpc <- df_gpc[df_gpc$cDiagnosis3 == 'CS',]

cs_gpc_races <- as.data.frame(plyr::count(csdf_gpc, 'cRace'))
cs_gpc_races$cRace[7] <- 'UNK' # renaming OT/UNK to UNK
cs_cogs2_races <- as.data.frame(plyr::count(csdf_cogs2, 'cRace'))

# rescale into proportions
cs_gpc_races$Proportion <- cs_gpc_races$freq / sum(cs_gpc_races$freq)
cs_cogs2_races$Proportion <- cs_cogs2_races$freq / sum(cs_cogs2_races$freq)

cs_gpc_races$cStudy <- rep('GPC', nrow(cs_gpc_races))
cs_cogs2_races$cStudy <- rep('COGS2', nrow(cs_cogs2_races))

cs_combined_races <- rbind(cs_gpc_races, cs_cogs2_races)

cs_race <- ggplot(data = cs_combined_races, aes(x = cRace, y = Proportion, fill = cStudy)) +
  geom_bar(position = position_dodge(), 
           stat = 'identity') +
  scale_fill_manual(values = c('orange', 'darkgreen')) +
  ggtitle('Proportions of Race in CS for GPC and COGS2') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

cs_race

# Hypothesis testing on CS
## Method: Use Wilson CI
## Use cogs2 as null distribution

score_cis_cogs2 <- as.data.frame(binom.confint(x = cs_cogs2_races$freq, n = sum(cs_cogs2_races$freq), methods = 'wilson', conf.level = 0.95))
cis_only_cogs2 <- score_cis_cogs2[,4:6]
colnames(cis_only_cogs2)[1] <- 'cogs2_prop'

cis_only_cogs2$Significant <- (cs_gpc_races$Proportion > cis_only_cogs2$upper) | (cs_gpc_races$Proportion < cis_only_cogs2$lower)

cs_cis_only_cogs2 <- cbind(cs_gpc_races$Proportion, cis_only_cogs2)

rownames(cs_cis_only_cogs2) <- factor(cs_cogs2_races$cRace)
colnames(cs_cis_only_cogs2)[1] <- 'gpc_prop'

View(cs_cis_only_cogs2)

## FINDING: all proportions are significantly different p < 0.05
### Most extreme differences: ~10% more CA in cogs2 than in gpc
### ~7% less AA in cogs2 than in gpc


# Race differences in Patients 
ptdf_cogs2 <- df_cogs2[df_cogs2$cDiagnosis3 != 'CS',]
ptdf_gpc <- df_gpc[df_gpc$cDiagnosis3 != 'CS',]


pt_cogs2_races <- as.data.frame(plyr::count(ptdf_cogs2, 'cRace'))
pt_gpc_races <- as.data.frame(plyr::count(ptdf_gpc, 'cRace'))
pt_gpc_races$cRace[7] <- 'UNK'

# rescale into proportions
pt_cogs2_races$Proportion <- pt_cogs2_races$freq / sum(pt_cogs2_races$freq)
pt_gpc_races$Proportion <- pt_gpc_races$freq / sum(pt_gpc_races$freq)

pt_gpc_races$cStudy <- rep('GPC', nrow(pt_gpc_races))
pt_cogs2_races$cStudy <- rep('COGS2', nrow(pt_cogs2_races))

pt_combined_races <- rbind(pt_gpc_races, pt_cogs2_races)

pt_race <- ggplot(data = pt_combined_races, aes(x = cRace, y = Proportion, fill = cStudy)) +
  geom_bar(position = position_dodge(), 
           stat = 'identity') +
  scale_fill_manual(values = c('orange', 'darkgreen')) +
  ggtitle('Proportions of Race in PT for GPC and COGS2') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

pt_race

# Hypothesis testing on CS
## Method: Use Wilson CI
## Use cogs2 as null distribution

score_cis_cogs2 <- as.data.frame(binom.confint(x = pt_cogs2_races$freq, n = sum(pt_cogs2_races$freq), methods = 'wilson', conf.level = 0.95))
cis_only_cogs2 <- score_cis_cogs2[,4:6]
colnames(cis_only_cogs2)[1] <- 'cogs2_prop'

cis_only_cogs2$Significant <- (pt_gpc_races$Proportion > cis_only_cogs2$upper) | (pt_gpc_races$Proportion < cis_only_cogs2$lower)

pt_cis_only_cogs2 <- cbind(pt_gpc_races$Proportion, cis_only_cogs2)

rownames(pt_cis_only_cogs2) <- factor(pt_cogs2_races$cRace)
colnames(pt_cis_only_cogs2)[1] <- 'gpc_prop'

View(pt_cis_only_cogs2)

# FINDING: No significant difference in AA proportions
## Significant differences everywhere else



















