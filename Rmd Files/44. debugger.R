---
  title: "42. Added Populations and DI"
author: "Daniel Zoleikhaeian"
date: "2023-06-28"
output: html_document
editor_options: 
  chunk_output_type: console
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Setup

## Importing COGS2 data
```{r}
library(grid)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
df_COGS2 <- read.csv('C:\\Users\\danie\\Documents\\Joshi Lab Materials\\3 Studies Dataset\\Dataset Merge\\bgc_merge_cDiag123_060723.csv')
df_COGS2 <- df_COGS2[df_COGS2$cStudy == 'COGS2', ]
nrow(df_COGS2)

head(df_COGS2)
# adding year column
df_COGS2$cEnrollmentYear <- as.numeric(substr(df_COGS2$cEnrollmentDateYear, 1,4))
head(df_COGS2)

unique(df_COGS2$cRace)

# re-encoding OT/UNK/MR into one group
df_COGS2$cRace2 <- df_COGS2$cRace
df_COGS2$cRace2[df_COGS2$cRace2 %in% c('OT', 'OT/UNK', 'MR', 'UNK')] <- 'OT/MR'
nrow(df_COGS2)

# removing San Diego observations
no_sd <- df_COGS2[df_COGS2$cLocationCity != 'San Diego', ]
```

### Helper function: Transforming by dictionary
```{r}
dict_transform <- function(keys, values, vec) {
  for (i in 1:length(keys)) {
    vec[vec == keys[i]] <- values[i]
  }
  return (vec)
}
```

### Helper function: multinomial distribution
```{r}
mv_binom <- function(p,x) {
  prod1 <- sum(x) * prod(p^x)
  for (i in 1:length(x)) {
    prod1 * 1/factorial(x[i])
  }
  return(prod1)
}
```

### Helper Function: multivariate hypergeometric distribution
```{r}
mv_hyper <- function(m, y) {
  prod <- 1
  if(length(m) != length(y)) {
    stop('Lengths are not equal')
  }
  
  res <- prod(choose(m,y)) / choose(sum(m), sum(y))
  return (res)
}
```


## Importing ACS Data
## Importing and checking data
```{r}
df_acs <- read.csv('C:\\Users\\danie\\Documents\\Joshi Lab Materials\\acs_cogs2_1014.csv')
unique(df_acs$CITY) # all cities except SD
head(df_acs)

# no missing data
sum(complete.cases(df_acs)) == nrow(df_acs)

```

## Encoding new race categories, binarizing hispan category
```{r}
df_acs$Race2 <- rep(0, nrow(df_acs))
df_acs$Hispan2 <- rep(0, nrow(df_acs))

sum(df_acs$HISPAN == 9) # everyone reported a hispanic status
df_acs$Hispan2 <- as.numeric(df_acs$HISPAN != 0) # 0 for not hispanic or latino, else 1

PI_raced <- c(680:699) # PI races

df_acs$Race2[df_acs$RACE == 1 ] <- 1 # White
df_acs$Race2[df_acs$RACE == 2 ] <- 2 # Black
df_acs$Race2[df_acs$RACE == 3 ] <- 3 # American Indian or Alaska Native
df_acs$Race2[df_acs$RACE %in% 4:6 & !(df_acs$RACED %in% PI_raced) ] <- 4 # Asian
df_acs$Race2[df_acs$RACE == 6 & df_acs$RACED %in% PI_raced ] <- 5 # Pacific Islander (or Native Hawaiian)
df_acs$Race2[df_acs$RACE %in% 7:9 ] <- 6 # Mixed/Other

# truncating the acs dataset
acs1865 <- df_acs[df_acs$AGE >= 18 & df_acs$AGE <= 65, ]
```

# Approximating Census Data

## Finding City Category Proportions using plyr::count
* NOTE: the Diversity Index is not 1-1
* As long as the category proportions are the same, then the resulting diversity index will be the same
* Need to check that the categories match too
* Method:
  - Use category proportions instead of diversity index as the target metric
```{r}
# collection of target proportions
cityprops <- vector("list", 4)

cogs_codes <- unique(acs1865$CITY)
colnames(acs1865)
for (i in 1:length(cogs_codes)) {
  acs_counts <- plyr::count(acs1865[acs1865$CITY == cogs_codes[i], ], c('Race2', 'Hispan2', 'SEX'), wt_var = 'PERWT')
  acs_counts$prop <- acs_counts$freq / sum(acs_counts$freq)
  
  # converting the acs codes to match COGS2 variables
  acs_counts$Race2 <- dict_transform(c(1:6),c('CA', 'AA', 'AE','AS','NH','OT/MR'), acs_counts$Race2)
  acs_counts$Hispan2 <- dict_transform(c(0,1), c('No', 'Yes'), acs_counts$Hispan2)
  acs_counts$SEX <- dict_transform(c(1,2),c('M','F'), acs_counts$SEX)
  acs_counts <- acs_counts[order(acs_counts$Race2,acs_counts$Hispan2,acs_counts$SEX),]
  
  cityprops[[i]] <- acs_counts
}

city_names <- factor(cogs_codes)
unique(no_sd$cLocationCity)
levels(city_names) <- c('Los Angeles', 'New York', 'Philadelphia', 'Seattle')
names(cityprops) <- city_names

cityprops
```

ACS Codes notes:
  - For sex: 1 = male, 2 = female
- For Race2: 
  1 = White
2 = Black
3 = AI/AN
4 = Asian
5 = PI/NH
6 = Mixed/Other
- For Hispan: 1 = Hispanic or Latino, 0 = Not hispanic or latino

## Finding the target counts using the ACS data
```{r}
sizes <- plyr::count(no_sd, c('cLocationCity', 'cDiagnosis4'))
city_pops <- plyr::count(acs1865, c('CITY'), wt_var = 'PERWT')
city_names <- factor(city_pops$CITY)
levels(city_names) <- c('Los Angeles', 'New York', 'Philadelphia', 'Seattle')
city_pops$CITY <- city_names
# study_count / city_pop = scaling factor
# then scaling factor * prop from cityprop * count from cityprop

sizes$scale_factor <- rep(0,nrow(sizes))
for (i in 1:nrow(sizes)) {
  sizes$scale_factor[i] <- sizes$freq[i] / city_pops$freq[city_pops$CITY == sizes$cLocationCity[i]]
}

cs_targets <- sizes[sizes$cDiagnosis4 == 'CS',]
sz_targets <- sizes[sizes$cDiagnosis4 == 'SZSAFD',]

# aggregate sizes and aggregate scaling factor
sizes2 <- plyr::count(no_sd, 'cLocationCity')
sizes2$scale_factor <- sizes2$freq / city_pops$freq
sizes2

for (i in 1:length(cityprops)) {
  if (names(cityprops)[i] == cs_targets$cLocationCity[i] & sz_targets$cLocationCity[i] == names(cityprops[i])) {
    
    in_cs <- cs_targets$scale_factor[i] * cityprops[[i]]$freq
    in_sz <- sz_targets$scale_factor[i] * cityprops[[i]]$freq
    in_ag <- sizes2$scale_factor[i] * cityprops[[i]]$freq
    
    # if the expected count is less than 1, set count to 0
    # else, round it 
    cityprops[[i]]$target_cs <- ifelse(in_cs < 1, 0, round(in_cs)) 
    cityprops[[i]]$target_sz <- ifelse(in_sz < 1, 0, round(in_sz))
    cityprops[[i]]$target_ag <- ifelse(in_ag < 1, 0, round(in_ag))
    
  }
}
cityprops
```

## Finding City Proportions for COGS2 - CS and SZSAFD
```{r}
cogsprops_cs <- vector("list", 4)
cogsprops_sz <- vector("list", 4) 

cogs_cities <- unique(no_sd$cLocationCity)
for (i in 1:length(cogs_codes)) {
  
  cogs_counts_cs <- plyr::count(no_sd[no_sd$cLocationCity == cogs_cities[i] & no_sd$cDiagnosis4 == 'CS', ], c('cRace2', 'cHispanicorLatino', 'cGender'))
  cogs_counts_sz <- plyr::count(no_sd[no_sd$cLocationCity == cogs_cities[i] & no_sd$cDiagnosis4 == 'SZSAFD', ], c('cRace2', 'cHispanicorLatino', 'cGender'))
  
  cogs_counts_cs$prop <- cogs_counts_cs$freq / sum(cogs_counts_cs$freq)
  cogsprops_cs[[i]] <- cogs_counts_cs[order(cogs_counts_cs$cRace2,cogs_counts_cs$cHispanicorLatino,cogs_counts_cs$cGender),]
  
  cogs_counts_sz$prop <- cogs_counts_sz$freq / sum(cogs_counts_sz$freq)
  cogsprops_sz[[i]] <- cogs_counts_sz[order(cogs_counts_sz$cRace2,cogs_counts_sz$cHispanicorLatino,cogs_counts_sz$cGender),]
}

names(cogsprops_cs) <- cogs_cities
names(cogsprops_sz) <- cogs_cities
```

## Creating a standard set of group codes
```{r}
unique(acs1865$Race2)
colnames(acs1865)
group_codes_df <- data.frame(Race2 = rep(c('AA', 'AE', 'AS','CA','NH','OT/MR'), rep(4,6)),
                             Hispan2 = rep(rep(c('No','Yes'), c(2,2)), 6),
                             Gender = rep(c('F','M'), 12),
                             group_code = 1:24)
group_codes_df
```

### Function to generate group codes for a dataframe of counts
```{r}
group_code_gen <- function(df, handbook) {
  #row_track <- 1
  df$group_code <- rep(0,nrow(df))
  for (n in 1:nrow(df)) {
    for (i in 1:nrow(handbook)) {
      if (df[n,1] == handbook[i,1] &&
          df[n,2] == handbook[i,2] &&
          df[n,3] == handbook[i,3]) {
        df$group_code[n] <- handbook$group_code[i]
        break
      } else {
        next
      }
    }
  }
  return (df)
}
```

### Using helper function to transform the existing list of dataframes
```{r}

cogsprops_cs
cogsprops_sz
# adds the group code
for (i in 1:length(cogsprops_cs)) {
  cogsprops_cs[[i]] <- group_code_gen(cogsprops_cs[[i]],group_codes_df)
  cogsprops_sz[[i]] <- group_code_gen(cogsprops_sz[[i]],group_codes_df)
  cityprops[[i]] <- group_code_gen(cityprops[[i]],group_codes_df)
}
```

## Finding the missing group codes and calculating rarity

```{r}
rarity <- function(cogs_list, diagnosis) {
  
  out <- data.frame(City = names(cogs_list), 
                    Rarity.hyper = rep(0,4),
                    Rarity.binom = rep(0,4))
  
  for (i in 1:length(cogs_list)) {
    cogs_sub <- cogs_list[[i]]
    acs_sub <- cityprops[[i]]
    acs_sub <- acs_sub[,5:ncol(acs_sub)]
    
    counts2_use <- numeric(0)
    props2_use <- numeric(0)
    
    # removing all rows that had 0 count
    if (diagnosis == 'CS') {
      acs_sub <- acs_sub[acs_sub$target_cs != 0,]
      
    } else {
      acs_sub <- acs_sub[acs_sub$target_sz != 0,]
      
    }
    
    # finding out what's missing from what
    not_in_cogs2_but_in_acs <- setdiff(acs_sub$group_code, cogs_sub$group_code)
    not_in_acs_but_in_cogs2 <- setdiff(cogs_sub$group_code, acs_sub$group_code)
    
    # collection of missing codes
    miss_codes <- c(not_in_acs_but_in_cogs2, not_in_cogs2_but_in_acs)
    
    # printing what was missing
    if(length(not_in_cogs2_but_in_acs) != 0 | length(not_in_acs_but_in_cogs2) != 0)   {
      print(names(cogs_list)[i])
      cat(c('In COGS but not in ACS:', not_in_acs_but_in_cogs2, '\n'))
      cat(c('In ACS but not in COGS2:', not_in_cogs2_but_in_acs, '\n'))
    }
    
    # collection of mutually present codes
    ## used to subset the acs and cogs stuff for the 
    ## rarity calculations
    present_codes <- intersect(acs_sub$group_code, cogs_sub$group_code)
    acs_sub <- acs_sub[acs_sub$group_code %in% present_codes,]
    cogs_sub <- cogs_sub[cogs_sub$group_code %in% present_codes,]
    
    if (diagnosis == 'CS') {
      counts2_use <- acs_sub$target_cs
      props2_use <- acs_sub$target_cs / sum(acs_sub$target_cs)
    } else {
      counts2_use <- acs_sub$target_sz
      props2_use <- acs_sub$target_sz / sum(acs_sub$target_sz)
    }
    
    
    
    # Calculating the rarity of sample using multivariate hypergeometric
    out$Rarity.hyper[i] <- mv_hyper(counts2_use, cogs_sub$freq)
    
    # Calculating rarity using multinomial
    out$Rarity.binom[i] <- mv_binom(props2_use,
                                    cogs_sub$freq)
  }
  return(out)
}


rarity_df_cs <- rarity(cogsprops_cs, 'CS')
rarity_df_sz <- rarity(cogsprops_sz, 'SZSAFD')

rarity_df_cs
rarity_df_sz
```

Conclusions:
  * Confirms previous findings that the study samples are significantly different from the ACS samples for the aggregate
* COGS2 seems to have a lot "rare" individuals that are not present in the ACS sample
* Tracks with findings from aggregate

## Focusing on the underrepresented

* This looks at the individuals whose expected counts are lower than what ACS would expect
* Uses expected counts with expected count = 0 if expected count is less than 1

```{r}
underrep_id <- function(cogs_list, diagnosis) {
  
  underrep <- data.frame(City = names(cogs_list),
                         Underrep_codes = rep('',4))
  for (i in 1:length(cogs_list)) {
    # same setup as previous loop
    cogs_sub <- cogs_list[[i]]
    acs_sub <- cityprops[[i]]
    acs_sub <- acs_sub[,5:ncol(acs_sub)]
    
    # removing all rows that had 0 count
    if (diagnosis == 'CS') {
      acs_sub <- acs_sub[acs_sub$target_cs != 0,]
      
    } else {
      acs_sub <- acs_sub[acs_sub$target_sz != 0,]
    }
    
    # finding out what is lower in cogs than it should be
    present_codes <- intersect(acs_sub$group_code, cogs_sub$group_code)
    acs_sub <- acs_sub[acs_sub$group_code %in% present_codes,]
    cogs_sub <- cogs_sub[cogs_sub$group_code %in% present_codes,]
    
    not_in_cogs2_but_in_acs <- setdiff(acs_sub$group_code, cogs_sub$group_code)
    lower_than_exp <- acs_sub$group_code[acs_sub$target_ag > 
                                           cogs_sub$freq]
    
    # collecting all the underrepresented codes
    underrep$Underrep_codes[i] <- toString(c(not_in_cogs2_but_in_acs,lower_than_exp))
  }
  return(underrep)
}

(underrep_cs <- underrep_id(cogsprops_cs, diagnosis = "CS"))
(underrep_sz <- underrep_id(cogsprops_sz, diagnosis = "SZ"))

```

### Comparing the proportions in underrepresented codes to the ACS proportions

# collecting the missing codes into a list
miss_codes_cs <- unname(lapply(sapply(underrep_cs[,2], strsplit, ', '), as.numeric))
miss_codes_sz <- unname(lapply(sapply(underrep_sz[,2], strsplit, ', '), as.numeric))

fn_underrep_hyp_test <- function(ms_codes, cogs_list, diagnosis) {
  
  out <- vector(mode='list',4)
  
  for (i in 1:length(miss_codes)) {
    cogs_sub <- cogs_list[[i]]
    acs_sub <- cityprops[[i]]
    acs_sub <- acs_sub[,5:ncol(acs_sub)]
    
    # removing all rows that had 0 count
    if (diagnosis == 'CS') {
      acs_sub <- acs_sub[acs_sub$target_cs != 0,]
      
    } else {
      acs_sub <- acs_sub[acs_sub$target_sz != 0,]
    }
    
    # finding out what is not in cogs but should be there
    not_in_cogs2_but_in_acs <- setdiff(acs_sub$group_code, cogs_sub$group_code)
    
    # finding out what is lower in cogs than it should be
    present_codes <- intersect(acs_sub$group_code, cogs_sub$group_code)
    acs_sub <- acs_sub[acs_sub$group_code %in% present_codes,]
    cogs_sub <- cogs_sub[cogs_sub$group_code %in% present_codes,]
    
    # subsetting by underrepresentation and performing hypothesis test
    out[[i]] <- cbind(cogs_sub[cogs_sub$prop < acs_sub$prop,c(1:3,6,4,5)], acs_sub$prop[acs_sub$prop > cogs_sub$prop])
    
    low_tail <- pbinom(cogs_sub$freq[cogs_sub$prop < acs_sub$prop], sum(cogsprops[[i]]$freq), prob = acs_sub$prop[cogs_sub$prop < acs_sub$prop])
    
    # two-sided p-value
    # out[[i]]$p_val <- ifelse(low_tail <= 0.5, 2*low_tail, 2*(1-low_tail))
    out[[i]]$p_val <- 2*low_tail
    out[[i]]$sig_code <- ifelse(out[[i]]$p_val < 0.05, '**','-')
    
    names(out[[i]])[7] <- 'acs prop'
    
    
  }
  names(out) <- names(cogs_list)
  return(out)
}

(underrep_hyp_test_cs <- fn_underrep_hyp_test(miss_codes_cs, cogsprops_cs, 'CS'))
(underrep_hyp_test_sz <- fn_underrep_hyp_test(miss_codes_sz, cogsprops_sz, 'SZSAFD'))


### Applying Holm-Correction

# Holm-Corrected P-values
length(underrep_hyp_test_sz) == length(underrep_hyp_test_cs)

for (d in 1:length(underrep_hyp_test_cs)) {
  underrep_hyp_test_cs[[d]] <- underrep_hyp_test_cs[[d]][order(underrep_hyp_test_cs[[d]]$p_val),]
  underrep_hyp_test_cs[[d]]$Holm_pval <- underrep_hyp_test_cs[[d]]$p_val * (nrow(underrep_hyp_test_cs[[d]]) + 1 - 1:nrow(underrep_hyp_test_cs[[d]]))
  underrep_hyp_test_cs[[d]]$sig_code_holm <- ifelse(underrep_hyp_test_cs[[d]]$Holm_pval < 0.05, '**','-')
  
  underrep_hyp_test_sz[[d]] <- underrep_hyp_test_sz[[d]][order(underrep_hyp_test_sz[[d]]$p_val),]
  underrep_hyp_test_sz[[d]]$Holm_pval <- underrep_hyp_test_sz[[d]]$p_val * (nrow(underrep_hyp_test_sz[[d]]) + 1 - 1:nrow(underrep_hyp_test_sz[[d]]))
  underrep_hyp_test_sz[[d]]$sig_code_holm <- ifelse(underrep_hyp_test_sz[[d]]$Holm_pval < 0.05, '**','-')
}
underrep_hyp_test_cs
underrep_hyp_test_sz




