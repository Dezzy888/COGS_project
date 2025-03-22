library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
library(readxl)

setwd('C:/Users/danie/Documents/Joshi Lab Materials/3 Studies Dataset/Dataset Merge')


# loading datatsets and viewing briefly
gpc_df <- read.csv('gpc_dat.csv')
bsnip_df <- read.csv('bsnip_dat.csv')
bsnip_df2 <- read.csv('bsnip_dat2.csv')
cogs2_df <- read_excel('cogs2.xlsx')

## Showing the one blank study
View(gpc_df[gpc_df$STUDY == '',])


# View(as.data.frame(plyr::count(cogs2_df, 'SCH_DEP')))

# Check the cogs2 GROUP to make sure they match with SCH_DEP
# View(data.frame(cogs2_df$GROUP, cogs2_df$SCH_DEP))

schz <- cogs2_df$GROUP == 2
schdep <- !(is.na(cogs2_df$SCH_DEP))

sum(schz)
sum(schdep)

inconsistencies <- schz != schdep
sum(inconsistencies)

ind_to_check <- which(inconsistencies)
ind_to_check

points_to_verify <- cogs2_df[ind_to_check, ]
View(points_to_verify)

# FINDING: 10 values that don't match up; GROUP says schizophrenia
## but SCH_DEP has NA; should these points be SZ, SAFD, or CTRL? 
## as coded now, these 10 values are left as SZ in the final analysis

class(cogs2_df$SCH_DEP)
levels(factor(cogs2_df$SCH_DEP))
sum(cogs2_df$SCH_DEP == 2, na.rm = TRUE)
sum(cogs2_df$SCH_DEP == 1, na.rm = TRUE)

# copies of original datasets
bsnip_copy <- bsnip_df
gpc_copy <- gpc_df
gpc_copy$STUDY <- rep('GPC', nrow(gpc_copy))
cogs2_copy <- cogs2_df

# cogs2 dates
# EDIT 6/7/23. Replaced with complete dates cogs2_dates <- read.csv('cogs2_dates.csv')
cogs2_dates <- read_xlsx('C:\\Users\\danie\\Documents\\Joshi Lab Materials\\3 Studies Dataset\\Dataset Merge\\COGS2_test_dates.xlsx')
# cogs2_dates <- cogs2_dates[which(cogs2_dates$SubjectID != ''),]
if (nrow(cogs2_dates) == nrow(cogs2_copy)) {
  print('number of entries match')
} else {
  print('number of entries do not match')
}

# two subjects that have demo info but were
# not included in the final dataset
setdiff(cogs2_dates$SubjectID, cogs2_copy$SUBJECTID)

# solution: drop those subjects from the dates dataset
cogs2_dates <- cogs2_dates[cogs2_dates$SubjectID %in% cogs2_copy$SUBJECTID, ]

# checking for correct positional alignment
all(cogs2_dates$SubjectID == cogs2_copy$SUBJECTID)

# substringing the dates; only getting the Year/month/day part
# getting the index of the space
cogs2_dates$DEMOG_DATE <- as.character(cogs2_dates$DEMOG_DATE)

cogs2_copy <- cbind(cogs2_copy, cogs2_dates[,2])
names(cogs2_copy)[length(names(cogs2_copy))] <- 'DATES'

# View(head(gpc_df))
# View(head(bsnip_df))
# View(head(bsnip_df2))
# View(head(cogs2_df))

# helper function for removing number prefixes
remove_nums <- function(df_col) {
  for (i in 1:length(df_col)) {
    if (nchar(df_col[i]) > 0) {
      first_space <- unlist(gregexpr(' ', df_col[i]))[1]
      df_col[i] <- substring(df_col[i], first_space + 1, nchar(df_col[i]))
    } 
  }
  return (df_col)
}

head(gpc_copy)
head(cogs2_copy)
# helper function to reformat dates to gpc format
format_dates <- function(str_v) {
  if (nchar(str_v) == 0) {
    return ('')
  }
  else {
    str_vec <- unlist(strsplit(str_v, '/'))
    for (i in 1:length(str_vec)) {
      if (nchar(str_vec[i]) == 1) {
        str_vec[i] <- paste('0', str_vec[i], sep = '')
      }
      if (i != 2) {
        str_vec[i] <- paste(str_vec[i], '-', sep = '')
      }
    }
      new_vec <- paste(str_vec[3], str_vec[1], str_vec[2], sep = '')
      return (new_vec)
    }
}

# helper function for transforming by a dictionary
dict_transform <- function(keys, values, vec) {
  for (i in 1:length(keys)) {
    vec[vec == keys[i]] <- values[i]
  }
  return (vec)
}


# No need to reformat dates after using new file from 06/07/23 
# reformatting cogs2 dates to gpc
# cogs2_copy$DATES <- sapply(cogs2_copy$DATES, format_dates)

# reformatting siteID
site_id_keys <- 1:5
site_id_vals <- c('UCSD',
                  'UCLA',
                  'Mt. Sinai',
                  'UPenn',
                  'University of Washington')
cogs2_copy$SITEID <- dict_transform(site_id_keys, site_id_vals, cogs2_copy$SITEID)

# reformatting cogs2 ethnicities
cogs2_eth_keys <- 1:2
cogs2_eth_vals <- c('Yes', 'No')
cogs2_copy$ETHNICITY <- dict_transform(cogs2_eth_keys, cogs2_eth_vals, cogs2_copy$ETHNICITY)

# Note: Native American --> american indian or alaska native
## Pacific islander --> Native Hawaiian
## Not reported --> Unknown 
# reformatting cogs2 races
cogs2_race_keys <- 1:7
cogs2_race_vals <- c('AE', 'AS', 'NH', 'AA', 'CA', 'MR', 'UNK')
cogs2_copy$RACE <- dict_transform(cogs2_race_keys, cogs2_race_vals, cogs2_copy$RACE) 

# reformatting diagnoses
cogs2_diag_keys <- 1:2
cogs2_diag_vals <- c('CTRL', 'SZ')
cogs2_copy$GROUP <- dict_transform(cogs2_diag_keys, cogs2_diag_vals, cogs2_copy$GROUP)

# incorporating SZ vs SAFD difference in cogs2
cogs2_copy$GROUP[cogs2_copy$SCH_DEP == 1] <- 'SAFD'

# Adding a study column
cogs2_copy$STUDY <- rep('COGS2', nrow(cogs2_copy))

# Adding additional location information
cogs2_unis <- levels(factor(cogs2_copy$SITEID))
cogs2_cities <- c('New York', 'Los Angeles', 'La Jolla', 'Seattle', 'Philadelphia')
cogs2_counties <- c('New York', 'Los Angeles', 'San Diego', 'King', 'Philadelphia')
cogs2_states <- c('NY', 'CA', 'CA', 'WA', 'PA')

cogs2_city_vec <- rep('', nrow(cogs2_copy))
cogs2_county_vec <- rep('', nrow(cogs2_copy))
cogs2_state_vec <- rep('', nrow(cogs2_copy))

for (i in 1:length(cogs2_cities)) {
  cogs2_city_vec[which(cogs2_copy$SITEID == cogs2_unis[i])] <- cogs2_cities[i]
  cogs2_county_vec[which(cogs2_copy$SITEID == cogs2_unis[i])] <- cogs2_counties[i]
  cogs2_state_vec[which(cogs2_copy$SITEID == cogs2_unis[i])] <- cogs2_states[i]
}

cogs2_copy$city <- cogs2_city_vec
cogs2_copy$county <- cogs2_county_vec
cogs2_copy$state <- cogs2_state_vec

# formatting genders in gpc
gender_keys <- c('Male', 'Female')
gender_vals <- c('M', 'F')

gpc_copy$Gender <- dict_transform(gender_keys, gender_vals, gpc_copy$Gender)


# formatting all the dates in bsnip
bsnip_copy$date_consent <- sapply(bsnip_copy$date_consent, format_dates)

# reformating gpc columns with number prefixes
names_gpc <- colnames(gpc_copy)
reform_cols <- which(names_gpc == 'STUDY' |
                     names_gpc == 'NEW.RACE' |
                     names_gpc == 'HispanicOrLatino' | 
                     names_gpc == 'EPI.LOCATION')

gpc_copy[, reform_cols] <- apply(gpc_copy[, reform_cols], 2, remove_nums)
# View(head(gpc_copy))


# ethnicities
eth_bsnip <- levels(factor(bsnip_copy$ethnicity))
eth_gpc <- levels(factor(gpc_copy$HispanicOrLatino))

eth_keys <- eth_bsnip[c(1, 3, 4, 2)]
eth_vals <- eth_gpc
bsnip_copy$ethnicity <- dict_transform(eth_keys, eth_vals, bsnip_copy$ethnicity)


# races
race_bsnip <- levels(factor(bsnip_copy$race))
race_gpc <- levels(factor(gpc_copy$NEW.RACE))


# making a dictionary of races, using format from BSNIP
# dictionary transforms "other/unknown" to "OT/UNK"

race_keys <- race_gpc
race_vals <- c('AA', 'AE', 'AS', 'MR', 'NH', 'OT/UNK', 'CA')

gpc_copy$NEW.RACE <- dict_transform(race_keys, race_vals, gpc_copy$NEW.RACE)

# View(bsnip_copy)
# View(gpc_copy)

# Location information for BSNIP

# filling in the georgia cities data to bsnip
replace_ind <- which(bsnip_df2$site_ga2 != "")
bsnip_copy$site[replace_ind] <- bsnip_df2$site_ga2[replace_ind]
bsnip_copy$site <- sapply(bsnip_copy$site, str_to_title)

# Johns hopkins not explicitly listed on website

bsnip_cities <- levels(factor(bsnip_copy$site))

bsnip_unis <- c('University of Georgia',
                'Augusta University',
                'Johns Hopkins',
                'Harvard Medical School',
                'University of Chicago',
                'University of Texas Southwestern Medical Center',
                'Olin Neuropsychiatry Research Center')

bsnip_counties <- c('Clarke',
                    'Richmond',
                    'Baltimore',
                    'Suffolk',
                    'Cook',
                    'Dallas',
                    'Hartford')

bsnip_states <- c('GA',
                  'GA',
                  'MD',
                  'MA',
                  'IL',
                  'TX',
                  'CT')

# Verifying positional matching
locations_bsnip <- cbind(bsnip_cities, bsnip_unis, bsnip_counties, bsnip_states)
# View(locations_bsnip)

bsnip_uni_vec <- rep('', nrow(bsnip_copy))
bsnip_county_vec <- rep('', nrow(bsnip_copy))
bsnip_state_vec <- rep('', nrow(bsnip_copy))

for (i in 1:length(bsnip_cities)) {
  bsnip_uni_vec[which(bsnip_copy$site == bsnip_cities[i])] <- bsnip_unis[i]
  bsnip_county_vec[which(bsnip_copy$site == bsnip_cities[i])] <- bsnip_counties[i]
  bsnip_state_vec[which(bsnip_copy$site == bsnip_cities[i])] <- bsnip_states[i]
}

bsnip_copy$unis <- bsnip_uni_vec
bsnip_copy$counties <- bsnip_county_vec
bsnip_copy$states <- bsnip_state_vec

# View(bsnip_copy)


# seeing how many universities
levels(factor(gpc_copy$EPI.LOCATION))

# View(gpc_copy[which(gpc_copy$EPI.LOCATION == 'Mexico'), ])
gpc_cities <- c('Los Angeles',
                'Atlanta',
                'Richmond',
                'Mexico',
                'New York',
                'New York',
                'London',
                'Brooklyn',
                'Stony Brook',
                'Syracuse',
                'Lubbock',
                'Los Angeles',
                'San Francisco',
                'Chapel Hill',
                'Los Angeles',
                'USC/MASS',
                'Dayton'
                )

gpc_counties <- c('Los Angeles',
                  'Fulton',
                  'Augusta-Richmond',
                  'Mexico',
                  'New York',
                  'New York',
                  'London',
                  'Kings',
                  'Suffolk',
                  'Onondaga',
                  'Lubbock',
                  'Los Angeles',
                  'San Francisco',
                  'Orange',
                  'Los Angeles',
                  'USC/MASS',
                  'Montgomery')

gpc_states <- c('CA',
                'GA',
                'CA',
                'Mexico',
                'NY',
                'NY',
                'London',
                'NY',
                'NY',
                'NY',
                'TX',
                'CA',
                'CA',
                'NC',
                'CA',
                'USC/MASS',
                'OH')


gpc_unis <- levels(factor(gpc_copy$EPI.LOCATION))

locations_gpc <- cbind(gpc_unis, gpc_cities, gpc_counties, gpc_states)
# View(locations_gpc)

gpc_city_vec <- rep('', nrow(gpc_copy))
gpc_county_vec <- rep('', nrow(gpc_copy))
gpc_state_vec <- rep('', nrow(gpc_copy))

for (i in 1:length(gpc_unis)) {
  gpc_city_vec[which(gpc_copy$EPI.LOCATION == gpc_unis[i])] <- gpc_cities[i]
  gpc_county_vec[which(gpc_copy$EPI.LOCATION == gpc_unis[i])] <- gpc_counties[i]
  gpc_state_vec[which(gpc_copy$EPI.LOCATION == gpc_unis[i])] <- gpc_states[i]
}

gpc_copy$cities <- gpc_city_vec
gpc_copy$counties <- gpc_county_vec
gpc_copy$states <- gpc_state_vec

# View(gpc_copy)

# constructing merged DF columns vector by vector
study_merg <- c(bsnip_copy$study, gpc_copy$STUDY, cogs2_copy$STUDY)
age_merg <- c(bsnip_copy$age, gpc_copy$Age, cogs2_copy$AGE)
gender_merg <- c(bsnip_copy$gender, gpc_copy$Gender, cogs2_copy$SEX)
date_merg <- c(bsnip_copy$date_consent, gpc_copy$DRAW_DATE, cogs2_copy$DATES)
diag_merg <- c(bsnip_copy$primary_disease, gpc_copy$DX_GPC_FINAL, cogs2_copy$GROUP)
race_merg <- c(bsnip_copy$race, gpc_copy$NEW.RACE, cogs2_copy$RACE)
his_or_lat_merg <- c(bsnip_copy$ethnicity, gpc_copy$HispanicOrLatino, cogs2_copy$ETHNICITY)
inst_merg <- c(bsnip_copy$unis, gpc_copy$EPI.LOCATION, cogs2_copy$SITEID)
city_merg <- c(bsnip_copy$site, gpc_copy$cities, cogs2_copy$city)
county_merg <- c(bsnip_copy$counties, gpc_copy$counties, cogs2_copy$county)
state_merg <- c(bsnip_copy$states, gpc_copy$states, cogs2_copy$state)

# empty dataframe for holding combined new data
merg_df <- data.frame(cStudy = study_merg, 
                     cAge = age_merg,
                     cDiagnosis = diag_merg,
                     cEnrollmentDateYear = date_merg,
                     cGender = gender_merg,
                     cRace = race_merg,
                     cHispanicorLatino = his_or_lat_merg,
                     cLocationInstitution = inst_merg,
                     cLocationCity = city_merg,
                     cLocationState = state_merg,
                     cLocationCounty = county_merg
                     )

merg_df <- as.data.frame(merg_df, stringsAsFactors = TRUE)
# View(merg_df)

# brief analytics
summary(merg_df)

vars <- c('cGender', 'cRace', 'cHispanicorLatino', 'cLocationState')

# counts
for (vbl in vars) {
  print((plyr::count(merg_df, vbl)))
  print('')
}

pad_bot_bsnip <- nrow(gpc_df) + nrow(cogs2_df)
pad_top_gpc <- nrow(bsnip_df)
pad_bot_gpc <- nrow(cogs2_df)
pad_top_cogs2 <- nrow(bsnip_df) + nrow(gpc_df)

# pad function to help with cbind
pad <- function(df_vec, top = 0, bot = 0) {
  df_vec <- c(rep(NA, top), df_vec, rep(NA, bot))
}

# extended original DFs so they can be bound to the merged df
bsnip_df2_ext <- apply(bsnip_df2, 2, pad, bot = pad_bot_bsnip)
gpc_df_ext <- apply(gpc_df, 2, pad, bot = pad_bot_gpc, top = pad_top_gpc)
cogs2_ext <- apply(cogs2_df, 2, pad, top = pad_top_cogs2)

# merging old data to new file
merg_raw <- cbind(merg_df, bsnip_df2_ext, gpc_df_ext, cogs2_ext)
# View(merg_raw)
if (nrow(merg_raw) == nrow(merg_df)) {
  print('merge successful')
}
# exporting CSVs to working directory
# write.csv(points_to_verify, file = 'cogs2_schdep_inconsistencies.csv')
# write.csv(merg_raw, file = 'bgc_raw.csv')
# write.csv(merg_df, file = 'bgc_only.csv')

## updates: 06/07/23
write.csv(merg_raw, file = 'bgc_raw_060723.csv')
write.csv(merg_df, file = 'bgc_only_060723.csv')
