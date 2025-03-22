# importing datasets
df <- read.csv('bgc_merge_cDiag123.csv')
df <- df[,-1]
df_check <- read_xlsx("C:/Users/danie/Documents/Joshi Lab Materials/3 Studies Dataset/Dataset Merge/010623/reproj_30_2010.xlsx")

# standardizing city names and getting rid of USC/MASS
df$cLocationCity[df$cLocationCity == 'Brooklyn'] <- 'New York'
df$cLocationCity[df$cLocationCity == 'La Jolla'] <- 'San Diego'
df$cLocationCity[df$cLocationCity == 'Stony Brook'] <- 'Stonybrook'
df <- df[df$cLocationCity != 'USC/MASS', ]

# checking city names
nov30_cities <- levels(factor(df_check$City))
df_cities <- levels(factor(df$cLocationCity))

df_cities
nov30_cities

#FINDING: nov30 does not have San Francisco

#checking race acronyms
nov30_cols <- colnames(df_check)
nov30_races <- sort(nov30_cols[2:10])
df_races <- levels(factor(df$cRace))

View(data.frame(nov30_races, df_races))

# DISCREPANCIES between nov30 and current df:
## PI is listed as NH --> not a problem
## NA listed as AE
## HA is listed as a race (Hispanic/Latinx), not ethnicity
## OT is separated into OT, OT/UNK, or UNK
## NH means not Hispanic or Latinx
View(head(df))
write.csv(df, file = 'bgc_cDiag123_cities_std.csv', row.names = FALSE)
