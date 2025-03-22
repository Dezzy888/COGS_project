# Total plot collection
pdf(file = 'gender_race_eth_plots.pdf')

gender
gen_within
gen_tots

race
race_within
race_tots

eth
eth_within
eth_tots

dev.off()

# Total counts/relative proportions collection

df_list = list(bsg_tot, bsr_tot, bse_tot)
csv_names <- c('gender_props.csv', 'race_props.csv', 'ethnicity_props.csv')

for (i in 1:length(df_list)) {
  write.csv(df_list[[i]], file = toString(csv_names[i]))
}
