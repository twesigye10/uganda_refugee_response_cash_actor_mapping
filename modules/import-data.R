# Data --------------------------------------------------------------------

display_in_title <- " across all Districts"
all_beneficiary_types <- c("Burundian refugees", "Congolese & other refugees", "Host community", "South Sudanese refugees")
refugee_districts <- c("ADJUMANI", "ISINGIRO", "KAMPALA", "KAMWENGE", "KIKUUBE", "KIRYANDONGO", "KYEGEGWA", "LAMWO", "OBONGI", "YUMBE","KOBOKO", "MADI OKOLLO & TEREGO"  )

# add data: All
dat<-read_rds(file = "data/data.rds")

# add data: Shapefile
df_shape<- dat$df_shape

# add data: CBI
cbi_df_data<- dat$cbi_df_data
cbi_beneficiary_types <- all_beneficiary_types

# add data: food security
fs_df_data <-dat$fs_df_data
fs_beneficiary_types <- all_beneficiary_types

# add data: livelihood
seo_df_data <-dat$seo_df_data
seo_beneficiary_types <- all_beneficiary_types

# add data: Access to Productive Assets
apa_df_data <-dat$apa_df_data
apa_beneficiary_types <- all_beneficiary_types

# add data: environmental and energy
epr_df_data <-dat$epr_df_data
epr_beneficiary_types <- all_beneficiary_types

aor_df_data <-dat$aor_df_data
aor_beneficiary_types <- all_beneficiary_types

ecs_df_data <-dat$ecs_df_data
ecs_beneficiary_types <- all_beneficiary_types

# add data: CBI_approach_in_sanitation_services
ss_df_data <-dat$ss_df_data
ss_beneficiary_types <- all_beneficiary_types

# add data: CBI_approach_in_WASH_NFI
wn_df_data <-dat$wn_df_data
wn_beneficiary_types <- all_beneficiary_types

# add data: shelter
shl_df_data <-dat$shl_df_data
shl_beneficiary_types <- all_beneficiary_types
