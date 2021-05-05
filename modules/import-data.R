# Data --------------------------------------------------------------------

display_in_title <- " for all Districts"
# add data: All
dat<-read_rds(file = "data/data.rds")

# add data: Shapefile
df_shape<- dat$df_shape

# add data: CBI
cbi_df_data<- dat$cbi_df_data
cbi_beneficiary_types <- cbi_df_data %>% 
  filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique() %>% sort()

# add data: food security
fs_df_data <-dat$fs_df_data
fs_beneficiary_types <- fs_df_data %>% 
  filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique() %>% sort()

# add data: livelihood
seo_df_data <-dat$els_df_data
seo_beneficiary_types <- seo_df_data %>% 
  filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()%>% sort()

# add data: Access to Productive Assets
apa_df_data <-dat$apa_df_data
apa_beneficiary_types <- apa_df_data %>% 
  filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()%>% sort()

# add data: environmental protection and restoration
epr_df_data <-dat$epr_df_data
epr_beneficiary_types <- epr_df_data %>% 
  filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()%>% sort()
