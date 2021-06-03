library(activityinfo)
library(tidyverse)

source("support_data/credentials.R")

# input credentials
account <- account
password <- password

# connect to activityInfo using the credentials
activityInfoLogin(account, password)


# Querying data -----------------------------------------------------------
# get form ID from the actual table you want to query. This can be done by copying the query directy from activityInfo table page
# get parent columns and add them to the query and add them to the query
# we can also query old fields using their label. eg  "Select Month"= "[Select Month]",
# see example below

?activityinfo::queryTable()


# parent form -------------------------------------------------------------

df_parent <- queryTable("a2072958662",
                        "Partner Name" = "a20729586620000000007.P00000102950000000001",
                        "Location District" = "i1713063250.i0219912987",
                        "Location Type" = "i1713063250.i0610240747",
                        "Location Name" = "i1713063250.i0098724746",
                        "Please specify location (if needed)" = "i1743673774",
                        "RRP/5W: CBI for basic needs" = "i1712870876")


# main table with data ----------------------------------------------------

df_main_table_with_data <- queryTable("cjtzz3ez62",
                                      "Parent ID" = "@parent",
                                      "Reporting Month" = "c2xls3ukmvy7kwe2",
                                      "Select Activity Status" = "Q1714986995",
                                      "Select Implementation Type" = "Q1715179369",
                                      "Select Funding Source(s)" = "Q1559548439",
                                      "Please specify funding source(s)" = "i1563203554",
                                      "Select Beneficiary Type" = "Q1563588303",
                                      "Select Modality" = "Q1561856933",
                                      "Select Targeting Criteria" = "Q1562049307",
                                      "Please specify targeting criteria" = "i1561279809",
                                      "Select PSN Type" = "Q1799937737",
                                      "Select Assistance Type" = "Q1554161955",
                                      "Select Delivery Mechanism" = "Q1554354329",
                                      "Please specify financial service provider" = "i1552815333",
                                      "Households receiving cash assistance for basic needs" = "i1551853461",
                                      "Households receiving voucher assistance for basic needs" = "i1551661087",
                                      "PSN households receiving cash assistance for basic needs (Child at risk)" = "i1557432320",
                                      "PSN households receiving voucher assistance for basic needs (Child at risk)" = "i1556470448",
                                      "PSN households receiving cash assistance for basic needs (Disability)" = "i1556855197",
                                      "PSN households receiving voucher assistance for basic needs (Disability)" = "i1556662822",
                                      "PSN households receiving cash assistance for basic needs (Older person at risk)" = "i1555700950",
                                      "PSN households receiving voucher assistance for basic needs (Older person at risk)" = "i1555508576",
                                      "PSN households receiving cash assistance for basic needs (Serious medical condition)" = "i1556085699",
                                      "PSN households receiving voucher assistance for basic needs (Serious medical condition)" = "i1555893325",
                                      "PSN households receiving cash assistance for basic needs (Single parent or caregiver)" = "i1554931452",
                                      "PSN households receiving voucher assistance for basic needs (Single parent or caregiver)" = "i1554739078",
                                      "PSN households receiving cash assistance for basic needs (Specific legal and physical protection needs)" = "i1555316201",
                                      "PSN households receiving voucher assistance for basic needs (Specific legal and physical protection needs)" = "i1555123827",
                                      "PSN households receiving cash assistance for basic needs (Unaccompanied or separated child)" = "i1547813598",
                                      "PSN households receiving voucher assistance for basic needs (Unaccompanied or separated child)" = "i1548390721",
                                      "PSN households receiving cash assistance for basic needs (Woman at risk)" = "i1548198347",
                                      "PSN households receiving voucher assistance for basic needs (Woman at risk)" = "i1547236474",
                                      "PSN households receiving cash assistance for basic needs (total)" = "i1552045836",
                                      "PSN households receiving voucher assistance for basic needs (total)" = "i1557239946",
                                      "Cash value per transfer per HH" = "i1552238210",
                                      "Cash value per transfer per PSN HH" = "i1557047571",
                                      "Total amount of cash transfers" = "i1557624695",
                                      "Comments" = "i1547044100")



# querying data with parent fields ----------------------------------------


# CBI ---------------------------------------------------------------------
# Cash for basic needs
# "Select Month"= "[Select Month]",
df_cash_basic_needs <- queryTable("cjtzz3ez62",
                                  "Partner Name" = "a20729586620000000007.P00000102950000000001",
                                  "Location District" = "i1713063250.i0219912987",
                                  "Location Type" = "i1713063250.i0610240747",
                                  "Location Name" = "i1713063250.i0098724746",
                                  "Please specify location (if needed)" = "i1743673774",
                                  "Parent ID" = "@parent",
                                  "Select Month"= "[Select Month]",
                                  "Reporting Month" = "c2xls3ukmvy7kwe2",
                                  "Select Activity Status" = "Q1714986995",
                                  "Select Implementation Type" = "Q1715179369",
                                  "Select Funding Source(s)" = "Q1559548439",
                                  "Please specify funding source(s)" = "i1563203554",
                                  "Select Beneficiary Type" = "Q1563588303",
                                  "Select Modality" = "Q1561856933",
                                  "Select Targeting Criteria" = "Q1562049307",
                                  "Please specify targeting criteria" = "i1561279809",
                                  "Select PSN Type" = "Q1799937737",
                                  "Select Assistance Type" = "Q1554161955",
                                  "Select Delivery Mechanism" = "Q1554354329",
                                  "Please specify financial service provider" = "i1552815333",
                                  "Households receiving cash assistance for basic needs" = "i1551853461",
                                  "Households receiving voucher assistance for basic needs" = "i1551661087",
                                  "PSN households receiving cash assistance for basic needs (Child at risk)" = "i1557432320",
                                  "PSN households receiving voucher assistance for basic needs (Child at risk)" = "i1556470448",
                                  "PSN households receiving cash assistance for basic needs (Disability)" = "i1556855197",
                                  "PSN households receiving voucher assistance for basic needs (Disability)" = "i1556662822",
                                  "PSN households receiving cash assistance for basic needs (Older person at risk)" = "i1555700950",
                                  "PSN households receiving voucher assistance for basic needs (Older person at risk)" = "i1555508576",
                                  "PSN households receiving cash assistance for basic needs (Serious medical condition)" = "i1556085699",
                                  "PSN households receiving voucher assistance for basic needs (Serious medical condition)" = "i1555893325",
                                  "PSN households receiving cash assistance for basic needs (Single parent or caregiver)" = "i1554931452",
                                  "PSN households receiving voucher assistance for basic needs (Single parent or caregiver)" = "i1554739078",
                                  "PSN households receiving cash assistance for basic needs (Specific legal and physical protection needs)" = "i1555316201",
                                  "PSN households receiving voucher assistance for basic needs (Specific legal and physical protection needs)" = "i1555123827",
                                  "PSN households receiving cash assistance for basic needs (Unaccompanied or separated child)" = "i1547813598",
                                  "PSN households receiving voucher assistance for basic needs (Unaccompanied or separated child)" = "i1548390721",
                                  "PSN households receiving cash assistance for basic needs (Woman at risk)" = "i1548198347",
                                  "PSN households receiving voucher assistance for basic needs (Woman at risk)" = "i1547236474",
                                  "PSN households receiving cash assistance for basic needs (total)" = "i1552045836",
                                  "PSN households receiving voucher assistance for basic needs (total)" = "i1557239946",
                                  "Cash value per transfer per HH" = "i1552238210",
                                  "Cash value per transfer per PSN HH" = "i1557047571",
                                  "Total amount of cash transfers" = "i1557624695",
                                  "Comments" = "i1547044100")

# write out the dataset
write_csv(x = df_cash_basic_needs, file = "data/api_cbi_CBI_for_basic_needs.csv")

# financial_inclusion
# "Select Month"= "[Select Month]",
df_financial_inclusion <- queryTable("cjtzyydbt3",
                                     "Partner Name" = "a10072924240000000007.P00000102950000000001",
                                     "Location District" = "i0404171301.i0219912987",
                                     "Location Type" = "i0404171301.i0610240747",
                                     "Location Name" = "i0404171301.i0098724746",
                                     "Please specify location (if needed)" = "i0403209428",
                                     "Parent ID" = "@parent",
                                     "Select Month"= "[Select Month]",
                                     "Reporting Month" = "c18cvahkmw4jkr92",
                                     "Select Activity Status" = "Q0406095045",
                                     "Select Implementation Type" = "Q0399361939",
                                     "Select Funding Source(s)" = "Q0398592442",
                                     "Please specify funding source(s)" = "i0402632305",
                                     "Select Beneficiary Type" = "Q0401670433",
                                     "Households enrolled in bank account" = "i0401285684",
                                     "Please specify financial service provider (bank account)" = "i0400131437",
                                     "Households enrolled in mobile money" = "i0399939063",
                                     "Please specify financial service provider (mobile money)" = "i0400516186",
                                     "Comments" = "i0400323812")

# write out the dataset
write_csv(x = df_financial_inclusion, file = "data/api_cbi_financial_inclusions.csv")

# Environment & Energy (Partners) ----------------------------------
# Environment Protection and Restoration
# RRP/5W: Forests, wetlands, shorelines protected and restored
# "Select Month"= "[Select Month]",
df_Forests_wetlands_shorelines_protected_and_restored <- queryTable("cjtzyfy2u2",
                                                        "Partner Name" = "a07833782800000000007.P00000102950000000001",
                                                        "District Name" = "i1552632004.i1084237577",
                                                        "Parent ID" = "@parent",
                                                        "Type of location" = "Q1552824378",
                                                        "Location name" = "i1544552277",
                                                        "Geographic point.latitude" = "i1544359902.latitude",
                                                        "Geographic point.longitude" = "i1544359902.longitude",
                                                        "Select Month"= "[Select Month]",
                                                        "Select Quarter" = "cf4p1xukmvwqafl7",
                                                        "Select Implementation Type" = "Q1546283647",
                                                        "Select Funding Source(s)" = "Q1539935290",
                                                        "Please specify funding source(s):" = "i1537434422",
                                                        "Select Beneficiary Type" = "Q1542628532",
                                                        "Select Modality" = "Q1542243783",
                                                        "Select Assistance Type" = "Q1540320039",
                                                        "Delivery mechanism" = "Q1540512413",
                                                        "Financial service provider" = "i1385073858",
                                                        "Cash value of transfer per individual / per day (UGX)" = "i1384111986",
                                                        "Number of labour / man days" = "i1383919611",
                                                        "Total cash value of cash for work (UGX)" = "i1384496734",
                                                        "Marking of trees for protection" = "i1384304360",
                                                        "Tree nursery type" = "Q1383342488",
                                                        "Beneficiaries (Female 18-35)" = "i1388344223",
                                                        "Beneficiaries (Male 18-35)" = "i1388151849",
                                                        "Beneficiaries (Female 36-59)" = "i1387189977",
                                                        "Beneficiaries (Male 36-59)" = "ck7n48plx2",
                                                        "Beneficiaries (Female 60+)" = "i1386997602",
                                                        "Beneficiaries (Female 60+)" = "i1387382351",
                                                        "Beneficiaries (Male 60+)" = "i1386420479",
                                                        "Beneficiaries (Total)" = "i1439275374",
                                                        "Seedlings being prepared in the Nursery - Indigenous" = "i1457453273",
                                                        "Seedlings being prepared in the Nursery - Fruit" = "i0876436461",
                                                        "Seedlings being prepared in the Nursery - Exotic" = "i1711072579",
                                                        "Seedlings provided (leaving) Nursery - Indigenous" = "i1114358506",
                                                        "Seedlings provided (leaving) Nursery - Fruit" = "i1379302624",
                                                        "Seedlings provided (leaving) Nursery - Exotic" = "i2021318110",
                                                        "Trees planted/maintained -  Type" = "Q1379879748",
                                                        "Number of trees" = "i1379110250",
                                                        "Operation of plantation, orchard, woodlot" = "Q1377956003",
                                                        "Area (hectares)" = "i1377186505",
                                                        "Restoration and maintenance of forest reserves (hectares)" = "i1376994131",
                                                        "Restoration of shorelines (hectares)" = "i1377571254",
                                                        "Restoration of wetlands (hectares)" = "i1377378880",
                                                        "Demarcation of wetlands (km)" = "i1382572990",
                                                        "Opening of forest boundaries (km)" = "i1382380615",
                                                        "Roadside rehabilitation (km roads)" = "i1381803492",
                                                        "Rainwater harvesting/valley dams" = "i1381611118",
                                                        "Comments" = "i1380841620",
                                                        "Record change/comment log (please indicate date, name and comment e.g. 01/07/2019 - John Doe - Record validated)" = "i0463387676")
# write out the dataset
write_csv(x = df_Forests_wetlands_shorelines_protected_and_restored, file = "data/api_env_Forests_wetlands_shorelines_protected_and_restored.csv")

# Access to sufficient & sustainable basic energy - Household

# households_using_alternative_and_or_renewable_energy
# "Select Month"= "[Select Month]",
df_households_using_alternative_and_or_renewable_energy <- queryTable("cjtzjlyomt",
                                                                      "Partner Name" = "a12499717150000000007.P00000102950000000001",
                                                                      "Location District" = "i0422696728.i0219912987",
                                                                      "Location Type" = "i0422696728.i0610240747",
                                                                      "Location Name" = "i0422696728.i0098724746",
                                                                      "Location description (e.g Zone)" = "i1136059363",
                                                                      "Parent ID" = "@parent",
                                                                      "Select Month"= "[Select Month]",
                                                                      "Select Quarter" = "cvapd3okmvyzvbly",
                                                                      "Enabling access alternative and/or renewable energy" = "Q0442703671",
                                                                      "Select Activity Status" = "Q0456939380",
                                                                      "Select Implementation Type" = "Q0456362257",
                                                                      "Select Funding Source(s)" = "Q0449436776",
                                                                      "Please specify funding source(s):" = "i0447513032",
                                                                      "Select Beneficiary Type" = "Q0447320657",
                                                                      "Select Modality" = "Q0451552895",
                                                                      "Select Assistance Type" = "Q0451168146",
                                                                      "Delivery mechanism" = "Q0450591023",
                                                                      "Financial service provider" = "i0442896045",
                                                                      "Households reached" = "i0440779926",
                                                                      "briquettes given (kg)" = "i0441357049",
                                                                      "Trainings done" = "i0441164675",
                                                                      "Individuals reached (Female <15)" = "i0446358785",
                                                                      "Individuals reached (Male <15)" = "i0446166411",
                                                                      "Individuals reached (Female 15-17)" = "i0446743534",
                                                                      "Individuals reached (Male 15-17)" = "i0445589287",
                                                                      "Individuals reached (Female 18-35)" = "i0445396913",
                                                                      "Individuals reached (Male 18-35)" = "i0445974036",
                                                                      "Individuals reached (Female 36-59)" = "i0445781662",
                                                                      "Individuals reached (Male 36-59)" = "i0444819790",
                                                                      "Individuals reached (Female 60+)" = "i0444627415",
                                                                      "Individuals reached (Male 60+)" = "i0445204538",
                                                                      "Individuals reached (Total)" = "i0738878312",
                                                                      "Enabling market-based energy kiosks" = "i0444050292",
                                                                      "Cash value of transfer per day per beneficiary (UGX)" = "i0443857917",
                                                                      "Total cash value of cash for work (UGX)" = "i0444242666",
                                                                      "Comments" = "i0289381234",
                                                                      "Record change/comment log (please indicate date, name and comment e.g. 01/07/2019 - John Doe - Record validated)" = "i2091544592")

# write out the dataset
write_csv(x = df_households_using_alternative_and_or_renewable_energy , file = "data/api_env_households_using_alternative_and_or_renewable_energy.csv")

# households_that_self_report_using_fuel_efficient_cook_stove_to_cook_the_main_meal
# "Select Month"= "[Select Month]",
df_households_using_fuel_efficient_cook_stove_to_cook <- queryTable("cjtzjlyoes",
                                                                    "Partner Name" = "a12499717150000000007.P00000102950000000001",
                                                                    "Location District" = "i0422696728.i0219912987",
                                                                    "Location Type" = "i0422696728.i0610240747",
                                                                    "Location Name" = "i0422696728.i0098724746",
                                                                    "Location description (e.g Zone)" = "i1136059363",
                                                                    "Parent ID" = "@parent",
                                                                    "Select Month"= "[Select Month]",
                                                                    "Select Quarter" = "co0w4rkmvyxf71p",
                                                                    "Energy saving practices/activities promoted" = "Q0459824997",
                                                                    "Type of Energy Saving stove" = "Q1509656301",
                                                                    "Select Implementation Type" = "Q0418849239",
                                                                    "Select Funding Source(s)" = "Q0417117869",
                                                                    "Please specify funding source(s):" = "i0421350107",
                                                                    "Select Beneficiary Type" = "Q0421157732",
                                                                    "Select Modality" = "Q0419426362",
                                                                    "Select Assistance Type" = "Q0461941116",
                                                                    "Delivery mechanism" = "Q0461363992",
                                                                    "Financial service provider" = "i0459247873",
                                                                    "Households reached" = "i0464441983",
                                                                    "Extension workers" = "i0361744937",
                                                                    "Trainings done (TOTs)" = "i0464249609",
                                                                    "Competititons/Demonstrations (events)" = "i0463095362",
                                                                    "Individuals supported/trained (Female <15)" = "i0463672486",
                                                                    "Individuals supported/trained (Male <15)" = "i0463480111",
                                                                    "Individuals supported/trained (Female 15-17)" = "i0462518239",
                                                                    "Individuals supported/trained (Male 15-17)" = "i0462902988",
                                                                    "Individuals supported/trained (Female 18-35)" = "i0462710613",
                                                                    "Individuals supported/trained (Male 18-35)" = "i0455592759",
                                                                    "Individuals supported/trained (Female 36-59)" = "i0455400384",
                                                                    "Individuals supported/trained (Male 36-59)" = "i0455977508",
                                                                    "Individuals supported/trained (Female 60+)" = "i0455785133",
                                                                    "Individuals supported/trained (Male 60+)" = "i0454823261",
                                                                    "Individuals supported/trained (Total)" = "i0163547629",
                                                                    "Cash value of transfer per day per beneficiary (UGX)" = "i0454630887",
                                                                    "Total cash value of cash for work (UGX)" = "i0454053763",
                                                                    "Comments" = "i0453861389",
                                                                    "Record change/comment log (please indicate date, name and comment e.g. 01/07/2019 - John Doe - Record validated)" = "i1089351973")

# write out the dataset
write_csv(x = df_households_using_fuel_efficient_cook_stove_to_cook, file = "data/api_env_households_that_self_report_using_fuel_efficient_cook_stove_to_cook_the_main_meal.csv")


# LIVELIHOODS & RESILIENCE (PARTNERS) --------------------------------------------------------------

# increased_access_to_productive_assets
# using only Select Quarter
df_increased_access_to_productive_assets <- queryTable("cjtx2djzd4",
                                                       "Partner Name" = "a19646060330000000007.P00000102950000000001",
                                                       "Location District" = "i1410730250.i0219912987",
                                                       "Location Type" = "i1410730250.i0610240747",
                                                       "Location Name" = "i1410730250.i0098724746",
                                                       "Please specify location (if needed)" = "i1574082696",
                                                       "Parent ID" = "@parent",
                                                       "Select Quarter" = "Q1410153127",
                                                       "Select Activity Status" = "Q1401111528",
                                                       "Select Implementation Type" = "Q1400342030",
                                                       "Select Funding Source(s)" = "Q1405728515",
                                                       "Please specify funding source(s):" = "i1403804770",
                                                       "Select Beneficiary Type" = "Q1445934775",
                                                       "Select Modality" = "Q1444972903",
                                                       "Please specify other Modality" = "i0955651763",
                                                       "Select Assistance Type" = "Q1444588154",
                                                       "Delivery mechanism" = "Q1444011030",
                                                       "Financial service provider" = "i1448628017",
                                                       "Agriculture inputs distributed (seeds in tons)" = "i1448435643",
                                                       "Agriculture inputs distributed (agriculture tools)" = "i1447473770",
                                                       "Agriculture inputs distributed (fertilizer in tons)" = "i1447281396",
                                                       "Agriculture inputs distributed (livestock - animals)" = "i1447858519",
                                                       "Agriculture inputs distributed (feeds in tons)" = "i1447666145",
                                                       "Agriculture inputs distributed (vaccines)" = "i1446704273",
                                                       "Agriculture inputs distributed (crop pest/disease)" = "i1447089022",
                                                       "HH having received productive assets (mechanics tools)" = "i1446896647",
                                                       "HH having received cash grant for productive assets" = "i1439778792",
                                                       "Cash value of transfer per HH (UGX)" = "i1439586418",
                                                       "Individuals receiving cash for work" = "cvdb5g5kmx542638",
                                                       "Cash value of transfer per person / per day (in UGX)" = "cgduddykmx510qj6",
                                                       "Number of labour / man days" = "clq1hd9kmx524du7",
                                                       "Total cash value of grants distributed for productive assets (UGX)" = "i1440163541",
                                                       "HH receiving emergency livelihood support" = "i1439971167",
                                                       "Market assessments completed" = "i1438816920",
                                                       "Comments" = "i1439394044")


# write out the dataset
write_csv(x = df_increased_access_to_productive_assets, file = "data/api_live_ELS_Increased_access_to_productive_assetss.csv")


# increased_access_to_short_term_employment_opportunities

df_access_to_short_term_employment_opportunities <- queryTable("cjtx2dk056",
                                                               "Partner Name" = "a19646060330000000007.P00000102950000000001",
                                                               "Location District" = "i1410730250.i0219912987",
                                                               "Location Type" = "i1410730250.i0610240747",
                                                               "Location Name" = "i1410730250.i0098724746",
                                                               "Please specify location (if needed)" = "i1574082696",
                                                               "Parent ID" = "@parent",
                                                               "Select Quarter" = "Q1267411285",
                                                               "Select Activity Status" = "Q1266257039",
                                                               "Select Implementation Type" = "Q1264910417",
                                                               "Select Funding Source(s)" = "Q1270296902",
                                                               "Please specify funding source(s):" = "i1268373157",
                                                               "Select Beneficiary Type" = "Q1261255303",
                                                               "Select Modality" = "Q1260870554",
                                                               "Please specify other Modality" = "i0723486712",
                                                               "Select Assistance Type" = "Q1259908682",
                                                               "Delivery mechanism" = "Q1259139184",
                                                               "Financial service provider" = "i1263948545",
                                                               "People engaged in short-term employment opportunities (Female 0-14)" = "i1263756171",
                                                               "People engaged in short-term employment opportunities (Male 0-14)" = "i1262601924",
                                                               "People engaged in short-term employment opportunities (Female 15-17)" = "i1263179047",
                                                               "People engaged in short-term employment opportunities (Male 15-17)" = "i1262986673",
                                                               "People engaged in short-term employment opportunities (Female 18-35)" = "i1261832426",
                                                               "People engaged in short-term employment opportunities (Male 18-35)" = "i1262409549",
                                                               "People engaged in short-term employment opportunities (Female 36-59)" = "i1262217175",
                                                               "People engaged in short-term employment opportunities (Male 36-59)" = "i1254906946",
                                                               "People engaged in short-term employment opportunities (Female 60+)" = "i1255484069",
                                                               "People engaged in short-term employment opportunities (Male 60+)" = "i1255291695",
                                                               "People engaged in short-term employment opportunities (Total)" = "i1945479867",
                                                               "Cash value of transfer per individual / per day (Male) - UGX" = "i1254329823",
                                                               "Cash value of transfer per individual / per day (Female) - UGX" = "i1254137448",
                                                               "Cash value of transfer per beneficiary (Total) - UGX" = "i0248275848",
                                                               "Number of labour / man days" = "cyq39srkmw6gj6b3",
                                                               "Total cash value of cash for work (UGX)" = "i1254714571",
                                                               "Livelihood public work activity (irrigation activities)" = "i1254522197",
                                                               "Livelihood public work activity (market infrastructure)" = "i1253560325",
                                                               "Education public work activity" = "i1253945074",
                                                               "Energy & Environment public work activity" = "i1253752699",
                                                               "Health public work activity" = "i1252790827",
                                                               "Shelter, Settlements and NFI public work activity" = "i1252983201",
                                                               "WASH public work activity" = "i1257984937",
                                                               "Comments" = "i1258562060")


# write out the dataset
write_csv(x = df_access_to_short_term_employment_opportunities, file = "data/api_live_ELS_Increased_access_to_short_term_employment_opportunities.csv")


# Shelter, Settlement & NFI (Partners) ------------------------------------

# refugee_households_with_specific_needs_assisted_with_semi-permanent_shelters
# "Select Month"= "[Select Month]",
df_refugee_hh_assisted_with_semi_permanent_shelters <- queryTable("ck53qjaq37",
                                                                  "Partner Name" = "a18552326900000000007.P00000102950000000001",
                                                                  "Location District" = "i1902739576.i0219912987",
                                                                  "Location Type" = "i1902739576.i0610240747",
                                                                  "Location Name" = "i1902739576.i0098724746",
                                                                  "Please specify location (if needed)" = "i0164298466",
                                                                  "Parent ID" = "@parent",
                                                                  "Select Month"= "[Select Month]",
                                                                  "Select Quarter" = "c5vocfqkmvtkm467",
                                                                  "Select Activity Status" = "Q1934288986",
                                                                  "Select Implementation Type" = "Q1934481360",
                                                                  "Select Funding Source(s)" = "Q1933711862",
                                                                  "Please specify funding source(s):" = "i1936597479",
                                                                  "Select Beneficiary Type" = "Q1936405105",
                                                                  "Select PSN Type" = "Q0344321418",
                                                                  "Select Modality" = "Q1930056748",
                                                                  "Select Shelter Assistance Category" = "Q1821173714",
                                                                  "Select Assistance Type" = "Q1929094876",
                                                                  "Select Delivery Mechanism" = "Q1928517752",
                                                                  "Please specify financial service provider" = "i1933134739",
                                                                  "Construction of HH semi-permanent PSN shelters (Child at risk)" = "i1931980492",
                                                                  "Construction of HH semi-permanent PSN shelters (Disability)" = "i1931788118",
                                                                  "Construction of HH semi-permanent PSN shelters (Older person at risk)" = "i1932172867",
                                                                  "Construction of HH semi-permanent PSN shelters (Serious medical condition)" = "i1931210994",
                                                                  "Construction of HH semi-permanent PSN shelters (Single parent)" = "i1931595743",
                                                                  "Construction of HH semi-permanent PSN shelters (Specific legal and physical protection needs)" = "i1931403369",
                                                                  "Construction of HH semi-permanent PSN shelters (Unaccompanied or separated child)" = "i1930441497",
                                                                  "Construction of HH semi-permanent PSN shelters (Woman at risk)" = "i1930249122",
                                                                  "Construction of HH semi-permanent PSN shelters (total)" = "i1932942365",
                                                                  "Repairs to existing semi-permanent PSN shelters (Child at risk)" = "i1930633871",
                                                                  "Repairs to existing semi-permanent PSN shelters (Disability)" = "i1972763876",
                                                                  "Repairs to existing semi-permanent PSN shelters (Older person at risk)" = "i1972571501",
                                                                  "Repairs to existing semi-permanent PSN shelters (Serious medical condition)" = "i1973148625",
                                                                  "Repairs to existing semi-permanent PSN shelters (Single parent)" = "i1972956250",
                                                                  "Repairs to existing semi-permanent PSN shelters (Specific legal and physical protection needs)" = "i1971994378",
                                                                  "Repairs to existing semi-permanent PSN shelters (Unaccompanied or separated child)" = "i1971802003",
                                                                  "Repairs to existing semi-permanent PSN shelters (Woman at risk)" = "i1972379127",
                                                                  "Repairs to existing semi-permanent PSN shelters (total)" = "i1930826246",
                                                                  "PSN households receiving cash-based assistance for semi-permanent shelter (Child at risk)" = "i1971224880",
                                                                  "PSN households receiving cash-based assistance for semi-permanent shelter (Disability)" = "i1971032506",
                                                                  "PSN households receiving cash-based assistance for semi-permanent shelter (Older person at risk)" = "i1971609629",
                                                                  "PSN households receiving cash-based assistance for semi-permanent shelter (Serious medical condition)" = "i1971417255",
                                                                  "PSN households receiving cash-based assistance for semi-permanent shelter (Single parent)" = "i1970455382",
                                                                  "PSN households receiving cash-based assistance for semi-permanent shelter (Specific legal and physical protection needs)" = "i1970263008",
                                                                  "PSN households receiving cash-based assistance for semi-permanent shelter (Unaccompanied or separated child)" = "i1970840131",
                                                                  "PSN households receiving cash-based assistance for semi-permanent shelter (Woman at risk)" = "i1970647757",
                                                                  "PSN households receiving cash-based assistance for semi-permanent shelter (total)" = "i1972186752",
                                                                  "Cash value per transfer per HH" = "i1975841867",
                                                                  "Total amount of semi-permanent PSN shelter cash transfers" = "i1975649492",
                                                                  "Comments" = "i1976034241")

# write out the dataset
write_csv(x = df_refugee_hh_assisted_with_semi_permanent_shelters, file = "data/api_shel_refugee_hh_assisted_with_semi_permanent_shelters.csv")


# newly_arrived_refugee_households_provided_with_minimum_emergency_shelter_support
# "Select Month"= "[Select Month]",
########## ########## ########## ########## cash fields empty
df_newly_arrived_refugee_hh_with_minimum_emergency_shelter <- queryTable("ck53qjaq68",
                                                                         "Partner Name" = "a18552326900000000007.P00000102950000000001",
                                                                         "Location District" = "i1902739576.i0219912987",
                                                                         "Location Type" = "i1902739576.i0610240747",
                                                                         "Location Name" = "i1902739576.i0098724746",
                                                                         "Please specify location (if needed)" = "i0164298466",
                                                                         "Parent ID" = "@parent",
                                                                         "Select Month"= "[Select Month]",
                                                                         "Select Quarter" = "citgsgakmvtlkjp7",
                                                                         "Select Activity Status" = "Q1964684149",
                                                                         "Select Implementation Type" = "Q1970070633",
                                                                         "Select Funding Source(s)" = "Q1969301136",
                                                                         "Please specify funding source(s)" = "i1959682413",
                                                                         "Select Beneficiary Type" = "Q1959490039",
                                                                         "Select Modality" = "Q1959297664",
                                                                         "Select Assistance Type" = "Q1958335792",
                                                                         "Select Delivery Mechanism" = "Q1963914651",
                                                                         "Please specify financial service provider" = "i1962375655",
                                                                         "New arrival HH emergency shelter kits distributed" = "i1961221409",
                                                                         "Households receiving emergency shelter-earmarked cash assistance" = "i1961029034",
                                                                         "Cash value per transfer per HH" = "i1961606158",
                                                                         "Total amount of emergency shelter-earmarked cash transfers" = "i1961413783",
                                                                         "Comments" = "i1954295928")

# write out the dataset
write_csv(x = df_newly_arrived_refugee_hh_with_minimum_emergency_shelter, file = "data/api_shel_newly_arrived_refugee_hh_with_minimum_emergency_shelter.csv")


# WASH --------------------------------------------------------------------
# CBI_approach_in_sanitation_services

df_CBI_approach_in_sanitation_services <- queryTable("cjtxwsbul8",
                                                     "Partner Name" = "a00824189710000000007.P00000102950000000001",
                                                     "Location District" = "i1288780922.i0219912987",
                                                     "Location Type" = "i1288780922.i0610240747",
                                                     "Location Name" = "i1288780922.i0098724746",
                                                     "Please specify location (if needed)" = "i0426642098",
                                                     "Parent ID" = "@parent",
                                                     "Start Date" = "i1325716816",
                                                     "End Date" = "i1326293940",
                                                     "Select Activity Status" = "Q1325139693",
                                                     "Select Implementation Type" = "Q1324370195",
                                                     "Select Funding Source(s)" = "Q1329756680",
                                                     "Please specify funding source(s):" = "i1327832935",
                                                     "Select Beneficiary Type" = "Q1327640561",
                                                     "Select Modality" = "Q1319560834",
                                                     "Please specify other modality" = "i1788644606",
                                                     "Select Assistance Type" = "Q1319176085",
                                                     "Delivery mechanism" = "Q1318598962",
                                                     "Financial service provider" = "i1322638825",
                                                     "Households receiving cash assistance for sanitation services" = "i1323215949",
                                                     "Cash value of transfer per HH (UGX)" = "i1323023574",
                                                     "Total cash value of cash grants (UGX)" = "i1322061702",
                                                     "Comments" = "i1322446451")


# write out the dataset
write_csv(x = df_CBI_approach_in_sanitation_services, file = "data/api_wash_CBI_approach_in_sanitation_services.csv")


# CBI_approach_in_WASH_NFI

df_CBI_approach_in_WASH_NFI <- queryTable("cjtxwsbub6",
                                          "Partner Name" = "a13882245390000000007.P00000102950000000001",
                                          "Location District" = "i0084593486.i0219912987",
                                          "Location Type" = "i0084593486.i0610240747",
                                          "Location Name" = "i0084593486.i0098724746",
                                          "Please specify location (if needed)" = "i0849550098",
                                          "Parent ID" = "@parent",
                                          "Start Date" = "i1304940376",
                                          "End Date" = "i1305517499",
                                          "Select Activity Status" = "Q1304363252",
                                          "Select Implementation Type" = "Q1303593755",
                                          "Select Funding Source(s)" = "Q1302824257",
                                          "Please specify funding source(s):" = "i1294167407",
                                          "Select Beneficiary Type" = "Q1294744530",
                                          "Select Modality" = "Q1298976768",
                                          "Please specify other modality" = "i0002987473",
                                          "Select Assistance Type" = "Q1298014896",
                                          "Delivery mechanism" = "Q1297437772",
                                          "Financial service provider" = "i1296860649",
                                          "Households receiving cash assistance for WASH NFI" = "i1289742794",
                                          "Cash value of transfer per HH (UGX)" = "i1289550420",
                                          "Total cash value of cash grants (UGX)" = "i1289935169",
                                          "Comments" = "i1288973296")

# write out the dataset
write_csv(x = df_CBI_approach_in_WASH_NFI, file = "data/api_wash_CBI_approach_in_WASH_NFI.csv")


# Food Security -----------------------------------------------------------

df_food_security <- queryTable("a1695644495",
                               "Partner Name" = "a16956444950000000007.P00000102950000000001",
                               "Location District" = "i0608101100.i0219912987",
                               "Location Type" = "i0608101100.i0610240747",
                               "Location Name" = "i0608101100.i0098724746",
                               "Select Quarter" = "Q0606946853",
                               "Select Beneficiary Type" = "Q0599636624",
                               "Select Modality" = "Q1777032217",
                               "Select Assistance Type" = "Q1369521836",
                               "Select Delivery Mechanism" = "Q1430709506",
                               "FS.I.1.1: Refugees receiving in-kind food assistance" = "i0597712879",
                               "FS.I.1.2: Refugees receiving cash" = "i0598097628",
                               "FS.I.1.2: Refugees receiving cash (cash value per transfer)" = "i0306098862",
                               "FS.I.1.2: Refugees receiving cash (total amount of cash transfers)" = "i0085837779",
                               "FS.I.1.3: HH with poor or borderline Food Consumption Score (<20%)" = "i2136535729",
                               "FS.I.2.1: Refugee households receiving targeted assistance" = "i0597905254",
                               "FS.I.2.2: HH with poor or borderline Food Consumption Score (PSN)" = "i0795116351",
                               "FS.I.2.3: Coping Strategy Score of targeted refugee households (EVIs, PSN)" = "i1177126036",
                               "FS.I.3.1: Refugees receiving food assistance and participating in livelihood programmes" = "i0596943381",
                               "FS.I.3.2: Host population participating in refugee livelihood activities" = "i0596751007",
                               "Comments" = "i0597328130")

# write out the dataset
write_csv(x = df_food_security, file = "data/api_fs_Food_Security.csv")

