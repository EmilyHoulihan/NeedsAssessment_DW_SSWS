#2023 Needs Assessment for Domestic Wells and State Small Water Systems

#Please refer to the SWRCB Needs Assessment webpage for methodology write-up and
#access to additional data.

#Updated 2/16/2023 by Emily Houlihan (Emily.Houlihan@Waterboards.ca.gov)

#This script is provided as a documentation of methodology steps. If you would like to 
#re-run this script, you will need additional reference files. Please check
#the Needs Assessment webpage, or contact Emily Houlihan.

#install libraries
library(tidyverse)
library(ggplot2)
library(sf)

#import data ####
#water quality data (Aquifer Risk Map)
wq <- read.csv("NA2023/arm2023.csv")

#water shortage data (Water Shortage Vulnerability Tool - Physical Vulnerability by Sections)
#https://data.cnra.ca.gov/dataset/i07-water-shortage-vulnerability-sections
ws <- read.csv("NA2023/i07wsvs.csv") %>%
  #only keep necessary columns
  select(c("MTRS", "rRC1a_Temperature_Change", "rRC1b_Sea_Level_Rise", "rRC1c_Wildfire_Projections", 
           "rRC2aa_Multiple_Dry_Years", "rRC2b_calfire", "rRC2c_Fractured_Rock_Area", "rRC2d_Subsidence", 
           "rRC2e_Saltwater_Intrusion", "rRC2f_Critically_Overdrafted", "rRC2g_Groundwater_Decline", 
           "rRC2i_SWRCB_Water_Quality_Ris", "rRC2j_Percent_Farmed_Score", "rRC3a_Well_Susceptibility", 
           "rRC3c_FRA_Dry_Wells", "rRC5a_Household_Water_Outage", "Total_Score_NewScalebyMax"))

#socioeconomic data (from OEHHA)
se <- read.csv("NA2023/FinalAffIndicators_withFinalAffScore_SendtoWB01112023.csv") %>% distinct() %>%
  select(-c("Aff_Final_top10cat", "Aff_Final_top20cat", "Aff_Sc_Sum", "TotalInd"))

#calculate which sections have a domestic well or state small water system
domesticwellsections <- wq %>% filter(DWR_dm_ > 0 | SSWS_2023 > 0)

#calculate water quality thresholds ####
wq <- wq %>% select(NAME, MTRS, GSA_ID, GSA_Name, Basin_Name, Basin_Subb, Basin_Su_1,
                    WQ_2023, SL1, SL2, DWR_dm_, SSWS_2023) %>%
  mutate(WQ_2023s = case_when(
    WQ_2023 == "high" ~ 1,
    WQ_2023 == "medium" ~ 0.25,
    WQ_2023 == "low" ~ 0),
    #apply category weight of 2
    WQ_2023s_wt = (2*WQ_2023s)) %>% distinct()

#water shortage thresholds (limiting percentile comparisons to only areas with DW/SSWS) ####
ws_lim <- ws %>% 
  filter(!MTRS %in% c("BAY/DELTA", "LAKETAHOE", "SALTONSEA")) %>%
  filter(MTRS %in% domesticwellsections$MTRS) %>%
  select(MTRS, Total_Score_NewScalebyMax) %>% distinct() %>%
  mutate(percentile = ntile(Total_Score_NewScalebyMax, 100)) 
ws <- ws %>% filter(!MTRS %in% c("BAY/DELTA", "LAKETAHOE", "SALTONSEA")) %>% distinct() %>%
  #calculate water shortage scores based on percentile distribution of areas with a domestic well
  mutate(WS_2023s_top20cat = case_when(
      Total_Score_NewScalebyMax >= min(ws_lim$Total_Score_NewScalebyMax[ws_lim$percentile == 80]) ~ 1,
      Total_Score_NewScalebyMax >= min(ws_lim$Total_Score_NewScalebyMax[ws_lim$percentile == 60]) ~ 0.25,
      Total_Score_NewScalebyMax < min(ws_lim$Total_Score_NewScalebyMax[ws_lim$percentile == 60]) ~ 0),
    #apply weights to water shortage scores
    WS_2023s20_wt = WS_2023s_top20cat*2)
  
#combined risk score calculations####
#join water quality, water shortage, and socioeconomic data tables
na2023 <- left_join(wq, ws) %>%
  left_join(., se)
#define columns with weighted category scores
mathcolsB <- c("WQ_2023s_wt", "WS_2023s20_wt", "Aff_FinalSc_top20cat")
#sum weighted category scores and divide by the number of categories with data
na2023 <- na2023 %>% rowwise() %>%
  mutate(valid_indicatorsB = sum(!is.na(c_across(c(mathcolsB)))),
         crB = sum(c_across(c(mathcolsB)), na.rm = T)/valid_indicatorsB,
         crRisk = case_when(
           crB >= 1 ~ "At-risk",
           crB >= 0.5 ~ "Potentially At-risk",
           crB >= 0 ~ "Not At-risk"))

importantcols <- c("NAME", "MTRS", "GSA_ID", "GSA_Name", "Basin_Name", "Basin_Subb", "Basin_Su_1", "DWR_dm_", "SSWS_2023", 
                   "WQ_2023s_wt", "WQ_2023", "SL1", "SL2",
                   "Total_Score_NewScalebyMax","WS_2023s20_wt", 
                   "Aff_FinalSc_top20cat", "valid_indicatorsB", "crB", "crRisk")

na2023_sm <- na2023[!duplicated(na2023[,c(importantcols)]),]
na2023_export <- na2023_sm %>% ungroup() %>% data.frame() %>%
  mutate(UniqueName = paste0(NAME, MTRS)) %>%
  select(-c(WQ_2023s_wt, WS_2023s20_wt))

#import distance to CWS
dw_disttoCWS <- read.csv("NA2023/PSW join/DW_Distance_to_CWS.csv") %>% select(-c("Relation_to_CWS"))
#ssws_disttoCWS <- read.csv("NA2023/PSW join/SSWs_Distance_to_CWS.csv") %>% select(-c("Relation_to_CWS"))

na2023_export <- left_join(na2023_export, dw_disttoCWS, by = c("UniqueName" = "UniquNm")) %>%
  mutate(Relation_to_CWS = case_when(
    NEAR_DIST > 3 ~ "d3",
    NEAR_DIST > 1 ~ "d2",
    NEAR_DIST > 0 ~ "d1",
    NEAR_DIST == 0 ~ "d0"))

#write shapefile and .csv
MTRS <- st_read("Publishing/WQ_SectionRisk_2023_AquiferRiskMap.shp") %>% 
  select(MTRS, NAME) #%>% st_transform(crs = 4326)
write.csv(na2023_export, paste0("NA2023/na2023_dwssws", Sys.Date(), ".csv"), row.names = F, na = "")
na2023_ex_shp <- left_join(MTRS, na2023_export)
st_write(na2023_ex_shp, paste0("NA2023/na23_dwssws", Sys.Date(), ".shp"))
