############ Reads in the raw health board DCRS dataset.  
############ Standard values used throughout the report are set before Scotland and health board level figures are aggregated 
############ Pre combined hospital/hospice data is also read in and standard values and aggregates are also set

Board <- hdr

# load DCRS HB data extracted from Dynamics

dcrs_raw <- read_csv("Data/DCRS-HBData.csv")


# create standard values from dataset -------------------------------------

#Set required extra values for identifying case types
dcrs_data <- dcrs_raw %>%
  mutate(NiO = case_when(`Case Status` == "Case not in Order" ~ "1",
                         `Case Status` == "Reported to PF" ~ "PF",
                         `Case Status` == "Case in Order" ~ "0" ),
         #identify electronic (1) or paper cases (0) using serial number
         emccd = case_when(substr(`Serial Number (MCCD)`,1,1) >= "5" & nchar(`Serial Number (MCCD)`) == 8 ~ 1,
                           TRUE ~ 0),
         #simplify outcome for reporting
         outcome = case_when(`Case Status` == "Case in Order" ~"No issues",
                             substr(`MCCD Status`,1,5) == "Email" | substr(`MCCD Status`,1,5) == "MCCD " ~"Email",
                             substr(`MCCD Status`,1,5) == "Repla" ~ "Replacement",
                             TRUE ~ `Case Status`),
         #identify as primary or secondary care case
         primary_secondary = case_when(placeofdeathcheck == "Community" ~ "Primary",
                                       placeofdeathcheck == "Hospital" | placeofdeathcheck == "Hospice"  ~ "Secondary",
                                       TRUE ~ "Uknown"),
         #Set certain financial years for reporting (1 is rarely used in this report)
         `Created On` = as.Date(`Created On`, "%d/%m/%Y"),
# update dates ------------------------------------------------------------
         Year = case_when(`Created On` >= "2022-04-01" & `Created On` <= "2023-03-31" ~ 1, 
                          `Created On` >= "2023-04-01" & `Created On` <= "2024-03-31" ~ 2,
                          `Created On` >= "2024-04-01" & `Created On` <= "2025-03-31" ~ 3))



# get aggregates for boards and years -------------------------------------

#set overall data totals for table 1

#Create board level
dcrs_data_total <- dcrs_data %>%
  filter(`Case Type` != "Enquiry" | `Case Type` != "Repatriation",
         `Created On` < end_date) %>%
  group_by(Year, `Health Board`) %>%
  summarise(total_standard = sum(`Case Type` == "Standard"),
            total_interested = sum(`Case Type` == "Interested Person"),
            total_registrar = sum(`Case Type` == "Registrar Referral"),
            total_for_cause = sum(`Case Type` == "For Cause")
            , .groups = "rowwise") %>%
  filter(Year == 1 | Year == 2 | Year == 3)

#Create Scotland level
dcrs_data_scotland_total <- dcrs_data_total %>% 
  group_by(Year) %>% 
  summarise_if(is.numeric, sum) %>%
  mutate(`Health Board` = 'Scotland')

#Combine board and Scotland level
dcrs_data_total_all <- rbind(dcrs_data_total %>% filter(`Health Board` == Board) %>% mutate(`Health Board` = 'Board'), dcrs_data_scotland_total)



# get aggregates for main values ------------------------------------------

#set data for main report

dcrs_data_report <- dcrs_data %>%
  filter(`Case Type` == "Standard",
         `Created On` < end_date) %>%
  group_by(Year, `Health Board`) %>%
  
            ###Table 2###
  summarise(total_in_order = sum(`Case Status` == "Case in Order"),
            total_not_in_order = sum(`Case Status` == "Case not in Order"),
            total_email_amendment = sum(substr(`MCCD Status`,1,5) == "Email" &
                                          `Case Status` == "Case not in Order"| 
                                          substr(`MCCD Status`,1,4) == "MCCD" &
                                          `Case Status` == "Case not in Order", na.rm = TRUE),
            total_replacement_mccd = sum(substr(`MCCD Status`,1,5) == "Repla" &
                                           `Case Status` == "Case not in Order", na.rm = TRUE),
            total_report_to_pf = sum(`Case Status` == "Reported to PF", na.rm = TRUE),
            
            ###Table 3###
            total_emccd = sum(emccd == 1,  na.rm = TRUE),
            total_mccd = sum(emccd != 1, na.rm = TRUE),
            emccd_in_order = sum(`Case Status` == "Case in Order" & emccd == 1,  na.rm = TRUE),
            emccd_to_pf = sum(`Case Status` == "Reported to PF" & emccd == 1,  na.rm = TRUE),
            mccd_in_order = sum(`Case Status` == "Case in Order" & emccd != 1,  na.rm = TRUE),
            mccd_to_pf = sum(`Case Status` == "Reported to PF" & emccd != 1,  na.rm = TRUE),
            total_ex_pf = sum(NiO != "PF" & NiO != "", na.rm = TRUE),
            
            ###Table 4###
            total_breach = sum(NiO != "" & `Breach Reason` != "" & `Breach Reason` != "Reported to the PF" &
                                 `Breach Reason` != "Paper record is lost" , na.rm = TRUE),
            total_cd_unavailable = sum((NiO != "" & `Breach Reason` == "CD unavailable") |
                                         (NiO != "" & `Breach Reason` == "CD on A/L")  , na.rm = TRUE),
            total_breach_other = sum(NiO != "" & `Breach Reason` != "CD unavailable" & `Breach Reason` != "" &
                                       `Breach Reason` != "Reported to the PF" & `Breach Reason` != "CD on A/L", na.rm = TRUE),
            total_breach_delay = sum(NiO != "" & `Breach Reason` == "Delay in receiving medical notes" |
                                       `Breach Reason` == "Delay in obtaining additional information" |
                                       `Breach Reason` == "Delay in receiving email amendment/replacement", na.rm = TRUE),
            total_breach_dcrsdelay = sum(NiO != "" & `Breach Reason` == "DCRS delay", na.rm = TRUE),
            total_breach_dual =sum(NiO != "" & `Breach Reason` == "Dual delay", na.rm = TRUE),
            total_breach_otheralt = sum(NiO != "" & `Breach Reason` == "Other", na.rm = TRUE),
            
            ###Table 5###
            total_level = sum(Level != "", na.rm = TRUE),
            level_1 = sum(Level == "Level 1", na.rm = TRUE),
            level_2 = sum(Level == "Level 2", na.rm = TRUE),
            level_1_hybrid = sum(Level == "Level 1 Hybrid", na.rm = TRUE),
            escalated = sum(`Escalation Reason` != "", na.rm = TRUE),
            level_1_total = level_1 + level_1_hybrid + escalated,
            
            ###Chart 5 - Clinical error data###
            cause_too_vague = sum(`Cause of death too vague1` == "Yes", na.rm = TRUE),
            closure_category = sum(`Closure Category - Clinical1` == "Yes", na.rm = TRUE),
            closure_category_admin = sum(`Closure Category - Administrative1` == "Yes", na.rm = TRUE),
            causal_timscale = sum(`Causal timescales incorrect1` == "Yes", na.rm = TRUE),
            cause_of_death = sum(`Cause of Death incorrect1` == "Yes", na.rm = TRUE),
            conditions_omitted = sum(`Conditions omitted1` == "Yes", na.rm = TRUE),
            disposal_hazard = sum(`Disposal Hazard incorrect1` == "Yes", na.rm = TRUE),
            sequence_of_cause = sum(`Sequence of Cause of Death incorrect1` == "Yes", na.rm = TRUE),
            
            ###Chart 6 - Cause of death too vague data###        
            histology = sum(`Histology 1` == "Yes", na.rm = TRUE),
            psite_metastatic_missing = sum(`Primary site or metastatic site(s) missing1` == "Yes", na.rm = TRUE),
            pneumonia_subtype = sum(`Pneumonia sub-type1` == "Yes", na.rm = TRUE),
            dementia_subtype = sum(`Dementia sub-type1` == "Yes", na.rm = TRUE),
            microbiology = sum(`Microbiology 1` == "Yes", na.rm = TRUE),
            sepsis_source = sum(`Source of sepsis1` == "Yes", na.rm = TRUE),
            diabetes_subtype = sum(`Diabetes sub-type1` == "Yes", na.rm = TRUE),
            stroke = sum(`tn_ccstrokesubtype` == "Yes", na.rm = TRUE),
            lifestyle_factor = sum(`Lifestyle factors (smoking, obesity, alcohol)1` == "Yes", na.rm = TRUE),
            
            ###Chart 7 - Administrative error data###
            closure_category_admin = sum(`Closure Category - Administrative1` == "Yes", na.rm = TRUE),
            spelling_error = sum(`Certifying Doctor Spelling error1` == "Yes", na.rm = TRUE),
            deceased_details_incorrect = sum(`Deceased details incorrect1`== "Yes", na.rm = TRUE),
            doctors_details_incorrect = sum(`Certifying Doctor's details incorrect1`== "Yes", na.rm = TRUE),
            abbreviations_used = sum(`Abbreviations used 1` == "Yes", na.rm = TRUE),
            time_incorrect = sum(`Date or time of death incorrect1`== "Yes", na.rm = TRUE),
            incorrectly_complete = sum(`Extra information (X Box) incorrectly complete1` == "Yes", na.rm = TRUE),
            attendance_incorrect = sum(`Attendance on the deceased incorrect1`== "Yes", na.rm = TRUE),
            address_incorrect = sum(`Place of death address incorrect1` == "Yes", na.rm = TRUE),
            pm_information_incorrect = sum(`PM information incorrect1` == "Yes", na.rm = TRUE),
            legibility = sum(`Legibility  1` == "Yes", na.rm = TRUE),
            consultant_incorrect = sum(`Consultant's name incorrect1` == "Yes", na.rm = TRUE),
            other_incorrect = sum(`Other Additional information incorrect1` == "Yes", na.rm = TRUE),
            
            ###Chart 8 - Report to PF data###
            total_report_to_pf = sum(`Case Status` == "Reported to PF", na.rm = TRUE),
            choking = sum(`tn_ccchoking` == "Yes", na.rm = TRUE),
            concerns_over_care = sum(`tn_ccconcernsovercare` == "Yes", na.rm = TRUE),
            drug_related = sum(`tn_ccdrugrelated` == "Yes", na.rm = TRUE),
            neglect_exposure = sum(`tn_ccneglectorlonglie` == "Yes", na.rm = TRUE),
            fracture_trauma = sum(`tn_ccfractureortrauma` == "Yes", na.rm = TRUE),
            industrial_disease = sum(`tn_ccindustrialdisease` == "Yes", na.rm = TRUE),
            infectious_disease = sum(`tn_ccinfectiousdisease` == "Yes", na.rm = TRUE),
            legal_order = sum(`tn_cclegalorder` == "Yes", na.rm = TRUE),
            flagged_error = sum(`tn_ccflaggedinerror` == "Yes", na.rm = TRUE),
            other_report_pf = sum(`tn_ccotherreporttopf` == "Yes", na.rm = TRUE)
            , .groups = "rowwise") %>%
  filter(Year == 2 | Year == 3)

#Create Scotland level
dcrs_data_report_scotland <- dcrs_data_report %>% 
  group_by(Year) %>% 
  summarise_if(is.numeric, sum) %>%
  mutate(`Health Board` = 'Scotland')

#Combine both
dcrs_data_report_all <- rbind(dcrs_data_report %>% filter(`Health Board` == Board) %>% mutate(`Health Board` = 'Board'), dcrs_data_report_scotland)


# get aggregates for enquiry data -----------------------------------------

###Chart 9 -   set enquiries data

dcrs_data_enquiry <- dcrs_data %>%
  filter(`Case Type` == "Enquiry",
         `Enquiry Category` != "Interested Person" & `Enquiry Category` != "Registrar" & `Enquiry Category` != "Repatriation",
         `Created On` < end_date) %>%
  group_by(Year, `Health Board`) %>%
  summarise(gp_clinical_advice = sum(`Enquiry Category` == "GP Clinical advice", na.rm = TRUE),
            gp_process_advice = sum(`Enquiry Category` == "GP Process advice", na.rm = TRUE),
            hospital_clinical_advice = sum(`Enquiry Category` == "Hospital Clinical advice", na.rm = TRUE),
            hospital_process_advice = sum(`Enquiry Category` == "Hospital Process advice", na.rm = TRUE),
            hospice_clinical_advice = sum(`Enquiry Category` == "Hospice Clinical advice", na.rm = TRUE),
            hospice_process_advice = sum(`Enquiry Category` == "Hospice Process advice", na.rm = TRUE),
            funeral_director = sum(`Enquiry Category` == "Funeral Director", na.rm = TRUE),
            informant_or_family = sum(`Enquiry Category` == "Informant or family", na.rm = TRUE),
            procurator_fiscal = sum(`Enquiry Category` == "Procurator Fiscal", na.rm = TRUE),
            signposted = sum(`Enquiry Category` == "Signposted", na.rm = TRUE),
            other = sum(`Enquiry Category` == "Other", na.rm = TRUE),
            enquiry_category_total = sum(`Enquiry Category` != "" , na.rm = TRUE)
            , .groups = "rowwise") %>%
  filter(Year == 2 | Year == 3)

#Create Scotland level
dcrs_data_scotland_enquiry <- dcrs_data_enquiry %>% 
  group_by(Year) %>% 
  summarise_if(is.numeric, sum) %>%
  mutate(`Health Board` = 'Scotland')

#Combine both
dcrs_data_enquiry_all <- rbind(dcrs_data_enquiry %>% filter(`Health Board` == Board) %>% mutate(`Health Board` = 'Board'), dcrs_data_scotland_enquiry)


#KIS Checks

#Identify all cases with no KIS and remove. Get total with KIS
dcrs_data_kis <- dcrs_data %>%
 filter(`Created On` < end_date,
        `Case Type` != "Repatriation",
        kischeckstatus != "No data",
        kischecknotes != "") %>%
 mutate(kischecknotes = iconv(paste(kischecknotes), from="UTF-8", to="UTF-8", sub="NA"), 
        kischecknotes = tolower(kischecknotes),
        kis_flag = case_when(str_detect(`kischecknotes`, fixed("no kis")) ~ 1,              #cases with "no kis" anywhere
                             str_detect(`kischecknotes`, fixed("no chi")) ~ 1,              #cases with "no chi" anywhere
                             str_detect(`kischecknotes`, fixed("no information")) ~ 1,      #cases with "no information" anywhere
                             str_detect(`kischecknotes`, fixed("no record")) ~ 1,           #cases with "no record" anywhere
                             str_detect(`kischecknotes`, fixed("patient not found")) ~ 1,   #cases with "patient not found" anywhere
                             str_detect(`kischecknotes`, fixed("no data")) ~ 1,             #cases with "no data" anywhere
                             substr(`kischecknotes`,1,6) == "no kis" ~ 1,                   #cases with "no kis" at the start with additional text
                             substr(`kischecknotes`,1,4) == "none" ~ 1,                     #cases with "none" at the start
                             substr(`kischecknotes`,1,3) == "nil" & nchar(`kischecknotes`) == 3 ~ 1,  #cases with "nil" at the start and no additional text
                             substr(`kischecknotes`,1,8) == "no check" ~ 1,                 #cases with "no check" at the start with additional text
                              TRUE ~ 0)) %>%
  filter(kis_flag != 1,
         Year == 2 | Year == 3) %>%
  group_by(Year, `Health Board`) %>%
  summarise(kis_count = n()) %>%
  ungroup()

#Scotland total
dcrs_data_scotland_kis <- dcrs_data_kis %>% 
  group_by(Year) %>% 
  summarise_if(is.numeric, sum) %>%
  mutate(`Health Board` = 'Scotland') %>%
  ungroup()

#Combine both and get percentage
dcrs_data_kis_all <- rbind(dcrs_data_kis %>% filter(`Health Board` == Board) %>% mutate(`Health Board` = 'Board'), dcrs_data_scotland_kis) %>%
                     left_join(dcrs_data_total_all, by = c("Year", "Health Board")) %>%
  mutate(case_total = total_standard + total_interested + total_registrar + total_for_cause) %>%
  select(Year, `Health Board`, kis_count, case_total) %>%
  mutate(kis_percent = round(kis_count/case_total * 100))


# get hospital aggregates -------------------------------------------------

#set hospital review data

###Table 6 - Hospital Review - Significant values
DCRS_Data_Hosp_sig <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         YEAR == paste0("Year ", y2) | YEAR == paste0("Year ", y3),
         instgrp2 == "NHS Hospital") %>%
  mutate(YEAR = case_when(YEAR == paste0("Year ", y2) ~ "Year1", YEAR == paste0("Year ", y3) ~ "Year2")) %>%
  group_by(YEAR, Locname) %>%
  summarise(case_in_order = sum(Review == "Case in Order"),
            case_not_in_order = sum(Review == "Case not in Order"),
            case_report_pf = sum(Review == "Reported to PF"),
            case_total = (case_in_order + case_not_in_order + case_report_pf)) %>%
  ungroup() %>%
  pivot_wider(names_from = c(YEAR), 
              values_from = c('case_in_order', 'case_not_in_order', 'case_report_pf', 'case_total')) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(case_total_Year1 = case_when(is.na(case_total_Year1) ~ 0, TRUE ~ case_total_Year1),
         case_total_Year2 = case_when(is.na(case_total_Year2) ~ 0, TRUE ~ case_total_Year2),
         denom1 = sum(case_total_Year1), denom2 = sum(case_total_Year2)) %>%
  filter(case_total_Year1 >= 1 & case_total_Year2 >= 1) %>%
  arrange(desc(case_total_Year2)) %>% #sort highest to lowest
  mutate(ci_mn = BinomDiffCI(x1 = case_in_order_Year1, n1 = case_total_Year1, x2 = case_in_order_Year2, n2 = case_total_Year2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         io_sig = case_when(case_total_Year1 >= 10 & case_total_Year2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                               TRUE ~ ""),
         ci_mn = BinomDiffCI(x1 = case_not_in_order_Year1, n1 = case_total_Year1, x2 = case_not_in_order_Year2, n2 = case_total_Year2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         nio_sig = case_when(case_total_Year1 >= 10 & case_total_Year2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                            TRUE ~ ""),
         ci_mn = BinomDiffCI(x1 = case_report_pf_Year1, n1 = case_total_Year1, x2 = case_report_pf_Year2, n2 = case_total_Year2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         pf_sig = case_when(case_total_Year1 >= 10 & case_total_Year2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                            TRUE ~ ""),
         io_rate = case_in_order_Year2/case_total_Year2,
         io_rate_old = case_in_order_Year1/case_total_Year1,
         nio_rate = case_not_in_order_Year2/case_total_Year2,
         nio_rate_old = case_not_in_order_Year1/case_total_Year1,
         pf_rate = case_report_pf_Year2/case_total_Year2,
         pf_rate_old = case_report_pf_Year1/case_total_Year1,
         io_direction = case_when(io_sig == "*" & io_rate > io_rate_old ~ "increase",
                                  io_sig == "*" & io_rate < io_rate_old ~ "decrease",
                               TRUE ~ ""),  
         io_sig = case_when(io_sig == "*" & io_rate > io_rate_old ~ "*^",
                            io_sig == "*" & io_rate < io_rate_old ~ "*v",
                            TRUE ~ ""),
         nio_direction = case_when(nio_sig == "*" & nio_rate > nio_rate_old ~ "increase",
                                  nio_sig == "*" & nio_rate < nio_rate_old ~ "decrease",
                                 TRUE ~ ""),  
         nio_sig = case_when(nio_sig == "*" & nio_rate > nio_rate_old ~ "*^",
                             nio_sig == "*" & nio_rate < nio_rate_old ~ "*v",
                             TRUE ~ ""),
         pf_direction = case_when(pf_sig == "*" & pf_rate > pf_rate_old ~ "increase",
                                  io_sig == "*" & pf_rate < pf_rate_old ~ "decrease",
                                 TRUE ~ ""),  
         pf_sig = case_when(pf_sig == "*" & pf_rate > pf_rate_old ~ "*^",
                            pf_sig == "*" & pf_rate < pf_rate_old ~ "*v",
                            TRUE ~ "")) %>%
  select(Locname, io_direction, io_sig, nio_direction, nio_sig, pf_direction, pf_sig)

DCRS_Data_Hosp <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         YEAR == paste0("Year ", y3),
         instgrp2 == "NHS Hospital") %>%
  group_by(Locname) %>%
  summarise(case_in_order = sum(Review == "Case in Order"),
            case_not_in_order = sum(Review == "Case not in Order"),
            case_report_pf = sum(Review == "Reported to PF"),
            case_total = (case_in_order + case_not_in_order + case_report_pf)) %>%
  arrange(desc(case_total)) %>% #sort highest to lowest
  adorn_totals("row") %>% #add row with overall totals
  left_join(DCRS_Data_Hosp_sig, by = "Locname") %>%
  mutate(io_sig = case_when(is.na(io_sig) ~ "", TRUE ~ io_sig),
         nio_sig = case_when(is.na(nio_sig) ~ "", TRUE ~ nio_sig),
         pf_sig = case_when(is.na(pf_sig) ~ "", TRUE ~ pf_sig),
         in_order_percent = paste0(percent(case_in_order / case_total, accuracy = 0.1), io_sig),
         not_in_order_percent = paste0(percent(case_not_in_order / case_total, accuracy = 0.1), nio_sig),
         report_pf_percent = paste0(percent(case_report_pf / case_total, accuracy = 0.1), pf_sig),
         total_all_percent =percent(case_total / (sum(case_total)/2), accuracy = 0.1)) %>%
  select(Locname, case_in_order, in_order_percent,
         case_not_in_order, not_in_order_percent, 
         case_report_pf, report_pf_percent,
         case_total, total_all_percent)


# get hospice aggregates --------------------------------------------------

#set hospice review data
hospice_count <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board, YEAR == paste0("Year ", y2) | YEAR == paste0("Year ", y3), instgrp2 == "Hospice") %>%
  summarise(cases = n()) %>% mutate(cases = case_when(cases < 10 ~ 0, TRUE ~ cases)) %>% pull(cases)

if(hospice_count >= 1) {
###Table 7 - Hospice Review - Significant values
DCRS_Data_Hospice_sig <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         YEAR == paste0("Year ", y2) | YEAR == paste0("Year ", y3),
         instgrp2 == "Hospice") %>%
  mutate(YEAR = case_when(YEAR == paste0("Year ", y2) ~ "Year1", YEAR == paste0("Year ", y3) ~ "Year2")) %>%
  group_by(YEAR, Locname) %>%
  summarise(case_in_order = sum(Review == "Case in Order"),
            case_not_in_order = sum(Review == "Case not in Order"),
            case_report_pf = sum(Review == "Reported to PF"),
            case_total = (case_in_order + case_not_in_order + case_report_pf)) %>%
  ungroup() %>%
  pivot_wider(names_from = c(YEAR), 
              values_from = c('case_in_order', 'case_not_in_order', 'case_report_pf', 'case_total')) %>%
  filter(case_total_Year1 >= 1 & case_total_Year2 >= 1) %>%
  mutate(ci_mn = BinomDiffCI(x1 = case_in_order_Year1, n1 = case_total_Year1, x2 = case_in_order_Year2, n2 = case_total_Year2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         io_sig = case_when(case_total_Year1 >= 10 & case_total_Year2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                            TRUE ~ ""),
         ci_mn = BinomDiffCI(x1 = case_not_in_order_Year1, n1 = case_total_Year1, x2 = case_not_in_order_Year2, n2 = case_total_Year2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         nio_sig = case_when(case_total_Year1 >= 10 & case_total_Year2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                             TRUE ~ ""),
         ci_mn = BinomDiffCI(x1 = case_report_pf_Year1, n1 = case_total_Year1, x2 = case_report_pf_Year2, n2 = case_total_Year2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         pf_sig = case_when(case_total_Year1 >= 10 & case_total_Year2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                            TRUE ~ ""),
         io_rate = case_in_order_Year2/case_total_Year2,
         io_rate_old = case_in_order_Year1/case_total_Year1,
         nio_rate = case_not_in_order_Year2/case_total_Year2,
         nio_rate_old = case_not_in_order_Year1/case_total_Year1,
         pf_rate = case_report_pf_Year2/case_total_Year2,
         pf_rate_old = case_report_pf_Year1/case_total_Year1,
         io_direction = case_when(io_sig == "*" & io_rate > io_rate_old ~ "increase",
                                  io_sig == "*" & io_rate < io_rate_old ~ "decrease",
                                  TRUE ~ ""),  
         io_sig = case_when(io_sig == "*" & io_rate > io_rate_old ~ "*^",
                            io_sig == "*" & io_rate < io_rate_old ~ "*v",
                            TRUE ~ ""),
         nio_direction = case_when(nio_sig == "*" & nio_rate > nio_rate_old ~ "increase",
                                   nio_sig == "*" & nio_rate < nio_rate_old ~ "decrease",
                                   TRUE ~ ""),  
         nio_sig = case_when(nio_sig == "*" & nio_rate > nio_rate_old ~ "*^",
                             nio_sig == "*" & nio_rate < nio_rate_old ~ "*v",
                             TRUE ~ ""),
         pf_direction = case_when(pf_sig == "*" & pf_rate > pf_rate_old ~ "increase",
                                  io_sig == "*" & pf_rate < pf_rate_old ~ "decrease",
                                  TRUE ~ ""),  
         pf_sig = case_when(pf_sig == "*" & pf_rate > pf_rate_old ~ "*^",
                            pf_sig == "*" & pf_rate < pf_rate_old ~ "*v",
                            TRUE ~ "")) %>%
  select(Locname, io_direction, io_sig, nio_direction, nio_sig, pf_direction, pf_sig)
} else {DCRS_Data_Hospice_sig <- data.frame(Locname = Board, io_direction = "", io_sig = "", 
                                            nio_direction = "", nio_sig = "", pf_direction = "", pf_sig = "")}

###Table 7 - Hospice Review
DCRS_Data_Hospice <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         YEAR == paste0("Year ", y3),
         instgrp2 == "Hospice") %>%
  group_by(Locname) %>%
  summarise(case_in_order = sum(Review == "Case in Order"),
            case_not_in_order = sum(Review == "Case not in Order"),
            case_report_pf = sum(Review == "Reported to PF"),
            case_total = (case_in_order + case_not_in_order + case_report_pf)) %>%
  arrange(desc(case_total)) %>% #sort highest to lowest
  adorn_totals("row") %>% #add row with overall totals
  left_join(DCRS_Data_Hospice_sig, by = "Locname") %>%
  mutate(io_sig = case_when(is.na(io_sig) ~ "", TRUE ~ io_sig),
         nio_sig = case_when(is.na(nio_sig) ~ "", TRUE ~ nio_sig),
         pf_sig = case_when(is.na(pf_sig) ~ "", TRUE ~ pf_sig),
         in_order_percent = paste0(percent(case_in_order / case_total, accuracy = 0.1), io_sig),
         not_in_order_percent = paste0(percent(case_not_in_order / case_total, accuracy = 0.1), nio_sig),
         report_pf_percent = paste0(percent(case_report_pf / case_total, accuracy = 0.1), pf_sig),
         total_all_percent =percent(case_total / (sum(case_total)/2), accuracy = 0.1)) %>%
  select(Locname, case_in_order, in_order_percent,
         case_not_in_order, not_in_order_percent,
         case_report_pf, report_pf_percent,
         case_total, total_all_percent)