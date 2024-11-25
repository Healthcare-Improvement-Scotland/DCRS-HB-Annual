#Total overview validation
total_check1 <- dcrs_data %>%
  filter(`Case Type` != "Enquiry",
         `Created On` < end_date) %>%
  group_by(Year, `Health Board`) %>%
  summarise(total = n())

total_check2 <- dcrs_data_total %>%
  mutate(breakdown_total = total_standard + total_interested + total_registrar + total_for_cause)

left_join(total_check1, total_check2, by = c("Year", "Health Board")) %>%
  mutate(mismatch_flag = case_when(total != breakdown_total ~ 1)) %>%
  View("Mismatched totals")

#Standard case breakdown validation
breakdown_check <- dcrs_data_report %>%
  mutate(breakdown_total = total_in_order + total_not_in_order + total_report_to_pf) %>%
  select(Year, `Health Board`, breakdown_total)

left_join(dcrs_data_total, breakdown_check, by = c("Year", "Health Board")) %>%
  mutate(mismatch_flag = case_when(total_standard != breakdown_total ~ 1)) %>%
  View("Mismatched Standard case")

#Not in order breakdown validation
dcrs_data_report %>%
  select(total_not_in_order, total_email_amendment, total_replacement_mccd) %>%
  mutate(mismatch_flag = case_when(total_not_in_order != (total_email_amendment + total_replacement_mccd) ~ 1)) %>%
  View("Mismatched not in order")

#MCCD type breakdown validation
breakdown_check <- dcrs_data_report %>%
  mutate(breakdown_total = total_mccd + total_emccd) %>%
  select(Year, `Health Board`, breakdown_total)

left_join(dcrs_data_total, breakdown_check, by = c("Year", "Health Board")) %>%
  mutate(mismatch_flag = case_when(total_standard != breakdown_total ~ 1)) %>%
  View("Mismatched standard MCCD")

#Breach validation
dcrs_data_report %>%
  select(total_breach, total_cd_unavailable, total_breach_otheralt, total_breach_delay, total_breach_dcrsdelay) %>%
  mutate(mismatch_flag = case_when(total_breach != (total_cd_unavailable + total_breach_otheralt + total_breach_delay + total_breach_dcrsdelay) ~ 1)) %>%
  View("Mismatched breach")

#Case level totals validation
left_join(dcrs_data_report, total_check2, by = c("Year", "Health Board")) %>%
  mutate(mismatch_flag = case_when(total_level != total_standard ~ 1)) %>%
  select(total_level, total_standard, mismatch_flag) %>%
  View("Mismatched case level") 

#Not in order totals validation
dcrs_data_scot_run %>%
  ungroup() %>%
  filter(`Created On` >= "2023-04-01" & `Created On` <= "2024-03-31" ) %>%
  summarise(nio_total = sum(total_not_in_order)) %>%
  mutate(mismatch_flag = case_when(nio_total != nio_total_scot ~ 1)) %>%
  View("Mismatched nio")

#Closure category validation
dcrs_data_report %>%
  mutate(category_total = causal_timscale + cause_of_death + conditions_omitted +
           disposal_hazard + sequence_of_cause + cause_too_vague,
         mismatch_flag = case_when(closure_category > category_total ~ 1)) %>%
  select(Year, `Health Board`, closure_category, category_total, mismatch_flag) %>%
  View("Mismatched closure category")   

#Cause too vague validation
dcrs_data_report %>%
  mutate(cause_total = histology + psite_metastatic_missing + pneumonia_subtype + dementia_subtype +
           microbiology + sepsis_source + diabetes_subtype + lifestyle_factor + stroke,
         mismatch_flag = case_when(cause_too_vague > cause_total ~ 1)) %>%
  select(Year, `Health Board`, cause_too_vague, cause_total, mismatch_flag) %>%
  View("Mismatched cause vague")   

#Admin closure validation
admin_closure_check <- dcrs_data_report %>%
  mutate(mismatch_flag = case_when(closure_category_admin > (spelling_error + deceased_details_incorrect + doctors_details_incorrect + abbreviations_used + 
                                                             time_incorrect + incorrectly_complete + attendance_incorrect + address_incorrect + 
                                                             pm_information_incorrect + legibility + consultant_incorrect + other_incorrect) ~ 1)) %>%
  select(Year, `Health Board`, closure_category_admin, mismatch_flag) %>%
  View("Mismatched admin closure")   

#Report to PF validation
dcrs_data_report %>%
  mutate(mismatch_flag = case_when(total_report_to_pf > (choking + concerns_over_care + drug_related + neglect_exposure + fracture_trauma + 
                                                           industrial_disease + infectious_disease + legal_order + flagged_error + other_report_pf) ~ 1)) %>%
  select(Year, `Health Board`, total_report_to_pf, mismatch_flag) %>%
  View("Mismatched report to pf")   

#Enquiry line validation
dcrs_data_enquiry %>%
  mutate(mismatch_flag = case_when(enquiry_category_total > (gp_clinical_advice + gp_process_advice + hospital_clinical_advice + hospital_process_advice +
                                                             hospice_clinical_advice + hospice_process_advice + funeral_director + informant_or_family + 
                                                             procurator_fiscal + signposted + other) ~ 1))  %>%
  select(Year, `Health Board`, enquiry_category_total, mismatch_flag) %>%
  View("Mismatched enquiry line") 

#Location validation
DCRS_DATA_Loc_DIAG %>%
  group_by(YEAR, Health_Board, Locname) %>%
  summarise(case_in_order = sum(Review == "Case in Order"),
            case_not_in_order = sum(Review == "Case not in Order"),
            case_report_pf = sum(Review == "Reported to PF"),
            case_total = n()) %>%
  mutate(mismatch_flag = case_when(case_total != (case_in_order + case_not_in_order + case_report_pf) ~1)) %>%
  select(YEAR, Health_Board, Locname, case_total, case_in_order, case_not_in_order, case_report_pf, mismatch_flag) %>%
  filter(mismatch_flag == 1) %>%
  View("Mismatched location")


#Not in order cause of death validation
dcrs_cd_check <- DCRS_DATA_Loc_DIAG %>%
  filter(YEAR == paste0("Year ", y3),
         Review == "Case not in Order",
         DiagGrp != "NA",
         instgrp2 != "NA") %>%
  group_by(prmccdhealthboard) %>%
  summarise(case_total = n()) %>%
  rename('Health Board' = prmccdhealthboard)

dcrs_data_report %>%
  filter(Year == 3) %>%
  select(`Health Board`, total_not_in_order) %>%
  left_join(dcrs_cd_check, by = "Health Board") %>%
  mutate(mismatch_flag = case_when(case_total != total_not_in_order ~ 1)) %>%
  View("Mismatched nio cause death")

dcrs_data_check <- DCRS_DATA_Loc_DIAG %>%
  filter(YEAR == 'Year 9',
         prmccdhealthboard == 'Highland',
         Review == "Case not in Order") %>%
  select(DiagGrp, `Underlying.cause.of.death.code`, MainDiag)

dcrs_data_check <- DCRS_DATA_Loc %>%
  filter(YEAR == 'Year 9',
         prmccdhealthboard == 'Highland',
         Review == "Case not in Order") %>%
  select(`Underlying.cause.of.death.code`)