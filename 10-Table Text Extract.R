###Tables Summary Text###

#Scot summary text values

#Table 1
#Overall total
Scot_total <- dcrs_table_data_total %>% filter(event == 'Total') %>% pull(`3_Scotland`)
Scot_total <- prettyNum(Scot_total, big.mark = ",", scientific = FALSE)

#Table 2
#In order percent
io_percent_scot <- dcrs_table_data %>% filter(event == 'total_in_order' ) %>% mutate(Scot_percent_year3 = gsub("\\*", "", Scot_percent_year3)) %>%  pull(Scot_percent_year3)

#Not in order total
nio_total_scot <- dcrs_table_data %>% filter(event == 'total_not_in_order' ) %>% pull('3_Scotland')

#Not in order percent
nio_percent_scot <- dcrs_table_data %>% filter(event == 'total_not_in_order' ) %>% mutate(Scot_percent_year3 = gsub("\\*", "", Scot_percent_year3)) %>% pull(Scot_percent_year3)

#Replacement MCCD total
mccd_total_scot <- dcrs_table_data %>% filter(event == 'total_replacement_mccd' ) %>% pull('3_Scotland')

#Replacement MCCD percent
mccd_percent_scot <- dcrs_table_data %>% filter(event == 'total_replacement_mccd' ) %>% mutate(Scot_percent_year3 = gsub("\\*", "", Scot_percent_year3)) %>% pull(Scot_percent_year3)

#Email amendment total
email_total_scot <- dcrs_table_data %>% filter(event == 'total_email_amendment' ) %>% pull('3_Scotland')

#Email amendment percent
email_percent_scot <- dcrs_table_data %>% filter(event == 'total_email_amendment' ) %>% mutate(Scot_percent_year3 = gsub("\\*", "", Scot_percent_year3)) %>% pull(Scot_percent_year3)

#PF report percent
pf_percent_scot <- dcrs_table_data %>% filter(event == 'total_report_to_pf') %>% mutate(Scot_percent_year3 = gsub("\\*", "", Scot_percent_year3)) %>% pull(Scot_percent_year3)

#Table 3
#Electronic MCCD percent
emccd_percent_scot <- dcrs_table_data5 %>% filter(event == 'total_emccd') %>% mutate(Scot_percent_year3 = gsub("\\*", "", Scot_percent_year3)) %>% pull(Scot_percent_year3)

#Paper MCCD percent
mccd_percent_scot2 <- dcrs_table_data5 %>% filter(event == 'total_mccd') %>% mutate(Scot_percent_year3 = gsub("\\*", "", Scot_percent_year3)) %>% pull(Scot_percent_year3)

#Table 4
#Breach overal percentage
total_breach_percent_scot <- dcrs_table_data_breach3 %>% filter(event == 'total_breach') %>% 
  mutate(Scot_percent_year3 = gsub("\\*", "", Scot_percent_year3)) %>% 
  pull(Scot_percent_year3)

#Certifying doctor percent
cd_breach_percent_scot <- dcrs_table_data_breach3 %>% filter(event == 'total_cd_unavailable') %>% 
  mutate(Scot_percent_year3 = gsub("\\*", "", Scot_percent_year3)) %>% pull(Scot_percent_year3)

#Other reason percent
other_breach_percent_scot <- dcrs_table_data_breach3 %>% filter(event == 'total_breach_other') %>% 
  mutate(Scot_percent_year3 = gsub("\\*", "", Scot_percent_year3)) %>% pull(Scot_percent_year3)

#Receiving info reason percent
delay_percent_scot <- breach_other %>% filter(event == 'total_breach_delay') %>% pull(Scot_other_percent)

#DCRS reason percent
dcrs_delay_percent_scot <- breach_other %>% filter(event == 'total_breach_dcrsdelay') %>% pull(Scot_other_percent)

dual_delay_percent_scot <- breach_other %>% filter(event == 'total_breach_dual') %>% pull(Scot_other_percent)

#Table 5
#Escalated level total
esc_level_scot <- dcrs_table_data_level3 %>% filter(event == 'escalated') %>% pull(`3_Scotland`)

#Escalated level percent
esc_level_percent_scot <- dcrs_table_data_level3 %>% filter(event == 'escalated') %>% 
  mutate(Scot_percent_year3 = gsub("\\*", "", Scot_percent_year3)) %>% pull(Scot_percent_year3)

#Main reason total
top_esc_scot <- dcrs_data %>% filter(Year == 3, `Escalation Reason` != "") %>% group_by(`Escalation Reason`) %>% 
  summarise(number = sum(`Escalation Reason` != "")) %>% filter(number == max(number)) %>% pull(`Escalation Reason`)

#Main reason percent
top_esc_percent_scot <- dcrs_data %>% filter(Year == 3, `Escalation Reason` != "") %>% group_by(`Escalation Reason`) %>% 
  summarise(number = sum(`Escalation Reason` != "")) %>% mutate(ecap_percent = percent(number / sum(number), accuracy = 0.1)) %>% 
  filter(number == max(number)) %>% pull(ecap_percent)


#HB summary text values

#Table 1
#Overall total
board_total <- dcrs_table_data_total %>% filter(event == 'Total') %>% pull(`3_Board`)
board_total <- prettyNum(board_total, big.mark = ",", scientific = FALSE)

#Table 2
#In order percent
io_percent <- dcrs_table_data %>% filter(event == 'total_in_order' ) %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)

#Not in order total
nio_total <- dcrs_table_data %>% filter(event == 'total_not_in_order' ) %>% pull('3_Board')

#Not in order total
nio_percent <- dcrs_table_data %>% filter(event == 'total_not_in_order' ) %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)

#Replacement MCCD total
mccd_total <- dcrs_table_data %>% filter(event == 'total_replacement_mccd' ) %>% pull('3_Board')

#Replacement MCCD percent
mccd_percent <- dcrs_table_data %>% filter(event == 'total_replacement_mccd' ) %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)

#Email amendment total
email_total <- dcrs_table_data %>% filter(event == 'total_email_amendment' ) %>% pull('3_Board')

#Email amendment percent
email_percent <- dcrs_table_data %>% filter(event == 'total_email_amendment' ) %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)

#PF report percent
pf_percent <- dcrs_table_data %>% filter(event == 'total_report_to_pf') %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)

#Table 3
#Electronic MCCD percent
emccd_percent <- dcrs_table_data5 %>% filter(event == 'total_emccd') %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)

#Paper MCCD percent
mccd_percent2 <- dcrs_table_data5 %>% filter(event == 'total_mccd') %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)

#Table 4
#Breach overall percentage
total_breach_percent <- dcrs_table_data_breach3 %>% filter(event == 'total_breach') %>% 
  mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)

#Certifying doctor percent
cd_breach_percent <- dcrs_table_data_breach3 %>% filter(event == 'total_cd_unavailable') %>% 
  mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)

#Other reason percent
other_breach_percent <- dcrs_table_data_breach3 %>% filter(event == 'total_breach_other') %>% 
  mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% 
  pull(HB_percent_year3)

#Receiving info reason percent
delay_percent <- breach_other %>% filter(event == 'total_breach_delay') %>% pull(HB_other_percent)

#DCRS reason percent
dcrs_delay_percent <- breach_other %>% filter(event == 'total_breach_dcrsdelay') %>% pull(HB_other_percent)

dual_delay_percent <- breach_other %>% filter(event == 'total_breach_dual') %>% pull(HB_other_percent)

#Table 5
#Escalated level total
esc_level <- dcrs_table_data_level3 %>% filter(event == 'escalated') %>% pull('3_Board')

#Escalated level percent
esc_level_percent <- dcrs_table_data_level3 %>% filter(event == 'escalated') %>% 
  mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)

#Main reason total
top_esc <- dcrs_data %>% filter(Year == 3, `Health Board` == Board, `Escalation Reason` != "") %>% group_by(`Escalation Reason`) %>% 
  summarise(number = sum(`Escalation Reason` != "")) %>% filter(number == max(number)) %>% pull(`Escalation Reason`)

#Main reason percent
top_esc_percent <- dcrs_data %>% filter(Year == 3, `Health Board` == Board, `Escalation Reason` != "") %>% group_by(`Escalation Reason`) %>% 
  summarise(number = sum(`Escalation Reason` != "")) %>% mutate(ecap_percent = percent(number / sum(number), accuracy = 0.1)) %>% 
  filter(number == max(number)) %>% pull(ecap_percent)

#Hospital/hospice summary text values

#Table 6
#Overall hospital total
hospital_total <- DCRS_Data_Hosp %>% filter(Locname == "Total") %>% pull(case_total) 

#Main hospital name
hospital_name <- DCRS_Data_Hosp %>% filter(Locname != "Total") %>% filter(case_total == max(case_total)) %>% pull(Locname) 

#Main hospital number
hospital_number_top <- DCRS_Data_Hosp %>% filter(Locname != "Total") %>% filter(case_total == max(case_total)) %>% pull(case_total) 

#Main hospital percent
hospital_percent_top <- DCRS_Data_Hosp %>% filter(Locname != "Total") %>% filter(case_total == max(case_total)) %>% pull(total_all_percent) 

#Main hospital not in order total
hospital_number_notinorder <- DCRS_Data_Hosp %>% filter(Locname != "Total") %>% filter(case_total == max(case_total)) %>% pull(case_not_in_order) 

#Main hospital not in order percent
hospital_percent_notinorder <- DCRS_Data_Hosp %>% filter(Locname != "Total") %>% filter(case_total == max(case_total)) %>% pull(not_in_order_percent)

#Table 7
#Overall hospice total
hospice_total <- DCRS_Data_Hospice %>% filter(Locname == "Total") %>% pull(case_total) 

#Main hospice name
hospice_name <- DCRS_Data_Hospice %>% filter(Locname != "Total") %>% filter(case_total == max(case_total)) %>% pull(Locname) 

#Main hospice number
hospice_number_top <- DCRS_Data_Hospice %>% filter(Locname != "Total") %>% filter(case_total == max(case_total)) %>% pull(case_total) 

#Main hospice percent
hospice_percent_top <- DCRS_Data_Hospice %>% filter(Locname != "Total") %>% filter(case_total == max(case_total)) %>% pull(total_all_percent) 

#Main hospice not in order total
hospice_number_notinorder <- DCRS_Data_Hospice %>% filter(Locname != "Total") %>% filter(case_total == max(case_total)) %>% pull(case_not_in_order)

#Main hospice not in order percent
hospice_percent_notinorder <- DCRS_Data_Hospice %>% filter(Locname != "Total") %>% filter(case_total == max(case_total)) %>% pull(not_in_order_percent)


#Scotland significance text for table 2

#check if any significant changes identified
sig_count <- dcrs_table_data %>%
  mutate(sig_flag = case_when(str_sub(Scot_percent_year3,-1) == "*" ~ 1)) %>%
  summarise(sig_flag = sum(sig_flag, na.rm = TRUE)) %>% pull(sig_flag)

#get denominators for both years (as chart is two different levels two different denominators are used)
stand_n1 <- dcrs_data_wrangle_total %>% filter(event == 'total_standard') %>% pull(`2_Scotland`)
stand_n2 <- dcrs_data_wrangle_total %>% filter(event == 'total_standard') %>% pull(`3_Scotland`)
nio_n1 <- dcrs_data_wrangle %>% filter(event == 'total_not_in_order') %>% pull(`2_Scotland`)
nio_n2 <- dcrs_data_wrangle %>% filter(event == 'total_not_in_order') %>% pull(`3_Scotland`)


if (sig_count >= 1) { #only create if significant changes are identified
  scot_table2_sig <- dcrs_table_data %>%
    mutate(sig_flag = case_when(str_sub(Scot_percent_year3,-1) == "*" ~ 1, TRUE ~ 0)) %>%
    filter(sig_flag == 1) %>%
    mutate(n1 = case_when(event == "total_email_amendment" | event == "total_replacement_mccd" ~ nio_n1, TRUE ~ stand_n1), #set denominator depending on table level
           n2 = case_when(event == "total_email_amendment" | event == "total_replacement_mccd" ~ nio_n2, TRUE ~ stand_n2),
           direction = case_when(`2_Scotland`/n1 > `3_Scotland`/n2 ~ "decrease",
                                 `2_Scotland`/n1 < `3_Scotland`/n2 ~ "increase", TRUE ~ ""),
           event = str_replace(event, "total_in_order", "'in order'"), 
           event = str_replace(event, "total_not_in_order", "'not in order'"),
           event = str_replace(event, "total_email_amendment", "'cases requiring email amendment'"),
           event = str_replace(event, "total_replacement_mccd", "'cases requiring replacement MCCD'"),
           event = str_replace(event, "total_report_to_pf", "'cases reported to PF'")) %>% 
    group_by(direction) %>%
    mutate(decrease = case_when(direction == "decrease" ~ paste0(event, collapse = ' '), TRUE ~ ""),
           increase = case_when(direction == "increase" ~ paste0(event, collapse = ' '), TRUE ~ "")) %>%
    ungroup() %>%
    mutate(direction = paste(direction, collapse = ''),
           decrease = paste0(decrease, collapse = ''),
           increase = paste0(increase, collapse = ''),
           sig = case_when(str_detect(`direction`, fixed("decrease")) & str_detect(`direction`, fixed("increase"))  ~ 
                             paste0("+ ", decrease, " saw a significant decrease while ", 
                                    increase," saw a significant increase compared to ", year2, "."),
                           str_detect(`direction`, fixed("decrease")) ~ paste0("+ ", decrease, 
                                                                               " saw a significant decrease compared to ", year2, "."),
                           str_detect(`direction`, fixed("increase")) ~ paste0("+ ", increase, 
                                                                               " saw a significant increase compared to ", year2, "."),
                           TRUE ~ "")) %>%
    filter(event == max(event)) %>% 
    pull(sig)
} else { scot_table2_sig = "" }

#HB significance text for table 2

#check if any significant changes identified
sig_count <- dcrs_table_data %>%
  mutate(sig_flag = case_when(str_sub(HB_percent_year3,-1) == "*" ~ 1)) %>%
  summarise(sig_flag = sum(sig_flag, na.rm = TRUE)) %>% pull(sig_flag)

#get denominators for both years (as chart is two different levels two different denominators are used)
stand_n1 <- dcrs_data_wrangle_total %>% filter(event == 'total_standard') %>% pull(`2_Board`)
stand_n2 <- dcrs_data_wrangle_total %>% filter(event == 'total_standard') %>% pull(`3_Board`)
nio_n1 <- dcrs_data_wrangle %>% filter(event == 'total_not_in_order') %>% pull(`2_Board`)
nio_n2 <- dcrs_data_wrangle %>% filter(event == 'total_not_in_order') %>% pull(`3_Board`)


if (sig_count >= 1) { #only create if significant changes are identified
  hb_table2_sig <- dcrs_table_data %>%
    mutate(sig_flag = case_when(str_sub(HB_percent_year3,-1) == "*" ~ 1, TRUE ~ 0)) %>%
    filter(sig_flag == 1) %>%
    mutate(n1 = case_when(event == "total_email_amendment" | event == "total_replacement_mccd" ~ nio_n1, TRUE ~ stand_n1),
           n2 = case_when(event == "total_email_amendment" | event == "total_replacement_mccd" ~ nio_n2, TRUE ~ stand_n2),
           direction = case_when(`2_Board`/n1 > `3_Board`/n2 ~ "decrease",
                                 `2_Board`/n1 < `3_Board`/n2 ~ "increase", TRUE ~ ""),
           event = str_replace(event, "total_in_order", "'in order'"), 
           event = str_replace(event, "total_not_in_order", "'not in order'"),
           event = str_replace(event, "total_email_amendment", "'cases requiring email amendment'"),
           event = str_replace(event, "total_replacement_mccd", "'cases requiring replacement MCCD'"),
           event = str_replace(event, "total_report_to_pf", "'cases reported to PF'")) %>% 
    group_by(direction) %>%
    mutate(decrease = case_when(direction == "decrease" ~ paste0(event, collapse = ' '), TRUE ~ ""),
           increase = case_when(direction == "increase" ~ paste0(event, collapse = ' '), TRUE ~ "")) %>%
    ungroup() %>%
    mutate(direction = paste(direction, collapse = ''),
           decrease = paste0(decrease, collapse = ''),
           increase = paste0(increase, collapse = ''),
           sig = case_when(str_detect(`direction`, fixed("decrease")) & str_detect(`direction`, fixed("increase"))  ~ 
                             paste0("+ ", decrease, " saw a significant decrease while ", 
                                    increase," saw a significant increase compared to ", year2, "."),
                           str_detect(`direction`, fixed("decrease")) ~ paste0("+ ", decrease, 
                                                                               " saw a significant decrease compared to ", year2, "."),
                           str_detect(`direction`, fixed("increase")) ~ paste0("+ ", increase, 
                                                                               " saw a significant increase compared to ", year2, "."),
                           TRUE ~ "")) %>%
    filter(event == max(event)) %>% 
    pull(sig)
} else { hb_table2_sig = "" }


#Scotland significance text for table 3

#check if any significant changes identified
sig_count <- dcrs_table_data5 %>%
  mutate(sig_flag = case_when(str_sub(Scot_percent_year3,-1) == "*" ~ 1)) %>%
  summarise(sig_flag = sum(sig_flag, na.rm = TRUE)) %>% pull(sig_flag)

#get denominators for both years (as chart is three different levels three different denominators are used)
total_n1 <- dcrs_data_wrangle_total %>% filter(event == 'total_standard') %>% pull(`2_Scotland`)
total_n2 <- dcrs_data_wrangle_total %>% filter(event == 'total_standard') %>% pull(`3_Scotland`)
emccd_n1 <- dcrs_data_wrangle2 %>% filter(event == 'total_emccd') %>% pull(`2_Scotland`)
emccd_n2 <- dcrs_data_wrangle2 %>% filter(event == 'total_emccd') %>% pull(`3_Scotland`)
mccd_n1 <- dcrs_data_wrangle2 %>% filter(event == 'total_mccd') %>% pull(`2_Scotland`)
mccd_n2 <- dcrs_data_wrangle2 %>% filter(event == 'total_mccd') %>% pull(`3_Scotland`)

if (sig_count >= 1) { #only create if significant changes are identified
  scot_table3_sig <- dcrs_table_data5 %>%
    mutate(sig_flag = case_when(str_sub(Scot_percent_year3,-1) == "*" ~ 1, TRUE ~ 0)) %>%
    filter(sig_flag == 1) %>%
    mutate(n1 = case_when(event == "total_emccd" | event == "total_mccd" ~ total_n1,
                          event == "emccd_in_order" | event == "emccd_to_pf" ~ emccd_n1, TRUE ~ mccd_n1),
           n2 = case_when(event == "total_emccd" | event == "total_mccd" ~ total_n2,
                          event == "emccd_in_order" | event == "emccd_to_pf" ~ emccd_n2, TRUE ~ mccd_n2),
           direction = case_when(`2_Scotland`/n1 > `3_Scotland`/n2 ~ "decrease",
                                 `2_Scotland`/n1 < `3_Scotland`/n2 ~ "increase", TRUE ~ ""),
           event = str_replace(event, "total_emccd", "'eMCCD cases'"), 
           event = str_replace(event, "emccd_in_order", "'eMCCD cases in order'"),
           event = str_replace(event, "emccd_to_pf", "'eMCCD cases reported to PF'"),
           event = str_replace(event, "total_mccd", "'MCCD cases'"),
           event = str_replace(event, "mccd_in_order", "'MCCD cases in order'"),
           event = str_replace(event, "mccd_to_pf", "'MCCD cases reported to PF'")) %>% 
    group_by(direction) %>%
    mutate(decrease = case_when(direction == "decrease" ~ paste0(event, collapse = ' '), TRUE ~ ""),
           increase = case_when(direction == "increase" ~ paste0(event, collapse = ' '), TRUE ~ "")) %>%
    ungroup() %>%
    mutate(direction = paste(direction, collapse = ''),
           decrease = paste0(decrease, collapse = ''),
           increase = paste0(increase, collapse = ''),
           sig = case_when(str_detect(`direction`, fixed("decrease")) & str_detect(`direction`, fixed("increase"))  ~ 
                             paste0("+ ", increase," saw a significant increase while ",
                                    decrease, " saw a significant decrease compared to ", 
                                     year2, "."),
                           str_detect(`direction`, fixed("decrease")) ~ paste0("+ ", decrease, 
                                                                               " saw a significant decrease compared to ", year2, "."),
                           str_detect(`direction`, fixed("increase")) ~ paste0("+ ", increase, 
                                                                               " saw a significant increase compared to ", year2, "."),
                           TRUE ~ "")) %>%
    filter(event == max(event)) %>% 
    pull(sig)
} else { scot_table3_sig = "" }

#HB significance text for table 3

#check if any significant changes identified
sig_count <- dcrs_table_data5 %>%
  mutate(sig_flag = case_when(str_sub(HB_percent_year3,-1) == "*" ~ 1)) %>%
  summarise(sig_flag = sum(sig_flag, na.rm = TRUE)) %>% pull(sig_flag)

#get denominators for both years (as chart is three different levels three different denominators are used)
total_n1 <- dcrs_data_wrangle_total %>% filter(event == 'total_standard') %>% pull(`2_Board`)
total_n2 <- dcrs_data_wrangle_total %>% filter(event == 'total_standard') %>% pull(`3_Board`)
emccd_n1 <- dcrs_data_wrangle2 %>% filter(event == 'total_emccd') %>% pull(`2_Board`)
emccd_n2 <- dcrs_data_wrangle2 %>% filter(event == 'total_emccd') %>% pull(`3_Board`)
mccd_n1 <- dcrs_data_wrangle2 %>% filter(event == 'total_mccd') %>% pull(`2_Board`)
mccd_n2 <- dcrs_data_wrangle2 %>% filter(event == 'total_mccd') %>% pull(`3_Board`)


if (sig_count >= 1) { #only create if significant changes are identified
  hb_table3_sig <- dcrs_table_data5 %>%
    mutate(sig_flag = case_when(str_sub(HB_percent_year3,-1) == "*" ~ 1, TRUE ~ 0)) %>%
    filter(sig_flag == 1) %>%
    mutate(n1 = case_when(event == "total_emccd" | event == "total_mccd" ~ stand_n1,
                          event == "emccd_in_order" | event == "emccd_to_pf" ~ emccd_n1, TRUE ~ mccd_n1),
           n2 = case_when(event == "total_emccd" | event == "total_mccd" ~ stand_n2,
                          event == "emccd_in_order" | event == "emccd_to_pf" ~ emccd_n2, TRUE ~ mccd_n2),
           direction = case_when(`2_Board`/n1 > `3_Board`/n2 ~ "decrease",
                                 `2_Board`/n1 < `3_Board`/n2 ~ "increase", TRUE ~ ""),
           event = str_replace(event, "total_emccd", "'eMCCD cases'"), 
           event = str_replace(event, "emccd_in_order", "'eMCCD cases in order'"),
           event = str_replace(event, "emccd_to_pf", "'eMCCD cases reported to PF'"),
           event = str_replace(event, "total_mccd", "'MCCD cases'"),
           event = str_replace(event, "mccd_in_order", "'MCCD cases in order'"),
           event = str_replace(event, "mccd_to_pf", "'MCCD cases reported to PF'")) %>% 
    group_by(direction) %>%
    mutate(decrease = case_when(direction == "decrease" ~ paste0(event, collapse = ' '), TRUE ~ ""),
           increase = case_when(direction == "increase" ~ paste0(event, collapse = ' '), TRUE ~ "")) %>%
    ungroup() %>%
    mutate(direction = paste(direction, collapse = ''),
           decrease = paste0(decrease, collapse = ''),
           increase = paste0(increase, collapse = ''),
           sig = case_when(str_detect(`direction`, fixed("decrease")) & str_detect(`direction`, fixed("increase"))  ~ 
                             paste0("+ ", increase," saw a significant increase while ",
                                    decrease, " saw a significant decrease compared to ", year2, "."),
                           str_detect(`direction`, fixed("decrease")) ~ paste0("+ ", decrease, 
                                                                               " saw a significant decrease compared to ", year2, "."),
                           str_detect(`direction`, fixed("increase")) ~ paste0("+ ", increase, 
                                                                               " saw a significant increase compared to ", year2, "."),
                           TRUE ~ "")) %>%
    filter(event == max(event)) %>% 
    pull(sig)
} else { hb_table3_sig = "" }


#Scotland significance text for table 4

#check if any significant changes identified
sig_count <- dcrs_table_data_breach3 %>%
  mutate(sig_flag = case_when(str_sub(Scot_percent_year3,-1) == "*" ~ 1)) %>%
  summarise(sig_flag = sum(sig_flag, na.rm = TRUE)) %>% pull(sig_flag)

#get denominators for both years (as chart is two different levels two different denominators are used)
total_n1 <- dcrs_data_wrangle_breach %>% filter(event == 'total_ex_pf') %>% pull(`2_Scotland`)
total_n2 <- dcrs_data_wrangle_breach %>% filter(event == 'total_ex_pf') %>% pull(`3_Scotland`)
breach_n1 <- dcrs_data_wrangle_breach %>% filter(event == 'total_breach') %>% pull(`2_Scotland`)
breach_n2 <- dcrs_data_wrangle_breach %>% filter(event == 'total_breach') %>% pull(`3_Scotland`)
 
if (sig_count >= 1) { #only create if significant changes are identified
  scot_table4_sig <- dcrs_table_data_breach3 %>%
    mutate(sig_flag = case_when(str_sub(Scot_percent_year3,-1) == "*" ~ 1, TRUE ~ 0)) %>%
    filter(sig_flag == 1) %>%
    mutate(n1 = case_when(event == "total_breach" ~ total_n1, TRUE ~ breach_n1),
           n2 = case_when(event == "total_breach" ~ total_n2, TRUE ~ breach_n2),
           direction = case_when(`2_Scotland`/n1 > `3_Scotland`/n2 ~ "decrease",
                                 `2_Scotland`/n1 < `3_Scotland`/n2 ~ "increase", TRUE ~ ""),
           event = str_replace(event, "total_breach", "'total breaches'"), 
           event = str_replace(event, "total_cd_unavailable", "'breaches due to CD unavailable'"),
           event = str_replace(event, "total_breach_other", "'breaches due to other reasons'")) %>% 
    group_by(direction) %>%
    mutate(decrease = case_when(direction == "decrease" ~ paste0(event, collapse = ' '), TRUE ~ ""),
           increase = case_when(direction == "increase" ~ paste0(event, collapse = ' '), TRUE ~ "")) %>%
    ungroup() %>%
    mutate(direction = paste(direction, collapse = ''),
           decrease = paste0(decrease, collapse = ''),
           increase = paste0(increase, collapse = ''),
           sig = case_when(str_detect(`direction`, fixed("decrease")) & str_detect(`direction`, fixed("increase"))  ~ 
                             paste0("+ ", decrease, " saw a significant decrease while ", 
                                    increase," saw a significant increase compared to ", year2, "."),
                           str_detect(`direction`, fixed("decrease")) ~ paste0("+ ", decrease, 
                                                                               " saw a significant decrease compared to ", year2, "."),
                           str_detect(`direction`, fixed("increase")) ~ paste0("+ ", increase, 
                                                                               " saw a significant increase compared to ", year2, "."),
                           TRUE ~ "")) %>%
    filter(event == max(event)) %>% 
    pull(sig)
} else { scot_table4_sig = "" }

#HB significance text for table 4

#check if any significant changes identified
sig_count <- dcrs_table_data_breach3 %>%
  mutate(sig_flag = case_when(str_sub(HB_percent_year3,-1) == "*" ~ 1)) %>%
  summarise(sig_flag = sum(sig_flag, na.rm = TRUE)) %>% pull(sig_flag)

#get denominators for both years (as chart is two different levels two different denominators are used)
total_n1 <- dcrs_data_wrangle_breach %>% filter(event == 'total_ex_pf') %>% pull(`2_Board`)
total_n2 <- dcrs_data_wrangle_breach %>% filter(event == 'total_ex_pf') %>% pull(`3_Board`)
breach_n1 <- dcrs_data_wrangle_breach %>% filter(event == 'total_breach') %>% pull(`2_Board`)
breach_n2 <- dcrs_data_wrangle_breach %>% filter(event == 'total_breach') %>% pull(`3_Board`)


if (sig_count >= 1) { #only create if significant changes are identified
  hb_table4_sig <- dcrs_table_data_breach3 %>%
    mutate(sig_flag = case_when(str_sub(HB_percent_year3,-1) == "*" ~ 1, TRUE ~ 0)) %>%
    filter(sig_flag == 1) %>%
    mutate(n1 = case_when(event == "total_breach" ~ total_n1, TRUE ~ breach_n1),
           n2 = case_when(event == "total_breach" ~ total_n2, TRUE ~ breach_n2),
           direction = case_when(`2_Board`/n1 > `3_Board`/n2 ~ "decrease",
                                 `2_Board`/n1 < `3_Board`/n2 ~ "increase", TRUE ~ ""),
           event = str_replace(event, "total_breach", "'total breaches'"), 
           event = str_replace(event, "total_cd_unavailable", "'breaches due to CD unavailable'"),
           event = str_replace(event, "total_breach_other", "'breaches due to other reasons'")) %>% 
    group_by(direction) %>%
    mutate(decrease = case_when(direction == "decrease" ~ paste0(event, collapse = ' '), TRUE ~ ""),
           increase = case_when(direction == "increase" ~ paste0(event, collapse = ' '), TRUE ~ "")) %>%
    ungroup() %>%
    mutate(direction = paste(direction, collapse = ''),
           decrease = paste0(decrease, collapse = ''),
           increase = paste0(increase, collapse = ''),
           sig = case_when(str_detect(`direction`, fixed("decrease")) & str_detect(`direction`, fixed("increase"))  ~ 
                             paste0("+ ", decrease, " saw a significant decrease while ", 
                                    increase," saw a significant increase compared to ", year2, "."),
                           str_detect(`direction`, fixed("decrease")) ~ paste0("+ ", decrease, 
                                                                               " saw a significant decrease compared to ", year2, "."),
                           str_detect(`direction`, fixed("increase")) ~ paste0("+ ", increase, 
                                                                               " saw a significant increase compared to ", year2, "."),
                           TRUE ~ "")) %>%
    filter(event == max(event)) %>% 
    pull(sig)
} else { hb_table4_sig = "" }
