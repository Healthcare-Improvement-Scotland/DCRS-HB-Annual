#############  Wrangles the data and creates the tables and corresponding figures for the narrative used in part 1 of the report

Board <- hdr

# table 1 total overview --------------------------------------------------

#pivot data table 1
#First stage of wrangling data into right layout
dcrs_data_wrangle_total <- dcrs_data_total_all %>%
  pivot_longer(cols = !c(Year, `Health Board`), 
               names_to = 'event', 
               values_to = 'count') %>%
  pivot_wider(names_from = c(Year, `Health Board`), 
              values_from = 'count')

#get percentages and CI test and set layout for table
dcrs_table_data_total <- dcrs_data_wrangle_total %>%
  mutate(HB_denom1 = sum(`2_Board`), HB_denom2 = sum(`3_Board`), 
         ci_mn = BinomDiffCI(x1 = `2_Board`, n1 = HB_denom1, x2 = `3_Board`, n2 = HB_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         HB_ci_sig = case_when(HB_denom1 >= 10 & HB_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                            TRUE ~ ""),
         Scot_denom1 = sum(`2_Scotland`), Scot_denom2 = sum(`3_Scotland`), 
         ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_denom1, x2 = `3_Scotland`, n2 = Scot_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         Scot_ci_sig = case_when(HB_denom1 >= 10 & HB_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                               TRUE ~ ""),
         HB_percent_year1 = case_when(`1_Board` / sum(`1_Board`) == 1 ~ percent(`1_Board` / sum(`1_Board`), accuracy = 1),
                                      TRUE ~ percent(`1_Board` / sum(`1_Board`), accuracy = 0.1)),
         HB_percent_year2 = case_when(`2_Board` / sum(`2_Board`) == 1 ~ percent(`2_Board` / sum(`2_Board`), accuracy = 1),
                                      TRUE ~ percent(`2_Board` / sum(`2_Board`), accuracy = 0.1)),
         HB_percent_year3 = case_when(`3_Board` / sum(`3_Board`) == 1 ~ percent(`3_Board` / sum(`3_Board`), accuracy = 1),
                                      TRUE ~ percent(`3_Board` / sum(`3_Board`), accuracy = 0.1)),
         Scot_percent_year1 = case_when(`1_Scotland` / sum(`1_Scotland`) == 1 ~ percent(`1_Scotland` / sum(`1_Scotland`), accuracy = 1),
                                        TRUE ~ percent(`1_Scotland` / sum(`1_Scotland`), accuracy = 0.1)),
         Scot_percent_year2 = case_when(`2_Scotland` / sum(`2_Scotland`) == 1 ~ percent(`2_Scotland` / sum(`2_Scotland`), accuracy = 1),
                                        TRUE ~ percent(`2_Scotland` / sum(`2_Scotland`), accuracy = 0.1)),
         Scot_percent_year3 = case_when(`3_Scotland` / sum(`3_Scotland`) == 1 ~ percent(`3_Scotland` / sum(`3_Scotland`), accuracy = 1),
                                        TRUE ~ percent(`3_Scotland` / sum(`3_Scotland`), accuracy = 0.1)),
         HB_percent_year3 = paste0(HB_percent_year3,HB_ci_sig),
         Scot_percent_year3 = paste0(Scot_percent_year3,Scot_ci_sig)) %>% 
  select(event, `1_Scotland`, Scot_percent_year1, `1_Board`, HB_percent_year1,
         `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3) %>%
  adorn_totals()



#create table 1

#Format shape for border
brdr <- fp_border(color = "black", width = 1.3)
#Set up flextable
dcrs_table_total <- flextable(dcrs_table_data_total)%>%
  set_header_labels(dcrs_table_data_total, event = "", `1_Board` = "Board", `2_Board` = "Board", `3_Board` = "Board", `1_Scotland` = "Scotland", 
                    `2_Scotland` = "Scotland", `3_Scotland` = "Scotland", Scot_percent_year1 = "%", Scot_percent_year2 = "%", Scot_percent_year3 = "%", 
                     HB_percent_year3 = "%", HB_percent_year1 = "%", HB_percent_year2 = "%") %>%  #Change header titles
  compose(i = 1, j = 1, as_paragraph(as_chunk("Standard"))) %>%  #Change row titles
  compose(i = 2, j = 1, as_paragraph(as_chunk("Interested Person"))) %>% 
  compose(i = 3, j = 1, as_paragraph(as_chunk("Registrar Referral"))) %>% 
  compose(i = 4, j = 1, as_paragraph(as_chunk("MR For Cause Referral"))) %>% 
  compose(i = 5, j = 1, as_paragraph(as_chunk("Total"))) %>%   
  bold(i = 1:5, j = 1, bold = TRUE) %>%
  padding(padding.top = 1, part = "all") %>%
  padding(padding.bottom = 1, part = "all") %>%
  add_header_row(values = c("", year1, year2, year3), 
                 colwidths = c(1,4,4,4)) %>% #Add header for years
  align(i = 1, part = "header", align = "center") %>%
  bold(i = 1:2, part = "header", bold = TRUE) %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>%
  merge_at(i = 2, j = 2:3, part = "header") %>%  
  merge_at(i = 2, j = 4:5, part = "header") %>%
  merge_at(i = 2, j = 6:7, part = "header") %>%
  merge_at(i = 2, j = 8:9, part = "header") %>% 
  merge_at(i = 2, j = 10:11, part = "header") %>% 
  merge_at(i = 2, j = 12:13, part = "header") %>% 
  align(i = 2, part = "header", align = "center") %>%  
  merge_at(i = 5, j = 2:3, part = "body") %>%  
  merge_at(i = 5, j = 4:5, part = "body") %>%
  merge_at(i = 5, j = 6:7, part = "body") %>%
  merge_at(i = 5, j = 8:9, part = "body") %>%  
  merge_at(i = 5, j = 10:11, part = "body") %>%  
  merge_at(i = 5, j = 12:13, part = "body") %>%
  align(i = 5, j = 2:13, part = "body", align = "center") %>%    
  fontsize(size = 7.5, part = "all") %>%
  vline(j=c(1, 3, 5, 7, 9, 11), border = brdr) %>% #Add borders (shape created earlier)
  border_outer(border = brdr) %>%
  fix_border_issues() %>%
  set_caption(caption = "Table 1: Number and percentage of cases received ") %>%
  width(j = 1, width = 1.3, unit = "in")  %>%
  width(j = 2:13, width = 0.45, unit = "in") %>%
  height_all(height = 0.23, part = "all") %>%
  height(i = 1:2, part = "header", height = 0.2) %>%
  hrule(rule = "exact", part = "all")  


dcrs_table_total

#Create caption for the table if a significant change is identified
sig_table1 <- dcrs_table_data_total %>%
  mutate(sig_flag = case_when(str_sub(Scot_percent_year3,-1) == "*" | str_sub(HB_percent_year3,-1) == "*" ~ 1)) %>% #Flag when significance as been found
  summarise(sig_flag = sum(sig_flag, na.rm = TRUE)) %>%
  mutate(sig_caption = case_when(sig_flag >= 1 ~ "*Significant change from previous year", TRUE ~ "")) %>% #Create label to add to table if significant value is found
  pull(sig_caption)

# table 2 in order/not in order -------------------------------------------

#pivot data table 2
#First stage of wrangling data into right layout
dcrs_data_wrangle <- dcrs_data_report_all %>%
  select(Year, `Health Board`, total_in_order, total_not_in_order, 
         total_email_amendment, total_replacement_mccd, total_report_to_pf ) %>%
  pivot_longer(cols = !c(Year, `Health Board`), 
               names_to = 'event', 
               values_to = 'count') %>%
  pivot_wider(names_from = c(Year, `Health Board`), 
              values_from = 'count')


#get percentages and CI test and set layout for table
#Part1 overall proportion
dcrs_table_data1 <- dcrs_data_wrangle %>%
  #Set dummy numbers for setting total percentages (1) and not in order breakdown percentage (2)
  filter(event == 'total_in_order' | event == 'total_not_in_order' | event == 'total_report_to_pf') %>% 
  mutate(HB_denom1 = sum(`2_Board`), HB_denom2 = sum(`3_Board`), 
         ci_mn = BinomDiffCI(x1 = `2_Board`, n1 = HB_denom1, x2 = `3_Board`, n2 = HB_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         HB_ci_sig = case_when(HB_denom1 >= 10 & HB_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                          TRUE ~ ""),
         Scot_denom1 = sum(`2_Scotland`), Scot_denom2 = sum(`3_Scotland`), 
         ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_denom1, x2 = `3_Scotland`, n2 = Scot_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         Scot_ci_sig = case_when(Scot_denom1 >= 10 & Scot_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                            TRUE ~ ""),
         HB_percent_year2 = percent(`2_Board` / sum(`2_Board`), accuracy = 0.1),
         HB_percent_year3 = paste0(percent(`3_Board` / sum(`3_Board`), accuracy = 0.1),HB_ci_sig),
         Scot_percent_year2 = percent(`2_Scotland` / sum(`2_Scotland`), accuracy = 0.1),
         Scot_percent_year3 = paste0(percent(`3_Scotland` / sum(`3_Scotland`), accuracy = 0.1),Scot_ci_sig)) %>% 
  select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)

#extract current and previous year denominator to use in CI test
HB_n1 <- dcrs_data_wrangle %>% filter(event == 'total_not_in_order') %>% pull(`2_Board`)
HB_n2 <- dcrs_data_wrangle %>% filter(event == 'total_not_in_order') %>% pull(`3_Board`)

#Part2 NiO proportions
if (HB_n1 == 0 | HB_n2 == 0) { #CI test only works when both denominators are greater than zero, otherwise error occurs 
  dcrs_table_data2 <- dcrs_data_wrangle %>%
  filter(event == 'total_email_amendment' | event == 'total_replacement_mccd') %>% 
  mutate(Scot_denom1 = sum(`2_Scotland`), Scot_denom2 = sum(`3_Scotland`), 
         ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_denom1, x2 = `3_Scotland`, n2 = Scot_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         Scot_ci_sig = case_when(Scot_denom1 >= 10 & Scot_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                 TRUE ~ ""),
         HB_percent_year2 = percent(`2_Board` / sum(`2_Board`), accuracy = 0.1),
         HB_percent_year3 = percent(`3_Board` / sum(`3_Board`), accuracy = 0.1),
         Scot_percent_year2 = percent(`2_Scotland` / sum(`2_Scotland`), accuracy = 0.1),
         Scot_percent_year3 = paste0(percent(`3_Scotland` / sum(`3_Scotland`), accuracy = 0.1),Scot_ci_sig)) %>% 
  select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)
} else {   #create dataframe with no CI test if first condition is not met
  dcrs_table_data2 <- dcrs_data_wrangle %>%
  filter(event == 'total_email_amendment' | event == 'total_replacement_mccd') %>% 
  mutate(HB_denom1 = sum(`2_Board`), HB_denom2 = sum(`3_Board`), 
         ci_mn = BinomDiffCI(x1 = `2_Board`, n1 = HB_denom1, x2 = `3_Board`, n2 = HB_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         HB_ci_sig = case_when(HB_denom1 >= 10 & HB_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                               TRUE ~ ""),
         Scot_denom1 = sum(`2_Scotland`), Scot_denom2 = sum(`3_Scotland`), 
         ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_denom1, x2 = `3_Scotland`, n2 = Scot_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         Scot_ci_sig = case_when(Scot_denom1 >= 10 & Scot_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                 TRUE ~ ""),
         HB_percent_year2 = percent(`2_Board` / sum(`2_Board`), accuracy = 0.1),
         HB_percent_year3 = paste0(percent(`3_Board` / sum(`3_Board`), accuracy = 0.1),HB_ci_sig),
         Scot_percent_year2 = percent(`2_Scotland` / sum(`2_Scotland`), accuracy = 0.1),
         Scot_percent_year3 = paste0(percent(`3_Scotland` / sum(`3_Scotland`), accuracy = 0.1),Scot_ci_sig)) %>% 
  select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)
}

#combine two dataframes for full table layout
dcrs_table_data <- rbind(dcrs_table_data1, dcrs_table_data2) %>%
  mutate(order = case_when(event == 'total_in_order' ~ 1,
                           event == 'total_not_in_order' ~ 2,
                           event == 'total_report_to_pf' ~ 5,
                           event == 'total_email_amendment' ~ 3,
                           event == 'total_replacement_mccd' ~ 4),
         HB_percent_year2 = case_when(is.na(HB_percent_year2) ~ "0.0%", TRUE ~ HB_percent_year2),
         HB_percent_year3 = case_when(is.na(HB_percent_year3) ~ "0.0%", TRUE ~ HB_percent_year3)) %>%
  arrange(order) %>% select(!(order))


#create table 2

#Set up flextable
dcrs_table <- flextable(dcrs_table_data)%>%
  set_header_labels(dcrs_table_data, event = "", `2_Board` = "Board", `3_Board` = "Board", `2_Scotland` = "Scotland", `3_Scotland` = "Scotland", Scot_percent_year2 = "%", Scot_percent_year3 = "%", HB_percent_year2 = "%", HB_percent_year3 = "%") %>%  #Change header titles
  compose(i = 1, j = 1, as_paragraph(as_chunk("In Order"))) %>%  #Change row titles
  compose(i = 2, j = 1, as_paragraph(as_chunk("Not In Order"))) %>% 
  compose(i = 3, j = 1, as_paragraph(as_chunk("Email Amendment"))) %>% 
  compose(i = 4, j = 1, as_paragraph(as_chunk("Replacement MCCD"))) %>% 
  compose(i = 5, j = 1, as_paragraph(as_chunk("Report to PF"))) %>%   
  bold(i = c(1, 2, 5), j = 1, bold = TRUE) %>%
  italic(i = 3:4, italic = TRUE) %>% # Format row titles
  #align(i = c(2, 3, 5, 6), j = 1, align = "right") %>%
  padding(padding.top = 1, part = "all") %>%
  padding(padding.bottom = 1, part = "all") %>%
  padding(i = 3:4, j = 1, padding.left = 20, part = "body") %>%
  add_header_row(values = c("", year2, year3), 
                 colwidths = c(1,4,4)) %>% #Add header for years
  align(i = 1, part = "header", align = "center") %>%
  bold(i = 1:2, part = "header", bold = TRUE) %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>%
  merge_at(i = 2, j = 2:3, part = "header") %>%  
  merge_at(i = 2, j = 4:5, part = "header") %>%
  merge_at(i = 2, j = 6:7, part = "header") %>%
  merge_at(i = 2, j = 8:9, part = "header") %>%  
  align(i = 2, part = "header", align = "center") %>%  
  fontsize(size = 8, part = "all") %>%
  vline(j=c(1, 3, 5, 7), border = brdr) %>% #Add borders (shape created earlier)
  border_outer(border = brdr) %>%
  fix_border_issues() %>%
  set_caption(caption = "Table 2: Breakdown of standard case status by number and percentage") %>%
  width(j = 1, width = 1.3, unit = "in")  %>%
  width(j = 2:9, width = 0.6, unit = "in") %>%
  height_all(height = 0.2, part = "all") %>%
  height(i = 4, height = 0.27, part = "body") %>%  
  hrule(rule = "exact", part = "all")  

dcrs_table

#Create caption for the table if a significant change is identified
sig_table2 <- dcrs_table_data %>%
  mutate(sig_flag = case_when(str_sub(Scot_percent_year3,-1) == "*" | str_sub(HB_percent_year3,-1) == "*" ~ 1)) %>% #Flag when significance as been found
  summarise(sig_flag = sum(sig_flag, na.rm = TRUE)) %>%
  mutate(sig_caption = case_when(sig_flag >= 1 ~ "*Significant change from previous year", TRUE ~ "")) %>% #Create label to add to table if significant value is found
  pull(sig_caption)

# table 3 eMCCD/MCCD ------------------------------------------------------


#r pivot data table 3
#First stage of wrangling data into right layout
dcrs_data_wrangle2 <- dcrs_data_report_all %>%
  select(Year, `Health Board`, total_emccd, emccd_in_order, emccd_to_pf,
         total_mccd, mccd_in_order, mccd_to_pf) %>%
  pivot_longer(cols = !c(Year, `Health Board`), 
               names_to = 'event', 
               values_to = 'count') %>%
  pivot_wider(names_from = c(Year, `Health Board`), 
              values_from = 'count')

#get percentages and CI test and set layout for part 1 or table
dcrs_table_data2 <- dcrs_data_wrangle2 %>%
  filter(event == 'total_emccd' | event == 'total_mccd') %>%
  mutate(HB_denom1 = sum(`2_Board`), HB_denom2 = sum(`3_Board`), 
         ci_mn = BinomDiffCI(x1 = `2_Board`, n1 = HB_denom1, x2 = `3_Board`, n2 = HB_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         HB_ci_sig = case_when(HB_denom1 >= 10 & HB_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                               TRUE ~ ""),
         Scot_denom1 = sum(`2_Scotland`), Scot_denom2 = sum(`3_Scotland`), 
         ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_denom1, x2 = `3_Scotland`, n2 = Scot_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         Scot_ci_sig = case_when(Scot_denom1 >= 10 & Scot_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                 TRUE ~ ""),
         HB_percent_year2 = percent(`2_Board` / sum(`2_Board`), accuracy = 0.1),
         HB_percent_year3 = paste0(percent(`3_Board` / sum(`3_Board`), accuracy = 0.1),HB_ci_sig),
         Scot_percent_year2 = percent(`2_Scotland` / sum(`2_Scotland`), accuracy = 0.1),
         Scot_percent_year3 = paste0(percent(`3_Scotland` / sum(`3_Scotland`), accuracy = 0.1),Scot_ci_sig)) %>% 
  select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)

#get percentages and set layout for part 2 of table
dcrs_table_data3 <- dcrs_data_wrangle2 %>%
  filter(event == 'total_emccd' | event == 'total_mccd') %>%
  mutate(Link = case_when(event == 'total_emccd' ~ 1, 
                          event == 'total_mccd' ~ 2))

#extract current and previous year denominator to use in CI test
HB_n1 <- dcrs_data_wrangle2 %>% filter(event == 'total_emccd') %>% pull(`2_Board`)
HB_n2 <- dcrs_data_wrangle2 %>% filter(event == 'total_emccd') %>% pull(`3_Board`)
Scot_n1 <- dcrs_data_wrangle2 %>% filter(event == 'total_emccd') %>% pull(`2_Scotland`)
Scot_n2 <- dcrs_data_wrangle2 %>% filter(event == 'total_emccd') %>% pull(`3_Scotland`)

#get percentages and set layout for part 3 of table (emccd)
if (HB_n1 == 0 | HB_n2 == 0) { #CI test only works when both denominators are greater than zero, otherwise error occurs 
dcrs_table_data3 <- dcrs_data_wrangle2 %>%
  filter(event == 'emccd_in_order' | event == 'emccd_to_pf') %>%
  mutate(ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_n1 , x2 = `3_Scotland`, n2 = Scot_n2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         Scot_ci_sig = case_when(Scot_n1 >= 10 & Scot_n2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                 TRUE ~ ""),
         HB_percent_year2 = percent(`2_Board` / HB_n1, accuracy = 0.1),
         HB_percent_year3 = percent(`3_Board` / HB_n2, accuracy = 0.1),
         Scot_percent_year2 = percent(`2_Scotland` / Scot_n1, accuracy = 0.1),
         Scot_percent_year3 = paste0(percent(`3_Scotland` / Scot_n2, accuracy = 0.1),Scot_ci_sig)) %>%
  select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)
} else {  #create dataframe with no CI test if first condition is not met
  dcrs_table_data3 <- dcrs_data_wrangle2 %>%
  filter(event == 'emccd_in_order' | event == 'emccd_to_pf') %>%
  #Set dummy numbers for setting electronic case percentages (1) and paper percentage (2)
    mutate(ci_mn = BinomDiffCI(x1 = `2_Board`, n1 = HB_n1 , x2 = `3_Board`, n2 = HB_n2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
           HB_ci_sig = case_when(HB_n1 >= 10 & HB_n2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                   TRUE ~ ""),
           ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_n1 , x2 = `3_Scotland`, n2 = Scot_n2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
           Scot_ci_sig = case_when(Scot_n1 >= 10 & Scot_n2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                   TRUE ~ ""),
           HB_percent_year2 = percent(`2_Board` / HB_n1, accuracy = 0.1),
           HB_percent_year3 = paste0(percent(`3_Board` / HB_n2, accuracy = 0.1),HB_ci_sig),
           Scot_percent_year2 = percent(`2_Scotland` / Scot_n1, accuracy = 0.1),
           Scot_percent_year3 = paste0(percent(`3_Scotland` / Scot_n2, accuracy = 0.1),Scot_ci_sig)) %>%
    select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
           `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)
}

#extract current and previous year denominator to use in CI test
HB_n1 <- dcrs_data_wrangle2 %>% filter(event == 'total_mccd') %>% pull(`2_Board`)
HB_n2 <- dcrs_data_wrangle2 %>% filter(event == 'total_mccd') %>% pull(`3_Board`)
Scot_n1 <- dcrs_data_wrangle2 %>% filter(event == 'total_mccd') %>% pull(`2_Scotland`)
Scot_n2 <- dcrs_data_wrangle2 %>% filter(event == 'total_mccd') %>% pull(`3_Scotland`)

#get percentages and set layout for part 3 of table (mccd)
if (HB_n1 == 0 | HB_n2 == 0) { #CI test only works when both denominators are greater than zero, otherwise error occurs 
  dcrs_table_data4 <- dcrs_data_wrangle2 %>%
    filter(event == 'mccd_in_order' | event == 'mccd_to_pf') %>%
    mutate(ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_n1 , x2 = `3_Scotland`, n2 = Scot_n2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
           Scot_ci_sig = case_when(Scot_n1 >= 10 & Scot_n2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                   TRUE ~ ""),
           HB_percent_year2 = percent(`2_Board` / HB_n1, accuracy = 0.1),
           HB_percent_year3 = percent(`3_Board` / HB_n2, accuracy = 0.1),
           Scot_percent_year2 = percent(`2_Scotland` / Scot_n1, accuracy = 0.1),
           Scot_percent_year3 = paste0(percent(`3_Scotland` / Scot_n2, accuracy = 0.1),Scot_ci_sig)) %>%
    select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
           `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)
} else {  #create dataframe with no CI test if first condition is not met
  dcrs_table_data4 <- dcrs_data_wrangle2 %>%
    filter(event == 'mccd_in_order' | event == 'mccd_to_pf') %>%
    #Set dummy numbers for setting electronic case percentages (1) and paper percentage (2)
    mutate(ci_mn = BinomDiffCI(x1 = `2_Board`, n1 = HB_n1 , x2 = `3_Board`, n2 = HB_n2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
           HB_ci_sig = case_when(HB_n1 >= 10 & HB_n2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                 TRUE ~ ""),
           ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_n1 , x2 = `3_Scotland`, n2 = Scot_n2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
           Scot_ci_sig = case_when(Scot_n1 >= 10 & Scot_n2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                   TRUE ~ ""),
           HB_percent_year2 = percent(`2_Board` / HB_n1, accuracy = 0.1),
           HB_percent_year3 = paste0(percent(`3_Board` / HB_n2, accuracy = 0.1),HB_ci_sig),
           Scot_percent_year2 = percent(`2_Scotland` / Scot_n1, accuracy = 0.1),
           Scot_percent_year3 = paste0(percent(`3_Scotland` / Scot_n2, accuracy = 0.1),Scot_ci_sig)) %>%
    select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
           `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)
}


#combine all parts into one table
dcrs_table_data5 <- rbind(dcrs_table_data2, dcrs_table_data3, dcrs_table_data4) %>%
  mutate(order = case_when(event == 'total_emccd' ~ 1,
                           event == 'emccd_in_order' ~ 2,
                           event == 'emccd_to_pf' ~ 3,
                           event == 'total_mccd' ~ 4,
                           event == 'mccd_in_order' ~ 5,
                           event == 'mccd_to_pf' ~ 6)) %>%
  arrange(order) %>%
  select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)


#create table 3

#Set up flextable
dcrs_table2 <- flextable(dcrs_table_data5)%>%
  set_header_labels(dcrs_table_data5, event = "", `2_Board` = "Board", `3_Board` = "Board", `2_Scotland` = "Scotland", `3_Scotland` = "Scotland", Scot_percent_year2 = "%", Scot_percent_year3 = "%", HB_percent_year2 = "%", HB_percent_year3 = "%") %>%  #Change header titles
  compose(i = 1, j = 1, as_paragraph(as_chunk("eMCCD"))) %>%  #Change row titles
  compose(i = 2, j = 1, as_paragraph(as_chunk("In Order"))) %>% 
  compose(i = 3, j = 1, as_paragraph(as_chunk("Report to PF"))) %>% 
  compose(i = 4, j = 1, as_paragraph(as_chunk("MCCD"))) %>% 
  compose(i = 5, j = 1, as_paragraph(as_chunk("In Order"))) %>% 
  compose(i = 6, j = 1, as_paragraph(as_chunk("Report to PF"))) %>%   
  bold(i = c(1, 4), j = 1, bold = TRUE) %>%
  italic(i = c(2, 3, 5, 6), italic = TRUE) %>% # Format row titles
  #align(i = c(2, 3, 5, 6), j = 1, align = "right") %>%
  padding(padding.top = 1, part = "all") %>%
  padding(padding.bottom = 1, part = "all") %>%
  padding(i = c(2, 3, 5, 6), j = 1, padding.left = 20, part = "body") %>%
  add_header_row(values = c("", year2, year3), 
                 colwidths = c(1,4,4)) %>% #Add header for years
  align(i = 1, part = "header", align = "center") %>%
  bold(i = 1:2, part = "header", bold = TRUE) %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>%
  merge_at(i = 2, j = 2:3, part = "header") %>%  
  merge_at(i = 2, j = 4:5, part = "header") %>%
  merge_at(i = 2, j = 6:7, part = "header") %>%
  merge_at(i = 2, j = 8:9, part = "header") %>%  
  align(i = 2, part = "header", align = "center") %>%    
  fontsize(size = 8, part = "all") %>%
  vline(j=c(1, 3, 5, 7), border = brdr) %>% #Add borders (shape created earlier)
  border_outer(border = brdr) %>%
  fix_border_issues() %>%
  set_caption(caption = "Table 3: Breakdown of MCCD type by number and percentage") %>%
  width(j = 1, width = 1.3, unit = "in")  %>%
  width(j = 2:9, width = 0.6, unit = "in") %>%
  height_all(height = 0.2, part = "all") %>%
  hrule(rule = "exact", part = "all")   

#check if significant change is present and creates note for bottom of table
sig_table3 <- dcrs_table_data5 %>%
  mutate(sig_flag = case_when(str_sub(Scot_percent_year3,-1) == "*" | str_sub(HB_percent_year3,-1) == "*" ~ 1)) %>% #Flag when significance as been found
  summarise(sig_flag = sum(sig_flag, na.rm = TRUE)) %>%
  mutate(sig_caption = case_when(sig_flag >= 1 ~ "*Significant change from previous year", TRUE ~ "")) %>% #Create label to add to table if significant value is found
  pull(sig_caption)




# table 4 breached cases --------------------------------------------------

#r pivot data table 4
#First stage of wrangling data into right layout
dcrs_data_wrangle_breach <- dcrs_data_report_all %>%
  select(Year, `Health Board`, total_ex_pf, total_breach,
         total_cd_unavailable, total_breach_other, total_breach_delay, 
         total_breach_dcrsdelay, total_breach_dual, total_breach_otheralt) %>%
  pivot_longer(cols = !c(Year, `Health Board`), 
               names_to = 'event', 
               values_to = 'count') %>%
  pivot_wider(names_from = c(Year, `Health Board`), 
              values_from = 'count')

#extract current and previous year denominator to use in CI test
HB_n1 <- dcrs_data_wrangle_breach %>% filter(event == 'total_ex_pf') %>% pull(`2_Board`)
HB_n2 <- dcrs_data_wrangle_breach %>% filter(event == 'total_ex_pf') %>% pull(`3_Board`)
Scot_n1 <- dcrs_data_wrangle_breach %>% filter(event == 'total_ex_pf') %>% pull(`2_Scotland`)
Scot_n2 <- dcrs_data_wrangle_breach %>% filter(event == 'total_ex_pf') %>% pull(`3_Scotland`)

#get percentages and set layout table 4 breach total
dcrs_table_data_breach1 <- dcrs_data_wrangle_breach %>%
  filter(event == 'total_breach') %>%
  mutate(ci_mn = BinomDiffCI(x1 = `2_Board`, n1 = HB_n1, x2 = `3_Board`, n2 = HB_n2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         HB_ci_sig = case_when(HB_n1>= 10 & HB_n2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                               TRUE ~ ""),
         ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_n1, x2 = `3_Scotland`, n2 = Scot_n2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         Scot_ci_sig = case_when(Scot_n1 >= 10 & Scot_n2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                 TRUE ~ ""),
         HB_percent_year2 = percent(`2_Board` / HB_n1, accuracy = 0.1),
         HB_percent_year3 = paste0(percent(`3_Board` / HB_n2, accuracy = 0.1),HB_ci_sig),
         Scot_percent_year2 = percent(`2_Scotland` / Scot_n1, accuracy = 0.1),
         Scot_percent_year3 = paste0(percent(`3_Scotland` / Scot_n2, accuracy = 0.1),Scot_ci_sig)) %>% 
  filter(event == 'total_breach') %>% 
  select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)

#extract current and previous year denominator to use in CI test
HB_n1 <- dcrs_data_wrangle_breach %>% filter(event == 'total_breach') %>% pull(`2_Board`)
HB_n2 <- dcrs_data_wrangle_breach %>% filter(event == 'total_breach') %>% pull(`3_Board`)

#get percentages and set layout table 4 breach breakdown
if (HB_n1 == 0| HB_n2 == 0) { #CI test only works when both denominators are greater than zero, otherwise error occurs 
dcrs_table_data_breach2 <- dcrs_data_wrangle_breach %>%
  filter(event == 'total_cd_unavailable' | event == 'total_breach_other') %>%
  mutate(Scot_denom1 = sum(`2_Scotland`), Scot_denom2 = sum(`3_Scotland`), 
         ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_denom1, x2 = `3_Scotland`, n2 = Scot_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         Scot_ci_sig = case_when(Scot_denom1 >= 10 & Scot_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                 TRUE ~ ""),
         HB_percent_year2 = percent(`2_Board` / sum(`2_Board`), accuracy = 0.1),
         HB_percent_year3 = percent(`3_Board` / sum(`3_Board`), accuracy = 0.1),
         Scot_percent_year2 = percent(`2_Scotland` / sum(`2_Scotland`), accuracy = 0.1),
         Scot_percent_year3 = paste0(percent(`3_Scotland` / sum(`3_Scotland`), accuracy = 0.1),Scot_ci_sig)) %>% 
  select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3) 
} else { #create dataframe with no CI test if first condition is not met
  dcrs_table_data_breach2 <- dcrs_data_wrangle_breach %>%
  filter(event == 'total_cd_unavailable' | event == 'total_breach_other') %>%
  mutate(HB_denom1 = sum(`2_Board`), HB_denom2 = sum(`3_Board`), 
         ci_mn = BinomDiffCI(x1 = `2_Board`, n1 = HB_denom1, x2 = `3_Board`, n2 = HB_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         HB_ci_sig = case_when(HB_denom1 >= 10 & HB_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                               TRUE ~ ""),
         Scot_denom1 = sum(`2_Scotland`), Scot_denom2 = sum(`3_Scotland`), 
         ci_mn = BinomDiffCI(x1 = `2_Scotland`, n1 = Scot_denom1, x2 = `3_Scotland`, n2 = Scot_denom2, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
         Scot_ci_sig = case_when(Scot_denom1 >= 10 & Scot_denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                                 TRUE ~ ""),
         HB_percent_year2 = percent(`2_Board` / sum(`2_Board`), accuracy = 0.1),
         HB_percent_year3 = paste0(percent(`3_Board` / sum(`3_Board`), accuracy = 0.1),HB_ci_sig),
         Scot_percent_year2 = percent(`2_Scotland` / sum(`2_Scotland`), accuracy = 0.1),
         Scot_percent_year3 = paste0(percent(`3_Scotland` / sum(`3_Scotland`), accuracy = 0.1),Scot_ci_sig)) %>% 
  select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)
}

#combine both for table 4
dcrs_table_data_breach3 <- rbind(dcrs_table_data_breach1, dcrs_table_data_breach2) %>%
  mutate(HB_percent_year2 = case_when(is.na(HB_percent_year2) ~ "0.0%", TRUE ~ HB_percent_year2),
         HB_percent_year3 = case_when(is.na(HB_percent_year3) ~ "0.0%", TRUE ~ HB_percent_year3))

#Find breakdown of "other" for narrative
breach_other <- dcrs_data_wrangle_breach %>% filter(event == 'total_breach_delay' | event == 'total_breach_dcrsdelay' |
                                                    event == 'total_breach_dual' | event == 'total_breach_otheralt') %>%
  mutate(HB_other_percent = percent(`3_Board` / sum(`3_Board`), accuracy = 0.1),
         Scot_other_percent = percent(`3_Scotland` / sum(`3_Scotland`), accuracy = 0.1))


#r create table 4

#Set up flextable
dcrs_table3 <- flextable(dcrs_table_data_breach3)%>%
  set_header_labels(dcrs_table_data_breach3, event = "", `2_Board` = "Board", `3_Board` = "Board", `2_Scotland` = "Scotland", `3_Scotland` = "Scotland", Scot_percent_year2 = "%", Scot_percent_year3 = "%", HB_percent_year2 = "%", HB_percent_year3 = "%") %>%  #Change header titles
  compose(i = 1, j = 1, as_paragraph(as_chunk("Breached Cases"))) %>%  #Change row titles
  compose(i = 2, j = 1, as_paragraph(as_chunk("CD not available"))) %>% 
  compose(i = 3, j = 1, as_paragraph(as_chunk("Other"))) %>% 
  bold(i = 1, j = 1, bold = TRUE) %>%
  italic(i = c(2, 3), j = 1, italic = TRUE) %>% # Format row titles
  #align(i = c(2, 3, 5, 6), j = 1, align = "right") %>%
  padding(padding.top = 1, part = "all") %>%
  padding(padding.bottom = 1, part = "all") %>%
  padding(i = c(2, 3), j = 1, padding.left = 20, part = "body") %>%
  add_header_row(values = c("", year2, year3), 
                 colwidths = c(1,4,4)) %>% #Add header for years
  align(i = 1, part = "header", align = "center") %>%
  bold(i = 1:2, part = "header", bold = TRUE) %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>%
  merge_at(i = 2, j = 2:3, part = "header") %>%  
  merge_at(i = 2, j = 4:5, part = "header") %>%
  merge_at(i = 2, j = 6:7, part = "header") %>%
  merge_at(i = 2, j = 8:9, part = "header") %>%  
  align(i = 2, part = "header", align = "center") %>%    
  fontsize(size = 8, part = "all") %>%
  vline(j=c(1, 3, 5, 7), border = brdr) %>% #Add borders (shape created earlier)
  border_outer(border = brdr) %>%
  fix_border_issues() %>%
  set_caption(caption = "Table 4: Breakdown of cases which breached SLA timescales by number and percentage") %>%
  width(j = 1, width = 1.3, unit = "in")  %>%
  width(j = 2:9, width = 0.6, unit = "in") %>%
  height_all(height = 0.2, part = "all") %>%
  hrule(rule = "exact", part = "all")  

#check if significant change is present and creates note for bottom of table
sig_table4 <- dcrs_table_data_breach3 %>%
  mutate(sig_flag = case_when(str_sub(Scot_percent_year3,-1) == "*" | str_sub(HB_percent_year3,-1) == "*" ~ 1)) %>% #Flag when significance as been found
  summarise(sig_flag = sum(sig_flag, na.rm = TRUE)) %>%
  mutate(sig_caption = case_when(sig_flag >= 1 ~ "*Significant change from previous year", TRUE ~ "")) %>% #Create label to add to table if significant value is found
  pull(sig_caption)

# table 5 case level ------------------------------------------------------

#pivot data table 5
#First stage of wrangling data into right layout
dcrs_data_wrangle_level <- dcrs_data_report_all %>%
  select(Year, `Health Board`, level_1, level_1_hybrid, level_2, level_1_total, total_level, escalated) %>%
  pivot_longer(cols = !c(Year, `Health Board`), 
               names_to = 'event', 
               values_to = 'count') %>%
  pivot_wider(names_from = c(Year, `Health Board`), 
              values_from = 'count')

#get percentages and set layout table 5 main breakdown
dcrs_table_data_level <- dcrs_data_wrangle_level %>%
  filter(event == 'level_1' | event == 'level_1_hybrid' | event == 'level_2' | event == 'total_level') %>% 
  mutate(HB_percent_year2 = case_when(event != "total_level" ~ percent(`2_Board` / sum(`2_Board`) * 2, accuracy = 0.1)),
         HB_percent_year3 = case_when(event != "total_level" ~ percent(`3_Board` / sum(`3_Board`) * 2, accuracy = 0.1)),
         Scot_percent_year2 = case_when(event != "total_level" ~ percent(`2_Scotland` / sum(`2_Scotland`) * 2, accuracy = 0.1)),
         Scot_percent_year3 = case_when(event != "total_level" ~ percent(`3_Scotland` / sum(`3_Scotland`) * 2, accuracy = 0.1))) %>% 
  select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3)

#get percentages and set layout table 5 escalation reason
dcrs_table_data_level2 <- dcrs_data_wrangle_level %>%
  #Set dummy numbers for setting subgroup totals
  filter(event == 'level_1_total' | event == 'escalated') %>% 
  mutate(HB_percent_year2 = percent(`2_Board` / lag(`2_Board`), accuracy = 0.1),
         HB_percent_year3 = percent(`3_Board` / lag(`3_Board`), accuracy = 0.1),
         Scot_percent_year2 = percent(`2_Scotland` / lag(`2_Scotland`), accuracy = 0.1),
         Scot_percent_year3 = percent(`3_Scotland` / lag(`3_Scotland`), accuracy = 0.1)) %>% 
  select(event, `2_Scotland`, Scot_percent_year2, `2_Board`, HB_percent_year2, 
         `3_Scotland`, Scot_percent_year3, `3_Board`, HB_percent_year3) %>%
  filter(event != 'level_1_total')

#combine both for table 5
dcrs_table_data_level3 <- rbind(dcrs_table_data_level, dcrs_table_data_level2)


#create table 5

#Set up flextable
dcrs_table_5 <- flextable(dcrs_table_data_level3)%>%
  set_header_labels(dcrs_table_data_level3, event = "", `2_Board` = "Board", `3_Board` = "Board", `2_Scotland` = "Scotland", `3_Scotland` = "Scotland", Scot_percent_year2 = "%", Scot_percent_year3 = "%", HB_percent_year2 = "%", HB_percent_year3 = "%") %>%  #Change header titles
  compose(i = 1, j = 1, as_paragraph(as_chunk("Level 1"))) %>%  #Change row titles
  compose(i = 2, j = 1, as_paragraph(as_chunk("Level 1 Hybrid"))) %>% 
  compose(i = 3, j = 1, as_paragraph(as_chunk("Level 2"))) %>% 
  compose(i = 4, j = 1, as_paragraph(as_chunk("Total"))) %>% 
  compose(i = 5, j = 1, as_paragraph(as_chunk("Escalated to Level 2"))) %>% 
  bold(i = 4, j = 1, bold = TRUE) %>%
  italic(i = c(5), j = 1, italic = TRUE) %>% # Format row titles
  #align(i = c(2, 3, 5, 6), j = 1, align = "right") %>%
  padding(padding.top = 1, part = "all") %>%
  padding(padding.bottom = 1, part = "all") %>%
  padding(i = 5, j = 1, padding.left = 10, part = "body") %>%
  add_header_row(values = c("", year2, year3), 
                 colwidths = c(1,4,4)) %>% #Add header for years
  align(i = 1, part = "header", align = "center") %>%
  bold(i = 1:2, part = "header", bold = TRUE) %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>%
  merge_at(i = 2, j = 2:3, part = "header") %>%  
  merge_at(i = 2, j = 4:5, part = "header") %>%
  merge_at(i = 2, j = 6:7, part = "header") %>%
  merge_at(i = 2, j = 8:9, part = "header") %>%  
  merge_at(i = 4, j = 2:3, part = "body") %>%  
  merge_at(i = 4, j = 4:5, part = "body") %>%
  merge_at(i = 4, j = 6:7, part = "body") %>%
  merge_at(i = 4, j = 8:9, part = "body") %>%
  align(i = 2, part = "header", align = "center") %>% 
  align(i = 4, j = 2:9, part = "body", align = "center") %>% 
  fontsize(size = 8, part = "all") %>%
  vline(j=c(1, 3, 5, 7), border = brdr) %>%
  hline(i= 4, border = brdr) %>% #Add borders (shape created earlier)
  border_outer(border = brdr) %>%
  fix_border_issues() %>%
  set_caption(caption = "Table 5: Standard Level 1 and Level 2 cases reviewed by number and percentage") %>%
  width(j = 1, width = 1.3, unit = "in")  %>%
  width(j = 2:9, width = 0.6, unit = "in") %>%
  height_all(height = 0.2, part = "all") %>%
  hrule(rule = "exact", part = "all")  

dcrs_table_5

#check if significant change is present and creates note for bottom of table
sig_table5 <- dcrs_table_data_level3 %>%
  mutate(sig_flag = case_when(str_sub(Scot_percent_year3,-1) == "*" | str_sub(HB_percent_year3,-1) == "*" ~ 1)) %>% #Flag when significance as been found
  summarise(sig_flag = sum(sig_flag, na.rm = TRUE)) %>%
  mutate(sig_caption = case_when(sig_flag >= 1 ~ "*Significant change from previous year", TRUE ~ "")) %>% #Create label to add to table if significant value is found
  pull(sig_caption)

# table 6 hospital review -------------------------------------------------

#set hospital review table using data from standard data
       
#Set up flextable
dcrs_table_hosp <- flextable(DCRS_Data_Hosp) %>%
  set_header_labels(DCRS_Data_Hosp, Locname = "Hospital", 
                    case_in_order = "Case in Order", 
                    case_not_in_order = "Case not in Order", 
                    case_report_pf = "Reported to PF",
                    case_total = "Total Number reviews") %>%
  merge_at(i = 1, j = 2:3, part = "header") %>%
  merge_at(i = 1, j = 4:5, part = "header") %>%
  merge_at(i = 1, j = 6:7, part = "header") %>%
  merge_at(i = 1, j = 8:9, part = "header") %>%
  align(i = 1, part = "header", align = "left") %>%
  bold(i = 1, part = "header", bold = TRUE) %>%
  fontsize(size = 8, part = "all") %>%
  vline(j=c(1, 3, 5, 7), border = brdr) %>%
  hline(i = (nrow(DCRS_Data_Hosp)-1), border = brdr) %>%
  border_outer(border = brdr) %>%
  fix_border_issues() %>%
set_caption(caption = paste0("Table 6: Breakdown of review outcome by hospital from ", year3)) %>%
  hline(i = 1, border = brdr, part = "header") %>%
  align(j = 2:9, part = "body", align = "center") %>%
  width(j = 1, width = 1.3, unit = "in")  %>%
  width(j = c(2,4,6,8), width = 0.35, unit = "in") %>%
  width(j = c(3,5,7,9), width = 0.55, unit = "in") %>%
  height(i = 1:nrow(DCRS_Data_Hosp), height = 0.05, unit = "in", part = "body") 

#dcrs_table_hosp

#check if significant change is present and creates note for bottom of table
sig_table6 <- DCRS_Data_Hosp %>%
  mutate(sig_flagup = case_when(str_sub(in_order_percent,-2) == "*^" | str_sub(not_in_order_percent,-2) == "*^" |
                                str_sub(report_pf_percent,-2) == "*^"  ~ 1),
         sig_flagdown = case_when(str_sub(in_order_percent,-2) == "*v" | str_sub(not_in_order_percent,-2) == "*v" |
                                  str_sub(report_pf_percent,-2) == "*v"  ~ 1)) %>% #Flag when significance as been found
  summarise(sig_flagup = sum(sig_flagup, na.rm = TRUE),
            sig_flagdown = sum(sig_flagdown, na.rm = TRUE)) %>%
  mutate(sig_caption = case_when(sig_flagup >= 1 & sig_flagdown >= 1 ~ 
                                   "*^ significant increase from previous year \n *v significant decrease from previous year", 
                                 sig_flagup >= 1 ~ "*^ significant increase from previous year",
                                 sig_flagdown >= 1 ~ "*v significant decrease from previous year",
                                 TRUE ~ "")) %>% #Create label to add to table if significant value is found
  pull(sig_caption)

# table 7 hospice review --------------------------------------------------

#set hospice review table using data from standard data

#Set up flextable
if (nrow(DCRS_Data_Hospice) > 1) { #only create table if hospice data is present
  dcrs_table_hospice <- flextable(DCRS_Data_Hospice) %>%
    set_header_labels(DCRS_Data_Hospice, Locname = "Hospice", 
                      case_in_order = "Case in Order", 
                      case_not_in_order = "Case not in Order", 
                      case_report_pf = "Reported to PF",
                      case_total = "Total Number reviews") %>%
    merge_at(i = 1, j = 2:3, part = "header") %>%
    merge_at(i = 1, j = 4:5, part = "header") %>%
    merge_at(i = 1, j = 6:7, part = "header") %>%
    merge_at(i = 1, j = 8:9, part = "header") %>%
    align(i = 1, part = "header", align = "left") %>%
    bold(i = 1, part = "header", bold = TRUE) %>%
    fontsize(size = 8, part = "all") %>%
    vline(j=c(1, 3, 5, 7), border = brdr) %>%
    hline(i = (nrow(DCRS_Data_Hospice)-1), border = brdr) %>%
    border_outer(border = brdr) %>%
    fix_border_issues() %>%
  set_caption(caption = paste0("Table 7: Breakdown of review outcome by hospice from ", year3)) %>%
    hline(i = 1, border = brdr, part = "header") %>%
    align(j = 2:9, part = "body", align = "center") %>%
    width(j = 1, width = 1.3, unit = "in")  %>%
    width(j = c(2,4,6,8), width = 0.35, unit = "in") %>%
    width(j = c(3,5,7,9), width = 0.55, unit = "in") %>%
    height(i = 1:nrow(DCRS_Data_Hospice), height = 0.05, unit = "in", part = "body")
  
#check if significant change is present and creates note for bottom of table  
sig_table7 <- DCRS_Data_Hospice %>%
    mutate(sig_flagup = case_when(str_sub(in_order_percent,-2) == "*^" | str_sub(not_in_order_percent,-2) == "*^" |
                                    str_sub(report_pf_percent,-2) == "*^"  ~ 1),
           sig_flagdown = case_when(str_sub(in_order_percent,-2) == "*v" | str_sub(not_in_order_percent,-2) == "*v" |
                                      str_sub(report_pf_percent,-2) == "*v"  ~ 1)) %>% #Flag when significance as been found
    summarise(sig_flagup = sum(sig_flagup, na.rm = TRUE),
              sig_flagdown = sum(sig_flagdown, na.rm = TRUE)) %>%
    mutate(sig_caption = case_when(sig_flagup >= 1 & sig_flagdown >= 1 ~ 
                                     "*^ significant increase from previous year \n *v significant decrease from previous year", 
                                   sig_flagup >= 1 ~ "*^ significant increase from previous year",
                                   sig_flagdown >= 1 ~ "*v significant decrease from previous year",
                                   TRUE ~ "")) %>% #Create label to add to table if significant value is found
    pull(sig_caption)
   
  } else {sig_table7 = ""} #create blank if no hospice data present





####Blank tables for DCRS (currently not in use)

#Key Information Summary table
#kis_table_data <- data.frame(Period = c("Total number of reviews", "Completed KIS", "Percentage"),
 #                            S1 = c("", "", ""),
  #                           B1 = c("", "", ""),
   #                          S2 = c("", "", ""),
    #                         B2 = c("", "", ""))


#KIS Table 
#Wrangle and format KIS data for KIS summary table
dcrs_data_wrangle_kis <- dcrs_data_kis_all %>%
  pivot_longer(cols = !c(Year, `Health Board`), 
               names_to = 'event', 
               values_to = 'count') %>%
  pivot_wider(names_from = c(Year, `Health Board`), 
              values_from = 'count') %>%
  select(event, `2_Scotland`, `2_Board`, `3_Scotland`, `3_Board`) %>%
  mutate(`2_Scotland` = as.character(`2_Scotland`),
         `2_Board` = as.character(`2_Board`),
         `3_Scotland` = as.character(`3_Scotland`),
         `3_Board` = as.character(`3_Board`),
         `2_Scotland` = case_when(event == 'kis_percent' ~ paste0(`2_Scotland`,"%"),
                                  TRUE ~ `2_Scotland`),
         `2_Board` = case_when(event == 'kis_percent' ~ paste0(`2_Board`,"%"),
                               TRUE ~ `2_Board`),
         `3_Scotland` = case_when(event == 'kis_percent' ~ paste0(`3_Scotland`,"%"),
                                  TRUE ~ `3_Scotland`),
         `3_Board` = case_when(event == 'kis_percent' ~ paste0(`3_Board`,"%"),
                               TRUE ~ `3_Board`))

#KIS Summary table
kis_table <- flextable(dcrs_data_wrangle_kis) %>%
  set_header_labels(event = "", `2_Scotland` = "Scotland", `2_Board` = "Board", `3_Scotland` = "Scotland", `3_Board` = "Board") %>%  #Change header titles
  compose(i = 1, j = 1, as_paragraph(as_chunk("Total number of reviews"))) %>% 
  compose(i = 2, j = 1, as_paragraph(as_chunk("Completed KIS"))) %>%  #Change row titles
  compose(i = 3, j = 1, as_paragraph(as_chunk("Percentage"))) %>% 
  padding(padding.top = 1, part = "all") %>%
  padding(padding.bottom = 1, part = "all") %>%
  add_header_row(values = c("Period", year2, year3), 
                 colwidths = c(1,2,2)) %>% #Add header for years
  align(i = 1, part = "header", align = "center") %>%
  bold(i = 1:2, part = "header", bold = TRUE) %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>%
  align(i = 1, j = 1, part = "header", align = "left") %>% 
  valign(i = 1, j = 1, part = "header", valign = "top") %>%
  align(i = 1:2, j = 2:5, part = "header", align = "center") %>%  
  bold(i = 1:3, part = "body", bold = TRUE) %>% 
  align(j = 2:5, part = "body", align = "right") %>%
  fontsize(size = 10, part = "all") %>%
  vline(j=1:4, border = brdr) %>% #Add borders (shape created earlier)
  hline(i=1:3, border = brdr) %>%
  border_outer(border = brdr) %>%
  fix_border_issues() %>%
  bg(bg = "#C4D9F3", part = "header") %>%
  bg(i=1:3, j=1, bg = "#C4D9F3", part = "body") %>%
  width(j = 1, width = 2.3, unit = "in")  %>%
  width(j = 2:5, width = 1, unit = "in")
 

kis_table


#Timeline of reduction in DCRS service during pandemic
reduction_data <- data.frame(D1 = c("24-Mar-20", "11-May-20", "10-Jun-20", "22-Jul-20", "03-Aug-20",
                                         "31-Aug-20", "24-Nov-20", "18-Jan-21", "15-Mar-21", "29-Mar-21", "10-May-21"),
                              S1 = c("Selection rate suspended", "4%", "8%", "10%", "12%", 
                                     "Normal selection rate and process implemented", "12%", "8%", "10%", 
                                      "12%", "Normal selection rate and process implemented"),
                              H1 = c("", "Yes", "Yes", "Yes", "Yes", "", "Yes", "Yes", "Yes", "Yes", ""),
                              B1 = c("", "", "", "", "", "", "", "", "", "", ""),
                              D2 = c("04-Oct-21", "29-Nov-21", "29-Dec-21", "24-Jan-22", "07-Feb-22", "07-Mar-22", 
                                     "09-Jan-23", "06-Feb-23", "20-Feb-23", "06-Mar-23", "20-Mar-23"),
                              S2 = c("8%", "10%", "8%", "10%", "12%", "Normal selection rate and process implemented",
                                     "6%", "8%", "10%", "12%", "Normal selection rate and process implemented"),
                              H2 = c("Yes", "Yes", "Yes", "Yes", "Yes", "", "Yes", "Yes", "Yes", "Yes", ""))


brdr2 <- fp_border(color = "white", width = 1.3)
reduction_table <- flextable(reduction_data) %>%
  set_header_labels(D1 = "Date", S1= "Selection Rate", H1 = "Hybrid Reviews", B1 = "",
                    D2 = "Date", S2= "Selection Rate", H2 = "Hybrid Reviews") %>%  #Change header titles
  bold(i = 1, part = "header", bold = TRUE) %>%
  fontsize(size = 9, part = "all") %>%
  vline(j=1:6, border = brdr) %>% #Add borders (shape created earlier)
  hline(i=1:11, border = brdr) %>% 
  surround(j=4, 
           border= brdr2,
           part = "all") %>%
  surround(border.left = brdr,
           border.right = brdr,
           part = "all") %>%
  merge_at(j = 4) %>%
  fix_border_issues() %>%
  bg(j=c(1,2,3,5,6,7), bg = "#C4D9F3", part = "header") %>%
  width(j = c(1, 5), width = 0.9, unit = "in")  %>%
  width(j = c(2, 6), width = 3, unit = "in") %>%
  width(j = c(3, 7), width = 0.7, unit = "in") %>%
  width(j = 4, width = 0.3, unit = "in") %>%
  height(i = 1, height = 0.4, unit = "in", part = "header") %>%
  hrule(rule = "exact", part = "body") 


reduction_table



