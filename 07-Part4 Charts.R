#############  Wrangles the data and creates the charts and corresponding figures for the narrative used in part 4 of the report

Board <- hdr

#######Part 4 - Charts used in service breakdown and corresponding values for narrative#######

# Chart 5 closure categories -----------------------------------------------------------------

##closure chart 5 - total breakdown##
#This chart is not selected for larger boards. More detailed primary/secondary care breakdown is used instead

  
#First stage of wrangling data into right layout and get percentages
dcrs_data_wrangle_closure <- dcrs_data_report_all %>%
  select(Year, `Health Board`, closure_category, causal_timscale, cause_of_death,
         conditions_omitted, disposal_hazard, sequence_of_cause, cause_too_vague ) %>%
  filter(Year == 3) %>%
  transmute(across(c(causal_timscale, 
                     cause_of_death,
                     conditions_omitted,
                     disposal_hazard,
                     sequence_of_cause,
                     cause_too_vague),list(~ ./closure_category),
                   .names="{.col}_rate")) %>%
  select(causal_timscale_rate, cause_of_death_rate, conditions_omitted_rate, disposal_hazard_rate,
         sequence_of_cause_rate, cause_too_vague_rate)

#Flip data
dcrs_data_wrangle_closure2 <- dcrs_data_wrangle_closure %>%
  pivot_longer(cols = !c(Year, `Health Board`), 
               names_to = 'closure_category', 
               values_to = 'closure_rate') %>%
  mutate(closure_category = str_wrap(closure_category , width = 18), #wrap to better fit text
         closure_percent = percent(closure_rate, accuracy = 0.1),
         rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(closure_rate), TRUE ~ 0))  %>%
  select(`Health Board`, closure_category, closure_rate, closure_percent, rank_cases) %>%
  filter(closure_rate != 0) #Remove any categories with no cases for neater chart

case_count <- dcrs_data_wrangle_closure2 %>%
  count(closure_category)

case_count_board <- dcrs_data_wrangle_closure2 %>%
  summarise(rank_cases = sum(rank_cases)) %>% pull(rank_cases)

if(case_count_board >= 1) {
  #Match back with wrangled dataset and filter out any rows with no data
  dcrs_data_wrangle_closure2 <- dcrs_data_wrangle_closure2 %>%
    left_join(case_count) %>%
    filter(n == 2) } else
    {dcrs_data_wrangle_closure2}

#Create bar chart 5 in ranked order
dcrs_closure_plot <- ggplot(dcrs_data_wrangle_closure2, aes(fct_reorder(closure_category, closure_rate), 
                                                            closure_rate,fill = `Health Board`)) +
  geom_bar(stat = "identity", position = 'dodge', width=0.8) + #set bar aesthetics
  scale_fill_manual(values = c("#8BB5E8", "#004578"), breaks=c('Scotland', 'Board'), name = "Location") +  #set bar aesthetics  
  labs(title = "Chart 5: Breakdown of clinical closure categories for ‘not in order’",   #set titles
       subtitle = year3,
       y = "",
       x = "") +
  scale_x_discrete(labels = c("causal_timscale_rate" = "Causal timescales incorrect",   #rename variables
                              "cause_of_death_rate" = "Cause of Death incorrect", "conditions_omitted_rate" = "Conditions omitted",
                              "disposal_hazard_rate" = "Disposal Hazard incorrect", 
                              "sequence_of_cause_rate" = "Sequence of Cause of Death incorrect", 
                              "cause_too_vague_rate" = "Cause of death too vague")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(dcrs_data_wrangle_closure2$closure_rate)*1.15),
                     labels = scales::percent_format(accuracy = 1)) +  #set percentage                       #set dynamic axis limit
  geom_text(aes(label=closure_percent), position = position_dodge(width = .9), hjust=-0.2, size = 2.5) +  #set labels
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  coord_flip()     #flip chart on side

dcrs_closure_plot        
#dcrs_data_wrangle_closure


# Chart 5 primary/secondary closure breakdown-----------------------------------------------


##closure chart 5 - primary/secondary care breakdown##
#This chart is not selected for smaller boards. More previous less detailed breakdown is used instead

#set data for case of death primary/secondary case breakdown
dcrs_data_cause_ps <- dcrs_data %>%
  filter(`Health Board` == Board, 
         `Case Type` == "Standard",
         `Created On` < end_date) %>%
  mutate(`Health Board` = 'Board') %>%
  group_by(Year, `Health Board`, primary_secondary) %>%
  summarise(cause_too_vague = sum(`Cause of death too vague1` == "Yes", na.rm = TRUE),
            causal_timscale = sum(`Causal timescales incorrect1` == "Yes", na.rm = TRUE),
            cause_of_death = sum(`Cause of Death incorrect1` == "Yes", na.rm = TRUE),
            conditions_omitted = sum(`Conditions omitted1` == "Yes", na.rm = TRUE),
            disposal_hazard = sum(`Disposal Hazard incorrect1` == "Yes", na.rm = TRUE),
            sequence_of_cause = sum(`Sequence of Cause of Death incorrect1` == "Yes", na.rm = TRUE)
            , .groups = "rowwise") %>%
  select(Year, `Health Board`, primary_secondary, causal_timscale, cause_of_death,
         conditions_omitted, disposal_hazard, sequence_of_cause, cause_too_vague) %>%
  filter(Year == 1 | Year == 2 | Year == 3) 

#Get clinical closure category totals for use in percentages (a case can have more than one clinical closure category)
dcrs_data_cause_closure <- dcrs_data %>%
  filter(`Health Board` == Board, 
         `Case Type` == "Standard",
         `Created On` < end_date) %>%
  mutate(`Health Board` = 'Board') %>%
  group_by(Year) %>%
  summarise(closure_category = sum(`Closure Category - Clinical1` == "Yes", na.rm = TRUE)) %>%
  select(Year,  closure_category) %>%
  filter(Year == 1 | Year == 2 | Year == 3) 

#Combine clincal closure category total with overall total
dcrs_data_cause_ps <- dcrs_data_cause_ps %>%
  left_join(dcrs_data_cause_closure) %>%
  mutate(Year = as.character(Year),
# update year references --------------------------------------------------
         Year = case_when(Year == "1" ~ y1,
                          Year == "2" ~ y2,
                          Year == "3" ~ y3))

#First stage of wrangling data into right layout and get percentages
dcrs_data_wrangle_ps <- dcrs_data_cause_ps %>%
  filter(`Health Board` == "Board",
         primary_secondary != "Uknown") %>%
  transmute(across(c(causal_timscale, 
                     cause_of_death,
                     conditions_omitted,
                     disposal_hazard,
                     sequence_of_cause,
                     cause_too_vague),list(~ ./closure_category),
                   .names="{.col}_rate")) %>%
  select(causal_timscale_rate, cause_of_death_rate, conditions_omitted_rate, disposal_hazard_rate, 
         sequence_of_cause_rate, cause_too_vague_rate)

#Flip data
dcrs_data_wrangle_ps2 <- dcrs_data_wrangle_ps %>%
  pivot_longer(cols = !c(Year, `Health Board`, primary_secondary), 
               names_to = 'closure_category', 
               values_to = 'closure_rate') %>%
  mutate(closure_rate = case_when(is.na(closure_rate) ~ 0, TRUE ~ closure_rate),
    closure_percent = percent(closure_rate, accuracy = 1)) %>%
  select(Year, primary_secondary, closure_category, closure_rate, closure_percent)

#Get overall rate to use in chart lables
ps_total <- dcrs_data_wrangle_ps2 %>%
  group_by(Year, closure_category) %>%
  mutate(closure_rate_total = case_when(Year == '9' ~ sum(closure_rate)))

#Combine primary/secondary care breakdown and overall rate.  Rename categories to be more readable
dcrs_data_wrangle_ps2 <- dcrs_data_wrangle_ps2 %>%
  left_join(ps_total) %>%
  mutate(closure_category = str_replace(closure_category, "causal_timscale_rate", "Causal timescales incorrect"), 
         closure_category = str_replace(closure_category, "cause_of_death_rate", "Cause of death incorrect"),
         closure_category = str_replace(closure_category, "conditions_omitted_rate", "Conditions omitted"), 
         closure_category = str_replace(closure_category, "disposal_hazard_rate", "Disposal hazard incorrect"),
         closure_category = str_replace(closure_category, "sequence_of_cause_rate", "Sequence of cause of death incorrect"),
         closure_category = str_replace(closure_category, "cause_too_vague_rate", "Cause of death too vague"),
         closure_category = str_wrap(closure_category, width = 10))


dcrs_closure_ps_plot <- ggplot(dcrs_data_wrangle_ps2, aes(Year, closure_rate, fill = primary_secondary)) +
  geom_col() +
  facet_wrap(~ fct_reorder(closure_category, closure_rate_total), strip.position = "bottom", nrow = 1) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(dcrs_data_wrangle_ps2$closure_rate_total)+0.02),
                     labels = scales::percent_format(accuracy = 1)) +  #set percentage
# update year references --------------------------------------------------
  labs(title = paste0("Chart 5: Breakdown of clinical closure categories ‘not in order’ for years ", y1, ", ", y2, " and ", y3, " by \nprimary and secondary care"), #set titles
       y = "", x = "Note: Year 7 - 2021/22     Year 8 - 2022/23     Year 9 - 2023/24") +
  scale_fill_manual(values = c("#143965", "#8BB5E8")) +
  geom_text(aes(label=closure_percent), size = 2, colour = "white", position = position_stack(vjust = .5)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(vjust = 0.5, hjust=0.2),
        strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1,0.81),
        legend.box.background = element_rect(colour = "grey"),
        panel.background = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 8,face="italic", hjust = 0.02),
        strip.background = element_blank(),
        panel.spacing = unit(0, 'points'))

dcrs_closure_ps_plot

#ggsave(dcrs_closure_plot, 
#       filename = "Charts/dcrs_closure_plot.png",
#       device = "png",
#       height = 3.2, width = 6, units = "in")


# chart 6 death too vague -------------------------------------------------

##death too vague chart 6##

#First stage of wrangling data into right layout and get rates
dcrs_data_wrangle_death_cause <- dcrs_data_report_all %>%
  select(Year, `Health Board`, cause_too_vague, histology, psite_metastatic_missing, pneumonia_subtype, dementia_subtype, microbiology, sepsis_source, diabetes_subtype, stroke, lifestyle_factor) %>%
  filter(Year == 3) %>%
  transmute(across(c(histology, 
                     psite_metastatic_missing,
                     pneumonia_subtype,
                     dementia_subtype,
                     microbiology,
                     sepsis_source,
                     diabetes_subtype,
                     stroke,
                     lifestyle_factor),list(~ ./cause_too_vague),
                   .names="{.col}_rate")) %>%
  select(histology_rate, psite_metastatic_missing_rate, pneumonia_subtype_rate, dementia_subtype_rate, microbiology_rate, sepsis_source_rate, diabetes_subtype_rate, stroke_rate, lifestyle_factor_rate)

#Flip data for charting layout
dcrs_data_wrangle_death_cause_2 <- dcrs_data_wrangle_death_cause %>%
  pivot_longer(cols = !c(Year, `Health Board`), 
               names_to = 'cause_too_vague', 
               values_to = 'cause_too_vague_rate') %>%
  mutate(cause_too_vague = str_wrap(cause_too_vague, width = 18)) %>% #wrap to better fit text
  mutate(cause_too_vague_percent = percent(cause_too_vague_rate, accuracy = 0.1),
         rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(cause_too_vague_rate),
                                TRUE ~ 0)) %>% 
  select(`Health Board`, cause_too_vague, cause_too_vague_rate, cause_too_vague_percent, rank_cases) %>%
  filter(cause_too_vague_rate != 0) #Remove any causes with no cases for neater chart

case_count <- dcrs_data_wrangle_death_cause_2 %>%
  count(cause_too_vague)

case_count_board <- dcrs_data_wrangle_death_cause_2 %>%
  summarise(rank_cases = sum(rank_cases)) %>% pull(rank_cases)

if(case_count_board >= 1) {
#Match back with wrangled dataset and filter out any rows with no data
dcrs_data_wrangle_death_cause_2 <- dcrs_data_wrangle_death_cause_2 %>%
  left_join(case_count) %>%
  filter(n == 2) } else
  {dcrs_data_wrangle_death_cause_2}

#Create bar chart 6 in ranked order
dcrs_closure_plot_death <- ggplot(dcrs_data_wrangle_death_cause_2, aes(fct_reorder(cause_too_vague, rank_cases), 
                                                                       cause_too_vague_rate,  fill = `Health Board`)) +
  geom_bar(stat = "identity", position = 'dodge', width=0.8) + #set bar aesthetics
  scale_fill_manual(values = c("#8BB5E8", "#004578"), breaks=c('Scotland', 'Board'), name = "Location") +  #set bar aesthetics
  labs(title = "Chart 6: Breakdown of Cause of Death too vague",   #set titles
       subtitle = year3,
       y = "",
       x = "") +
  scale_x_discrete(labels = c("histology_rate" = "Histology",   #rename variables
                              "psite_metastatic_missing_rate" = "Primary site or metastatic site(s) missing", 
                              "pneumonia_subtype_rate" = "Pneumonia sub-type",
                              "dementia_subtype_rate" = "Dementia sub-type", 
                              "microbiology_rate" = "Microbiology",
                              "sepsis_source_rate" = "Source of sepsis",
                              "diabetes_subtype_rate" = "Diabetes sub-type", 
                              "stroke_rate" = "Stroke",
                              "lifestyle_factor_rate" = "Lifestyle factors (smoking, obesity, alcohol")) +                                   
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(dcrs_data_wrangle_death_cause_2$cause_too_vague_rate)*1.15),
                     labels = scales::percent_format(accuracy = 1)) +  #set percentage                       #set dynamic axis limit
  geom_text(aes(label=cause_too_vague_percent), position = position_dodge(width = .9), hjust=-0.2, size = 3) +  #set labels
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  coord_flip()     #flip chart on side


dcrs_closure_plot_death
#dcrs_data_wrangle__death_cause_2

ggsave(dcrs_closure_plot_death, 
       filename = "Charts/dcrs_closure_plot_death.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

# Chart 7 closure admin ---------------------------------------------------
##closure admin chart 7##

#First stage of wrangling data into right layout and getting percentages
dcrs_data_wrangle_admin_closure <- dcrs_data_report_all %>%
  select(Year, `Health Board`, closure_category_admin, spelling_error, deceased_details_incorrect,  doctors_details_incorrect, abbreviations_used, time_incorrect, incorrectly_complete, 
         attendance_incorrect, address_incorrect, pm_information_incorrect, legibility, 
         consultant_incorrect, other_incorrect) %>%
  filter(Year == 3) %>%
  transmute(across(c(spelling_error, 
                     deceased_details_incorrect,
                     doctors_details_incorrect,
                     abbreviations_used,
                     time_incorrect,
                     incorrectly_complete,
                     attendance_incorrect,
                     address_incorrect,
                     pm_information_incorrect,
                     legibility,
                     consultant_incorrect,
                     other_incorrect),list(~ ./closure_category_admin),
                   .names="{.col}_rate")) %>%
  select(spelling_error_rate, deceased_details_incorrect_rate, doctors_details_incorrect_rate,
         abbreviations_used_rate, time_incorrect_rate, incorrectly_complete_rate, 
         attendance_incorrect_rate, address_incorrect_rate, pm_information_incorrect_rate, legibility_rate, 
         consultant_incorrect_rate, other_incorrect_rate)

#Flip data for charting layout
dcrs_data_wrangle_admin_closure2 <- dcrs_data_wrangle_admin_closure %>%
  pivot_longer(cols = !c(Year, `Health Board`), 
               names_to = 'closure_category_admin', 
               values_to = 'closure_rate_admin') %>%
  mutate(closure_category_admin = str_wrap(closure_category_admin , width = 18)) %>% #wrap to better fit text
  mutate(closure_percent_admin = percent(closure_rate_admin, accuracy = 0.1),
         rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(closure_rate_admin),
                         TRUE ~ 0)) %>% 
  select(`Health Board`, closure_category_admin, closure_rate_admin, closure_percent_admin, rank_cases) %>%
  filter(closure_rate_admin != 0) #Remove any categories with no cases for neater chart

case_count <- dcrs_data_wrangle_admin_closure2 %>%
  count(closure_category_admin)

case_count_board <- dcrs_data_wrangle_admin_closure2 %>%
  summarise(rank_cases = sum(rank_cases)) %>% pull(rank_cases)

if(case_count_board >= 1) {
  #Match back with wrangled dataset and filter out any rows with no data
  dcrs_data_wrangle_admin_closure2 <- dcrs_data_wrangle_admin_closure2 %>%
    left_join(case_count) %>%
    filter(n == 2) } else
    {dcrs_data_wrangle_admin_closure2}

#Create bar chart 7 in ranked order
dcrs_closure_plot_admin <- ggplot(dcrs_data_wrangle_admin_closure2, aes(fct_reorder(closure_category_admin, rank_cases), 
                                                                        closure_rate_admin,  fill = `Health Board`)) +
  geom_bar(stat = "identity", position = 'dodge', width=0.8) + #set bar aesthetics
  scale_fill_manual(values = c("#8BB5E8", "#004578"), breaks=c('Scotland', 'Board'), name = "Location") +  #set bar aesthetics
  labs(title = "Chart 7: Breakdown of administrative closure categories for ‘not in order’",   #set titles
       subtitle = year3,
       y = "",
       x = "") +
  scale_x_discrete(labels = c("spelling_error_rate" = "Certifying Doctor spelling error",   #rename variables
                              "deceased_details_incorrect_rate" = "Deceased details incorrect", 
                              "doctors_details_incorrect_rate" = "Certifying Doctor's details incorrect",
                              "abbreviations_used_rate" = "Abbreviations used", 
                              "time_incorrect_rate" = "Date or time of death incorrect",
                              "incorrectly_complete_rate" = "Extra information (X box) incorrectly complete",
                              "attendance_incorrect_rate" = "Attendance on the deceased incorrect", 
                              "address_incorrect_rate" = "Place of death address incorrect",
                              "pm_information_incorrect_rate" = "PM information incorrect",
                              "legibility_rate" = "Legibility", 
                              "consultant_incorrect_rate" = "Consultant's name incorrect",
                              "other_incorrect_rate" = "Other additional information incorrect")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(dcrs_data_wrangle_admin_closure2$closure_rate_admin)*1.15),
                     labels = scales::percent_format(accuracy = 1)) +  #set percentage                        #set dynamic axis limit
  geom_text(aes(label=closure_percent_admin), position = position_dodge(width = .9), hjust=-0.2, size = 2) +  #set labels
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  coord_flip()     #flip chart on side

dcrs_closure_plot_admin
#dcrs_data_wrangle__two_closure

ggsave(dcrs_closure_plot_admin, 
       filename = "Charts/dcrs_closure_plot_admin.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

# chart 8 PF breakdown ----------------------------------------------------
##PF breakdown chart 8##

#Third stage of wrangling data into right layout and get percentages
PF_data_wrangle <- dcrs_data_report_all %>%
  select(Year,`Health Board`, total_report_to_pf, choking, concerns_over_care, drug_related, neglect_exposure, fracture_trauma, industrial_disease, infectious_disease, legal_order, flagged_error, other_report_pf) %>%
  filter(Year == 3 ) %>%
  transmute(across(c(choking, 
                     concerns_over_care,
                     drug_related,
                     neglect_exposure,
                     fracture_trauma,
                     industrial_disease,
                     infectious_disease,
                     legal_order,
                     flagged_error,
                     other_report_pf),list(~ ./total_report_to_pf),
                   .names="{.col}_rate")) %>%
  select(choking_rate, concerns_over_care_rate, drug_related_rate, neglect_exposure_rate, fracture_trauma_rate, industrial_disease_rate, infectious_disease_rate, legal_order_rate, flagged_error_rate, other_report_pf_rate)

if (nrow(PF_data_wrangle != 0)) {

#Flip data for charting layout
PF_data_wrangle_2 <- PF_data_wrangle %>%
  pivot_longer(cols = !c(Year, `Health Board`), 
               names_to = 'total_report_to_pf', 
               values_to = 'total_report_to_pf_rate') %>%
  mutate(total_report_to_pf_percent = percent(total_report_to_pf_rate, accuracy = 0.1),
         rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(total_report_to_pf_rate),
                                TRUE ~ 0)) %>% 
  select(`Health Board`, total_report_to_pf, total_report_to_pf_rate, total_report_to_pf_percent, rank_cases) %>%
  filter(total_report_to_pf_rate != 0) #Remove any categories with no cases for neater chart


#Find cases with no data to be filtered out
case_count <- PF_data_wrangle_2 %>%
  count(total_report_to_pf)

case_count_board <- PF_data_wrangle_2 %>%
  summarise(rank_cases = sum(rank_cases)) %>% pull(rank_cases)

if(case_count_board >= 1) {
  #Match back with wrangled dataset and filter out any rows with no data
  PF_data_wrangle_2 <- PF_data_wrangle_2 %>%
    left_join(case_count) %>%
    filter(n == 2) } else
    {PF_data_wrangle_2}

#Create bar chart 8 in ranked order
dcrs_PF_plot <- ggplot(PF_data_wrangle_2, aes(fct_reorder(total_report_to_pf, rank_cases), 
                                              total_report_to_pf_rate,  fill = `Health Board`)) +
  geom_bar(stat = "identity", position = 'dodge', width=0.8) + #set bar aesthetics
  scale_fill_manual(values = c("#8BB5E8", "#004578"), breaks=c('Scotland', 'Board'), name = "Location") +
  labs(title = "Chart 8: Breakdown of cases reported to Procurator Fiscal",   #set titles
       subtitle = year3,
       y = "",
       x = "") +
  scale_x_discrete(labels = c("choking_rate" = "Choking",   #rename variables
                              "concerns_over_care_rate" = "Concerns Over Care",
                              "drug_related_rate" = "Drug Related", 
                              "neglect_exposure_rate" = "Neglect or Exposure",
                              "fracture_trauma_rate" = "Fracture or Trauma",
                              "industrial_disease_rate" = "Industrial Disease",
                              "infectious_disease_rate" = "Infectious Disease",
                              "legal_order_rate" = "Legal Order",
                              "flagged_error_rate" = "Flagged in Error",
                              "other_report_pf_rate" = "Other Report to PF")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(PF_data_wrangle_2$total_report_to_pf_rate)*1.15),
                     labels = scales::percent_format(accuracy = 1)) +  #set percentage                      #set dynamic axis limit
  geom_text(aes(label=total_report_to_pf_percent), position = position_dodge(width = .9), hjust=-0.2, size = 3) +  #set labels
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  coord_flip()     #flip chart on side

#If no data avialble for the board provide message instead of chart    
} else {"Not enough data"}

dcrs_PF_plot
#dcrs_data_wrangle_enquiry

ggsave(dcrs_PF_plot, 
       filename = "Charts/dcrs_PF_plot.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

# chart 9 enquiry line ----------------------------------------------------
##enquiry line chart 9##

#Wrangale data into layout for initial analysis and get percentages
enquiry_data_wrangle <- dcrs_data_enquiry_all %>%
  select(Year, `Health Board`, enquiry_category_total, gp_clinical_advice, gp_process_advice, hospital_clinical_advice, 
         hospital_process_advice, hospice_clinical_advice, hospice_process_advice, funeral_director, informant_or_family, 
         procurator_fiscal, signposted, other) %>%
  filter(Year == 3) %>%
  transmute(across(c(gp_clinical_advice, 
                     gp_process_advice,
                     hospital_clinical_advice,
                     hospital_process_advice,
                     hospice_clinical_advice,
                     hospice_process_advice,
                     funeral_director,
                     informant_or_family,
                     procurator_fiscal,
                     signposted,
                     other),list(~ ./enquiry_category_total),
                   .names="{.col}_rate")) %>%
  select(gp_clinical_advice_rate, gp_process_advice_rate, hospital_clinical_advice_rate, hospital_process_advice_rate,
         hospice_clinical_advice_rate, hospice_process_advice_rate, funeral_director_rate, informant_or_family_rate,  
         signposted_rate, procurator_fiscal_rate, other_rate)

  #Flip data for charting layout
enquiry_data_wrangle_2 <- enquiry_data_wrangle %>%
    pivot_longer(cols = !c(Year, `Health Board`), 
                 names_to = 'enquiry_category_total', 
                 values_to = 'enquiry_rate') %>%
    mutate(enquiry_percent = percent(enquiry_rate, accuracy = 0.1),
           rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(enquiry_rate),
                                  TRUE ~ 0)) %>% 
    select(`Health Board`, enquiry_category_total, enquiry_rate, enquiry_percent, rank_cases) %>%
    filter(enquiry_rate != 0)

#Find cases with no data to be filtered out
case_count <- enquiry_data_wrangle_2 %>%
    count(enquiry_category_total)

case_count_board <- enquiry_data_wrangle_2 %>%
  summarise(rank_cases = sum(rank_cases)) %>% pull(rank_cases)

if(case_count_board >= 1) {
  #Match back with wrangled dataset and filter out any rows with no data
  enquiry_data_wrangle_2 <- enquiry_data_wrangle_2 %>%
    left_join(case_count) %>%
    filter(n == 2) } else
    {enquiry_data_wrangle_2}

#Create bar chart 9 in ranked order
dcrs_enquiry_plot <- ggplot(enquiry_data_wrangle_2, aes(fct_reorder(enquiry_category_total, rank_cases), 
                                                          enquiry_rate,  fill = `Health Board`)) +
    geom_bar(stat = "identity", position = 'dodge', width=0.8) + #set bar aesthetics
    scale_fill_manual(values = c("#8BB5E8", "#004578"), breaks=c('Scotland', 'Board'), name = "Location") +  #set bar aesthetics
    labs(title = "Chart 9: Enquiry line calls - Type of advice required",   #set titles
         subtitle = year3,
         y = "",
         x = "") +
    scale_x_discrete(labels = c("gp_clinical_advice_rate" = "GP Clinical Advice",   #rename variables
                                "gp_process_advice_rate" = "GP Process Advice",
                                "hospital_clinical_advice_rate" = "Hospital Clinical Advice", 
                                "hospital_process_advice_rate" = "Hospital Process Advice",
                                "hospice_clinical_advice_rate" = "Hospice Clinical Advice",
                                "hospice_process_advice_rate" = "Hospice Process Advice",
                                "funeral_director_rate" = "Funeral Director",
                                "informant_or_family_rate" = "Informant or Family",
                                "procurator_fiscal_rate" = "Procurator Fiscal",
                                "signposted_rate" = "Signposted", 
                                "other_rate" = "Other")) +
    scale_y_continuous(expand=c(0, 0), limits=c(0, max(enquiry_data_wrangle_2$enquiry_rate)*1.15),
                       labels = scales::percent_format(accuracy = 1)) +  #set percentage                     #set dynamic axis limit
    geom_text(aes(label=enquiry_percent), position = position_dodge(width = .9), hjust=-0.2, size = 2.5) +  #set labels
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),  #set simple theme
          legend.title=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) + 
    coord_flip()     #flip chart on side


dcrs_enquiry_plot
#dcrs_data_wrangle_enquiry

ggsave(dcrs_enquiry_plot, 
       filename = "Charts/dcrs_enquiry_plot.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

# chart 10 hospital review ------------------------------------------------
##set hospital chart review data chart 10 - Done in 3 parts for each year then combined##

#First year data wrangle
DCRS_Data_Hosp1 <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         instgrp2 == "NHS Hospital",
         YEAR == paste0("Year ", y1)) %>%
  group_by(Locname) %>%
  summarise(case_in_order = sum(Review == "Case in Order"),
            case_not_in_order = sum(Review == "Case not in Order"),
            case_report_pf = sum(Review == "Reported to PF"),
            case_total = (case_in_order + case_not_in_order + case_report_pf)) %>%
  mutate(YEAR = y1) %>%
  select(Locname, YEAR, case_in_order,
         case_not_in_order,
         case_report_pf,
         case_total)

#Second Year data wrangle
DCRS_Data_Hosp2 <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         instgrp2 == "NHS Hospital",
         YEAR == paste0("Year ", y2)) %>%
  group_by(Locname) %>%
  summarise(case_in_order = sum(Review == "Case in Order"),
            case_not_in_order = sum(Review == "Case not in Order"),
            case_report_pf = sum(Review == "Reported to PF"),
            case_total = (case_in_order + case_not_in_order + case_report_pf)) %>%
  mutate(YEAR = y2) %>%
  select(Locname, YEAR, case_in_order,
         case_not_in_order,
         case_report_pf,
         case_total)

#Final (latest) year data wrangle
DCRS_Data_Hosp3 <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         instgrp2 == "NHS Hospital",
         YEAR == paste0("Year ", y3)) %>%
  group_by(Locname) %>%
  summarise(case_in_order = sum(Review == "Case in Order"),
            case_not_in_order = sum(Review == "Case not in Order"),
            case_report_pf = sum(Review == "Reported to PF"),
            case_total = (case_in_order + case_not_in_order + case_report_pf)) %>%
  mutate(YEAR = y3) %>%
  select(Locname, YEAR, case_in_order,
         case_not_in_order,
         case_report_pf,
         case_total)

#Combine 3 years
DCRS_Data_Hosp_Chart <- rbind(DCRS_Data_Hosp1, DCRS_Data_Hosp2, DCRS_Data_Hosp3)

#Get ranking for latest year
hosp_ranking <- DCRS_Data_Hosp3 %>%
  mutate(ranking = rank(desc(case_total)),
         ranking = case_when(ranking > 3 ~ 4,  #Find top 3 hospitals and set all others as "other" (request from DCRS)
                             TRUE ~ ranking)) %>%
  arrange(desc(case_total)) %>% #Set order from highest to lowest ranked
  select(Locname, ranking)

#Attach rankings for latest year, aggregate figures naming top 3 and "other", and get percentages
DCRS_Data_Hosp_Chart2 <- left_join(DCRS_Data_Hosp_Chart, hosp_ranking) %>%
  mutate(ranking = case_when(is.na(ranking) ~ 4,  #Find top 3 hospitals and set all others as "other"
                             TRUE ~ ranking),
         Locname = case_when(ranking > 3 | is.na(ranking) ~ "Other",
                             TRUE ~ Locname)) %>%
  arrange(desc(case_total)) %>% #  
  group_by(YEAR, Locname, ranking) %>%   #Aggregate all hospitals identified as "other"
  summarise(case_in_order = sum(case_in_order),
            case_not_in_order = sum(case_not_in_order),
            case_report_pf = sum(case_report_pf),
            case_total = sum(case_total)) %>%
  select(Locname, YEAR, ranking,
         case_in_order,
         case_not_in_order,
         case_report_pf) %>%
  pivot_longer(cols = !c(YEAR, Locname, ranking)) %>%
  group_by(YEAR, Locname) %>%
  mutate(case_percent = value / sum(value)) %>%
  ungroup() %>%
  mutate(label_flag = case_when(value/max(value) >= 0.07 ~ 1, TRUE ~ 0), #Only show label if more than 7% otherwise the label is too compressed
         case_percentage = case_when(label_flag == 1 ~ percent(case_percent, accuracy = 0.1)),
         case_percentage = case_when(case_percent != 0 ~ case_percentage),
         name = case_when(name == "case_in_order" ~ "Case in Order",
                          name == "case_not_in_order" ~ "Case Not in Order",
                          name == "case_report_pf" ~ "Case Report to PF"),
         name = factor(name, levels=c("Case Report to PF", "Case Not in Order", "Case in Order")),
         Locname = str_wrap(Locname , width = 18))

max_value <- DCRS_Data_Hosp_Chart %>% summarise(max_value = max(case_total)) %>% pull(max_value)

##create hospital bar chart 10##
hosp_review_chart <- ggplot(DCRS_Data_Hosp_Chart2, aes(YEAR, value, fill=name)) + 
  geom_col() +
  facet_wrap(~ fct_reorder(Locname, ranking), strip.position = "bottom", nrow = 1) +
  scale_x_discrete(labels = function(Review) str_wrap(Review, width = 10)) + 
# update year references --------------------------------------------------
  labs(title = paste0("Chart 10: Number of standard reviews by hospital and closure category for years ", y1, ", ", y2, "\nand ",y3),   #set titles
       y = "", x = "Note: Year 7 - 2021/22     Year 8 - 2022/23     Year 9 - 2023/24") +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#097480", "#8BB5E8", "#0F3D6B")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max_value+0.07)) +
  geom_text(aes(label=case_percentage), size = 2.5, check_overlap = TRUE, position = position_stack(vjust = 0.5), colour = "white") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.box.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 8,face="italic", hjust = 0.01),
        strip.background = element_blank(),
        panel.spacing = unit(0, 'points')) 

hosp_review_chart

ggsave(hosp_review_chart, 
       filename = "Charts/hosp_review_chart.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

# chart 11 hospice review -------------------------------------------------

##set hospice chart review data chart 11 - Done in 3 parts for each year then combined##

#First year data wrangle
DCRS_Data_Hospice1 <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         instgrp2 == "Hospice",
         YEAR == paste0("Year ", y1)) %>%
  group_by(Locname) %>%
  summarise(case_in_order = sum(Review == "Case in Order"),
            case_not_in_order = sum(Review == "Case not in Order"),
            case_report_pf = sum(Review == "Reported to PF"),
            case_total = (case_in_order + case_not_in_order + case_report_pf)) %>%
  mutate(YEAR = y1) %>%
  select(Locname, YEAR, case_in_order,
         case_not_in_order,
         case_report_pf,
         case_total)

#Second Year data wrangle
DCRS_Data_Hospice2 <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         instgrp2 == "Hospice",
         YEAR == paste0("Year ", y2)) %>%
  group_by(Locname) %>%
  summarise(case_in_order = sum(Review == "Case in Order"),
            case_not_in_order = sum(Review == "Case not in Order"),
            case_report_pf = sum(Review == "Reported to PF"),
            case_total = (case_in_order + case_not_in_order + case_report_pf)) %>%
  mutate(YEAR = y2) %>%
  select(Locname, YEAR, case_in_order,
         case_not_in_order,
         case_report_pf,
         case_total)

#Final (latest) year data wrangle
DCRS_Data_Hospice3 <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         instgrp2 == "Hospice",
         YEAR == paste0("Year ", y3)) %>%
  group_by(Locname) %>%
  summarise(case_in_order = sum(Review == "Case in Order"),
            case_not_in_order = sum(Review == "Case not in Order"),
            case_report_pf = sum(Review == "Reported to PF"),
            case_total = (case_in_order + case_not_in_order + case_report_pf)) %>%
  mutate(YEAR = y3) %>%
  select(Locname, YEAR, case_in_order,
         case_not_in_order,
         case_report_pf,
         case_total)

#Combine 3 years
DCRS_Data_Hospice_Chart <- rbind(DCRS_Data_Hospice1, DCRS_Data_Hospice2, DCRS_Data_Hospice3)

#Get ranking for latest year
hospice_ranking <- DCRS_Data_Hospice3 %>%
  mutate(ranking = rank(desc(case_total)),
         ranking = case_when(ranking > 3 ~ 4,  #Find top 3 hospitals and set all others as "other" (request from DCRS)
                             TRUE ~ ranking)) %>%
  arrange(desc(case_total)) %>% #Set order from highest to lowest ranked
  select(Locname, ranking)

#Attach rankings for latest year, aggregate figures naming top 3 and "other", and get percentages
DCRS_Data_Hospice_Chart2 <- left_join(DCRS_Data_Hospice_Chart, hospice_ranking) %>%
  mutate(ranking = case_when(is.na(ranking) ~ 4,  #Find top 3 hospitals and set all others as "other"
                             TRUE ~ ranking),
         Locname = case_when(ranking > 3 | is.na(ranking) ~ "Other",
                             TRUE ~ Locname)) %>%
  arrange(desc(case_total)) %>% #  
  group_by(YEAR, Locname, ranking) %>%   #Aggregate all hospitals identified as "other"
  summarise(case_in_order = sum(case_in_order),
            case_not_in_order = sum(case_not_in_order),
            case_report_pf = sum(case_report_pf),
            case_total = sum(case_total)) %>%
  select(Locname, YEAR, ranking,
         case_in_order,
         case_not_in_order,
         case_report_pf) %>%
  pivot_longer(cols = !c(YEAR, Locname, ranking)) %>%
  group_by(YEAR, Locname) %>%
  mutate(case_percent = value / sum(value)) %>%
  ungroup() %>%
  mutate(label_flag = case_when(value/max(value) >= 0.07 ~ 1, TRUE ~ 0),  #Only show label if more than 7% otherwise the label is too compressed
         case_percentage = case_when(label_flag == 1 ~ percent(case_percent, accuracy = 0.1)),
         case_percentage = case_when(case_percent != 0 ~ case_percentage),
         name = case_when(name == "case_in_order" ~ "Case in Order",
                          name == "case_not_in_order" ~ "Case Not in Order",
                          name == "case_report_pf" ~ "Case Report to PF"),
         name = factor(name, levels=c("Case Report to PF", "Case Not in Order", "Case in Order")),
         Locname = str_wrap(Locname , width = 18))

max_value <- DCRS_Data_Hospice_Chart %>% summarise(max_value = max(case_total)) %>% pull(max_value)

if (nrow(DCRS_Data_Hospice_Chart2 != 0)) {

##create hospice bar chart 11##
hospice_review_chart <- ggplot(DCRS_Data_Hospice_Chart2, aes(YEAR, value, fill=name)) + 
  geom_col() +
  facet_wrap(~ fct_reorder(Locname, ranking), strip.position = "bottom", nrow = 1) +
  scale_x_discrete(labels = function(Review) str_wrap(Review, width = 10)) + 
# update year references --------------------------------------------------
  labs(title = paste0("Chart 11: Number of standard reviews by hospice and closure category for years ", y1, ", ", y2, "\nand ", y3),   #set titles
       y = "", x = "Note: Year 7 - 2021/22     Year 8 - 2022/23     Year 9 - 2023/24") +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#097480", "#8BB5E8", "#0F3D6B")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max_value +0.07)) +
  geom_text(aes(label=case_percentage), size = 2.5, check_overlap = TRUE, position = position_stack(vjust = 0.5), colour = "white") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.box.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 8,face="italic", hjust = 0.01),
        strip.background = element_blank(),
        panel.spacing = unit(0, 'points')) 

hospice_review_chart

ggsave(hospice_review_chart, 
       filename = "Charts/hospice_review_chart.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

} else {"Not enough data"}

# chart 12 cause and place of death ------------------------------------------------------

##set cause and place data chart 12##

#Get cause of death data and percentages
DCRS_Data_Chapter <- DCRS_DATA_Loc_DIAG %>%
  filter(prmccdhealthboard == Board,
         YEAR == paste0("Year ", y3),
         Review == "Case not in Order",
         DiagGrp != "NA",
         instgrp2 != "NA") %>%
  group_by(DiagGrp, instgrp2) %>%
  summarise(case_total = n()) %>%
  mutate(case_percent = case_total / sum(case_total),
         case_percentage = percent(case_percent, accuracy = 1)) 

#Rank values
DCRS_Data_Chapter_rank <- DCRS_Data_Chapter %>%
  group_by(DiagGrp) %>%
  summarise(case_total = sum(case_total)) %>%
  mutate(top5 = min_rank(desc(case_total))) %>%
  select(DiagGrp, top5)

#Add in ranking and select only top 5 for charting (on request from DCRS)
DCRS_Data_Chapter <- left_join(DCRS_Data_Chapter, DCRS_Data_Chapter_rank, by="DiagGrp") %>%
  filter(top5 <= 5)


if (nrow(DCRS_Data_Chapter != 0)) {

##create cause and place bar chart 12##
diag_chart <- ggplot(DCRS_Data_Chapter, aes(fill=instgrp2, fct_reorder(DiagGrp, desc(case_total)), case_total)) + 
  geom_bar(position=position_dodge2(preserve="single"), stat="identity") +
  scale_x_discrete(labels = function(DiagGrp) str_wrap(DiagGrp, width = 10)) + 
  labs(title = "Chart 12: Top 5 Number (and percentage) of not in order cases by cause and place of death",
       subtitle = year3, #set titles
       y = "Number",
       x = "") +
  scale_fill_manual(values = c("#0F3D6B", "#E3CEF6", "#097480", "#A9D0F5")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(DCRS_Data_Chapter$case_total)*1.08)) +
  geom_text(aes(label=case_percentage), size = 2.5, vjust = -0.2, position = position_dodge(width = 0.9), check_overlap = TRUE) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        legend.position="bottom",
        legend.key.size = unit(0.5, 'cm'),
        legend.box.background = element_blank(),
        text = element_text(size = 9),
        panel.background = element_blank())   

diag_chart

ggsave(diag_chart, 
       filename = "Charts/diag_chart.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

} else {"Not enough data"}


# summary text closure categories -----------------------------------------

##Chart 5 closure categories summary text pt1##

#Percent of all not in order
closure_nio <- dcrs_data_report_all %>%
  select(Year, `Health Board`, closure_category, total_not_in_order) %>%
  filter(Year == 3 & `Health Board` == "Board") %>%
  mutate(closure_nio_percent = percent(closure_category / total_not_in_order, accuracy = 0.1)) %>%
  pull(closure_nio_percent)

#Set full names
dcrs_data_wrangle_closure2 <-  dcrs_data_wrangle_closure2 %>%
  mutate(closure_category = str_replace(closure_category, "causal_timscale_rate", "'causal timescales incorrect'"),
         closure_category = str_replace(closure_category, "cause_of_death_rate", "'cause of death incorrect'"),
         closure_category = str_replace(closure_category, "conditions_omitted_rate", "'conditions omitted'"), 
         closure_category = str_replace(closure_category, "disposal_hazard_rate", "'disposal hazard incorrect'"),
         closure_category = str_replace(closure_category, "sequence_of_cause_rate", "'sequence of cause of death incorrect'"), 
         closure_category = str_replace(closure_category, "cause_too_vague_rate", "'cause of death too vague'"))

#Find max closure percent
max_closure_value <- dcrs_data_wrangle_closure2 %>% filter(closure_rate == max(closure_rate)) %>% 
  filter(closure_category == max(closure_category)) %>% pull(closure_percent)

#Find name of max closure category
#max_closure_name <- dcrs_data_wrangle_closure2 %>% filter(closure_rate == max(closure_rate)) %>% 
#  filter(closure_category == max(closure_category)) %>% pull(closure_category)



##Chart 5 closure categories summary text pt2##

#SETTING UP FOR MORE DYNAMIC NARRATIVE
#Find max closure category
max_closure <- dcrs_data_wrangle_closure2 %>% 
  filter(closure_rate == max(closure_rate))

#Combine names if multiple have same max value
max_closure_multiple <- max_closure %>%
  select(closure_category) %>%
  mutate(closure_names = paste(closure_category, collapse = ' and ')) %>%
  filter(closure_category == max(closure_category)) %>% 
  pull(closure_names)

#Test if multiple have same max values and assign correct name(s)
max_closure_name <- max_closure %>%
  mutate(alt_value = case_when(nrow(max_closure) == 1 ~ closure_category,
                               nrow(max_closure) >= 2 ~ max_closure_multiple)) %>%
  filter(closure_category == max(closure_category)) %>% 
  pull(alt_value)

#Test if multiple have same max value and set correct plural for category  
closure_value1 <- max_closure %>%
  mutate(alt_value = case_when(nrow(max_closure) == 1 ~ "y",
                               nrow(max_closure) >= 2 ~ "ies")) %>%
  filter(closure_category == max(closure_category)) %>% 
  pull(alt_value)

#Test if multiple have same max value and set intro for percent
closure_value2 <- max_closure %>%
  mutate(alt_value = case_when(nrow(max_closure) == 1 ~ "",
                               nrow(max_closure) >= 2 ~ "each")) %>%
  filter(closure_category == max(closure_category)) %>% 
  pull(alt_value)

#Text if primary/secondary care chart is used
care_breakdown_text <- if(Board != "Borders" & Board != "Dumfries and Galloway" & Board != "Shetland" & Board != "Orkney" 
                          & Board != "Western Isles" & Board != "National Golden Jubilee"){
  "Over the three years displayed, 'cause of death too vague' has been consistently the most common reason for a clinical error being recorded. A larger proportion of these certificates come from primary care compared to secondary care." 
} else { "" }


# summary text cause too vague --------------------------------------------

##Chart 6 cause too vague summary text pt1##

#Percent cause too vague not in order
closure_too_vauge_nio <- dcrs_data_report_all %>%
  select(Year, `Health Board`, cause_too_vague, total_not_in_order) %>%
  filter(Year == 3 & `Health Board` == "Board") %>%
  mutate(closure_nio_percent = percent(cause_too_vague / total_not_in_order, accuracy = 0.1)) %>%
  pull(closure_nio_percent)

#Set full names too vague
dcrs_data_wrangle_death_cause_2 <-  dcrs_data_wrangle_death_cause_2 %>%
  filter(`Health Board` == "Board") %>%
  mutate(cause_too_vague = str_replace(cause_too_vague, 
                                       "histology_rate", "'histology'"),
         cause_too_vague = str_replace(cause_too_vague, 
                                       "psite_metastatic_missing_rate", "'primary site or metastatic site(s) missing'"),
         cause_too_vague = str_replace(cause_too_vague, 
                                       "pneumonia_subtype_rate", "'pneumonia sub-type'"),
         cause_too_vague = str_replace(cause_too_vague, 
                                       "dementia_subtype_rate", "'dementia sub-type'"),
         cause_too_vague = str_replace(cause_too_vague, 
                                       "microbiology_rate", "'microbiology'"),
         cause_too_vague = str_replace(cause_too_vague, 
                                       "sepsis_source_rate", "'source of sepsis'"),
         cause_too_vague = str_replace(cause_too_vague, 
                                       "diabetes_subtype_rate", "'diabetes sub-type'"),
         cause_too_vague = str_replace(cause_too_vague, 
                                       "stroke_rate", "'stroke'"),
         cause_too_vague = str_replace(cause_too_vague, 
                                       "lifestyle_factor_rate", "'lifestyle factors (smoking, obesity, alcohol'"))

#Find max vague percent
max_vague_value <- dcrs_data_wrangle_death_cause_2 %>% 
  filter(cause_too_vague_rate == max(cause_too_vague_rate)) %>% 
  filter(cause_too_vague == max(cause_too_vague)) %>% pull(cause_too_vague_percent)

#Second highest value
max_vague_value2 <- dcrs_data_wrangle_death_cause_2 %>% 
  filter(cause_too_vague_rate == max(cause_too_vague_rate[cause_too_vague_rate != max(cause_too_vague_rate)])) %>% pull(cause_too_vague_percent)

#Second hightest cause name
max_vague_name2 <- dcrs_data_wrangle_death_cause_2 %>% 
  filter(cause_too_vague_rate == max(cause_too_vague_rate[cause_too_vague_rate != max(cause_too_vague_rate)])) %>% pull(cause_too_vague)


##Chart 6 cause too vague summary text pt2##

#Find max too vague breakdown

#Highest value rate
max_too_vague <- dcrs_data_wrangle_death_cause_2 %>% 
  filter(cause_too_vague_rate == max(cause_too_vague_rate))

#Highest value cause name
max_too_vague_multiple <- max_too_vague %>%
  select(cause_too_vague) %>%
  mutate(vague_names = paste(cause_too_vague, collapse = ' and ')) %>%
  filter(cause_too_vague == max(cause_too_vague)) %>% 
  pull(vague_names)

#Test if individual cause is max or multiple cause share same max value
max_vague_name <- max_too_vague %>%
  mutate(alt_value = case_when(nrow(max_too_vague) == 1 ~ cause_too_vague,
                               nrow(max_too_vague) >= 2 ~ max_too_vague_multiple)) %>%
  filter(cause_too_vague == max(cause_too_vague)) %>% 
  pull(alt_value)

#Extra syntax if single or mutliple cause identified for max
vague_value1 <- max_too_vague %>%
  mutate(alt_value = case_when(nrow(max_too_vague) == 1 ~ "y",
                               nrow(max_too_vague) >= 2 ~ "ies")) %>%
  filter(cause_too_vague == max(cause_too_vague)) %>% 
  pull(alt_value)

#Extra syntax if single or mutliple cause identified for max
vague_value2 <- max_too_vague %>%
  mutate(alt_value = case_when(nrow(max_too_vague) == 1 ~ "",
                               nrow(max_too_vague) >= 2 ~ "each")) %>%
  filter(cause_too_vague == max(cause_too_vague)) %>% 
  pull(alt_value)



# summary text admin ------------------------------------------------------

##Chart 7 admin summary text pt1##

#Percent admin not in order
closure_admin_nio <- dcrs_data_report_all %>%
  select(Year, `Health Board`, closure_category_admin, total_not_in_order) %>%
  filter(Year == 3 & `Health Board` == "Board") %>%
  mutate(closure_nio_percent = percent(closure_category_admin / total_not_in_order, accuracy = 0.1)) %>%
  pull(closure_nio_percent)

#Set full names
dcrs_data_wrangle_admin_closure2 <-  dcrs_data_wrangle_admin_closure2 %>%
  filter(`Health Board` == "Board") %>%
  mutate(closure_category_admin = str_replace(closure_category_admin, 
                                              "spelling_error_rate", "'certifying doctor spelling error'"),
         closure_category_admin = str_replace(closure_category_admin, 
                                              "deceased_details_incorrect_rate", "'deceased details incorrect'"),
         closure_category_admin = str_replace(closure_category_admin, 
                                              "doctors_details_incorrect_rate", "'certifying doctor's details incorrect'"),
         closure_category_admin = str_replace(closure_category_admin, 
                                              "abbreviations_used_rate", "'abbreviations used'"),
         closure_category_admin = str_replace(closure_category_admin, 
                                              "time_incorrect_rate", "'date or time of death incorrect'"),
         closure_category_admin = str_replace(closure_category_admin, 
                                              "incorrectly_complete_rate", "'extra information (X box) incorrectly complete'"),
         closure_category_admin = str_replace(closure_category_admin, 
                                              "attendance_incorrect_rate", "'attendance on the deceased incorrect'"),
         closure_category_admin = str_replace(closure_category_admin, 
                                              "address_incorrect_rate", "'place of death address incorrect'"),
         closure_category_admin = str_replace(closure_category_admin, 
                                              "pm_information_incorrect_rate", "'PM information incorrect'"),
         closure_category_admin = str_replace(closure_category_admin, 
                                              "legibility_rate", "'legibility'"),
         closure_category_admin = str_replace(closure_category_admin, 
                                              "consultant_incorrect_rate", "'consultant's name incorrect'"),
         closure_category_admin = str_replace(closure_category_admin, 
                                              "other_incorrect_rate", "'other additional information incorrect'"))

#Find max admin percent
max_admin_value <- dcrs_data_wrangle_admin_closure2 %>% filter(closure_rate_admin == max(closure_rate_admin)) %>% 
  filter(closure_category_admin == max(closure_category_admin)) %>% pull(closure_percent_admin)


##Chart 7 admin summary text pt2##

#Find max admin category
max_admin <- dcrs_data_wrangle_admin_closure2 %>% 
  filter(closure_rate_admin == max(closure_rate_admin))

max_admin_multiple <- max_admin %>%
  select(closure_category_admin) %>%
  mutate(admin_names = paste(closure_category_admin, collapse = ' and ')) %>%
  filter(closure_category_admin == max(closure_category_admin)) %>% 
  pull(admin_names)

#Test if individual cause is max or multiple cause share same max value
max_admin_name <- max_admin %>%
  mutate(alt_value = case_when(nrow(max_admin) == 1 ~ closure_category_admin,
                               nrow(max_admin) >= 2 ~ max_admin_multiple)) %>%
  filter(closure_category_admin == max(closure_category_admin)) %>% 
  pull(alt_value)

#Extra syntax if single or mutliple cause identified for max
admin_value1 <- max_admin %>%
  mutate(alt_value = case_when(nrow(max_admin) == 1 ~ "y",
                               nrow(max_admin) >= 2 ~ "ies")) %>%
  filter(closure_category_admin == max(closure_category_admin)) %>% 
  pull(alt_value)

#Extra syntax if single or mutliple cause identified for max
admin_value2 <- max_admin %>%
  mutate(alt_value = case_when(nrow(max_admin) == 1 ~ "",
                               nrow(max_admin) >= 2 ~ "each")) %>%
  filter(closure_category_admin == max(closure_category_admin)) %>% 
  pull(alt_value)


# summary text PF ---------------------------------------------------------

##Chart 8 PF summary text pt1##

#PF not in order percent
PF_nio <- dcrs_data_report_all %>%
  select(Year, `Health Board`, total_report_to_pf, total_not_in_order) %>%
  filter(Year == 3 & `Health Board` == "Board") %>%
  mutate(closure_nio_percent = percent(total_report_to_pf / total_not_in_order, accuracy = 0.1)) %>%
  pull(closure_nio_percent)

#Set full names PF
PF_data_wrangle_2  <-  PF_data_wrangle_2  %>%
  filter(`Health Board` == "Board") %>%
  mutate(total_report_to_pf = str_replace(total_report_to_pf, 
                                          "choking_rate", "'choking'"),
         total_report_to_pf = str_replace(total_report_to_pf, 
                                          "concerns_over_care_rate", "'concerns over care'"),
         total_report_to_pf = str_replace(total_report_to_pf, 
                                          "drug_related_rate", "'drug related'"),
         total_report_to_pf = str_replace(total_report_to_pf, 
                                          "neglect_exposure_rate", "'neglect or exposure'"),
         total_report_to_pf = str_replace(total_report_to_pf, 
                                          "fracture_trauma_rate", "'fracture or trauma'"),
         total_report_to_pf = str_replace(total_report_to_pf, 
                                          "industrial_disease_rate", "'industrial disease'"),
         total_report_to_pf = str_replace(total_report_to_pf, 
                                          "infectious_disease_rate", "'infectious disease'"),
         total_report_to_pf = str_replace(total_report_to_pf, 
                                          "legal_order_rate", "'legal order'"),
         total_report_to_pf = str_replace(total_report_to_pf, 
                                          "flagged_error_rate", "'flagged in error'"),
         total_report_to_pf = str_replace(total_report_to_pf, 
                                          "other_report_pf_rate", "'other report to PF'"))

#Find max PF percent
max_PF_value <- PF_data_wrangle_2 %>% filter(`Health Board` == "Board") %>% filter(total_report_to_pf_rate == max(total_report_to_pf_rate)) %>% 
  filter(total_report_to_pf == max(total_report_to_pf)) %>% pull(total_report_to_pf_percent)

#Max PF total
total_PF <- dcrs_data_report_all %>% select(`Health Board`, total_report_to_pf) %>%
  filter(Year == 3, `Health Board` == "Board") %>% pull(total_report_to_pf)

#Second highest PF percent
max_PF_value2 <- PF_data_wrangle_2 %>% filter(`Health Board` == "Board") %>%
  filter(total_report_to_pf_rate == max(total_report_to_pf_rate[total_report_to_pf_rate != max(total_report_to_pf_rate)])) %>% pull(total_report_to_pf_percent)

#Second highest PF total
max_PF_name2 <- PF_data_wrangle_2 %>% filter(`Health Board` == "Board") %>%
  filter(total_report_to_pf_rate == max(total_report_to_pf_rate[total_report_to_pf_rate != max(total_report_to_pf_rate)])) %>% pull(total_report_to_pf)


##Chart 8 PF summary text pt2##

#Find max PF 
max_PF <- PF_data_wrangle_2  %>% 
  filter(total_report_to_pf_rate == max(total_report_to_pf_rate))

#Highest PF name
max_PF_multiple <- max_PF %>%
  select(total_report_to_pf) %>%
  mutate(PF_names = paste(total_report_to_pf, collapse = ' and ')) %>%
  filter(total_report_to_pf == max(total_report_to_pf)) %>% 
  pull(PF_names)

#Test if individual cause is max or multiple cause share same max value
max_PF_name <- max_PF %>%
  mutate(alt_value = case_when(nrow(max_PF) == 1 ~ total_report_to_pf,
                               nrow(max_PF) >= 2 ~ max_PF_multiple)) %>%
  filter(total_report_to_pf == max(total_report_to_pf)) %>% 
  pull(alt_value)

#Extra syntax if single or mutliple cause identified for max
max_PF_txt1 <- max_PF %>%
  mutate(alt_value = case_when(nrow(max_PF) == 1 ~ "y",
                               nrow(max_PF) >= 2 ~ "ies")) %>%
  filter(total_report_to_pf == max(total_report_to_pf)) %>% 
  pull(alt_value)

#Extra syntax if single or mutliple cause identified for max
max_PF_txt2 <- max_PF %>%
  mutate(alt_value = case_when(nrow(max_PF) == 1 ~ "",
                               nrow(max_PF) >= 2 ~ "each")) %>%
  filter(total_report_to_pf == max(total_report_to_pf)) %>% 
  pull(alt_value)



# summary text enquiry categories -----------------------------------------

#Chart 9 enquiry categories summary text

enquiry_data_wrangle_2 <-  enquiry_data_wrangle_2 %>%
  filter(`Health Board` == "Board") %>%
  mutate(enquiry_category_total = str_replace(enquiry_category_total, 
                                              "gp_clinical_advice_rate", "'GP clinical advice'"),
         enquiry_category_total = str_replace(enquiry_category_total, 
                                              "gp_process_advice_rate", "'GP process advice'"),
         enquiry_category_total = str_replace(enquiry_category_total, 
                                              "hospital_clinical_advice_rate", "'hospital clinical advice'"),
         enquiry_category_total = str_replace(enquiry_category_total, 
                                              "hospital_process_advice_rate", "'hospital process advice'"),
         enquiry_category_total = str_replace(enquiry_category_total, 
                                              "hospice_clinical_advice_rate", "'hospice clinical advice'"),
         enquiry_category_total = str_replace(enquiry_category_total, 
                                              "hospice_process_advice_rate", "'hospice process advice'"),
         enquiry_category_total = str_replace(enquiry_category_total, 
                                              "funeral_director_rate", "'funeral director'"),
         enquiry_category_total = str_replace(enquiry_category_total, 
                                              "informant_or_family_rate", "'informant or family'"),
         enquiry_category_total = str_replace(enquiry_category_total, 
                                              "procurator_fiscal_rate", "'procurator fiscal'"),
         enquiry_category_total = str_replace(enquiry_category_total, 
                                              "signposted_rate", "'signposted'"),
         enquiry_category_total = str_replace(enquiry_category_total, 
                                              "other_rate", "'other'"))

#if no enquiry cases then skip and set as not enough data
if (nrow(enquiry_data_wrangle != 0)) {
  
#Total enquiries  
total_enquiry <- dcrs_data_enquiry_all %>% select(`Health Board`, enquiry_category_total) %>%
    filter(Year == 3, `Health Board` == "Board") %>% pull(enquiry_category_total)

#Max enquiry percent  
max_enquiry_value <- enquiry_data_wrangle_2 %>% filter(enquiry_rate == max(enquiry_rate)) %>% pull(enquiry_percent)

#Max enquiry category name  
max_enquiry_name <- enquiry_data_wrangle_2 %>% filter(enquiry_rate == max(enquiry_rate)) %>% pull(enquiry_category_total)

#Max enquiry total  
max_enquiry_number <- dcrs_data_enquiry_all %>%
    select(Year, `Health Board`, gp_clinical_advice, gp_process_advice, hospital_clinical_advice, hospital_process_advice, hospice_clinical_advice,
           hospice_process_advice, funeral_director, informant_or_family, procurator_fiscal, signposted, other) %>%
    filter(Year == 3, `Health Board` == "Board") %>%
    pivot_longer(cols = !c(Year, `Health Board`), names_to = 'enquiry_category', values_to = 'enquiry_number') %>%
    filter(enquiry_number == max(enquiry_number)) %>% pull(enquiry_number)

#Second highest percent  
max_enquiry_value2 <- enquiry_data_wrangle_2 %>% 
    filter(enquiry_rate == max(enquiry_rate[enquiry_rate != max(enquiry_rate)])) %>% pull(enquiry_percent)

#Second highest category name  
max_enquiry_name2 <- enquiry_data_wrangle_2 %>% 
    filter(enquiry_rate == max(enquiry_rate[enquiry_rate != max(enquiry_rate)])) %>% pull(enquiry_category_total)

#Second highest total  
max_enquiry_number2 <- dcrs_data_enquiry_all %>%
    select(Year, `Health Board`, gp_clinical_advice, gp_process_advice, hospital_clinical_advice, hospital_process_advice, hospice_clinical_advice,
           hospice_process_advice, funeral_director, informant_or_family, procurator_fiscal, signposted, other) %>%
    filter(Year == 3, `Health Board` == "Board") %>%
    pivot_longer(cols = !c(Year, `Health Board`), names_to = 'enquiry_category', values_to = 'enquiry_number') %>%
    filter(enquiry_number == max(enquiry_number[enquiry_number != max(enquiry_number)])) %>% pull(enquiry_number)
  
} else {"Not enough data"}


# summary text cause of death---------------------------------------------

##Chart 12 cause of death text##

#Identify top cause
top_cause <- DCRS_Data_Chapter_rank %>% filter(top5 == 1)

#Combine mutliple causes if more than 1 are top ranked
top_cause_multiple <- DCRS_Data_Chapter_rank %>% filter(top5 == 1) %>% 
  select(DiagGrp) %>%
  mutate(DiagGrp = paste(DiagGrp, collapse = ' and ')) %>%
  filter(row_number()==1) %>%
  pull(DiagGrp)

#Get top name depending if one or multiple causes
top_cause_name <- top_cause %>%
  mutate(alt_value = case_when(nrow(top_cause) == 1 ~ DiagGrp,
                               nrow(top_cause) >= 2 ~ top_cause_multiple)) %>%
  filter(row_number()==1) %>%
  pull(alt_value)

#Max total
top_cause_number <- DCRS_Data_Chapter %>% group_by(DiagGrp) %>% summarise(case_total = sum(case_total)) %>% filter(case_total == max(case_total)) %>% pull(case_total)

#Max location type (while stepping dow capital letters)
top_cause_place <- DCRS_Data_Chapter %>% filter(DiagGrp == top_cause_name, case_total == max(case_total)) %>% pull(instgrp2)
top_cause_place <- tolower(top_cause_place)

#Max percent
top_cause_place_percentage <- DCRS_Data_Chapter %>% filter(DiagGrp == top_cause_name, case_total == max(case_total)) %>% pull(case_percentage)

#Step down capital letter and contain in quotations
top_cause_name <- paste0("'",tolower(top_cause_name),"'")