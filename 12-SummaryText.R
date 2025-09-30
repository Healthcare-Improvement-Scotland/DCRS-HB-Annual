#############  Alternative narrative from any CI signals identified to use the summary

#Table 2 CI signal summary
if (hb_table2_sig != "") { #only if a signal has been identified
  table2_sig <- data.frame(summary = hb_table2_sig) #create a dataframe with any narrative produced so it can be altered
  table2_sig_sum <- table2_sig %>% 
    mutate(summary = gsub("\\+ ", "", summary),  #Remove the bullet point original narrative
           summary = paste("- Cases that were", summary)) %>% #Add in new bullet point with alternative narrative
    pull(summary)
} else {table2_sig_sum <- ""} #create blank if no signal had been identified (will not create anything in the summary)


#Table 3 CI summary
if (hb_table3_sig != "") {
  table3_sig <- data.frame(summary = hb_table3_sig)
  table3_sig_sum <- table3_sig %>%
    mutate(summary = gsub("\\+ ", "", summary),
           summary = gsub(" compared to ", "", summary),  #remove any references to comparison year to simplify summary narrative
           summary = gsub(year2, "", summary),
           summary = paste("- ", summary)) %>%
    pull(summary)
} else {table3_sig_sum <- ""}


#Table 4 CI summary
if (hb_table4_sig != "") {
  table4_sig <- data.frame(summary = hb_table4_sig)
  table4_sig_sum <- table4_sig %>%
    mutate(summary = gsub("\\+ ", "", summary),
           summary = gsub(" compared to ", "", summary),
           summary = gsub(year2, "", summary),
           summary = paste("- ", summary)) %>%
    pull(summary)
} else {table4_sig_sum <- ""}


#Chart 5 CI summary
if (hb_closure_sig != "") {
closure_sig <- data.frame(summary = hb_closure_sig)
closure_sig_sum <- closure_sig %>%
  mutate(summary = gsub("\\+ In ", "", summary),
         summary = paste("- Certificates completed by doctors in", summary)) %>%
  pull(summary)
} else {closure_sig_sum <- ""}


#Chart 6 CI summary
if (hb_cause_sig != "") {
  cause_sig <- data.frame(summary = hb_cause_sig)
  cause_sig_sum <- cause_sig %>%
    mutate(summary = gsub("\\+ In NHS ", "", summary),
           summary = gsub(Board, "", summary), #Remove any board reference to simplify the summary narrative
           summary = gsub(",", "", summary),
           summary = gsub(" compared to ", "", summary),
           summary = gsub(year2, "", summary),
           summary = paste("- Closure category errors for the reason 'cause of death too vague'", summary)) %>%
    pull(summary)
} else {cause_sig_sum <- ""}


#Chart 7 CI summary
if (hb_admin_sig != "") {
  admin_sig <- data.frame(summary = hb_admin_sig)
  admin_sig_sum <- admin_sig %>%
    mutate(summary = gsub("\\+ In NHS ", "", summary),
           summary = gsub(Board, "", summary),
           summary = gsub(",", "", summary),
           summary = gsub(" compared to ", "", summary),
           summary = gsub(year2, "", summary),
           summary = paste("- Closure categories with administation errors", summary)) %>%
    pull(summary)
} else {admin_sig_sum <- ""}


#Chart 8 CI summary
if (hb_pf_sig != "") {
  pf_sig <- data.frame(summary = hb_pf_sig)
  pf_sig_sum <- pf_sig %>%
    mutate(summary = gsub("\\+ In NHS ", "", summary),
           summary = gsub(Board, "", summary),
           summary = gsub(",", "", summary),
           summary = gsub(" compared to ", "", summary),
           summary = gsub(year2, "", summary),
           summary = paste("- Cases that had been referred to PF for the reason", summary)) %>%
    pull(summary)
} else {pf_sig_sum <- ""}


#Chart 9 CI summary
if (hb_enquiry_sig != "") {
  enquiry_sig <- data.frame(summary = hb_enquiry_sig)
  enquiry_sig_sum <- enquiry_sig %>%
    mutate(summary = gsub("\\+ In NHS ", "", summary),
           summary = gsub(Board, "", summary),
           summary = gsub(",", "", summary),
           summary = gsub(" compared to ", "", summary),
           summary = gsub(year2, "", summary),
           summary = paste("- Calls to the enquiry line for the type of advice", summary)) %>%
    pull(summary)
} else {enquiry_sig_sum <- ""}