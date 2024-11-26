############ Function to link to R Markdown script and create individual reports for each Health Board

library(rmarkdown)
library(tidyverse)

#Set report script to run
renderHBReport <- function(hdr, OutName) {
  rmarkdown::render("02-DCRS-HB-Report v1.14.rmd", params = list(
    hdr = hdr),
    output_file = OutName)
}

date <- "DATE"

#run script for each HB and create reports
renderHBReport(hdr <<- "Ayrshire and Arran", OutName = paste0('Reports/',date,'_AA_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Borders", OutName = paste0('Reports/',date,'_BO_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Dumfries and Galloway", OutName = paste0('Reports/',date,'_DG_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Fife", OutName = paste0('Reports/',date,'_FI_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Forth Valley", OutName = paste0('Reports/',date,'_FV_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Highland", OutName = paste0('Reports/',date,'_HI_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Grampian", OutName = paste0('Reports/',date,'_GR_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Greater Glasgow and Clyde", OutName = paste0('Reports/',date,'_GG_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Lanarkshire", OutName = paste0('Reports/',date,'_LA_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Lothian", OutName = paste0('Reports/',date,'_LO_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Shetland", OutName = paste0('Reports/',date,'_SH_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Orkney", OutName = paste0('Reports/',date,'_OR_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Tayside", OutName = paste0('Reports/',date,'_TA_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "Western Isles", OutName = paste0('Reports/',date,'_WI_6_monthly_review_and_comparative_data_report v01'))
renderHBReport(hdr <<- "National Golden Jubilee", OutName = paste0('Reports/',date,'_GJ_6_monthly_review_and_comparative_data_report v01'))