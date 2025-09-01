############ Reads in the raw place and cause DCRS dataset and NRS deaths datasets.  
############ Combines both using CHI or name & date of birth when no CHI link found

library(tidyverse)
library(data.table)
library(haven)
library(lubridate)


######## Read in DCRS data ########

#Load DCRS data
DCRS_data <- read_csv("Data/DCRS - Place and Cause Annual.csv")

### Select only the DCRS case types needed for the reports
DCRS_data <- DCRS_data %>%
  filter(
         #statuscode != "Cancelled",
         #statuscode != "Rejected" & statuscode != "Merged",
         #casetypecode != "Enquiry",
         casetypecode == "Standard"
         )

names(DCRS_data) <- gsub("\\_", "", names(DCRS_data))

#remove " " from all variables
names(DCRS_data) <- gsub(" ", "", names(DCRS_data))

#remove () from all variables
names(DCRS_data) <- gsub("\\(", "", names(DCRS_data))
names(DCRS_data) <- gsub("\\)", "", names(DCRS_data))

#Rename selected variables to match old form
DCRS_data <- rename(DCRS_data, c(casenumber = ticketnumber,
                                 dateentered = createdon,
                                 datemodified = modifiedon,
                                 prcdpgmcnumber = tnmccddngmcnumber,
                                 prchinumber = tnchinumber,
                                 prclosurecategory = tnclosurecategoryclinical,
                                 prclosurecategoryother = tnclosurecategoryother,
                                 prdeceasedforename = tndeceasedforename,
                                 prdeceasedsurname = tndeceasedsurname,
                                 prdob = tndateofbirth,
                                 prdod = tndateofdeath,
                                 prlevel = tnlevel,
                                 prmccdhealthboard = tnhealthboard,
                                 prmccdplaceofdeath = tnmccddnplaceofdeath,
                                 prmccdplaceofdeath2 = tnmccddnplaceofdeath2,
                                 prmccdplaceofdeath3 = tnmccddnplaceofdeath3,
                                 prmccdplaceofdeath4 = tnmccddnplaceofdeath4,
                                 prmccdplaceofdeathpc = tnplaceofdeathpostcode,
                                 proriginallevel = tnoriginallevel,
                                 prprocuratorfiscal = tnmccddndeathreportedtotheprocuratorfisca,
                                 prstatuscode = statuscode,
                                 prtype = casetypecode,
                                 prrecordstatus = tnrecordstatus,
                                 prcaseinorderflag = tncaseinordertimestamp,
                                 prcasenotinorderflag = tncasenotinordertimestamp,
                                 prcaseclosedflag = tncaseclosedon,
                                 prdatestatus = tndatestatus,
                                 prmccdserialnumber = tnmccdserialnumber,
                                 CauseofDeathincorrect = tncccauseofdeathincorrect,
                                 SequenceofCauseofDeathincorrect = tnccsequenceofcauseofdeathincorrect,
                                 Causeofdeathtoovague = tncccauseofdeathtoovague))

table(DCRS_data$prmccdhealthboard, exclude = TRUE)

###Reformat columns and create new columns
DCRS_for_linkage <- DCRS_data %>% 
  mutate(format = ifelse(prmccdserialnumber==" ", "NA",
                         ifelse(prmccdserialnumber %like% "^5", "electronic", "manual")),
         dateentered = dmy(dateentered),
         datemodified = dmy(datemodified),
         prdatestatus = dmy(prdatestatus),
         prdob = dmy(prdob),
         prdod = dmy(prdod),
         prstatuscode = as.factor(prstatuscode),
         Review = prstatuscode, 
# update year -------------------------------------------------------------
         YEAR = "",
         YEAR = ifelse(dateentered >= "2015-05-13" & dateentered < "2016-04-01", "Year 1", YEAR),
         YEAR = ifelse(dateentered >= "2016-04-01" & dateentered < "2017-04-01", "Year 2", YEAR),
         YEAR = ifelse(dateentered >= "2017-04-01" & dateentered < "2018-04-01", "Year 3", YEAR),
         YEAR = ifelse(dateentered >= "2018-04-01" & dateentered < "2019-04-01", "Year 4", YEAR),
         YEAR = ifelse(dateentered >= "2019-04-01" & dateentered < "2020-04-01", "Year 5", YEAR),
         YEAR = ifelse(dateentered >= "2020-04-01" & dateentered < "2021-04-01", "Year 6", YEAR),
         YEAR = ifelse(dateentered >= "2021-04-01" & dateentered < "2022-04-01", "Year 7", YEAR),
         YEAR = ifelse(dateentered >= "2022-04-01" & dateentered < "2023-04-01", "Year 8", YEAR),
         YEAR = ifelse(dateentered >= "2023-04-01" & dateentered < "2024-04-01", "Year 9", YEAR),
         YEAR = ifelse(dateentered >= "2024-04-01" & dateentered < "2025-04-01", "Year 10", YEAR),  #need to make sure incomplete months aren't included
         origin = "DRCS",
         CHI = as.character(prchinumber),
         CHI = if_else(nchar(CHI) < 10, paste0("0", CHI), CHI)) %>% #Add 0 when leading 0 is missing (CHI needs to be 10 character)
  filter(YEAR != "") #%>%
  #filter(dateentered >= "2022-01-01" & dateentered < "2022-09-01") ####### For test data only

# Check the dates that are NA
DCRS_for_linkage %>% filter(is.na(dateentered) | is.na(datemodified) | is.na(prdob) | is.na(prdod)) %>% View("Dates with NAs")


###Identifying duplicate CHIs                      
DCRS_CHI_issue = DCRS_for_linkage %>% 
  filter(nchar(CHI) >= 10) %>% 
  group_by(CHI) %>% count() %>% filter(n>1)

DCRS_for_linkage = DCRS_for_linkage %>%
  mutate(CHI = ifelse(CHI %in% DCRS_CHI_issue$CHI, NA, CHI)) #change all duplicate and missing CHIs to NA, match later using dob and name

######## Read in NRS Data  ########
NRSdata2022 = read.csv("Data/2022DCRSv2.tsv", sep = "\t")

NRSdata2015 = read.csv("Data/2015DCRS.tsv", sep = "\t") 

NRSdata2023 = read.csv("Data/2023DCRSv3.tsv", sep = "\t") 

NRSdata2024 = read.csv("Data/2024DCRS.tsv", sep = "\t") 

NRSdata2025 = read.csv("Data/2025DCRS-Jan-April.tsv", sep = "\t") 

table(NRSdata2025$Date.of.registration)


# update files to combine -------------------------------------------------
###Combine old a new NRS datasets
NRSdata_for_linkage <- rbind(NRSdata2015, NRSdata2022, NRSdata2023, NRSdata2024, NRSdata2025) %>% 
  rename(CHI = chinumber) %>%
  mutate(Date.of.registration = as.Date(Date.of.registration,"%d%b%Y"),
         Date.of.death = as.Date(Date.of.death,"%d%b%Y"),
         Date.of.birth = as.Date(Date.of.birth,"%d%b%Y"),
         CHI = as.character(CHI),
         CHI = if_else(nchar(CHI) < 10, paste0("0", CHI), CHI),
         #Deceased.forename = Deceased.forename,
         Deceased.foreinitial = substring(str_to_title(str_replace_all(Deceased.forename, "[^[:alnum:]]", " ")),1,3),
         Deceased.surname = str_to_title(Deceased.surname))

table(NRSdata_for_linkage$Health.board.of.occurrence)

###Create version with no CHI errors for linking to DCRS data
NRS_CHI_issue = NRSdata_for_linkage %>% 
  group_by(CHI) %>% count() %>% filter(n>1)

NRSdata_for_linkage_CHI <- NRSdata_for_linkage %>%
  filter(CHI != "NA") %>% 
  filter(!(CHI %in% NRS_CHI_issue$CHI)) # Remove duplicate CHIs


### Rather than remove CHIs with issues, make them NA and try matching by date and name next
#Combine DCRS and NRS data
CHI_linked <- DCRS_for_linkage %>% 
  inner_join(NRSdata_for_linkage_CHI) # Join on NRS data

###Find cases still not matched
Not_CHI_Linked <- DCRS_for_linkage %>%
  anti_join(CHI_linked, by = 'CHI') %>%
  mutate(prdeceasedforeinitial = substring(str_to_title(str_replace_all(prdeceasedforename, "[^[:alnum:]]", " ")),1,3),
         prdeceasedsurname = str_to_title(prdeceasedsurname))

table(Not_CHI_Linked$YEAR, exclude = FALSE)

###Attempt to match using name and date of birth
NameDOB_linked <- Not_CHI_Linked %>% 
  select(-CHI) %>% 
  inner_join(NRSdata_for_linkage,
            keep = TRUE,
            by = c('prdob'='Date.of.birth',
                   #'prdeceasedforename'="Deceased.forename",
                   'prdeceasedforeinitial'="Deceased.foreinitial",
                   'prdeceasedsurname'="Deceased.surname")) %>%
  select(-prdeceasedforeinitial)


###Combine data linked by CHI and data linked by name and dob
DCRS_NRS_linked <-  rbind(CHI_linked, NameDOB_linked)


### CHECK: View remaining unmatched
DCRS_Not_linked_final = DCRS_for_linkage %>% 
  anti_join(DCRS_NRS_linked, by = 'casenumber') %>%
  add_column(Date.of.registration = NA,
             Date.of.death = NA,
             Institution = NA,
             Date.of.birth = NA,
             Underlying.cause.of.death.code = NA,
             Deceased.surname = NA,
             Deceased.forename = NA,
             Health.board.of.occurrence = NA,
             yr = NA,
             rdno = NA,
             entry = NA,
             origin_flag = NA,
             chosenpc = NA,
             Deceased.foreinitial = NA)

table(DCRS_Not_linked_final$YEAR) #Years with large number of missing links?

table(DCRS_Not_linked_final$prdod) #Month with large number of missing links?

NRSdata2024 <- NRSdata2024 %>% #Previous month gaps have been due to not enough data yet collected from NRS. Eg many late march 2023 cases were registered after march 2023 and so not included in 2024 dataset

mutate(Date.of.registration = as.Date(Date.of.registration,"%d%b%Y"),
       Date.of.death = as.Date(Date.of.death,"%d%b%Y"))

#Check if deaths were likely registered after range of data submission       
table(NRSdata2024$Date.of.registration) 
table(NRSdata2024$Date.of.death) 

#Final linkage including DCRS cases not matched to NRS.  
DCRS_NRS_linked_final <-  rbind(DCRS_NRS_linked, DCRS_Not_linked_final) 

### CHECK: View matched by HB and year
DCRS_NRS_linked_final %>%
 group_by(prmccdhealthboard, YEAR, Review) %>%
 summarise(case_total = n()) %>% View("Matched by HB and year")

#table(DCRS_NRS_linked_final$prmccdhealthboard)

### CHECK: View name and dob matches
NameDOB_linked %>%
  select(casenumber, prchinumber,
         dateentered, Date.of.registration,
         prdeceasedforename, Deceased.forename, 
         prdeceasedsurname,  Deceased.surname,
         prdob, Date.of.birth, 
         prdod, Date.of.death, 
         prmccdhealthboard, Health.board.of.occurrence) %>% 
  mutate(dodx = prdod != Date.of.death) %>% 
  View("Check nameDOB matches")

save(DCRS_NRS_linked_final, file="Data/DCRS_NRS_linked_final.Rda")

##################################################
