# dcrs-hb-annual
Annual HB report for DCRS

# What is DCRS
The Death Certification Review Service reviews medical certificates of cause of death. Every death in Scotland must be certified by a doctor. They complete a Medical Certificate of Cause of Death (MCCD). A number of these are passed to the Death Certification Review Service for random checks.

https://www.healthcareimprovementscotland.scot/inspections-reviews-and-regulation/death-certification-review-service-dcrs/

# What are DCRS HB Annual reports
This report provides a summary of information about death certification reviews registered within individual health boards. There are breakdowns of case types, services provided, location of deaths, and main causes of death.  The majority of this report has been prepared on cases received for the latest financial year, with some tables also comparing previous financial year year and occasional analysis also using data going back to January 2019. The report outputs are in Microsoft Word.

# How is data presented
The report uses a range of visualisations to summarise the various breakdowns of MCCD cases:

- Tables
- Bar Charts
- Run Charts
- Funnel Plots

The narrative also gives figures relating to the corresponding analysis for further detail or to help clarify results.

# What data is included
Data from the majority of the report is sourced from DCRSs own data collection of Dynamics365.  A licence will need to be granted to access this data:
LINK?

Data for the hospital/hospice and cause of death breakdown is sourced from NRS via annual data submission to DMBI on request. This is combined with the DCRS data using CHI numbers to match cases.  These are also combined with a location lookup dataset to help identify location of death.

# The R scripts
Reports have been created using R and the R Markdown package. The main scripts in this repository are:
- 01-Create HB Reportsv2:  Used running creating all individual health board reports in one go (you will also need to run part of ths to start. See below for details)
- 02-DCRS-HB-Report v1.14: R Markdown script that creates the reports
- 03-HB Standard Data: Takes the raw DCRS dataset and provides some basic wrangling for analysis
- 04-Part1 Tables: Produces all tables used in the report
- 05-Part2 Run Charts: Produces all run charts used in the report
- 06-Part3 FunnelPlots2: Produces all funnel plots used in the report
- 07-Part4 Charts: Produces all bar charts used in the report
- 08-Hospital Read and match data: Reads and combines raw DCRS and NRS data using CHI numbers
- 09-Hospital Add Extra Information: Link combined DCRS/NRS data with locations and cause of death

Other scripts include functions and code that are used by the main scripts. These are: 

- 'stable' and 'stable2': Run chart functions that are selected depending on board size.  Note, the three island boards do not contain enough data for run chart analysis and run charts are excluded for them
- '20230320 plot_funnel': Funnel plot function used for both included in the report

# Getting started

1. Download or fork a copy of files.
2. Create a Reports folder to have separate space for output reports
3. Request access to Dynamics 365 for DCRS data and a data extract from NRS.  DMBI staff can access existing extracts from the DCRS HB report folder on the N Drive.
4. In 01-Create HB Reportsv2, on row 15, run the section of the code 'hdr <<- "Ayrshire and Arran"' to set a named health board in the global environment. This will be needed to run other scripts. (yoy can also do this for the other health boards)
5. Run the whole of 02-DCRS-HB-Report v1.14 which will automatically go to all other scripts and updates all charts, tables, and narrative for the current period.
6. Run the whole of 01-Create HB Reportsv2 to create reports for all health boards
7. If it's successful, report will be saved in the Reports folder. (UPDATE LINKS?)
