# Codes for HbA1c tests
# Produces txt file with fields separated by tabs and Windows-style (\r\n aka CR LF) line endings

############################################################################################

# Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
rm(list=ls())

############################################################################################

# Import Aurum medcode lookup table
## Need to make sure medcodes are imported as characters otherwise can lose precision in long medcodes
# aurum_medcodes <- read.delim("../../Reference files/202005_Lookups_CPRDAurum/202005_EMISMedicalDictionary.txt", colClasses=rep("character",8), header=TRUE)
aurum_medcodes <- read.delim("CPRDAurumMedical.txt", colClasses=rep("character",8), header=TRUE)

############################################################################################

# 1. Find codes which have a reference to haemoglobin and one to glycation/a1
hb_terms <- c("hb|haemoglobin|hemoglobin")
gly_terms <- c("a1|glycosylated|glycated")

hba1c_codes <- aurum_medcodes[grepl(hb_terms,aurum_medcodes$Term,ignore.case=TRUE),]
hba1c_codes <- hba1c_codes[grepl(gly_terms,hba1c_codes$Term,ignore.case=TRUE),]


#2. Remove codes relating to monitoring/reference ranges/targets/information/test request
to_remove <- c("reference range|monitoring range|target|written information|test request")

hba1c_codes <- hba1c_codes[!grepl(to_remove,hba1c_codes$Term,ignore.case=TRUE),]


############################################################################################

# Export
## Produces txt file with fields separated by tabs and Windows-style (\r\n aka CR LF) line endings
write.table(hba1c_codes, file="codelists/exeter_medcodelist_hba1c.txt", sep="\t", row.names=FALSE, quote=FALSE)
