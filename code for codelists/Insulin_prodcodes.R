# Finds prodcodes for insulin (NOT including equipment used for delivering insulin)

# insulin_names_cprd.csv used + searching for 'insulin' in drug substance

# In CPRD Aurum prodcodes lookup, product name can contain generic or brand name, drug substance name only contains generic names

# Haven't used BNF chapter as in Mastermind code as missing for most

# Produces txt file with fields separated by tabs and Windows-style (\r\n aka CR LF) line endings

############################################################################################

# Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(fuzzyjoin)
rm(list=ls())

############################################################################################

# Import Aurum prodcode lookup table
## Need to make sure prodcodes are imported as characters otherwise can lose precision in long prodcodes
aurum_prodcodes <- read.delim("CPRDAurumProduct.txt", colClasses=rep("character",10), header=TRUE)

## Replace product name with 'term from EMIS' where product name is missing (where product name isn't missing, appears to just be term from EMIS with manufacturer's name removed)
aurum_prodcodes$ProductName <- ifelse(aurum_prodcodes$ProductName=="",aurum_prodcodes$Term.from.EMIS,aurum_prodcodes$ProductName)


############################################################################################

# 1. Import csv file containing generic and brand names of insulin (both are in same column of csv), and make string of these for grepping

ins_names <- read_csv("insulin_names_cprd_2023.csv", col_types = cols(.default = "c"))


############################################################################################

# 2. Initial selection based on matching on these in drug substance and product name
## Check ones which don't contain 'insulin' in DrugSubstanceName

ins_name_codes_a <- aurum_prodcodes %>%
  regex_inner_join(ins_names, by=c("DrugSubstanceName"="name"), ignore_case=TRUE)

ins_name_codes_b <- aurum_prodcodes %>%
  regex_inner_join(ins_names, by=c("ProductName"="name"), ignore_case=TRUE)

ins_name_codes <- rbind(ins_name_codes_a, ins_name_codes_b) %>%
  select(-name)

rm(ins_name_codes_a, ins_name_codes_b)


check <- ins_name_codes %>%
  filter(!grepl("insulin", DrugSubstanceName, ignore.case=TRUE))

# All look good - have decided not to remove Exubera bits which aren't strictly insulin


############################################################################################

# 3. Check if any extras with 'insulin' in drug substance or product name

## Insulin in drug substance = definitely correct

ins_codes <- aurum_prodcodes %>%
  filter(grepl("insulin", DrugSubstanceName, ignore.case=TRUE))

check <- ins_codes %>% anti_join(ins_name_codes, by="ProdCodeId")
# 3 generic ones - add in

to_add <- check %>% mutate(basal_bolus="short") 
ins_name_codes <- rbind(ins_name_codes, to_add)


## Look at those with insulin in ProductName and check all found in part 2.

to_check <- aurum_prodcodes %>%
  filter(grepl("insulin", ProductName, ignore.case=TRUE) & !grepl("insulin", DrugSubstanceName, ignore.case=TRUE)) %>%
  filter(!grepl("syringe", ProductName, ignore.case=TRUE) & !grepl("needle", ProductName, ignore.case=TRUE) & !grepl("pen reusable", ProductName, ignore.case=TRUE))

check <- to_check %>% anti_join(ins_name_codes, by="ProdCodeId")
# 5 weird extras - don't include


############################################################################################

# 4. Make sure 1 line per prodcodeid and sort out basal/bolus insulin

ins_codes <- ins_name_codes %>%
  group_by(ProdCodeId) %>%
  mutate(basal=any(basal_bolus=="basal"),
         bolus=any(basal_bolus=="bolus"),
         intermediate=any(basal_bolus=="intermediate")) %>%
  mutate(drug_substance=ifelse(basal==TRUE, "Basal insulin",
                        ifelse(intermediate==TRUE, "Intermediate insulin","Bolus insulin"))) %>%
  select(-c(basal, bolus, intermediate, basal_bolus)) %>%
  slice(1)


############################################################################################

# Export
## Produces txt file with fields separated by tabs and Windows-style (\r\n aka CR LF) line endings
write.table(ins_codes, file="codelists//exeter_prodcodelist_insulin.txt", sep="\t", row.names=FALSE, quote=FALSE)




