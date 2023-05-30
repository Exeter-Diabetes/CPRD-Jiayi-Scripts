# Finds prodcodes for diabetes medications, NOT including insulin

# Uses Diabetes_medications_2021.csv file for generic and brand drug names
## Word doc (Diabetes Medications 2021) has same information as csv file

# In CPRD Aurum prodcodes lookup, product name can contain generic or brand name, drug substance name only contains generic names

# Haven't used BNF chapter as in Mastermind code as missing for most

# Add in drug classes and also which drug within class

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

# 1. Import csv file containing generic and brand names of drugs with drug classes and drug substances
## Join rather than use grepping so keep class and substance

drug_names <- read_csv("Diabetes_medications_2023.csv", col_types = cols(.default = "c"))

## A. Extract generic names (none of these refer to combinations)

generic_names <- drug_names %>%
  select(generic_name=`Generic name`,drug_class=`Drug class`) %>%
  filter(!grepl("\\+",drug_class)) %>%
  group_by(generic_name) %>%
  slice(1) %>%
  mutate(drug_substance=generic_name) %>%
  ungroup()


## B. Extract brand names
### First need to reformat so where brand name says 'a or b', have two different rows for a and b
### Then do the same for drug combinations - so only 1 drug class/substance per line - long format
### Unnesting drug class and substance gives duplicates which need to remove
### Also need to get rid of row with missing brand name

brand_names <- drug_names %>% 
  select(drug_substance=`Generic name`,brand_name=`Brand name`,drug_class=`Drug class`) %>% 
  mutate(brand_name=strsplit(brand_name," or ")) %>% 
  unnest(brand_name) %>% 
  filter(!is.na(brand_name)) %>% 
  mutate(drug_class=strsplit(drug_class,"\\+"), drug_substance=strsplit(drug_substance," \\+ ")) %>% 
  unnest(drug_class) %>%
  unnest(drug_substance) %>%
  select(brand_name,drug_class,drug_substance)

brand_names <- brand_names %>% 
  filter(!(drug_class=="MFN" & drug_substance!="Metformin") & 
           !(drug_class!="MFN" & drug_substance=="Metformin") &
           !(drug_class=="SGLT2" & !grepl("flozin",drug_substance)) & 
           !(drug_class!="SGLT2" & grepl("flozin",drug_substance)) &
           !(drug_class=="TZD" & !grepl("zone",drug_substance)) & 
           !(drug_class!="TZD" & grepl("zone",drug_substance)) &
           !(drug_class=="INS" & drug_substance!="Insulin") & 
           !(drug_class!="INS" & drug_substance=="Insulin"))

############################################################################################

# 2. Initial selection based on generic names (in drug substance, or if this is missing, product name)
## No need to check these as generic names are very specific

generic_codes_a <- aurum_prodcodes %>%
  regex_inner_join(generic_names, by=c("DrugSubstanceName"="generic_name"), ignore_case=TRUE)

generic_codes_b <- aurum_prodcodes %>%
  filter(DrugSubstanceName=="") %>%
  regex_inner_join(generic_names, by=c("ProductName"="generic_name"), ignore_case=TRUE)

generic_codes <- rbind(generic_codes_a, generic_codes_b) %>%
  select(-generic_name)

rm(generic_codes_a, generic_codes_b)


############################################################################################

# 3. Find based on brand names in product name field
## Need to check these afterwards

brand_codes <- aurum_prodcodes %>%
  regex_inner_join(brand_names, by=c("ProductName"="brand_name"), ignore_case=TRUE) %>%
  select(-brand_name)


# Only need to check those which were not already identified by generic names above
check <- brand_codes %>%
  anti_join(generic_codes, by="ProdCodeId")

## Need to remove those with 'lactose'

brand_codes <- brand_codes %>%
  filter(!grepl("lactose", ProductName, ignore.case=TRUE))


############################################################################################

# 4. Combine above codes (still in long format with multiple lines for combinations)

non_ins_codes <- unique(rbind(generic_codes,brand_codes))


############################################################################################

# 5. Convert long -> wide
## For drug classes: separate binary yes/no column for each class
## For drug substances: drug_substance1, drug_substance2 columns

non_ins_codes <- non_ins_codes %>%
  mutate(fill=TRUE) %>%
  pivot_wider(names_from=drug_class, values_from=fill, values_fill=list(fill=FALSE)) %>%
  group_by(ProdCodeId) %>%
  mutate(Acarbose=as.numeric(any(Acarbose)),
         DPP4=as.numeric(any(DPP4)),
         Glinide=as.numeric(any(Glinide)),
         GLP1=as.numeric(any(GLP1)),
         INS=as.numeric(any(INS)),
         MFN=as.numeric(any(MFN)),
         SGLT2=as.numeric(any(SGLT2)),
         SU=as.numeric(any(SU)),
         TZD=as.numeric(any(TZD)))



# Sort out exenatide - can be short or extended release
exenatide <- non_ins_codes %>%
  filter(drug_substance=="Exenatide" | drug_substance=="Exenatide prolonged-release") %>%
  group_by(ProdCodeId) %>%
  slice(1) %>%
  mutate(drug_substance=ifelse(grepl("prolonged", ProductName, ignore.case=TRUE), "Exenatide prolonged-release", "Exenatide"))

non_ins_codes <- non_ins_codes %>%
  filter(drug_substance!="Exenatide" & drug_substance!="Exenatide prolonged-release")

non_ins_codes <- rbind(non_ins_codes, exenatide)



non_ins_codes <- non_ins_codes %>%
  group_by(ProdCodeId) %>%
  # Replace drug substance for basal or bolus insulin
  mutate(drug_substance=ifelse(drug_substance=="Insulin", "Basal insulin", drug_substance)) %>%
  mutate(new_id=paste0("drug_substance",row_number())) %>%
  spread(new_id, drug_substance) %>%
  ungroup()


# Check that all have drug class
test <- non_ins_codes %>% filter(Acarbose==0 & DPP4==0 & Glinide==0 & GLP1==0 & INS==0 & MFN==0 & SGLT2==0 & SU==0 & TZD==0)
#n=0

# Check all have drug substance
test <- non_ins_codes %>% filter(is.na(drug_substance1))
#n=0


############################################################################################

# Export
## Produces txt file with fields separated by tabs and Windows-style (\r\n aka CR LF) line endings
write.table(non_ins_codes, file="codelists/exeter_prodcodelist_ohas.txt", sep="\t", row.names=FALSE, quote=FALSE, na="")


oha_lookup <- non_ins_codes %>% select(ProdCodeId, Acarbose, DPP4, Glinide, GLP1, MFN, SGLT2, SU, TZD, INS, drug_substance1, drug_substance2)

write.table(oha_lookup, file="oha_lookup.txt", sep="\t", row.names=FALSE, quote=FALSE, na="")


drug_substance_class_lookup <- oha_lookup %>%
  filter(is.na(drug_substance2)) %>%
  rename(drug_substance=drug_substance1) %>%
  select(-c(ProdCodeId, drug_substance2)) %>%
  distinct() %>%
  mutate(across(c(Acarbose, DPP4, Glinide, GLP1, INS, MFN, SGLT2, SU, TZD), ~case_when(. == 1 ~ cur_column()), .names = 'new_drug_class_{col}')) %>%
  unite(drug_class, starts_with('new_drug_class_'), na.rm = TRUE) %>%
  select(drug_substance, drug_class) %>%
  add_row(drug_substance=c("Basal insulin","Intermediate insulin","Bolus insulin"),
          drug_class=rep("INS",3))

write.table(drug_substance_class_lookup, file="drug_substance_class_lookup.txt", sep="\t", row.names=FALSE, quote=FALSE, na="")
