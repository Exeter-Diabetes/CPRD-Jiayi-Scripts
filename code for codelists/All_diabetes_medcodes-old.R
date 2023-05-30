# Code to extract all diabetes medcodes
# Based on searching term description
# Initial extraction of CPRD Aurum based on having one of these codes BUT I have refined list slightly (removed codes) since then, so some people in download won't have one of these codes
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

# 1. Find all codes containing *diab*, *IDDM*, *d m*, *DM* in description
## NB: *iddm* finds both IDDM (insulin-dependent diabetes mellitus) and NIDDM (non-insulin dependent DM)

diabetes <- c("diab|iddm| d m |^d m |d m$| dm |^dm | dm$")

diabetes_match <- aurum_medcodes[grepl(diabetes,aurum_medcodes$Term,ignore.case=TRUE),]



# 2. Remove non-diabetes codes which have been picked up (identified manually by inspection)
not_diabetes <- c("remediable|actifed|diastolic murmur|dystrophia myotonica|dermatomyositis|parents evaluation of developmental status")

diabetes_match <- diabetes_match[!grepl(not_diabetes,diabetes_match$Term,ignore.case=TRUE),]



# 3. Add in other syndromic/atypical/genetic diabetes terms not picked up by the above
extra_diabetes <- c("fibrocalculous pancreatopathy|rabson-mendenhall|wolfram|type a.*insulin resistance|insulin resistance.*type a|MODY")

diabetes_match <- unique(rbind(diabetes_match, aurum_medcodes[grepl(extra_diabetes,aurum_medcodes$Term,ignore.case=TRUE),]))

## Remove "h(a)emodynamic monitoring" codes introduced by including MODY
diabetes_match <- diabetes_match[!grepl("emodynamic monitoring",diabetes_match$Term,ignore.case=TRUE),]


# 4. Add in insulin therapy codes - can't just include *insulin* wholesale as so many hypo/hyperinsulinemia/insulin antibody/insulin shock therapy/insulin-like growth factor/insulinoma etc. codes
insulin_treatment <- c("insulin therapy|conversion to insulin|insulin initiation|insulin storage|insulin started|insulin needles changed|insulin injection technique|insulin dose|commencement of insulin|insulin treatment|teach insulin injection|insulin alert|insulin.*administ|administ.*insulin|insulin.*lipohypertrophy|lipohypertrophy.*insulin|insulin.*subcut|subcut.*insulin|insulin passport|insulin poisoning|adverse reactions to insulin|drug resistance to insulin")

insulin_treatment <- aurum_medcodes[grepl(insulin_treatment,aurum_medcodes$Term,ignore.case=TRUE),]


diabetes_match <- unique(rbind(diabetes_match, insulin_treatment))



# 5. Remove codes not indicative of a diabetes diagnosis:
## A.Remove drugs and allergic reactions to these drugs (original read code prefix DRG/ALLERGY)
drug_allergy <- c("^DRG|^ALLERGY")

diabetes_match <- diabetes_match[!grepl(drug_allergy,diabetes_match$OriginalReadCode,ignore.case=FALSE),]


## B. Remove codes about family history/conditions relating to being the child of a mother with diabetes during pregnancy
family_history <- c("family history|fh:|family/carer|diabetic child|diabetic relative|infant of|maternal gestational diabetes|diabetes mellitus in mother|maternal diabetes with hypoglycaemia affecting f")

diabetes_match <- diabetes_match[!grepl(family_history,diabetes_match$Term,ignore.case=TRUE),]


## C. Remove codes about occupation
diabetes_match <- diabetes_match[!grepl("Staff group: Nursing - Diabetic Nursing/Liaison",diabetes_match$Term,ignore.case=TRUE),]


## D. Remove codes which indicate lack of/suspicion of diabetes
not_really_diabetes <- c("no h/o: diabetes|non-diabet|diabetes mellitus excluded|suspected diabetes mellitus|diabetic monitoring not required|consider the patient not to have diabetes|more time needed to decide on diabetes status|no longer diabetic|no longer characterised as diabetic")

diabetes_match <- diabetes_match[!grepl(not_really_diabetes,diabetes_match$Term,ignore.case=TRUE),]


## E. Remove codes about risk assessment/tests/screening for diabetes
### NB: "congestive heart failure" search is for CHADS2 score terms - acronym not always included
### NB: have checked and none of the terms containing "prevention" are for symptoms of DM, all are for prevention of DM itself
### NB: couldn't find out what Liverpool Diabetes Pilot was so removed

risk <- c("risk of diabetes|high risk review|risk score|rsk scre|qdiabetes|congestive heart failure, hypertension, age.*score|score.*congestive heart failure, hypertension, age|test for diabetes|prevention|screening for diabetes|diabetes mellitus screen|diabetes screen|referred by diabetes uk|pre-diabet|prediabet|liverpool diabetes pilot|walking away from diabetes")

diabetes_match <- diabetes_match[!grepl(risk,diabetes_match$Term,ignore.case=TRUE),]


## F. Remove diabetes insipidus codes
diabetes_match <- diabetes_match[!grepl("insipidus",diabetes_match$Term,ignore.case=TRUE),]

############################################################################################


# Export
## Produces txt file with fields separated by tabs and Windows-style (\r\n aka CR LF) line endings
write.table(diabetes_match, file="codelists/exeter_medcodelist_all_diabetes.txt", sep="\t", row.names=FALSE, quote=FALSE)

