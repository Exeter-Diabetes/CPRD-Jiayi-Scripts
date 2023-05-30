# Comparing new codelists(2023 version) with old ones(2020 version)
# Generate new codelists based on new/old
library(compareDF)
library(tidyverse)
setwd("/Users/wangjiayi/OneDrive - University of Exeter/dissertation/data")


# Read in old dictionary (version 2020)
old_medcodes <- read.delim("new_medcodes.txt", 
                              colClasses=rep("character"), header=TRUE)
old_prodcodes <- read.delim("new_prodcodes.txt", 
                              colClasses=rep("character"), header=TRUE)


# for all diabetes medical codes ----
base <- read.delim("old codelists/all_diabetes_medcodes_v2.txt",
                   colClasses=rep("character",8),header=TRUE) %>%
        arrange(MedCodeId)
compare <- read.delim("codelists/exeter_medcodelist_all_diabetes.txt",
                      colClasses=rep("character",9),header=TRUE) %>%
           arrange(MedCodeId) %>% select(-c("Observations"))
ctable = compare_df(base, compare, group_col="MedCodeId")
create_output_table(ctable)
# create_output_table(ctable, output_type = 'xlsx',
#                     file_name = "compare/diff_alldia.xlsx")

# Filter out the code only exist in new code lists and check if it exists in old
# dictionary
code_in_new <- ctable$comparison_df %>%
               filter(chng_type == "-")
new_code <- code_in_new %>% 
            filter(code_in_new$MedCodeId %in% old_medcodes$X.N) %>%
            select(-c("chng_type"))
new_lst <- rbind(base,new_code) %>% arrange(MedCodeId)

# Export
## Produces txt file with fields separated by tabs and Windows-style (\r\n aka CR LF) line endings
write.table(new_lst, file="codelists/exeter_medcodelist_all_diabetes_V2.txt", 
            sep="\t", row.names=FALSE, quote=FALSE)


# for hba1c medical codes ----
base <- read.delim("old codelists/exeter_medcodelist_hba1c.txt", 
                   colClasses=rep("character",8),header=TRUE) %>%
        arrange(MedCodeId)
compare <- read.delim("codelists/exeter_medcodelist_hba1c.txt",
                      colClasses=rep("character",9),header=TRUE) %>%
           arrange(MedCodeId) %>% select(-c("Observations"))
ctable = compare_df(base, compare, group_col="MedCodeId")
create_output_table(ctable)
# create_output_table(ctable, output_type = 'xlsx',
#                     file_name = "compare/diff_hba1c.xlsx")

# Filter out the code only exist in new code lists and check if it exists in old
# dictionary
code_in_new <- ctable$comparison_df %>%
               filter(chng_type == "-")
new_code <- code_in_new %>% 
        filter(!code_in_new$MedCodeId %in% old_medcodes$X.N) %>%
        select(-c("chng_type"))
new_lst <- rbind(base,new_code) %>% arrange(MedCodeId)

# Export
## Produces txt file with fields separated by tabs and Windows-style (\r\n aka CR LF) line endings
write.table(new_lst, file="codelists/exeter_medcodelist_hba1c_V2.txt", 
            sep="\t", row.names=FALSE, quote=FALSE)


# for insulin product codes ----
base <- read.delim("old codelists/exeter_prodcodelist_insulin.txt",
                   colClasses=rep("character",10), header=TRUE) %>%
        arrange(ProdCodeId) %>% 
        select(c("ProdCodeId", "dmdid","Term.from.EMIS"))
compare <- read.delim("codelists/exeter_prodcodelist_insulin.txt",
                      colClasses=rep("character",10), header=TRUE) %>%
           arrange(ProdCodeId) %>% 
           select(c("ProdCodeId", "dmdid","Term.from.EMIS"))
ctable = compare_df(base, compare, group_col="ProdCodeId")
create_output_table(ctable)
# create_output_table(ctable, output_type = 'xlsx',
#                     file_name = "compare/diff_insu.xlsx")

# Filter out the code only exist in new code lists and check if it exists in old
# dictionary
code_in_new <- ctable$comparison_df %>%
               filter(chng_type == "-")
test <- code_in_new %>% filter(!code_in_new$ProdCodeId %in% old_prodcodes$X.N)


# for non-insulin product codes ----
base <- read.delim("old codelists/exeter_prodcodelist_ohas.txt",
                   colClasses=rep("character",10),header=TRUE) %>%
        arrange(ProdCodeId) %>% 
        select(c("ProdCodeId", "dmdid","Term.from.EMIS"))
compare <- read.delim("codelists/exeter_prodcodelist_ohas.txt",
                      colClasses=rep("character",10),header=TRUE) %>%
           arrange(ProdCodeId) %>% 
           select(c("ProdCodeId", "dmdid","Term.from.EMIS"))
ctable = compare_df(base, compare, group_col="ProdCodeId")
create_output_table(ctable)
# create_output_table(ctable, output_type = 'xlsx',
#                     file_name = "compare/diff_noninsu.xlsx")

# Filter out the code only exist in new code lists and check if it exists in old
# dictionary
code_in_new <- ctable$comparison_df %>%
  filter(chng_type == "-")
test <- code_in_new %>% filter(!code_in_new$ProdCodeId %in% old_prodcodes$X.N)
