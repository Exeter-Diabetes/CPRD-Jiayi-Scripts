library(devtools)
library(aurum)
library(tidyverse)
library(dplyr)
library(ggplot2)
cprd = CPRDData$new(cprdEnv = "analysis", cprdConf = "~/.aurum.yaml")
codesets = cprd$codesets()
codes = codesets$getAllCodeSetVersion(v = "5/6/2023")
analysis = cprd$analysis("jy")


###############################################################################
# Merge Drug Issue table with products code
insulin_pts <- cprd$tables$drugIssue %>% 
               inner_join(codes$insulin, by="prodcodeid") %>%
               excludeInvalidDates(issuedate) %>%
               select("patid") %>% 
               distinct() %>% 
               analysis$cached("insulin_pts", indexes=c("patid"))
               
noninsulin_pts <- cprd$tables$drugIssue %>% 
                  inner_join(codes$noninsulin, by="prodcodeid") %>%
                  excludeInvalidDates(issuedate) %>%
                  select("patid") %>% 
                  distinct() %>% 
                  analysis$cached("noninsulin_pts", indexes=c("patid"))

# Merge observation table with medical (diabetes) code
diabetes_pts <- cprd$tables$observation %>% 
                inner_join(codes$all_diabetes, by="medcodeid") %>%
                excludeInvalidDates(obsdate) %>%
                select("patid") %>% 
                distinct() %>% 
                analysis$cached("diabetes_pts", indexes=c("patid"))

# Combine three data above
# Summary all patients with diabetes
all_dia_pts <- insulin_pts %>% 
               union(noninsulin_pts) %>%
               union(diabetes_pts) %>% 
               analysis$cached("all_dia_pts", indexes=c("patid"))

# Filter out patient without diabetes
no_dia_pts <- cprd$tables$patient %>% 
              anti_join(all_dia_pts, by="patid") %>% 
              filter(patienttypeid==3 & acceptable==1) %>% 
              mutate(mob=ifelse(is.na(mob), "7", mob),
                     birthdt=as.Date(paste0(yob, mob, "1", sep="-")),
                     gender=ifelse(gender==1, "Male", "Female")) %>% 
              select(c("patid", "gender", "birthdt")) %>% 
              analysis$cached("no_dia_pts", indexes=c("patid"))

# Merge observation table with hba1c medical code within non-diabetes patients
hba1c_obs = cprd$tables$observation %>%
            inner_join(codes$hba1c, by="medcodeid") %>%
            excludeInvalidDates(obsdate) %>%
            filter(!is.na(testvalue) & testvalue != 0) %>% 
            # transform % unit value into mmol/mol value
            mutate(testvalue=ifelse(testvalue<=20, (testvalue-2.152)/0.09148, testvalue)) %>% 
            filter(testvalue>=20 & testvalue<=195) %>% 
            filter(obsdate>=as.Date("1990-01-01")) %>%
            select(c("patid", "obsdate", "testvalue")) %>% 
            analysis$cached("hba1c_obs", indexes=c("patid", "obsdate"))

# Check distribution of hba1c records per patients
hba1c_distribution = hba1c_obs %>% group_by(patid) %>% count() %>% collect()
summary(hba1c_distribution$n)
quantile(hba1c_distribution$n,seq(0,1,by=0.05))
# 95% percentile is 23
# Get patient id with more than 23 hba1c records
hba1c_normal_amount_test = hba1c_obs %>% group_by(patid) %>% count() %>% 
                           filter(n<=23) %>% select("patid") %>% 
                           analysis$cached("hba1c_normal_amount_test", 
                                           indexes=c("patid"))

# Keep one record per patient
lst_hba1c_before =  hba1c_obs %>% 
                    filter(obsdate<=as.Date("2020-02-01")) %>% 
                    group_by(patid) %>%
                    slice_max(order_by = obsdate, n = 1)

lst_hba1c_after =  hba1c_obs %>% 
                   filter(obsdate>as.Date("2020-02-01")) %>% 
                   group_by(patid) %>%
                   slice_min(order_by = obsdate, n = 1)

hba1c_unique = union(lst_hba1c_before, lst_hba1c_after) %>%
               group_by(patid) %>%
               slice_min(order_by = obsdate, n = 1) %>% 
               distinct(patid, .keep_all = TRUE) %>% 
               analysis$cached("hba1c_unique", indexes=c("patid", "obsdate"))

# Merge observation table with ethnicity code within non-diabetes patients
eth_obs = cprd$tables$observation %>%
          inner_join(codes$ethnicity_5cat, by="medcodeid") %>%
          inner_join(codes$ethnicity_16cat, by="medcodeid") %>%
          excludeInvalidDates(obsdate) %>%
          mutate(ethnicity_5cat_n=ethnicity_5cat_cat,
                 ethnicity_16cat_n=ethnicity_16cat_cat) %>% 
          select(c("patid", "ethnicity_5cat_n", "ethnicity_16cat_n", "obsdate")) %>% 
          mutate(ord=ifelse(ethnicity_16cat_n<16, 1, 2)) %>% 
          analysis$cached("eth_obs", indexes=c("patid", "obsdate"))

# Map ethnicity number with character
# eth_obs$ethnicity_16cat <- factor(eth_obs$ethnicity_16cat_n,
#                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
#                                             11, 12, 13, 14, 15, 16, 17),
#                                  labels = c('British','Irish','Other White','White and Black Caribbean',
#                                             'White and Black African','White and Asian','Other Mixed',
#                                             'Indian','Pakistani','Bangladeshi','Other Asian','Caribbean',
#                                             'African','Other Black','Chinese','Other ethnic group',
#                                             'Not Stated')) 

eth_unique = eth_obs %>%
             group_by(patid, ord, ethnicity_16cat_n, ethnicity_5cat_n) %>%
             summarise(count=n_distinct(ethnicity_16cat_n), dat=max(obsdate), 
                       .groups="drop") %>% 
             dbplyr::window_order(ord, desc(count), desc(dat)) %>% 
             distinct(patid, .keep_all = TRUE) %>% 
             analysis$cached("eth_unique", indexes=c("patid"))

# Merge all data set above into one
# Health population
all_nodia_cohort <- hba1c_unique %>% 
                    inner_join(no_dia_pts, by="patid") %>% 
                    inner_join(hba1c_normal_amount_test, by="patid") %>%
                    left_join(eth_unique, by="patid") %>% 
                    select(-c("dat", "ord", "count")) %>%
                    mutate(age=round(datediff(obsdate, birthdt)/365.25, 2),
                           ethnicity_5cat=case_when(ethnicity_5cat_n==0 ~ "White",
                                                    ethnicity_5cat_n==0 ~ "South Asian",
                                                    ethnicity_5cat_n==0 ~ "Black",
                                                    ethnicity_5cat_n==0 ~ "Other",
                                                    ethnicity_5cat_n==0 ~ "Mixed",
                                                    ethnicity_5cat_n==0 ~ "Not Stated")) %>% 
                    arrange(patid, obsdate) %>%
                    analysis$cached("all_nodia_cohort", indexes=c("patid", "obsdate"))




############For checking purpose
hba1c_obs_nofilter = cprd$tables$observation %>%
  inner_join(codes$hba1c, by="medcodeid") %>%
  excludeInvalidDates(obsdate) %>%
  filter(!is.na(testvalue) & testvalue != 0) %>% 
  analysis$cached("hba1c_obs_nofilter", indexes=c("patid", "obsdate"))

nodia_with_hba1c_nofilter <- hba1c_obs_nofilter %>% 
  inner_join(no_dia_pts, by="patid") %>% 
  analysis$cached("nodia_with_hba1c_nofilter", indexes=c("patid", "obsdate"))

