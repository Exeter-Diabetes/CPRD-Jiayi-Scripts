###############################Stats analysis###################################
label(dat$patid) = "Patient id"
label(dat$obsdate) = "Observation date"
label(dat$testvalue) = "HbA1c in mmol/mol"
label(dat$gender) = "Sex"
label(dat$birthdt) = "Birth date"
label(dat$age) = "Age"
label(dat$ethnicity_5cat) = "Ethnicity category (5)"
label(dat$ethnicity_16cat) = "Ethnicity category (16)"
dat <- dat %>% mutate(age_group=case_when((age<18) ~ "<18",
                                          (18<=age & age<30) ~ "18-29",
                                          (30<=age & age<40) ~ "30-39",
                                          (40<=age & age<50) ~ "40-49",
                                          (50<=age & age<60) ~ "50-59",
                                          (60<=age & age<70) ~ "60-69",
                                          (70<=age & age<80) ~ "70-79",
                                          age>=80 ~ ">=80"),
                      age_group=factor(age_group, levels= c("<18", "18-29",
                                                            "30-39","40-49",
                                                            "50-59","60-69",
                                                            "70-79",">=80")),
                      ethnicity_5cat=ifelse(is.na(ethnicity_5cat)|ethnicity_5cat=="Not Stated", 
                                            "Unknown", ethnicity_5cat),
                      ethnicity_16cat=ifelse(is.na(ethnicity_16cat)|ethnicity_16cat=="Not Stated", 
                                             "Unknown", ethnicity_16cat),
                      ethnicity_5cat=factor(ethnicity_5cat, levels=c("White", "South Asian",
                                                                     "Black", "Other",
                                                                     "Mixed", "Unknown")),
                      ethnicity_16cat=factor(ethnicity_16cat, levels=c('British','Irish',
                                                                       'Other White',
                                                                       'White and Black Caribbean',
                                                                       'White and Black African',
                                                                       'White and Asian','Other Mixed',
                                                                       'Indian','Pakistani','Bangladeshi',
                                                                       'Other Asian','Caribbean','African',
                                                                       'Other Black','Chinese',
                                                                       'Other ethnic group','Unknown')))

all_dat <- all_nodia_cohort %>% collect() %>% 
  mutate(age_group=case_when((age<18) ~ "<18",
                             (18<=age & age<30) ~ "18-29",
                             (30<=age & age<40) ~ "30-39",
                             (40<=age & age<50) ~ "40-49",
                             (50<=age & age<60) ~ "50-59",
                             (60<=age & age<70) ~ "60-69",
                             (70<=age & age<80) ~ "70-79",
                             age>=80 ~ ">=80"),
         age_group=factor(age_group, levels= c("<18", "18-29",
                                               "30-39","40-49",
                                               "50-59","60-69",
                                               "70-79",">=80")),
         ethnicity_5cat=ifelse(is.na(ethnicity_5cat)|ethnicity_5cat=="Not Stated", 
                               "Unknown", ethnicity_5cat),
         ethnicity_16cat=ifelse(is.na(ethnicity_16cat)|ethnicity_16cat=="Not Stated", 
                                "Unknown", ethnicity_16cat),
         ethnicity_5cat=factor(ethnicity_5cat, levels=c("White", "South Asian",
                                                        "Black", "Other",
                                                        "Mixed", "Unknown")),
         ethnicity_16cat=factor(ethnicity_16cat, levels=c('British','Irish',
                                                          'Other White',
                                                          'White and Black Caribbean',
                                                          'White and Black African',
                                                          'White and Asian','Other Mixed',
                                                          'Indian','Pakistani','Bangladeshi',
                                                          'Other Asian','Caribbean','African',
                                                          'Other Black','Chinese',
                                                          'Other ethnic group','Unknown')))

# 1. Descriptive analysis ----
## distribution & kernel density curve of HbA1c & normality test
density_hba1c <- ggplot(dat, aes(x=testvalue)) + 
  geom_histogram(aes(y=after_stat(density)),
                 fill="#69b3a2", color="white",alpha=0.5, bins=50) +
  geom_density(color='#FF6666',bw=1) +
  geom_vline(aes(xintercept=mean(testvalue), color="mean"),
             linewidth=1,linetype="dashed") + 
  theme(text = element_text(size = 14), plot.title = element_text(hjust = 0.5)) + 
  labs(title="Histogram with fitted density \n curve of HbA1c levels",
       x="HbA1c levels")
## create Q-Q plot
qq_hba1c <- ggplot(dat, aes(sample=testvalue)) +
  stat_qq(size=2.5, color='#FF6666') + 
  stat_qq_line() +
  theme(text = element_text(size = 14), plot.title = element_text(hjust = 0.5)) + 
  labs(title="Q-Q plot",
       y = "HbA1c")

## distribution & kernel density curve of age
density_age <- ggplot(dat, aes(x=age)) + 
  geom_histogram(aes(y=after_stat(density)),bins=15,
                 fill="#69b3a2", color="white",alpha=0.5) +
  geom_density(color='#FF6666', bw=3) +
  geom_vline(aes(xintercept=mean(age), color="mean"),
             linewidth=1,linetype="dashed") + 
  theme(text = element_text(size = 14), plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Histogram with fitted density \n curve of age")
## create Q-Q plot
qq_age <- ggplot(dat, aes(sample=age)) +
  stat_qq(size=2.5, color='#FF6666') + 
  stat_qq_line() +
  theme(text = element_text(size = 14), plot.title = element_text(hjust = 0.5)) + 
  labs(title="Q-Q plot",y = "Age")
summary(dat$age)

my_plots <- list(density_hba1c, qq_hba1c, density_age, qq_age)
my_layout <- rbind(c(1, 2), c(3, 4))
grid.arrange(grobs = my_plots, layout_matrix = my_layout)

## distribution of gender
ggplot(dat, aes(x=gender,
                y=(..count..)/sum(..count..),
                fill=gender)) +
  geom_bar(position="dodge", alpha=0.8) +
  labs(x="Sex", y="Percentage", fill="Sex") + 
  theme(text = element_text(size = 18))

## distribution of ethnicity
ggplot(all_dat, aes(x=ethnicity_5cat,
                    y=(after_stat(count))/sum(after_stat(count)),
                    fill=ethnicity_5cat)) +
  geom_bar(position="dodge", alpha=0.8) +
  labs(x="Ethnicity", y="Percentage", fill="Ethnicity") + 
  theme(text = element_text(size = 18))

## Demographic table
render.categorical <- function(x, ...) {
  c("", sapply(stats.apply.rounding(stats.default(x)), 
               function(y) with(y,sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT))))}
my.render.cont <- function(x) {
  with(stats.default(x), 
       c("",
         
         "Mean (SD)" = sprintf("%s (%s)",
                               round_pad(MEAN, 1),
                               round_pad(SD, 1)),
         
         "Median (Min, Max)" = sprintf("%s (%s, %s)",
                                       round_pad(MEDIAN, 1), 
                                       round_pad(MIN, 1), 
                                       round_pad(MAX, 1)))
  )
}

demo.tbl <- as.data.frame(table1(~ age + gender + testvalue| age_group,
                                 data=dat, overall=c(left="Total"),
                                 render.continuous=my.render.cont,
                                 render.categorical=render.categorical))
colnames(demo.tbl)[1] <- "term"
demo.tbl[1,] <- ifelse(demo.tbl[1,]!="",
                       paste0("(N =",format(as.numeric(substring(demo.tbl[1,], 4,nchar(demo.tbl[1,])-1)),big.mark=",",scientific=FALSE), ")"), "")


eth16 <- dat %>%  group_by(age_group, ethnicity_5cat, ethnicity_16cat) %>% 
  summarise(count = n() ) %>%
  mutate(prop = count / sum(count))

eth5 <- dat %>%  group_by(age_group, ethnicity_5cat) %>% 
  summarise(count = n() ) %>%
  mutate(prop = count / sum(count))

tot_eth16 <- dat %>%  group_by(ethnicity_5cat, ethnicity_16cat) %>% 
  summarise(count = n() ) %>%
  mutate(prop = count / sum(count),
         age_group="Total")

tot_eth5 <- dat %>%  group_by(ethnicity_5cat) %>% 
  summarise(count = n() ) %>%
  mutate(prop = count / sum(count),
         age_group="Total")

eth <- rbind(eth16, eth5, tot_eth16, tot_eth5) %>% 
  arrange(age_group, ethnicity_5cat, desc(count)) %>% 
  mutate(term=ifelse(is.na(ethnicity_16cat), 
                     paste0("",ethnicity_5cat), 
                     paste0("&nbsp;&nbsp;",ethnicity_16cat)), 
         stat=paste0(formatC(count,big.mark=","), "(", sprintf("%.1f", prop*100),"%)")) %>% 
  filter(prop!=1) %>%  
  pivot_wider(id_cols=c(ethnicity_5cat, term), names_from=age_group,values_from=stat)

eth <- rbind(list(term="Ethnicity"), eth) %>% select(-c(ethnicity_5cat))
eth[is.na(eth)] <- ""

final.demo.tbl <- rbind(demo.tbl ,eth) %>% 
  gt() %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "white",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white") %>% 
  tab_style(style=list(cell_text(weight = "bold"),
                       cell_borders(sides = c("top", "bottom"),
                                    color = "#D3D3D3")),
            locations=cells_body(rows=Total=="")) %>%
  tab_style(style=list(cell_borders(sides = c("bottom"),
                                    color = "black",
                                    weight=px(2))),
            locations=cells_body(rows=term=="")) %>%  
  cols_label(term="Characteristics") %>%
  cols_width(term ~ px(220), everything() ~ px(150)) %>%
  fmt_markdown(columns=term) %>% 
  gtsave("OneDrive - University of Exeter/dissertation/writeup/demo.png", 
         expand=10, vwidth=2500)


# 2. Uni-variate linear regression ----
## Age
uni.model1 <- lm(testvalue ~ age, data=all_dat)
par(mfrow =c(2,2))
plot(uni.model1)
uni.sum <- summary(uni.model1)$coefficients
uni.plot1 <- ggplot(dat, aes(x=age, y=testvalue)) + 
  geom_point(size=0.5, shape=1, alpha=0.3, color="#E69F00")+
  geom_smooth(method=lm, color="#999999") + 
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,80), breaks = seq(20, 80, 10)) +
  scale_x_continuous("Age (years)") + 
  theme(text = element_text(size = 18))


## Sex
uni.model2 <- lm(testvalue ~ gender, data=dat)
par(mfrow =c(2,2))
plot(uni.model2)
uni.sum <- rbind(uni.sum, summary(uni.model2)$coefficients)
uni.plot2 <- ggplot(dat, aes(x=gender, y=testvalue, color=gender)) + 
  geom_jitter(size=0.5, shape=1, alpha=0.3, color="#E69F00")+ 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,80), breaks = seq(20, 80, 10)) +
  scale_x_discrete("Sex") +
  stat_summary(fun = "mean", color = "#12FFA7", 
               position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE) +
  labs(color = "Sex") + 
  theme(text = element_text(size = 18))

ggplot(dat, aes(x=testvalue, color=gender)) +
  geom_density(bounds=c(-Inf, 80), bw=1)

## Ethnicity 5
uni.model3 <- lm(testvalue ~ ethnicity_5cat, data=all_dat)
par(mfrow =c(2,2))
plot(uni.model3)
uni.sum <- rbind(uni.sum, summary(uni.model3)$coefficients)
uni.plot3 <- ggplot(all_dat, aes(x=ethnicity_5cat, y=testvalue, color=ethnicity_5cat)) + 
  geom_jitter(size=0.5, shape=1, alpha=0.3, color="#E69F00")+ 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,80), breaks = seq(20, 80, 10)) +
  scale_x_discrete("Ethnicity (5 categories)") +
  stat_summary(fun = "mean", color = "#12FFA7", 
               position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE) + 
  theme(text = element_text(size = 18))

## Ethnicity 16
uni.model4 <- lm(testvalue ~ ethnicity_16cat, data=dat)
uni.sum <- rbind(uni.sum, summary(uni.model4)$coefficients)
uni.plot4 <- ggplot(dat, aes(x=ethnicity_16cat, y=testvalue, color=ethnicity_16cat)) +
  geom_jitter(size=0.5, shape=1, alpha=0.3, color="#E69F00")+
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous("HbA1c (mmol/mol)",
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929,
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,80), breaks = seq(20, 80, 10)) +
  scale_x_discrete("Age groups (years)") +
  theme(text = element_text(size = 18))

## Uni regression table
uni.sum <- as.data.frame(cbind(term = rownames(uni.sum), uni.sum))
uni.results <- uni.sum %>% 
  filter(term != "(Intercept)") %>% 
  mutate(lower=round(as.numeric(Estimate)-1.96*as.numeric(`Std. Error`), 2),
         upper=round(as.numeric(Estimate)+1.96*as.numeric(`Std. Error`), 2),
         ci=paste0("(",sprintf("%.2f",lower), ", ", sprintf("%.2f", upper),")"),
         coefficient=round(as.numeric(Estimate), 2),
         pvalue=round(as.numeric(`Pr(>|t|)`),3),
         pvalue=ifelse(pvalue<0.001, "<0.001", pvalue),
         var=ifelse(grepl("16",term), "Ethnicity (16)", 
                    ifelse(grepl("5",term), "Ethnicity (5)", 
                           "")),
         term=ifelse(term=="genderMale", "Male(ref: Female)", term),
         term=ifelse(grepl("5",term), paste0(substring(term, 15)," (ref: White)"), term),
         term=ifelse(grepl("16",term), paste0(substring(term, 16)," (ref: British)"), term),
         term=ifelse(term=="age", "Age", term))

uni.tbl <- uni.results %>% 
  select(var,term,coefficient,ci,pvalue) %>% 
  gt(groupname_col="var", rowname_col="term") %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white") %>%
  cols_label(term="HbA1c", coefficient="Coefficient", ci="95% CI", pvalue="p-value") %>%
  tab_spanner(label="Unadjusted", columns=c(coefficient, ci, pvalue)) %>% 
  cols_align(align="left", columns=everything()) %>%
  tab_row_group(label = "White",rows=matches(c('British','Irish','Other White'))) %>% 
  tab_row_group(label = "Black",rows=matches(c('Caribbean','African',
                                               'Other Black'))) %>%
  tab_row_group(label = "Mixed",rows = matches(c('White and Black Caribbean',
                                                 'White and Black African',
                                                 'White and Asian','Other Mixed'))) %>%
  tab_row_group(label = "South Asian",rows = matches(c('Indian','Pakistani','Bangladeshi',
                                                       'Other Asian'))) %>% 
  tab_row_group(label = "Other",rows = matches(c('Chinese','Other ethnic group'))) %>%
  tab_row_group(label = "Unknown",rows = term=='Unknown (ref: British)') %>%
  row_group_order(groups = c("","Ethnicity (5)","White","South Asian","Black",
                             "Other","Mixed","Unknown")) %>% 
  gtsave("OneDrive - University of Exeter/dissertation/writeup/univariate_regression.png",
         expand=10)


# 3. ANOVA/t-test ----
## Age group with sex group ----
### 1) interaction and multilinearity check ----
check1 <- lm(testvalue ~ age + gender, data=dat)
check2 <- lm(testvalue ~ age * gender, data=dat)
summary(check1)
vif(check)
bi.model1.female <- lm(testvalue ~ age, data=filter(dat,gender=="Female"))
bi.model1.male <- lm(testvalue ~ age, data=filter(dat,gender=="Male"))
### 2) scatter plot with regression line by gender ----
bi.re.plot1 <- ggplot(dat, aes(x=age, y=testvalue, color=gender)) + 
  geom_point(size=0.5, shape=1, alpha=0.3,color="#E69F00")+
  geom_smooth(method=lm, fill=NA) + 
  scale_colour_manual(values = c("#E21818","#00235B")) +
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,60), breaks = seq(20, 80, 10)) +
  labs(x="Age (years)", color="Sex") + 
  theme(text = element_text(size = 18))
### 3) box plot ----
bi.plot1 <- ggplot(dat, aes(x=age_group, y=testvalue, fill=gender)) + 
  geom_jitter(size=0.5, shape=1, alpha=0.3, color="#E69F00") + 
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun = "mean", color = "white", 
               position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE) +  
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,60), breaks = seq(20, 80, 10)) +
  labs(x="Age (years)", color="Sex") + 
  theme(text = element_text(size = 18))
### 4) summary table ----
bi.sum1 <- dat %>% group_by(age_group, gender) %>% 
  summarize(mean=mean(testvalue), sd=sd(testvalue), n=n()) %>% 
  as.data.frame() %>% 
  mutate(mean_sd=paste0(sprintf("%.1f",mean), " (", sprintf("%.1f", sd), ")")) 
### ANOVA test 
bi.within.p <- data.frame(age_group=character(), gender=character(), mean_sd=character()) 
for (i in c("Male", "Female")){
  tmp <- dat %>% filter(gender==i)
  bi.aov1 <- aov(testvalue ~ age_group, data=tmp)
  bi.within.p[nrow(bi.within.p)+1,] <- c("p-value",i,summary(bi.aov1)[[1]][1,5])}
### t-test
bi.between.p <- data.frame(age_group=character(), pvalue=character()) 
for (i in c("18-29","30-39","40-49","50-59","60-69","70-79",">=80")){
  tmp <- dat %>% filter(age_group==i)
  bi.t1 <- t.test(testvalue ~ gender, data=tmp)
  bi.between.p[nrow(bi.between.p)+1,] <- c(i,bi.t1$p.value)}

### pvalue format
bi.between.p$pvalue <- ifelse(as.numeric(bi.between.p$pvalue)<0.001, 
                              "<0.001", 
                              sprintf("%.3f",as.numeric(bi.between.p$pvalue)))
bi.within.p$mean_sd <- ifelse(as.numeric(bi.within.p$mean_sd)<0.001, 
                              "<0.001", 
                              sprintf("%.3f",as.numeric(bi.within.p$mean_sd)))

### Merge output
bi.result1 <- bind_rows(bi.sum1, bi.within.p) %>%  
  pivot_wider(id_cols=age_group,names_from=gender,values_from=c(mean,mean_sd,n)) %>% 
  merge(bi.between.p,all.x = TRUE) %>% 
  mutate(N=formatC(n_Female+n_Male, big.mark=","),
         n_Female=formatC(n_Female, big.mark=","),
         n_Male=formatC(n_Male, big.mark=","),
         age_group=factor(age_group, levels= c("<18", "18-29",
                                               "30-39","40-49",
                                               "50-59","60-69",
                                               "70-79",">=80",
                                               "p-value")),
         diff=round(mean_Male-mean_Female,1)) %>%
  select(-c(mean_Male, mean_Female)) %>% 
  arrange(age_group)
bi.result1 <- bi.result1 %>% replace(is.na(bi.result1)|bi.result1=="NA", " ") %>%
  gt() %>%
  cols_move(columns=N,after=age_group) %>%
  cols_move(columns=diff,after=n_Male) %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white") %>%
  cols_label(age_group="Age Group", N="N", n_Male="n", 
             mean_sd_Male="Mean(SD)", n_Female="n", 
             mean_sd_Female="Mean(SD)",diff="Difference<br>(Male-Female)",
             pvalue="p-value",.fn = md) %>%
  tab_spanner(label="All", columns=c(N)) %>% 
  tab_spanner(label="Male", columns=c(n_Male, mean_sd_Male)) %>%
  tab_spanner(label="Female", columns=c(n_Female, mean_sd_Female)) %>%
  cols_align(align="left", columns=everything()) %>% 
  gtsave("OneDrive - University of Exeter/dissertation/writeup/anova_age_sex.png",
         expand=10)

## Age  group with ethnicity (5) ----
### 1) interaction and multilinearity check ----
check1 <- lm(testvalue ~ age + ethnicity_5cat, data=dat)
check2 <- lm(testvalue ~ age * ethnicity_5cat, data=dat)
anova(check1, check2)
summary(check)
vif(check)
bi.model2.white <- lm(testvalue ~ age, data=filter(dat,ethnicity_5cat=="White"))
bi.model2.southasian <- lm(testvalue ~ age, data=filter(dat,ethnicity_5cat=="South Asian"))
bi.model2.black <- lm(testvalue ~ age, data=filter(dat,ethnicity_5cat=="Black"))
bi.model2.other <- lm(testvalue ~ age, data=filter(dat,ethnicity_5cat=="Other"))
bi.model2.mixed <- lm(testvalue ~ age, data=filter(dat,ethnicity_5cat=="Mixed"))
bi.model2.unkonwn <- lm(testvalue ~ age, data=filter(dat,ethnicity_5cat=="Unknown"))
### 2) scatter plot with regression line by ethnicity/ (5) ----
bi.re.plot2 <- ggplot(dat, aes(x=age, y=testvalue, color=ethnicity_5cat)) + 
  geom_point(size=0.5, shape=1, alpha=0.3,color="#E69F00")+
  geom_smooth(method=lm, fill = NA, linewidth=0.5) + 
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,60), breaks = seq(20, 80, 10)) +
  scale_x_continuous("Age (years)") + 
  theme(text = element_text(size = 18))
### 3) box plot ----
bi.plot2 <- ggplot(dat, aes(x=age_group, y=testvalue, fill=ethnicity_5cat)) + 
  geom_jitter(size=0.5, shape=1, alpha=0.3, color="#E69F00") + 
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun = "mean", color = "white", 
               position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE) +  
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,60), breaks = seq(20, 80, 10)) +
  scale_x_discrete("Age groups (years)") + 
  theme(text = element_text(size = 18))
### 4) summary table ----
bi.sum1 <- dat %>% group_by(age_group, ethnicity_5cat) %>% 
  summarize(mean=mean(testvalue), sd=sd(testvalue), n=n()) %>% 
  as.data.frame() %>% 
  mutate(mean_sd=paste0(sprintf("%.1f",mean), " (", sprintf("%.1f", sd), ")")) 
### ANOVA test 
bi.within.p <- data.frame(age_group=character(), ethnicity_5cat=character(), mean_sd=character()) 
for (i in c("White", "South Asian","Black", "Other","Mixed", "Unknown")){
  tmp <- dat %>% filter(ethnicity_5cat==i)
  bi.aov1 <- aov(testvalue ~ age_group, data=tmp)
  bi.within.p[nrow(bi.within.p)+1,] <- c("p-value",i,summary(bi.aov1)[[1]][1,5])}
### ANOVA test
bi.between.p <- data.frame(age_group=character(), pvalue=character()) 
for (i in c("18-29","30-39","40-49","50-59","60-69","70-79",">=80")){
  tmp <- dat %>% filter(age_group==i)
  bi.t1 <- aov(testvalue ~ ethnicity_5cat, data=tmp)
  bi.between.p[nrow(bi.between.p)+1,] <- c(i,summary(bi.aov1)[[1]][1,5])}

### pvalue format
bi.between.p$pvalue <- ifelse(as.numeric(bi.between.p$pvalue)<0.001, 
                              "<0.001", 
                              sprintf("%.3f",as.numeric(bi.between.p$pvalue)))
bi.within.p$mean_sd <- ifelse(as.numeric(bi.within.p$mean_sd)<0.001, 
                              "<0.001", 
                              sprintf("%.3f",as.numeric(bi.within.p$mean_sd)))

### Merge output
bi.result2 <- bind_rows(bi.sum1, bi.within.p) %>%  
  pivot_wider(id_cols=age_group,names_from=ethnicity_5cat,values_from=c(mean,mean_sd,n)) %>% 
  merge(bi.between.p,all.x = TRUE) %>%  
  mutate(N=formatC(n_White+n_Black+`n_South Asian`+n_Mixed+n_Other+n_Unknown, big.mark=","),
         n_White=formatC(n_White, big.mark=","),
         n_Black=formatC(n_Black, big.mark=","),
         `n_South Asian`=formatC(`n_South Asian`, big.mark=","),
         n_Mixed=formatC(n_Mixed, big.mark=","),
         n_Other=formatC(n_Other, big.mark=","),
         n_Unknown=formatC(n_Unknown, big.mark=","),
         age_group=factor(age_group, levels= c("<18", "18-29",
                                               "30-39","40-49",
                                               "50-59","60-69",
                                               "70-79",">=80",
                                               "p-value"))) %>%
  select(age_group,starts_with("mean_sd"), starts_with("n_"),N,pvalue) %>% 
  arrange(age_group)
bi.result2 <- bi.result2 %>% replace(is.na(bi.result2)|bi.result2=="NA", " ") %>% 
  gt() %>%
  cols_move(columns=N,after=age_group) %>%
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white") %>%
  cols_label(age_group="Age Group", N="N", 
             n_White="n", mean_sd_White="Mean(SD)", 
             n_Black="n", mean_sd_Black="Mean(SD)",
             `n_South Asian`="n", `mean_sd_South Asian`="Mean(SD)",
             n_Mixed="n", mean_sd_Mixed="Mean(SD)",
             n_Other="n", mean_sd_Other="Mean(SD)",
             n_Unknown="n", mean_sd_Unknown="Mean(SD)",
             pvalue="p-value",.fn = md) %>%
  tab_spanner(label="All", columns=c(N)) %>% 
  tab_spanner(label="White", columns=c(n_White, mean_sd_White)) %>%
  tab_spanner(label="Black", columns=c(n_Black, mean_sd_Black)) %>%
  tab_spanner(label="South Asian", columns=c(`n_South Asian`, `mean_sd_South Asian`)) %>%
  tab_spanner(label="Mixed", columns=c(n_Mixed, mean_sd_Mixed)) %>%
  tab_spanner(label="Other", columns=c(n_Other, mean_sd_Other)) %>%
  tab_spanner(label="Unknown", columns=c(n_Unknown, mean_sd_Unknown)) %>%
  cols_align(align="left", columns=everything()) %>% 
  gtsave("OneDrive - University of Exeter/dissertation/writeup/anova_age_eth.png",
         expand=10, vwidth = 1500, vheight = 1000)


## Ethnicity group with sex group ----
### 1) interaction and multilinearity check ----
check1 <- lm(testvalue ~ ethnicity_5cat + gender, data=dat)
check2 <- lm(testvalue ~ ethnicity_5cat * gender, data=dat)
summary(check)
vif(check)
### 3) box plot ----
bi.plot3 <- ggplot(dat, aes(x=ethnicity_5cat, y=testvalue, fill=gender)) + 
  geom_jitter(size=0.5, shape=1, alpha=0.3, color="#E69F00") + 
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun = "mean", color = "white", 
               position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE) +  
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,60), breaks = seq(20, 80, 10)) +
  labs(x="Ethnicity", color="Sex") + 
  theme(text = element_text(size = 18))
### 4) summary table ----
bi.sum1 <- dat %>% group_by(ethnicity_5cat, gender) %>% 
  summarize(mean=mean(testvalue), sd=sd(testvalue), n=n()) %>% 
  as.data.frame() %>% 
  mutate(mean_sd=paste0(sprintf("%.1f",mean), " (", sprintf("%.1f", sd), ")")) 
### ANOVA test 
bi.within.p <- data.frame(ethnicity_5cat=character(), gender=character(), mean_sd=character()) 
for (i in c("Male", "Female")){
  tmp <- dat %>% filter(gender==i)
  bi.aov1 <- aov(testvalue ~ ethnicity_5cat, data=tmp)
  bi.within.p[nrow(bi.within.p)+1,] <- c("p-value",i,summary(bi.aov1)[[1]][1,5])}
### t-test
bi.between.p <- data.frame(ethnicity_5cat=character(), pvalue=character()) 
for (i in c("White","South Asian","Black","Other","Mixed","Unknown")){
  tmp <- dat %>% filter(ethnicity_5cat==i)
  bi.t1 <- t.test(testvalue ~ gender, data=tmp)
  bi.between.p[nrow(bi.between.p)+1,] <- c(i,bi.t1$p.value)}

### pvalue format
bi.between.p$pvalue <- ifelse(as.numeric(bi.between.p$pvalue)<0.001, 
                              "<0.001", 
                              sprintf("%.3f",as.numeric(bi.between.p$pvalue)))
bi.within.p$mean_sd <- ifelse(as.numeric(bi.within.p$mean_sd)<0.001, 
                              "<0.001", 
                              sprintf("%.3f",as.numeric(bi.within.p$mean_sd)))

### Merge output
bi.result3 <- bind_rows(bi.sum1, bi.within.p) %>% 
  pivot_wider(id_cols=ethnicity_5cat,names_from=gender,values_from=c(mean,mean_sd,n)) %>%  
  merge(bi.between.p,all.x = TRUE) %>% 
  mutate(N=formatC(n_Female+n_Male, big.mark=","),
         n_Female=formatC(n_Female, big.mark=","),
         n_Male=formatC(n_Male, big.mark=","),
         ethnicity_5cat=factor(ethnicity_5cat, levels= c("White","South Asian",
                                                         "Black","Other",
                                                         "Mixed","Unknown",
                                                         "p-value")),
         diff=round(mean_Male-mean_Female,1)) %>%
  select(-c(mean_Male, mean_Female)) %>% 
  arrange(ethnicity_5cat)
bi.result3 <- bi.result3 %>% replace(is.na(bi.result3)|bi.result3=="NA", " ") %>%
  gt() %>%
  cols_move(columns=N,after=ethnicity_5cat) %>%
  cols_move(columns=diff,after=n_Male) %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white") %>%
  cols_label(ethnicity_5cat="Ethnicity", N="N", n_Male="n", 
             mean_sd_Male="Mean(SD)", n_Female="n", 
             mean_sd_Female="Mean(SD)",diff="Difference<br>(Male-Female)",
             pvalue="p-value",.fn = md) %>%
  tab_spanner(label="All", columns=c(N)) %>% 
  tab_spanner(label="Male", columns=c(n_Male, mean_sd_Male)) %>%
  tab_spanner(label="Female", columns=c(n_Female, mean_sd_Female)) %>%
  cols_align(align="left", columns=everything()) %>% 
  gtsave("OneDrive - University of Exeter/dissertation/writeup/anova_eth_sex.png",
         expand=10)
### Full model ----
full.model <- lm(testvalue ~ age + gender + ethnicity_5cat + 
                   gender * age + gender*ethnicity_5cat + age*ethnicity_5cat, data=dat)
summary(full.model)
vif(full.model, type="predictor")
layout(matrix(1:4, byrow = T, ncol = 2))
plot(full.model, which = 1:5)

final.plot.females <- ggplot(dat, aes(x=age, y=testvalue, color=ethnicity_5cat)) + 
  geom_smooth(method=lm, fill = NA, linewidth=0.5) + 
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,60), breaks = seq(20, 80, 10)) +
  scale_x_continuous("Age (years)") +
  scale_color_discrete("Ethnicity") +
  theme(text = element_text(size = 14))
final.plot.females + facet_wrap(~gender)


# 4. Sensitivity analysis ----
sen_dat <- dat %>% filter(testvalue<48)
sen.full.model <- lm(testvalue ~ age + gender + ethnicity_5cat + 
                       gender * age + gender*ethnicity_5cat + age*ethnicity_5cat, data=sen_dat)

sen.full.model2 <- lm(testvalue ~ age + gender + ethnicity_16cat + 
                        gender * age + gender*ethnicity_16cat + age*ethnicity_16cat, data=dat)
library(stargazer)
data <- as.data.frame(cbind(a = rnorm(30), b = rnorm(30)))
fit_lm <- lm(data, formula = a ~ b)
stargazer(fit_lm, type = "html", out = "fit_lm.html")
## run above code again

# 5. Exploration analysis for menopause----
meno_dat <- dat %>% filter(40<=age & age<65) %>% 
  mutate(meno=ifelse(age<45, "40-44",
                     ifelse(age>=45 & age<50, "45-49", 
                            ifelse(age>=50 & age<55,"50-54",
                                   ifelse(age>=55 & age<60, "55-59", "60-64"))))) %>% 
  mutate(meno=factor(meno, c("40-44","45-49",
                             "50-54","55-59","60-64")))
meno.male1 <- lm(testvalue ~ age + ethnicity_5cat, data=filter(meno_dat,gender=="Male" & meno=="40-44"))
meno.male2 <- lm(testvalue ~ age + ethnicity_5cat, data=filter(meno_dat,gender=="Male" & meno=="45-49"))
meno.male3 <- lm(testvalue ~ age + ethnicity_5cat, data=filter(meno_dat,gender=="Male" & meno=="50-54"))
meno.male4 <- lm(testvalue ~ age + ethnicity_5cat, data=filter(meno_dat,gender=="Male" & meno=="55-59"))
meno.female1 <- lm(testvalue ~ age + ethnicity_5cat, data=filter(meno_dat,gender=="Female" & meno=="40-44"))
meno.female2 <- lm(testvalue ~ age + ethnicity_5cat, data=filter(meno_dat,gender=="Female" & meno=="45-49"))
meno.female3 <- lm(testvalue ~ age + ethnicity_5cat, data=filter(meno_dat,gender=="Female" & meno=="50-54"))
meno.female4 <- lm(testvalue ~ age + ethnicity_5cat, data=filter(meno_dat,gender=="Female" & meno=="55-59"))
meno.female <- lm(testvalue ~ age, data=filter(meno_dat,gender=="Female"))
meno.male <- lm(testvalue ~ age, data=filter(meno_dat,gender=="Male"))

### 1) scatter plot with regression line by gender ----
meno.plot1 <- ggplot(meno_dat, aes(x=age, y=testvalue, color=gender)) + 
  geom_smooth(method=lm, fill=NA) + 
  scale_colour_manual(values = c("#E21818","#00235B")) +
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,60), breaks = seq(20, 80, 10)) +
  labs(x="Age (years)", color="Sex") + 
  theme(text = element_text(size = 18))

### 2) box plot ----
ggplot(meno_dat, aes(x=meno, y=testvalue, fill=gender)) + 
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun = "mean", color = "white", 
               position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE) +
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,60), breaks = seq(20, 80, 10)) +
  labs(x="Age group", fill="Sex") + 
  theme(text = element_text(size = 18))

### 2) summary table ----
bi.sum1 <- meno_dat %>% group_by(meno, gender) %>% 
  summarize(mean=mean(testvalue), sd=sd(testvalue), n=n()) %>% 
  as.data.frame() %>% 
  mutate(mean_sd=paste0(sprintf("%.1f",mean), " (", sprintf("%.1f", sd), ")")) 
### ANOVA test 
bi.within.p <- data.frame(meno=character(), gender=character(), mean_sd=character()) 
for (i in c("Male", "Female")){
  tmp <- meno_dat %>% filter(gender==i)
  bi.aov1 <- aov(testvalue ~ meno, data=tmp)
  bi.within.p[nrow(bi.within.p)+1,] <- c("p-value",i,summary(bi.aov1)[[1]][1,5])}
### t-test
bi.between.p <- data.frame(meno=character(), pvalue=character()) 
for (i in c("40-44","45-49","50-54","55-59","60-64")){
  tmp <- meno_dat %>% filter(meno==i)
  bi.t1 <- t.test(testvalue ~ gender, data=tmp)
  bi.between.p[nrow(bi.between.p)+1,] <- c(i,bi.t1$p.value)}

### pvalue format
bi.between.p$pvalue <- ifelse(as.numeric(bi.between.p$pvalue)<0.001, 
                              "<0.001", 
                              sprintf("%.3f",as.numeric(bi.between.p$pvalue)))
bi.within.p$mean_sd <- ifelse(as.numeric(bi.within.p$mean_sd)<0.001, 
                              "<0.001", 
                              sprintf("%.3f",as.numeric(bi.within.p$mean_sd)))

### Merge output
meno.result <- bind_rows(bi.sum1, bi.within.p) %>%  
  pivot_wider(id_cols=meno,names_from=gender,values_from=c(mean,mean_sd,n)) %>% 
  merge(bi.between.p,all.x = TRUE) %>% 
  mutate(N=formatC(n_Female+n_Male, big.mark=","),
         n_Female=formatC(n_Female, big.mark=","),
         n_Male=formatC(n_Male, big.mark=","),
         diff=round(mean_Male-mean_Female,1),
         meno=factor(meno, c("40-44","45-49","50-54","55-59","60-64", "p-value"))) %>%
  select(-c(mean_Male, mean_Female)) %>% 
  arrange(meno)
meno.result <- meno.result %>% replace(is.na(meno.result)|meno.result=="NA", " ") %>%
  gt() %>%
  cols_move(columns=N,after=meno) %>%
  cols_move(columns=diff,after=n_Male) %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white") %>%
  cols_label(meno="Age Group", N="N", n_Male="n", 
             mean_sd_Male="Mean(SD)", n_Female="n", 
             mean_sd_Female="Mean(SD)",diff="Difference<br>(Male-Female)",
             pvalue="p-value",.fn = md) %>%
  tab_spanner(label="All", columns=c(N)) %>% 
  tab_spanner(label="Male", columns=c(n_Male, mean_sd_Male)) %>%
  tab_spanner(label="Female", columns=c(n_Female, mean_sd_Female)) %>%
  cols_align(align="left", columns=everything()) %>% 
  gtsave("OneDrive - University of Exeter/dissertation/writeup/anova_age_sex_meno.png",
         expand=10)

# 6. Exploration analysis for menarche ----
mena_dat <- all_dat %>%  filter(5<=age & age<20) %>% 
  mutate(mena=ifelse(age<10, "5-9",
                     ifelse(age>=10 & age<15, "10-14", "15-19"))) %>% 
  mutate(mena=factor(mena, c("5-9", "10-14", "15-19")))
mena.male1 <- lm(testvalue ~ age, data=filter(mena_dat,gender=="Male" & mena=="5-9"))
mena.male2 <- lm(testvalue ~ age, data=filter(mena_dat,gender=="Male" & mena=="10-14"))
mena.male3 <- lm(testvalue ~ age, data=filter(mena_dat,gender=="Male" & mena=="15-19"))
mena.female1 <- lm(testvalue ~ age, data=filter(mena_dat,gender=="Female" & mena=="5-9"))
mena.female2 <- lm(testvalue ~ age, data=filter(mena_dat,gender=="Female" & mena=="10-14"))
mena.female3 <- lm(testvalue ~ age, data=filter(mena_dat,gender=="Female" & mena=="15-19"))

### 1) demo ----
label(mena_dat$testvalue) = "HbA1c in mmol/mol"
label(mena_dat$gender) = "Sex"
label(mena_dat$age) = "Age"
label(mena_dat$ethnicity_5cat) = "Ethnicity category (5)"
table1(~ age + gender + testvalue + ethnicity_5cat | mena, mena_dat, overall=c(left="Total"),
       render.continuous=c("Median (IQR)"="Median (Q1 - Q3)", .="[Min, Max]"),
       render.categorical=render.categorical))
### 2) box plot ----
ggplot(mena_dat, aes(x=mena, y=testvalue, fill=gender)) + 
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun = "mean", color = "white", 
               position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE) +
  scale_y_continuous("HbA1c (mmol/mol)", 
                     sec.axis = sec_axis(~ (. + 23.49735) / 10.929, 
                                         name = "HbA1c (%)",
                                         breaks = seq(4,9,1)),
                     limits = c(20,60), breaks = seq(20, 80, 10)) +
  labs(x="Age group", fill="Sex") + 
  theme(text = element_text(size = 18))

### 2) summary table ----
bi.sum1 <- mena_dat %>% group_by(mena, gender) %>% 
  summarize(mean=mean(testvalue), sd=sd(testvalue), n=n()) %>% 
  as.data.frame() %>% 
  mutate(mean_sd=paste0(sprintf("%.1f",mean), " (", sprintf("%.1f", sd), ")")) 
### ANOVA test 
bi.within.p <- data.frame(mena=character(), gender=character(), mean_sd=character()) 
for (i in c("Male", "Female")){
  tmp <- mena_dat %>% filter(gender==i)
  bi.aov1 <- aov(testvalue ~ mena, data=tmp)
  bi.within.p[nrow(bi.within.p)+1,] <- c("p-value",i,summary(bi.aov1)[[1]][1,5])}
### t-test
bi.between.p <- data.frame(mena=character(), pvalue=character()) 
for (i in c("5-9", "10-14", "15-19")){
  tmp <- mena_dat %>% filter(mena==i)
  bi.t1 <- t.test(testvalue ~ gender, data=tmp)
  bi.between.p[nrow(bi.between.p)+1,] <- c(i,bi.t1$p.value)}

### pvalue format
bi.between.p$pvalue <- ifelse(as.numeric(bi.between.p$pvalue)<0.001, 
                              "<0.001", 
                              sprintf("%.3f",as.numeric(bi.between.p$pvalue)))
bi.within.p$mean_sd <- ifelse(as.numeric(bi.within.p$mean_sd)<0.001, 
                              "<0.001", 
                              sprintf("%.3f",as.numeric(bi.within.p$mean_sd)))

### Merge output
mena.result <- bind_rows(bi.sum1, bi.within.p) %>%  
  pivot_wider(id_cols=mena,names_from=gender,values_from=c(mean,mean_sd,n)) %>% 
  merge(bi.between.p,all.x = TRUE) %>% 
  mutate(N=formatC(n_Female+n_Male, big.mark=","),
         n_Female=formatC(n_Female, big.mark=","),
         n_Male=formatC(n_Male, big.mark=","),
         diff=round(mean_Male-mean_Female,1),
         mena=factor(mena, c("5-9", "10-14", "15-19", "p-value"))) %>%
  select(-c(mean_Male, mean_Female)) %>% 
  arrange(mena)
mena.result <- mena.result %>% replace(is.na(mena.result)|mena.result=="NA", " ") %>%
  gt() %>%
  cols_move(columns=N,after=mena) %>%
  cols_move(columns=diff,after=n_Male) %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white") %>%
  cols_label(mena="Age Group", N="N", n_Male="n", 
             mean_sd_Male="Mean(SD)", n_Female="n", 
             mean_sd_Female="Mean(SD)",diff="Difference<br>(Male-Female)",
             pvalue="p-value",.fn = md) %>%
  tab_spanner(label="All", columns=c(N)) %>% 
  tab_spanner(label="Male", columns=c(n_Male, mean_sd_Male)) %>%
  tab_spanner(label="Female", columns=c(n_Female, mean_sd_Female)) %>%
  cols_align(align="left", columns=everything()) %>% 
  gtsave("OneDrive - University of Exeter/dissertation/writeup/anova_age_sex_mena.png",
         expand=10)

