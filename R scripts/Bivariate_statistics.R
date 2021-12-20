#Model Exploration
library(tidyverse)
library(MASS)
library(here)
library(janitor)
library(kableExtra)
getwd()
load("data/df2.Rda")

#Bivariate Analysis
lgbt_support_summary <- df2 %>% 
  group_by(lgbt_support) %>% 
  summarise(sum = sum(POP))
lgbt_support_summary %>% adorn_totals()

lgbtsupport_gender <- df2 %>% group_by(lgbt_support,gender) %>% 
  summarize(sum = sum(POP))
lgbtsupport_age <- df2 %>% group_by(lgbt_support,age_group) %>% 
  summarize(sum = sum(POP))
lgbtsupport_age %>% adorn_totals()
lgbtsupport_gender %>% adorn_totals()

#pivot wider to make into contingency table?
lgbtsupport_gender <- lgbtsupport_gender%>% pivot_wider(names_from=lgbt_support,values_from=sum)
lgbtsupport_age <- lgbtsupport_age%>% pivot_wider(names_from=lgbt_support,values_from=sum)

lgbtsupport_age %>% adorn_totals()
lgbtsupport_gender %>% adorn_totals()

has_rownames(lgbtsupport_age)
has_rownames(lgbtsupport_gender)
#Change to rownames for analysis 
lgbtsupport_gender <- column_to_rownames(lgbtsupport_gender,var="gender")
lgbtsupport_gender
lgbtsupport_age <- column_to_rownames(lgbtsupport_age,var="age_group")
lgbtsupport_age
lgbt_support_summary
lgbtsupport_age
lgbtsupport_gender

#Conduct Chi Squared Test
chisq.test(lgbtsupport_age,correct=F)
chisq.test(lgbtsupport_gender,correct=F)
chisq <- chisq.test(lgbtsupport_age)
chisq1 <- chisq.test(lgbtsupport_gender)
library(corrplot)
contrib <- 100*chisq$residuals^2/chisq$statistic
corrplot(contrib,is.corr=F)
contrib1 <- 100*chisq1$residuals^2/chisq1$statistic

#GLM page 394 - contigency tables
#exploring male vs. female data
male <- subset(df2,gender == "Male")
female <- subset(df2,gender == "Female")
ls(male)
table.m <-prop.table(xtabs(POP~lgbt_support+age_group,data=male),1)
table.f <-prop.table(xtabs(POP~lgbt_support+age_group,data=female),1)
#proportions of male and females by age group and LGBT support
round(table.m*100)
round(table.f*100)
chisq.test(df2$n_millions,y=df2$Gender)

