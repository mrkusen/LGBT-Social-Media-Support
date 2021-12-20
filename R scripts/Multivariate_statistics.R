library(tidyverse)
library(here)
library(janitor)
library(skimr)
library(stargazer)
log_gen_data <- read_csv("data/Facebook data_13 Sept 2021_log_gen.csv",
                     col_types = list(level = col_factor(),
                                      lgbt_support = col_factor(),
                                      gender = col_factor(),
                                      age_group = col_factor(),
                                      support = col_double(),
                                      no_evidence = col_double(),
                                      country = col_factor(),
                                      date_extracted = col_date(),
                                      source = col_character()))
skim(log_gen_data)
save(log_gen_data,file="data/log_gen_data.Rda")

options(scipen=999)
#Create new age groups
#15-24 : Gen Z
#25-34 : Millenials
#36-44 : Gen X - group 1
#45-54 : Gen X - Group 2
#55-64 : Baby Boomers

levels(log_gen_data$age_group)
class(log_gen_data$age_group)
log_gen_data$age_group <- fct_recode(log_gen_data$age_group,
                            gen_x_young = "gen_x_1",
                            gen_x_older ="gen_x_2")

ls(log_gen_data)
logita <- glm(cbind(support,no_evidence)~gender+age_group+gender*age_group,
              data=log_gen_data,
              family=binomial(link="logit"))
summary(logita)

stargazer(logita, type = "text")
stargazer(logita,
          type="html", out="FB_logit1.doc",
          digits=3)

#odds ratio
stargazer(logita, apply.coef = exp, type = "html",
          out="FB_logit_OR.doc")


library(oddsratio)
or_logita <- or_glm(log_gen_data,logita)
or_logita <- or_logita %>% mutate(change_in_odds = (or_logita$oddsratio-1)*100)
or_logita



#create new graph
df <- log_gen_data %>% group_by(gender,age_group) %>% 
  summarize(total = sum(support,no_evidence))
df2 <- df %>% group_by(age_group) %>% summarize(sum(total))
df2[2]
ls(log_gen_data)

df1 <- cbind(df,log_gen_data)
df1 <- df1 %>% mutate(RR = support/total)
df1 <- df1 %>% rename(gender_1 = gender...1,
               gender_2 = gender...6,
               age_group_1 = age_group...2,
               age_group_2 = age_group...7)
ls(df1)
graph1 <- ggplot(df1,aes(y=RR,x=age_group_1,color=gender_1))+
  geom_line(aes(group=gender_1))+
  geom_point()+
  labs(y="Proportion of LGBT supportive users",
       x="Age Groups",
       title="Proportion of Facebook and Instagram Users
       demonstrating support for LGBTIQ+ community",
       caption="Source: Facebook marketing data - September 2021",
       color="Gender")
graph1  


# New Bivariate tables for age
age_summary_gen <- log_gen_data %>% group_by(age_group) %>% 
  summarize(sum_support = sum(support),
            sum_no_support = sum(no_evidence))
age_summary_gen <- column_to_rownames(age_summary_gen,var="age_group")
age_summary_gen
#Conduct Chi Squared Test - age
chisq.test(age_summary_gen,correct=F)

