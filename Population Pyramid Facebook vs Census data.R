library(tidyverse)
library(skimr)
library(modelr)
library(reticulate)
library(patchwork)
library(viridis)
library(ggpol)
library(apyramid)
library(ggcharts)
load("/Users/matthew/Desktop/M.A. Demography/6. Research Practicum/Semester Paper:Manuscript/Research Practicum R/df.Rda")

df = df %>% mutate(count = ifelse(gender == 'Male', 
                                  as.double(n_millions * -1), 
                                  as.double(n_millions)))
df1 <- df %>% filter(level == "Population")
df1
df2 <- df %>% filter(level == "National")
df2

py1 <- age_pyramid(df1,age_group=age_group,
                   split_by=gender,
                   count = n_millions,
                   show_midpoint=F)+
  labs(y="population in millions",title="US population in 2019", caption= "source:UNDESA")+
  scale_fill_manual(values = c("#7FC5DC", "#8B008B"))
py1

#Advanced Population Pyramid
ggplot() +
  geom_col(data=df1,
           mapping=aes(x = age_group,y = count, fill = gender),
           alpha=.6,
           color="grey") +
  scale_fill_manual(values = c('#4682b4','#ee7989')) + 
  coord_flip() + 
  facet_share(~ gender,
              dir = 'h',
              scales = 'free',
              reverse_num = TRUE)+
  geom_col(data=df2,
           mapping=aes(x = age_group,y = count, fill = gender),
           alpha=1,
           width=.6,
           color="black")+
  labs(title="United States Population and US Facebook user population",
       y="Number in millions",
       x="Age Groups",
       caption="\nSource: Facebook.com user data and US Census Population Estimates
       \n Dark shading/smaller box: As of Septmeber 13, 2021 \n
       lightshading larger frame: As of June 2020")+
  theme_light()
