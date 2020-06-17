library(tidyverse)
library(tidylog)

source("blog_circular_funs.R")

# dir <- "N:/Project/51003_COVID-19_Phone_Survey/Restricted/DC1/Data/Output/MRP/"
dir <- "N:/Transfer/cdelannoy/covid/"
wave <- 2

ken_dat <- read_csv(file.path(dir, "MRP", "Kenya2_blog_MRP_overall.csv")) %>% mutate(country = "Kenya")
ngr_dat <- read_csv(file.path(dir, "MRP", "Nigeria2_blog_MRP_overall.csv")) %>% mutate(country = "Nigeria")
sa_dat <- read_csv(file.path(dir, "MRP", "SouthAfrica2_blog_MRP_overall.csv")) %>% mutate(country = "South Africa")
rda_dat <- read_csv(file.path(dir, "MRP", "Rwanda2_blog_MRP_overall.csv")) %>% mutate(country = "Rwanda")
uga_dat <- read_csv(file.path(dir, "MRP", "Uganda2_blog_MRP_overall.csv")) %>% mutate(country = "Uganda")

mrp_dat <- bind_rows(ken_dat, ngr_dat, sa_dat, rda_dat, uga_dat)

# names of variables and their associated section
analysis_vars <- readxl::read_xlsx(file.path(dir, "vars_for_analysis.xlsx"), sheet = 1) %>% 
  rename("section" = "group")

all_dat <- left_join(mrp_dat, analysis_vars %>% select(Shortname, `Indicator title`, section, keep, shade), by = c("outcome" = "Shortname")) %>% 
  filter(keep == 1) %>% 
  mutate(sectiona = str_extract(section, "[0-9]"),
         sectionb = str_extract(section, "[a-z]"),
         sectionb = if_else(is.na(sectionb), "a", sectionb)) %>% 
  arrange(country, sectiona, sectionb) %>% 
  mutate_at(vars(country, sectionb, shade), factor) %>% 
  select(country, outcome = `Indicator title`, estimate, sd, sectiona, sectionb, shade)
  
create_dial(all_dat, "1")
create_dial(all_dat, "2")
create_dial(all_dat, "3")

create_dial2(all_dat, "1")
create_dial2(all_dat, "2")
create_dial2(all_dat, "3")

create_dial3(all_dat, "1")
create_dial3(all_dat, "2")
create_dial3(all_dat, "3")
