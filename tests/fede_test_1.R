# ------------------------------------------------------------------------------
# Installing packages 
want = c("here", "rio","tidyverse","lme4")   # list of required packages
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# Attaching the needed packages
suppressPackageStartupMessages(lapply(want, library, character.only = TRUE))
suppressWarnings(rm(have, want, to_update, tmp))
rm(list = ls())
# ------------------------------------------------------------------------------
# Sourcing needed functions 
invisible(lapply(list.files(here("functions"), full.names = TRUE), source))


# ------------------------------------------------------------------------------
# Load data from PAV2
data <- readRDS("~/Dropbox/PAV_2/data/data_rec.rds")

fm_isei <- as.formula(
  isei ~
    group_forhec + group_forhic + group_other + 
    edu_lo + edu_hi + age_std + 
    mar_separ + mar_single
)

cf_extr_isei <- l1_reg(
  fm_isei,
  min_n = 20, min_ng = 5, 
  # focal_var = c("(Intercept)", "group_forhec"),
  method = "linear",
  data_c,  group = c("region", "year"))


fm_occ <- as.formula(
  occ ~
    group_forhec + group_forhic + group_other + 
    edu_lo + edu_hi + age_std + 
    mar_separ + mar_single
)

cf_extr_occ <- l1_reg(
  fm_occ,
  min_n = 20, min_ng = 5, 
  focal_var = "(all)",
  method = "logit",
  data_c,  group = c("region", "year"))


cf_extr_occ %>% 
  arrange(region, year) %>% 
  export("~/Dropbox/PAV_2/data/coef_occ.xlsx")

cf_extr_isei %>% 
  arrange(region, year) %>% 
  export("~/Dropbox/PAV_2/data/coef_isei.xlsx")

data %>% 
  filter(region == "DE80", year %in% c(2006:2011)) %>% 
  group_by(region, year) %>% 
  summarize(
    group_forhic = sum(group_forhic),
    group_other = sum(group_other)
  )

summary(
  lm(isei ~
       group_forhec + group_forhic + group_other + 
       edu_lo + edu_hi + age_std + 
       mar_separ + mar_single,
     data = filter(data, region == "DE80", year %in% c(2009)))
)

