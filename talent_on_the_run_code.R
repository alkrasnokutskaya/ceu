install.packages("tidyverse")
library(tidyverse)
install.packages("car")
library(car)
install.packages("plm")
library(plm)
install.packages("stargazer")
library(stargazer)
install.packages("sandwich")
library(sandwich)

###############
# Data
df <- read_csv("talent_on_the_run_data.csv")
# Inversing the variable
df <- df %>%
  mutate(mig_int_mil = -net_mig_mil)
# write.csv(df, "talent_on_the_run_data.csv", row.names = FALSE)

###############
# Multicollinearity and other issues
# VIF
vif(lm(res_app_mil ~ net_mig_mil + edu_exp_ter + fdi_net + gdppcppp + internet_mil + polstab + rd_exp + ruleoflaw + researchers + trade + pop_t, data = df))

vif(lm(res_app_mil ~ mig_int_mil_t3 + edu_exp_ter + fdi_net + gdppcppp + internet_mil + polstab + rd_exp + ruleoflaw + researchers + trade + pop_t,
       data = pdata))

# Checking unique values
cv <- sapply(df[, c("net_mig", "edu_exp_ter", "fdi_net", "gdppcppp", "internet", "polstab", "rd_exp", "trade", "pop_t")],
             function(x) length(unique(x)))
print(cv)

# Checking correlations
cor_matrix <- cor(clean %>% select_if(is.numeric), use = "pairwise.complete.obs")
print(cor_matrix)


###############################################################################
# OLS LAGGED
clean <- df %>%
  dplyr::select(cnt, year, res_app_mil, mig_int_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  group_by(cnt) %>%
  filter(n_distinct(year) >= 5) %>%
  ungroup()
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)
pdata <- pdata %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))
lm11l <- lm(res_app_mil ~ mig_int_mil_t3 + log(gdppcppp) + internet_mil + trade + log(pop_t) + enroll_ter_mil + fdi_net + polstab + rd_exp + researchers,
            data = pdata)
summary(lm11l)

lm21l <- lm(res_app_mil ~ mig_int_mil_t3 + log(gdppcppp) + internet_mil + trade + log(pop_t) + enroll_ter_mil + fdi_net + ruleoflaw + rd_exp + researchers,
           data = pdata)
summary(lm21l)

lm31l <- lm(res_app_mil ~ mig_int_mil_t3 + log(gdppcppp) + internet_mil + trade + log(pop_t) + enroll_ter_mil + fdi_net + ruleoflaw + polstab + rd_exp + researchers,
           data = pdata)
summary(lm31l)


clean <- df %>%
  dplyr::select(cnt, year, res_app_mil, mig_int_mil, edu_at_ter_mil, fdi_net, gdppcppp, internet_mil, polstab, ruleoflaw, rd_exp, researchers, trade, pop_t) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.))))
pdata <- pdata.frame(clean, index = c("cnt", "year"))
#pdata$net_mig_mil_t1 <- lag(pdata$net_mig_mil, k = 1)
#pdata$net_mig_mil_t2 <- lag(pdata$net_mig_mil, k = 2)
#pdata$net_mig_mil_t3 <- lag(pdata$net_mig_mil, k = 3)
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)
pdata <- pdata %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))

lm41l <- lm(res_app_mil ~ mig_int_mil_t3 + log(gdppcppp) + internet_mil + trade + log(pop_t) + edu_at_ter_mil + fdi_net + polstab + rd_exp + researchers,
           data = pdata)
summary(lm41l)

lm51l <- lm(res_app_mil ~ mig_int_mil_t3 + log(gdppcppp) + internet_mil + trade + log(pop_t) + edu_at_ter_mil + fdi_net + ruleoflaw + rd_exp + researchers,
            data = pdata)
summary(lm51l)

lm61l <- lm(res_app_mil ~ mig_int_mil_t3 + log(gdppcppp) + internet_mil + trade + log(pop_t) + edu_at_ter_mil + fdi_net + polstab + ruleoflaw + rd_exp + researchers,
           data = pdata)
summary(lm61l)

stargazer(lm41l, lm51l, lm61l,
          type = "latex", 
          title = "Panel Regression Summary (3-Year Lag)", 
          column.labels = c("polstab", "ruleoflaw", "both", "edu at", "edu at both"),
          model.names = FALSE)

# ERRORS
library(lmtest)
library(sandwich)
robust_se4 <- sqrt(diag(vcovHC(lm11l, type = "HC1")))
robust_se5 <- sqrt(diag(vcovHC(lm21l, type = "HC1")))
robust_se6 <- sqrt(diag(vcovHC(lm31l, type = "HC1")))
stargazer(lm11l, lm21l, lm31l,
          se = list(robust_se4, robust_se5, robust_se6),
          type = "latex",
          title = "OLS Panel Regression Summary with Robust Standard Errors",
          dep.var.labels = "Resident Patent Applications per Million",
          column.labels = c("Model 1", "Model 2", "Model 3"),
          align = TRUE,
          no.space = TRUE)


# VIF
vif(lm31l)
vif_values <- vif(lm31l)
vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = round(vif_values, 2)
)
library(kableExtra)
kable(vif_table, format = "latex", booktabs = TRUE, caption = "Variance Inflation Factors (VIF)",
      label = "vif") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

######################################################################
# FE
clean <- df %>%
  dplyr::select(cnt, year, res_app_mil, mig_int_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  group_by(cnt) %>%
  filter(n_distinct(year) >= 5) %>%
  ungroup()
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)
pdata <- pdata %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))

library(fixest)
model_fe_country <- feols(
  res_app_mil ~ mig_int_mil_t3 + gdppcppp + internet_mil + trade + pop_t + 
    enroll_ter_mil + fdi_net + ruleoflaw + polstab + rd_exp + researchers | cnt,
  data = pdata,
  vcov = "hetero"
)
summary(model_fe_country)

etable(model_fe_country, se = "hetero", tex = TRUE, dict = c(
  mig_int_mil_t3 = "Emigration (3-year lag)",
  gdppcppp = "GDPpc (PPP)",
  internet_mil = "Internet",
  trade = "Trade (% GDP)",
  pop_t = "Population",
  enroll_ter_mil = "Tertiary Enrollment",
  fdi_net = "FDI",
  ruleoflaw = "Rule of Law",
  polstab = "Political Stability",
  rd_exp = "R&D Expenditure",
  researchers = "Researchers"
))

#############
# LINECHART - REGIONS

clean <- df %>%
  dplyr::select(cnt, region, year, res_app_mil, mig_int_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  group_by(cnt) %>%
  filter(n_distinct(year) >= 5) %>%
  ungroup()
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)


library(dplyr)
library(ggplot2)

avg_trend <- pdata %>%
  group_by(region, year) %>%
  summarise(avg_res_app_mil = mean(res_app_mil, na.rm = TRUE)) %>%
  ungroup()

avg_trend$year <- as.numeric(as.character(avg_trend$year))
ggplot(avg_trend, aes(x = year, y = avg_res_app_mil, color = region)) +
  geom_line(size = 1) +
  labs(x = "Year",
       y = "Average Patent Applications (per million)",
       color = "Region") +
  theme_minimal()

# LINECHART - INCOMES
clean <- df %>%
  dplyr::select(cnt, income, year, res_app_mil, mig_int_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  group_by(cnt) %>%
  filter(n_distinct(year) >= 5) %>%
  ungroup()
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)

avg_trend <- pdata %>%
  group_by(income, year) %>%
  summarise(avg_res_app_mil = mean(res_app_mil, na.rm = TRUE)) %>%
  ungroup()

avg_trend$year <- as.numeric(as.character(avg_trend$year))
ggplot(avg_trend, aes(x = year, y = avg_res_app_mil, color = income)) +
  geom_line(size = 1) +
  labs(x = "Year",
       y = "Average Patent Applications (per million)",
       color = "Region") +
  theme_minimal()


# Europe
clean <- df %>%
  dplyr::select(cnt, region, year, res_app_mil, mig_int_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  group_by(cnt) %>%
  filter(n_distinct(year) >= 5) %>%
  ungroup()
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)
pdata <- pdata %>% filter(region == "Europe and Central Asia")
pdata <- pdata %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))

library(fixest)
fe_reg_eu <- feols(
  res_app_mil ~ mig_int_mil_t3 + gdppcppp + internet_mil + trade + pop_t + 
    enroll_ter_mil + fdi_net + ruleoflaw + polstab + rd_exp + researchers | cnt,
  data = pdata,
  vcov = "hetero"
)
summary(fe_reg_eu)

# East Asia
clean <- df %>%
  dplyr::select(cnt, region, year, res_app_mil, mig_int_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  group_by(cnt) %>%
  filter(n_distinct(year) >= 5) %>%
  ungroup()
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)
pdata <- pdata %>% filter(region == "East Asia and Pacific")
pdata <- pdata %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))

library(fixest)
fe_reg_as <- feols(
  res_app_mil ~ mig_int_mil_t3 + gdppcppp + internet_mil + trade + pop_t + 
    enroll_ter_mil + fdi_net + ruleoflaw + polstab + rd_exp + researchers | cnt,
  data = pdata,
  vcov = "hetero"
)
summary(fe_reg_as)

etable(fe_reg_eu, fe_reg_as, se = "hetero", dict = c(
  mig_int_mil_t3 = "Emigration (3-year lag)",
  gdppcppp = "GDPpc (PPP)",
  internet_mil = "Internet",
  trade = "Trade (% GDP)",
  pop_t = "Population",
  enroll_ter_mil = "Tertiary Enrollment",
  fdi_net = "FDI",
  ruleoflaw = "Rule of Law",
  polstab = "Political Stability",
  rd_exp = "R&D Expenditure",
  researchers = "Researchers"),
  tex = TRUE)

# INCOME UPPER
clean <- df %>%
  dplyr::select(cnt, income, year, res_app_mil, mig_int_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  group_by(cnt) %>%
  filter(n_distinct(year) >= 5) %>%
  ungroup()
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)
pdata <- pdata %>% filter(income == "High income")
pdata <- pdata %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))

library(fixest)
fe_in_h <- feols(
  res_app_mil ~ mig_int_mil_t3 + gdppcppp + internet_mil + trade + pop_t + 
    enroll_ter_mil + fdi_net + ruleoflaw + polstab + rd_exp + researchers | cnt,
  data = pdata,
  vcov = "hetero"
)
summary(fe_in_h)

# UPPER MIDDLE
clean <- df %>%
  dplyr::select(cnt, income, year, res_app_mil, mig_int_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  group_by(cnt) %>%
  filter(n_distinct(year) >= 5) %>%
  ungroup()
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)
pdata <- pdata %>% filter(income == "Upper middle income")
pdata <- pdata %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))

library(fixest)
fe_in_um <- feols(
  res_app_mil ~ mig_int_mil_t3 + gdppcppp + internet_mil + trade + pop_t + 
    enroll_ter_mil + fdi_net + ruleoflaw + polstab + rd_exp + researchers | cnt,
  data = pdata,
  vcov = "hetero"
)
summary(fe_in_um)

clean <- df %>%
  dplyr::select(cnt, income, year, res_app_mil, mig_int_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  group_by(cnt) %>%
  filter(n_distinct(year) >= 5) %>%
  ungroup()
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)
pdata <- pdata %>% filter(income == "Lower middle income")
pdata <- pdata %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))

library(fixest)
fe_in_lm <- feols(
  res_app_mil ~ mig_int_mil_t3 + gdppcppp + internet_mil + trade + pop_t + 
    enroll_ter_mil + fdi_net + ruleoflaw + polstab + rd_exp + researchers | cnt,
  data = pdata,
  vcov = "hetero"
)
summary(fe_in_lm)

etable(fe_in_h, fe_in_um, fe_in_lm, se = "hetero", dict = c(
  mig_int_mil_t3 = "Emigration (3-year lag)",
  gdppcppp = "GDPpc, PPP (log)",
  internet_mil = "Internet",
  trade = "Trade",
  pop_t = "Population (log)",
  enroll_ter_mil = "Enrollment",
  fdi_net = "FDI",
  ruleoflaw = "Rule of Law",
  polstab = "Political Stability",
  rd_exp = "R&D Expenditure",
  researchers = "Researchers"),
  tex = TRUE)

#############
# DESCRIPTIVE STATS
clean <- df %>%
  select(cnt, year, res_app_mil, net_mig_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, edu_at_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  drop_na() %>%
  mutate(across(where(is.factor), ~ as.numeric(as.character(.))))
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$net_mig_mil_t1 <- lag(pdata$net_mig_mil, k = 1)
pdata$net_mig_mil_t2 <- lag(pdata$net_mig_mil, k = 2)
pdata$net_mig_mil_t3 <- lag(pdata$net_mig_mil, k = 3)
pdata <- pdata %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))
stargazer(pdata, type = "latex", title = "Descriptive Statistics")

# INCOME AND REGION
library(knitr)
library(kableExtra)

clean <- df %>%
  dplyr::select(cnt, year, region, income, res_app_mil, mig_int_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.))))
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)
region_counts <- pdata %>% count(region)
income_counts <- pdata %>% count(income)
region_counts$Type <- "Region"
income_counts$Type <- "Income"
names(region_counts)[2] <- "Observations"
names(income_counts)[2] <- "Observations"

combined <- bind_rows(region_counts, income_counts) %>%
  dplyr::select(Type, region = region, Observations) %>%
  mutate(region = ifelse(Type=="Income", as.character(region), as.character(region)))

kable(combined, format = "latex", booktabs = TRUE, caption = "Observations by Region and Income") %>%
  kable_styling(latex_options = c("hold_position"))

############
# IVs - Correlation
#install.packages("xtable")
library(xtable)
cor_data <- df[, c("res_app_mil", "mig_int_mil", "diaspora_mil", "displ_mil")]
cor_matrix <- cor(cor_data, use = "complete.obs")
latex_cor <- xtable(cor_matrix, 
                    caption = "Correlation Matrix of Key Variables", 
                    label = "tab:correlation")
print(latex_cor, type = "latex", comment = FALSE)

# IV - diaspora
# install.packages("lfe")
library(lfe)

clean <- df %>%
  dplyr::select(cnt, year, res_app_mil, mig_int_mil, diaspora_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  group_by(cnt) %>%
  filter(n_distinct(year) >= 5) %>%
  ungroup()
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)
pdata <- pdata %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))

iv_dia <- felm(res_app_mil ~ enroll_ter_mil + fdi_net + 
                 gdppcppp + internet_mil + polstab + rd_exp + 
                 ruleoflaw + researchers + trade + pop_t  | 0 | (mig_int_mil_t3 ~ diaspora_mil), data=pdata)
summary(iv_dia)

scaled <- pdata %>%
  mutate(across(where(is.numeric) & !c(year), ~ scale(.)[,1]))

fe_iv_dia <- plm(res_app_mil ~ mig_int_mil_t3 + enroll_ter_mil + fdi_net + log(gdppcppp) + 
                     internet_mil + polstab + ruleoflaw + rd_exp + researchers + 
                     trade + log(pop_t) | 
                     diaspora_mil + enroll_ter_mil + fdi_net + log(gdppcppp) + 
                     internet_mil + polstab + ruleoflaw + rd_exp + researchers + 
                     trade + log(pop_t), 
                   data = scaled, 
                   model = "within",
                   effect = "individual",
                   inst.method = "bvk")
summary(fe_iv_dia)

# IV - displacement
clean <- df %>%
  dplyr::select(cnt, year, res_app_mil, mig_int_mil, displ_mil, gdppcppp, internet_mil, trade, pop_t, enroll_ter_mil, fdi_net, polstab, ruleoflaw, rd_exp, researchers) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  group_by(cnt) %>%
  filter(n_distinct(year) >= 5) %>%
  ungroup()
pdata <- pdata.frame(clean, index = c("cnt", "year"))
pdata$mig_int_mil_t3 <- lag(pdata$mig_int_mil, k = 3)
pdata <- na.omit(pdata)
pdata <- pdata %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))

iv_displ <- felm(res_app_mil ~ enroll_ter_mil + fdi_net + 
                   gdppcppp + internet_mil + polstab + rd_exp + 
                   ruleoflaw + researchers + trade + pop_t  | 0 | (mig_int_mil_t3 ~ displ_mil), data=pdata)
summary(iv_displ)

scaled <- pdata %>%
  mutate(across(where(is.numeric) & !c(year), ~ scale(.)[,1]))
fe_iv_displ <- plm(res_app_mil ~ mig_int_mil_t3 + enroll_ter_mil + fdi_net + log(gdppcppp) + 
                     internet_mil + polstab + ruleoflaw + rd_exp + researchers + 
                     trade + log(pop_t) | 
                     displ_mil + enroll_ter_mil + fdi_net + log(gdppcppp) + 
                     internet_mil + polstab + ruleoflaw + rd_exp + researchers + 
                     trade + log(pop_t), 
                   data = scaled, 
                   model = "within",
                   effect = "individual",
                   inst.method = "bvk")
summary(fe_iv_displ)

# ============================================================================
# LaTeX tables - Models with IVs
stargazer(iv_dia, fe_iv_dia, iv_displ, fe_iv_displ,
          type = "latex", 
          title = "Two Stage Regression Summary with Instrumental Variables", 
          column.labels = c("Diaspora", "Diaspora Fixed Effects", "Disasters", "Disasters Fixed Effects"),
          model.names = FALSE)