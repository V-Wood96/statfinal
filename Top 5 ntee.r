
library(dplyr)
library(tidyverse)
library(caret)
library(tree)
library(randomForest)
library(class)
library(glmnet)
library(forcats)
library(cluster)
library(factoextra)
library(ggplot2)


tax <- read.csv("/Users/veronicawood/School/GM Fall 2025 /STAT 515 - Applied Statistics and Visualization for Analytics/Final project/tax_2024_12.9.csv")


str(tax.2024)

tax %>% 
  count(NTEE_MAJOR)

tax <- tax %>%
  mutate(
    Income_To_Revenue = ifelse(
      REVENUE_AMT > 0,
      INCOME_AMT / REVENUE_AMT,
      NA))

tax <- tax %>%
  mutate(
    Income_To_Revenue_Pct = Income_To_Revenue * 100)


summary(tax$Income_To_Revenue_Pct)

tax <- tax %>% 
  filter(!is.na(Income_To_Revenue_Pct))

head(tax %>% select(INCOME_AMT, REVENUE_AMT, Income_To_Revenue_Pct))

summary(tax$Income_To_Revenue_Pct)

tax <- tax %>%
  mutate(
    Revenue_To_Income = ifelse(
      INCOME_AMT > 0,
      REVENUE_AMT / INCOME_AMT,
      NA))

tax <- tax %>%
  mutate(
    Revenue_To_Income_Pct = Revenue_To_Income * 100)

tax <- tax %>% 
  filter(
    !is.na(INCOME_AMT),
    !is.na(REVENUE_AMT),
    !is.na(ASSET_AMT),
    !is.na(NTEE_MAJOR))
###_______

tax2 <- tax %>% 
  filter(!is.na(ASSET_AMT),
         !is.na(INCOME_AMT),
         !is.na(REVENUE_AMT),
         !is.na(Revenue_To_Income))

tax2 <- tax2 %>%
  mutate(
    NTEE_B = ifelse(NTEE_MAJOR == "B", 1, 0),
    NTEE_P = ifelse(NTEE_MAJOR == "P", 1, 0),
    NTEE_A = ifelse(NTEE_MAJOR == "A", 1, 0),
    NTEE_N = ifelse(NTEE_MAJOR == "N", 1, 0),
    NTEE_X = ifelse(NTEE_MAJOR == "X", 1, 0))


formula5 <- NTEE ~ ASSET_AMT + INCOME_AMT + REVENUE_AMT + Revenue_To_Income

# tax2 <- tax2 %>%
  mutate(
    ASSET_s = scale(ASSET_AMT),
    INCOME_s = scale(INCOME_AMT),
    REV_s = scale(REVENUE_AMT),
    RtoI_s = scale(Revenue_To_Income))


# Education (B)
model_B <- glm(
  NTEE_B ~ ASSET_AMT + INCOME_AMT + REVENUE_AMT + Revenue_To_Income,
  data = tax2,
  family = binomial)

# Human Services (P)
model_P <- glm(
  NTEE_P ~ ASSET_AMT + INCOME_AMT + REVENUE_AMT + Revenue_To_Income,
  data = tax2,
  family = binomial)

# Arts (A)
model_A <- glm(
  NTEE_A ~ ASSET_AMT + INCOME_AMT + REVENUE_AMT + Revenue_To_Income,
  data = tax2,
  family = binomial)

# Youth Development (N)
model_N <- glm(
  NTEE_N ~ ASSET_AMT + INCOME_AMT + REVENUE_AMT + Revenue_To_Income,
  data = tax2,
  family = binomial)

# Religion Support (X)
model_X <- glm(
  NTEE_X ~ ASSET_AMT + INCOME_AMT + REVENUE_AMT + Revenue_To_Income,
  data = tax2,
  family = binomial)

results <- list(
  Education_B      = tidy(model_B, exponentiate = TRUE, conf.int = FALSE),
  HumanServices_P  = tidy(model_P, exponentiate = TRUE, conf.int = FALSE),
  Arts_A           = tidy(model_A, exponentiate = TRUE, conf.int = FALSE),
  Youth_N          = tidy(model_N, exponentiate = TRUE, conf.int = FALSE),
  ReligionSupport_X= tidy(model_X, exponentiate = TRUE, conf.int = FALSE))

wald_cis <- lapply(
  list(model_B, model_P, model_A, model_N, model_X),
  function(m) exp(confint.default(m)))

for (i in seq_along(results)) {
  results[[i]]$conf.low  <- wald_cis[[i]][, 1]
  results[[i]]$conf.high <- wald_cis[[i]][, 2]
}

results$Education_B

library(broom)

results <- list(
  Education_B = tidy(model_B, exponentiate = TRUE, conf.int = TRUE),
  HumanServices_P = tidy(model_P, exponentiate = TRUE, conf.int = TRUE),
  Arts_A = tidy(model_A, exponentiate = TRUE, conf.int = TRUE),
  Youth_N = tidy(model_N, exponentiate = TRUE, conf.int = TRUE),
  ReligionSupport_X = tidy(model_X, exponentiate = TRUE, conf.int = TRUE))

results
