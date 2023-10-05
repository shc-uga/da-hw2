library(stargazer)
library(ggplot2)
library(naniar)

setwd("./")
df <- read.csv("qog_bas_ts_jan23.csv")

# FROM HW1
# convert democracy to factor
df$bmr_dem <- as.factor(df$bmr_dem)
# create new subset
df_subset <- df[, c('ti_cpi', 'wdi_acel', 'bmr_dem')]
# rename variables
colnames(df_subset) <- c('Corruption', 'Development', 'Democracy')
df_subset <- na.omit(df_subset)

# Q1
model1 <- lm(Development ~ Corruption, data = df_subset)
summary(model1)
stargazer::stargazer(model1, type = "text")

# Q2
cat("Assumption Model: Development = \\beta_0 + \\beta_1 \\times Corruption + \\epsilon\n")

# Q3
fit <- lm(Development ~ Corruption, data = df_subset)
plot(fit, which = 1) # residual vs fitted
plot(fit, which = 2) # q-q
plot(fit, which = 3) # scale-location
plot(fit, which = 4) # cook

# Q5
fit_dem <- lm(Development ~ Corruption + Democracy, data = df_subset)
summary(fit_dem)
stargazer::stargazer(fit_dem, type = "text")

