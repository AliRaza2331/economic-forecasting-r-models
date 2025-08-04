library(dplyr)
data <- readRDS("~/Spring 25/acs_2009to2018.rds")

summary(data)
head(data)

#View(data)

mean(data$LF) #LF = Labor Force

dim(data)
dim(data)[1] * mean(data$LF)

table(data$LF)

data.frame(levels(data$marst))

mean(data$marstid)
table(data$marst)
hist(data$marstid)

reg1 <- lm(school ~ LF, data = data)
summary(reg1)

reg2 <- lm(school ~ LF + age, data = data)
summary(reg2)

data <- data %>%
  mutate(
    married = case_when(
      as.numeric(marst) %in% 1:2 ~ 1, # Method 1
      T ~ 0
    ),
    divorced = case_when(
      as.numeric(marst) %in% c(3,4) ~ 1, # Method 2, vector format
      T ~ 0 
    ),
    widowed = case_when(
      as.numeric(marst) == 5 ~ 1, # Method 3, logical operator
      T ~ 0
    ),
    nomarried = case_when(
      as.numeric(marst) == 6 ~ 1,
      T ~ 0
    )
  )

table(data$marst)

sum(data$married)

reg7 <- lm(inctot ~ married + age, data = data)
summary(reg7)

reg8 <- lm(inctot ~ married + age + married*age, data = data)
summary(reg8)


data2 <- data %>%
  filter(inctot > 0, school > 0)

reg9 <- lm(log(inctot) ~ log(school), data = data2)
summary(reg9)


data$agesq <- data$age^2

reg10 <- lm(inctot ~ age + agesq, data = data)
summary(reg10)


