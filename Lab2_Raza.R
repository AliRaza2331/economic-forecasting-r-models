library(dplyr)

getwd()
#setwd() in Files, using "more" drop down option button on the right side
setwd("~/Spring 25/Econ 300")

getwd()
data <- read.csv("card_95_sample.csv")
summary(data)

reg1 <- lm(ed76 ~ momed, data = data)
summary(reg1)

reg1$fitted.values

reg1$residuals

reg2 <- lm(wage76 ~ ed76, data = data)
summary(reg2)

reg3 <- lm(wage76 ~ ed76 + age76, data = data)
summary(reg3)


data2 <- data %>% 
  select(wage76, ed76, age76) %>%
  na.omit()

summary(data2)

fwl1 <- lm(wage76 ~ ed76, data = data2)
fwl1.resid <- fwl1$residuals

fwl2 <- lm(age76 ~ ed76, data = data2)
fwl2.resid <- fwl2$residuals

fwl3 <- lm(fwl1.resid ~ fwl2.resid)
fwl3.resid <- fwl3$residuals

summary(fwl3)

data$highered <- data$ed76 - 12

reg4 <- lm(wage76 ~ ed76 + highered, data = data)
summary(reg4)

