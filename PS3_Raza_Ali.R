library(haven)
library(dplyr)

data <- readRDS("~/Spring 25/acsre_extract.rds")

summary(data)

data.frame(levels(data$educrec))
# 3(a): There are 10 categories in educrec. when educrec == 6, it means the
# has completed school til Grade 10. 


#3(b)

data <- data %>%
  mutate(
    college = case_when(
      as.numeric(educrec) %in% 9:10 ~ 1,
      TRUE ~ 0
    ),
    highschool = case_when(
      as.numeric(educrec) %in% 5:8 ~ 1,
      TRUE ~ 0
    ),
    other = case_when(
      as.numeric(educrec) %in% 1:4 ~ 1,
      TRUE ~ 0
    )
  )

reg1 <- lm(incwage ~ college + highschool, data = data)
summary(reg1)


# 3(c)

reg2 <- lm(incwage ~ educyears + female + educyears * female, data = data)
summary(reg2)

# Being Female decreases one's return of education

# 3(d)

data <- data %>%
  mutate(exper2 = exper^2)

reg3 <- lm(incwage ~ exper + exper2, data = data)
summary(reg3)

# The coefficient is -79.524
# The negative and statistically significant coefficient on the 
# quadratic term provides strong evidence that the impact of experience 
# on income is nonlinear, so quadratic term is needed. 

# 3(e)

data <- data %>%
  filter(yrsusa1 > 0) %>%
  mutate(log_yrsusa1 = log(yrsusa1))

reg4 <- lm(incwage ~ log_yrsusa1, data = data)
summary(reg4)

# coefficient on log_yrsusa1 = 8241.5
# A 1% increase in the number of years someone has lived in the U.S. 
# is associated with an increase of approximately $8241.5 in income, on average.

