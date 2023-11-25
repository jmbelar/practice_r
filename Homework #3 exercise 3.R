library(titanic)
library(tidyverse)
library(forcats)
data("titanic_train")

age_survival <- titanic_train %>% select(Survived, Age) %>% 
  mutate(Survived = ifelse(Survived == 0, "Dead", "Survived")) %>% 
  group_by(Survived) %>% 
  arrange(desc(Age))

slice(age_survival, 1:5)

ggplot(age_survival, aes(x = Age)) +
  geom_histogram() +
  facet_wrap(~Survived, ncol = 1) +
  labs(y = "Number of passengers") +
  ggtitle("Age distribution of Titanic passengers")

mean_age <- age_survival %>% filter(!is.na(Age)) %>% 
  summarise(mean_age = mean(Age))

number_survived <- count(age_survival, Survived)

number_na <- age_survival %>% filter(is.na(Age)) %>% 
  count()

df <- list(mean_age, number_survived, number_na)
df %>% reduce(full_join, by = "Survived")

df2 <- full_join(mean_age, number_survived, by = "Survived") %>% 
  full_join(number_na, by = "Survived")
colnames(df2) <- c("","Age mean",
                   "Number of people",
                   "Missing age data")

t.test(df2$`Age mean`)
age_survival %>% filter(!is.na(Age)) %>% 
  group_by(Age) %>% 
  t.test(Age)

t.test(age_survival$Age)
#How to add this to new column??
factor(titanic_train$Survived, levels = c("Dead", "Survived"))

# I need help!!!!
# titanic_train %>% select(Survived, Age) %>% 
#  mutate(factor(levels = c("0", "1"),
#         labels = c("Dead", "Survived"))) %>% 
#           head()

