library(tidyverse)
library(janitor)
library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)

respondents <- read_csv("emp1.csv")
cs <- read_csv("emp2.csv")

# respondents' background
  # p1 - age 
age_distribution <- ggplot(respondents, aes(x = "", fill=age)) +
  geom_bar(width=1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title="    respondents' age")
print(age_distribution)
  # p2 - speak how many lang
lang_num <- as.character(respondents$speaks_lang_num)
lang_chart <- ggplot(respondents, aes(x = "", fill=lang_num)) +
  geom_bar(width=1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title="    distribution of the number of languages \n    spoken by the respondents")
print(lang_chart)
  # p3 - English proficiency
en_prof <- ggplot(respondents, aes(x = "", fill=en_prof)) +
  geom_bar(width=1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title="    respondents' English proficiency")
print(en_prof)


# cs

  ########### t1
cs_zh <- cs[respondents$native_lang=="zh",]
long_df <- cs_zh %>%
  select(response, ends_with("s")) %>%
  pivot_longer(cols = ends_with("s"), names_to = "category", values_to = "value")
t1 <- ggplot(long_df, aes(x = category, fill = value)) +
  geom_bar() +
  labs(x = "Categories", y = "Count", title = "Factors that affect CS for \nChinese participants in spoken dialogues") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#F0E442", "#009E73"),
                    labels = c("Definitely", "Likely", "Unlikely", "Never")) +
  theme_minimal()
print(t1)
  ############## t2
long_df <- cs_zh %>%
  select(response, ends_with("w")) %>%
  pivot_longer(cols = ends_with("w"), names_to = "category", values_to = "value")
t2 <- ggplot(long_df, aes(x = category, fill = value)) +
  geom_bar() +
  labs(x = "Categories", y = "Count", title = "Factors that affect CS for \nChinese participants in written dialogues") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#F0E442", "#009E73"),
                    labels = c("Definitely", "Likely", "Unlikely", "Never")) +
  theme_minimal()
print(t2)

  ################ t3
cs_fa <- cs[respondents$native_lang=="fa",]
long_df <- cs_fa %>%
  select(response, ends_with("s")) %>%
  pivot_longer(cols = ends_with("s"), names_to = "category", values_to = "value")
t3 <- ggplot(long_df, aes(x = category, fill = value)) +
  geom_bar() +
  labs(x = "Categories", y = "Count", title = "Factors that affect CS for \nIranian participants in spoken dialogues") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#F0E442", "#009E73"),
                    labels = c("Definitely", "Likely", "Unlikely", "Never")) +
  theme_minimal()
print(t3)
  ################## t4
long_df <- cs_fa %>%
  select(response, ends_with("w")) %>%
  pivot_longer(cols = ends_with("w"), names_to = "category", values_to = "value")
t4 <- ggplot(long_df, aes(x = category, fill = value)) +
  geom_bar() +
  labs(x = "Categories", y = "Count", title = "Factors that affect CS for \nIranian participants in written dialogues") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#F0E442", "#009E73"),
                    labels = c("Definitely", "Likely", "Unlikely", "Never")) +
  theme_minimal()
print(t4)

  ################ t5
cs_pl <- cs[respondents$native_lang=="pl",]
long_df <- cs_pl %>%
  select(response, ends_with("s")) %>%
  pivot_longer(cols = ends_with("s"), names_to = "category", values_to = "value")
t5 <- ggplot(long_df, aes(x = category, fill = value)) +
  geom_bar() +
  labs(x = "Categories", y = "Count", title = "Factors that affect CS for \nPolish participants in spoken dialogues") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#F0E442", "#009E73"),
                    labels = c("Definitely", "Likely", "Unlikely", "Never")) +
  theme_minimal()
print(t5)
  ################ t6
long_df <- cs_pl %>%
  select(response, ends_with("w")) %>%
  pivot_longer(cols = ends_with("w"), names_to = "category", values_to = "value")
t6 <- ggplot(long_df, aes(x = category, fill = value)) +
  geom_bar() +
  labs(x = "Categories", y = "Count", title = "Factors that affect CS for \nPolish participants in written dialogues") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#F0E442", "#009E73"),
                    labels = c("Definitely", "Likely", "Unlikely", "Never")) +
  theme_minimal()
print(t6)

  ################ t7
long_df <- cs %>%
  select(response, starts_with("f")) %>%
  pivot_longer(cols = starts_with("f"), names_to = "category", values_to = "value")

long_df$value <- factor(
  long_df$value,
  levels = c("definitely", "likely", "unlikely", "never"),
  ordered = TRUE
)

t7 <- ggplot(long_df, aes(x = category, fill = value)) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  labs(x = "Category", y = "Count", title="Factors that affect CS for \nall participants in both dialogues ") +
  scale_fill_manual(
    values = c(
      "definitely" = "#E69F00",
      "likely" = "#56B4E9",
      "unlikely" = "#F0E442",
      "never" = "#009E73"
    )
  ) +
  theme_minimal()
print(t7)
