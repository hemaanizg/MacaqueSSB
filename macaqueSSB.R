# Comparing Female with SSB male relatives to Female without SSB relatives
library(readxl)
library(dplyr)

behavior_data <- read_excel("/Users/hz/Desktop/behaviour.xlsx")
pedigree_data <- read_excel("/Users/hz/Desktop/pedigree.xlsx")

# Keep only SSB, remove DSB and NA 
behavior_ssb <- behavior_data %>% filter(Direction == "SSB")

# Select and group up the ID of actor and recipient
unique_actors <- unique(behavior_ssb$`Actor ID`)
unique_recipients <- unique(behavior_ssb$`Recipient ID`)
unique_individuals <- unique(c(unique_actors, unique_recipients))

# Identify their mother
specific_mothers <- pedigree_data %>%
  filter(ID %in% unique_individuals) %>%
  pull(MOTHER) %>%
  unique()

# Count the childbirth of these specific female
children_count_specific_mothers <- pedigree_data %>%
  filter(MOTHER %in% specific_mothers) %>%
  group_by(MOTHER) %>%
  summarise(Children = n(), .groups = 'drop')

# Count the childbirth of all female
children_count_all_mothers <- pedigree_data %>%
  group_by(MOTHER) %>%
  summarise(Children = n(), .groups = 'drop')

# Count teh childbirth of the rest of the female
children_count_other_mothers <- pedigree_data %>%
  filter(!MOTHER %in% specific_mothers) %>%
  group_by(MOTHER) %>%
  summarise(Children = n(), .groups = 'drop')

# Summarize the Data
summary_specific <- summary(children_count_specific_mothers$Children)

summary_other <- summary(children_count_other_mothers$Children)


# Calculate effect sizes
r2 <- test_result2$statistic / (nrow(children_count_specific_mothers) * nrow(children_count_other_mothers))

# Mann-Whitney U test
test_result2 <- wilcox.test(children_count_specific_mothers$Children, children_count_other_mothers$Children, alternative = "two.sided")

print(summary_specific)
print(summary_other)
print(test_result2)
print(r2)
#Compare within the SSB group divided by SSB times 

# Count the times of each participant has (actor+recipient)
actor_counts <- behavior_ssb %>%
  group_by(`Actor ID`) %>%
  summarise(SSB_Count = n())

recipient_counts <- behavior_ssb %>%
  group_by(`Recipient ID`) %>%
  summarise(SSB_Count = n())

participant_counts <- bind_rows(
  actor_counts %>% rename(ID = `Actor ID`),
  recipient_counts %>% rename(ID = `Recipient ID`)
) %>%
  group_by(ID) %>%
  summarise(SSB_Count = sum(SSB_Count))

# Identify the mother of the participants
participant_mothers <- pedigree_data %>%
  filter(ID %in% participant_counts$ID) %>%
  select(ID, MOTHER)

# Combine those data
participant_counts <- participant_counts %>%
  left_join(participant_mothers, by = "ID")

# Remove the repeats
specific_mothers <- unique(participant_counts$MOTHER)

# Count the child birth of the specific females
children_count_specific_mothers <- pedigree_data %>%
  filter(MOTHER %in% specific_mothers) %>%
  group_by(MOTHER) %>%
  summarise(Children = n(), .groups = 'drop')

# Combine mother's birth counts and participants SSB times
mother_ssb_counts <- participant_counts %>%
  group_by(MOTHER) %>%
  summarise(SSB_Count = sum(SSB_Count)) %>%
  left_join(children_count_specific_mothers, by = "MOTHER")

# Divide the group into 4
mother_ssb_counts <- mother_ssb_counts %>%
  mutate(Group = ntile(SSB_Count, 4))

# Run ANOVA and Kruskal-Wallis test
anova_result <- aov(Children ~ as.factor(Group), data = mother_ssb_counts)
kruskal_result <- kruskal.test(Children ~ as.factor(Group), data = mother_ssb_counts)

print(summary(anova_result))
print(kruskal_result)

library(dplyr)
library(readxl)

# Load your data
behavior_data <- read_excel("/Users/hz/Desktop/behaviour.xlsx")
pedigree_data <- read_excel("/Users/hz/Desktop/pedigree.xlsx")

# Assuming SSB counts and mother's children counts are already prepared
# Calculate total SSB counts for each participant
total_ssb_counts <- behavior_ssb %>%
  group_by(ID = `Actor ID`) %>%
  summarise(SSB_Count = n(), .groups = 'drop') %>%
  bind_rows(behavior_ssb %>%
              group_by(ID = `Recipient ID`) %>%
              summarise(SSB_Count = n(), .groups = 'drop')) %>%
  group_by(ID) %>%
  summarise(SSB_Count = sum(SSB_Count), .groups = 'drop')

# Get the total children count for each mother
total_children_counts <- pedigree_data %>%
  group_by(MOTHER) %>%
  summarise(Children = n(), .groups = 'drop')

# Combine SSB counts with children counts by mother
combined_data <- total_ssb_counts %>%
  left_join(pedigree_data %>% select(ID, MOTHER), by = "ID") %>%
  group_by(MOTHER) %>%
  summarise(Total_SSB = sum(SSB_Count), .groups = 'drop') %>%
  left_join(total_children_counts, by = "MOTHER")

# Perform Spearman's rank correlation test
correlation_test <- cor.test(combined_data$Total_SSB, combined_data$Children, method = "spearman")

# Print the results
print(correlation_test)

library(dplyr)
library(readxl)
library(Hmisc)
# Assuming the rest of your data processing code remains the same

# Divide the group into 4, but only extract the highest and lowest for comparison
mother_ssb_counts <- mother_ssb_counts %>%
  mutate(Group = ntile(SSB_Count, 4))

# Select only the highest and lowest quartiles
comparison_groups <- mother_ssb_counts %>%
  filter(Group == 1 | Group == 4)

# Perform Mann-Whitney U test between the two groups
test_result <- wilcox.test(Children ~ Group, data = comparison_groups, exact = FALSE)

# Print the results
print(test_result)

library(ggplot2)
library(dplyr)

# Prepare data by calculating means and standard errors
group_data <- bind_rows(
  data.frame(Group = "Specific Females", Children = children_count_specific_mothers$Children),
  data.frame(Group = "Remaining Females", Children = children_count_other_mothers$Children)
) %>%
  mutate(Group = factor(Group, levels = c("Specific Females", "Remaining Females"))) %>%  # Ensure the order of groups
  group_by(Group) %>%
  summarise(
    Mean = mean(Children),
    SE = sd(Children) / sqrt(n()),  # Calculate Standard Error
    .groups = 'drop'
  )

# Create bar chart with error bars
p1 <- ggplot(group_data, aes(x = Group, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.4, fill = c("gray", "white"), color = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.1, position = position_dodge(0.7), color = "black") +
  annotate("text", x = 2, y = max(group_data$Mean + group_data$SE) + 0.5, label = "*", size = 1) +  # Add star for significance
  labs(title = "Comparison of Average Offspring Counts Between Groups of Females",
       x = "",  # Remove x-axis label for cleaner look
       y = "Average Number of Offspring") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    legend.position = "none",  # No legend necessary
    panel.grid.major = element_blank(),  # Remove grid lines for a cleaner look
    panel.grid.minor = element_blank(),
    
  )

print(p1)
p2_modified_with_xlim <- ggplot(mother_ssb_counts, aes(x = Children, y = SSB_Count)) +
  geom_point(size = 3, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Number of Offspring vs. SSB Participation Count",
       x = "Number of Offspring",
       y = "SSB Participation Count") +
  xlim(0, 20) +
  ylim(0, 40) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        legend.position = "none")  # Adjust limits and remove unnecessary details

print(p2_modified_with_xlim)
