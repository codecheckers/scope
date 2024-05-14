# Create a summary table of outcomes list
summary_outcomes <- fulldata %>%
  group_by(direct, outcomes_class_reordered, outcomes_specific_reordered) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2)) %>%
  rename(`Outcomes` = direct,
         `Outcomes Class` = outcomes_class_reordered, 
         `Outcomes Specific` = outcomes_specific_reordered, 
         `n` = Count, 
         `%` = Percentage)

# Print the summary table
gt::gt(summary_outcomes)

##EGM1
library(ggplot2)
library(dplyr)
library(tidyr)

# Aggregate data to count occurrences of each combination without using group_by
df_count1 <- fulldata %>%
  count(outcomes_class_reordered, intervention_class, studydesign_reordered) 

# Create a complete matrix for the background
complete_matrix <- expand_grid(outcomes_class_reordered = unique(fulldata$outcomes_class), intervention_class = unique(fulldata$intervention_class)) %>%
  left_join(df_count1, by = c("outcomes_class_reordered", "intervention_class")) %>%
  replace_na(list(n = 0))

# Create evidence gap map 1 (egm1)
egm1 <- ggplot(complete_matrix, aes(x = outcomes_class_reordered, y = intervention_class, fill = n)) +
  geom_tile(color = "grey") + 
  scale_fill_gradient(low = "white", high = "steelblue", name = "Total studies") +
  geom_point(data = df_count1, aes(size = n, color = studydesign_reordered), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.3), alpha = 0.6) +
  scale_size_continuous(range = c(3, 15), breaks = c(1,5,10,15,20), labels = function(x) round(x,0)) +  # Adjust size range as needed
  labs(title = "",
       x = "Outcomes Class",
       y = "Intervention Class",
       size = "Number of studies",
       color = "Study design") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  


egm1

# Save the plot as a JPEG file
ggsave("egm1.jpg", plot = egm1, width = 14, height = 8, dpi = 300, limitsize = FALSE)
