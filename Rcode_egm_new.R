library(readxl)
fulldata <- read_excel("fulldata.xlsx")
View(fulldata)

library(ggplot2)
library(dplyr)
library(tidyr)

## Data cleaning
# Extract year from the author_year column
fulldata$year <- as.numeric(sub(".*\\((\\d{4})\\).*", "\\1", fulldata$author_year))

# Ensure data is cleaned  by removing all line breaks
fulldata[] <- lapply(fulldata, function(x) gsub("\n|\r", "", x))

# Change to lower case
fulldata[] <- lapply(fulldata, tolower)

# Direct and proxy
fulldata <- fulldata %>%
  mutate(direct = ifelse(direct == 1, "Direct outcomes", "Proxy outcomes"))

# Clean values
fulldata$study_design_short <- gsub("post_only", "post only", fulldata$study_design_short)
fulldata$outcomes_specific <- gsub("type-i error reduction", "type-I error reduction", fulldata$outcomes_specific)

## Reordering
# Reordering study design
fulldata$study_design_short <- factor(fulldata$study_design_short)
new_order_studydesign <- levels(fulldata$study_design_short)[c(1,5,3,4,2)]
fulldata$studydesign_reordered <- factor(fulldata$study_design_short, levels = new_order_studydesign)

# Reordering the levels of 'outcomes_class'
fulldata$outcomes_class <- factor(fulldata$outcomes_class)
new_order_outcomes_class <- levels(fulldata$outcomes_class)[c(1,7,6,4,5,2,8,3)]
fulldata$outcomes_class_reordered <- factor(fulldata$outcomes_class, levels = new_order_outcomes_class)

# Reordering the levels of 'outcomes_specific'
fulldata$outcomes_specific <- factor(fulldata$outcomes_specific)
new_order_outcomes_specific <- levels(fulldata$outcomes_specific)[c(6,14,13,15,17,10,16,5,12,8,3,7,4,2,1,11,9)]
fulldata$outcomes_specific_reordered <- factor(fulldata$outcomes_specific, levels = new_order_outcomes_specific)

## Evidence Gap Map 1
# Group data
df_count1 <- fulldata %>%
  count(outcomes_specific_reordered, intervention_class, studydesign_reordered) %>%
  arrange(desc(n))

# Create a complete matrix
complete_matrix1 <- expand_grid(outcomes_specific_reordered = unique(fulldata$outcomes_specific_reordered), 
                                intervention_class = unique(fulldata$intervention_class)) %>%
  left_join(df_count1, by = c("outcomes_specific_reordered", "intervention_class")) %>%
  replace_na(list(n = 0))

# Create evidence gap map 1 (egm1)
egm1 <- ggplot(complete_matrix1, aes(x =outcomes_specific_reordered, y = intervention_class, fill = n)) +
  geom_tile(color = "grey") + 
  scale_fill_gradient(low = "white", high = "steelblue", name = "Total studies") +
  geom_point(data = df_count1, aes(size = n, color = studydesign_reordered), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), alpha = 0.6) +
  scale_y_discrete(limits=rev)+
  scale_size_continuous(range = c(3, 12), breaks = c(1,5,10,15,20), labels = function(x) round(x,0)) + 
  labs(title = "",
       x = "Outcomes",
       y = "Class of interventions",
       size = "Number of studies per study design",
       color = "Study design",
       caption = "Notes on study design: 
       between = comparative (between-subject comparison)
       within = comparative (within-subject comparison/repeated measures design)
       post only = posttest design (only a post measurement after the implementation of an intervention and the intervention is explicitly mentioned)
       other = other designs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  

egm1

# Save the plot as a JPEG file
ggsave("egm1.jpg", plot = egm1, width = 14, height = 8, dpi = 300, limitsize = FALSE)




## Evidence gap map 2
# Group data
df_count2 <- aggregate(author_year ~ intervention_class + outcomes_class_reordered + studydesign_reordered + direct, data = fulldata, FUN = length)

# Create evidence gap map 2 (egm2)
egm2 <- ggplot(df_count2, aes(x = outcomes_class_reordered, y = intervention_class, size = author_year, color = studydesign_reordered)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), alpha = 0.6) +
  scale_y_discrete(limits=rev)+
  facet_wrap(~direct, scales = "free_x") +
  scale_size_continuous(range = c(3, 12), breaks = c(1,5,10,15,20), labels = function(x) round(x,0)) +
  labs(x = "Outcomes", y = "Class of interventions", size = "Number of studies", color = "Study design",
       caption = "Notes on study design: 
       between = comparative (between-subject comparison)
       within = comparative (within-subject comparison/repeated measures design)
       post only = posttest design (only a post measurement after the implementation of an intervention and the intervention is explicitly mentioned)
       other = other designs")+
  theme(plot.caption = element_text(hjust=0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "right",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1))

egm2

# Save the plot as a JPEG file
ggsave("egm2.jpg", plot = egm2, width = 16, height = 8, dpi = 300, limitsize = FALSE)

