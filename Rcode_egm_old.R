library(readxl)
fulldata <- read_excel("fulldata.xlsx")
View(fulldata)

library(ggplot2)
library(dplyr)

## Data cleaning and replacement ##
fulldata$study_design_short <- gsub("post_only", "post only", fulldata$study_design_short)
fulldata$intervention_class <- gsub("Other", "Other interventions", fulldata$intervention_class)
fulldata$intervention_exact <- gsub("(p-value calibraration) - statistical technique", "p-value calibration", fulldata$intervention_exact)

fulldata$outcomes_short <- ifelse(fulldata$outcomes_class == "reporting specific elements", "other outcomes", fulldata$outcomes_short)
fulldata$outcomes_short <- gsub("reporting specific elements", "reporting", fulldata$outcomes_short)
fulldata$outcomes_short <- gsub("completeness of reporting", "reporting", fulldata$outcomes_short)
fulldata$outcomes_short <- gsub("type-I error reduction", "type I error", fulldata$outcomes_short)
fulldata$outcomes_short <- gsub("percentage inconsistencies", "inconsistencies", fulldata$outcomes_short)
fulldata$outcomes_short <- gsub("data-sharing statements", "statements", fulldata$outcomes_short)
fulldata$outcomes_short <- gsub("other", "other outcomes", fulldata$outcomes_short)

fulldata$outcomes_specific <- gsub("Results replicability", "Results Replicability", fulldata$outcomes_specific)
fulldata$outcomes_class <- gsub("Inferential reproducibility", "Direct reproducibility", fulldata$outcomes_class)

# Make proper case
fulldata$outcomes_class <- tools::toTitleCase(fulldata$outcomes_class)
fulldata$outcomes_short <- tools::toTitleCase(fulldata$outcomes_short)
fulldata$outcomes_specific <- tools::toTitleCase(fulldata$outcomes_specific)

fulldata$intervention_class <- tools::toTitleCase(fulldata$intervention_class)
fulldata$intervention_specific <- tools::toTitleCase(fulldata$intervention_specific)

fulldata$academic_field <- tools::toTitleCase(fulldata$academic_field)
fulldata$academic_field_short <- tools::toTitleCase(fulldata$academic_field_short)

fulldata$study_design_short <- factor(fulldata$study_design_short)
fulldata$study_design_short <- factor(tools::toTitleCase(as.character(fulldata$study_design_short)))
fulldata$study_design_short <- gsub("Post Only", "Post only", fulldata$study_design_short)

# Extract year from the author_year column
fulldata$year <- as.numeric(sub(".*\\((\\d{4})\\).*", "\\1", fulldata$author_year))


#  Show unique values from the fulldata data frame
unique(fulldata$outcomes_short)
unique(fulldata$study_design)
unique(fulldata$study_design_short)
unique(new_order_studydesign)
levels(fulldata$outcomes_short)
unique(fulldata$intervention_exact)
unique(fulldata$outcomes_class)
levels(fulldata$outcomes_class)
unique(fulldata$study_design_short)
unique(fulldata$outcomes_specific)
levels(fulldata$outcomes_specific)

# Reordering the levels of 'outcomes_short'
# First, ensure that 'outcomes_short' is a factor with the correct number of levels
fulldata$outcomes_short <- factor(fulldata$outcomes_short)

# Define the new order for the levels of outcome_short
new_order_outcomes_short <- levels(fulldata$outcomes_short)[c(11,12,2,6,17,3,9,14,1,4,15,8,5,10,13,16,7)]

# Create a new variable 'outcomes_short_reordered' with the specified order
fulldata$outcomes_short_reordered <- factor(fulldata$outcomes_short, levels = new_order_outcomes_short)

# Reordering the levels of 'study_design_short'
fulldata$study_design_short <- factor(fulldata$study_design_short)
new_order_studydesign <- levels(fulldata$study_design_short)[c(1,5,3,4,2)]
fulldata$studydesign_reordered <- factor(fulldata$study_design_short, levels = new_order_studydesign)

# Reordering the levels of 'outcomes_class'
fulldata$outcomes_class <- factor(fulldata$outcomes_class)
new_order_outcomes_class <- levels(fulldata$outcomes_class)[c(1,2,6,4,8,7,3,5)]
fulldata$outcomes_class_reordered <- factor(fulldata$outcomes_class, levels = new_order_outcomes_class)


## Evidence Gap Map 1
# Aggregate
df_count1 <- aggregate(author_year ~ intervention_class + outcomes_short_reordered + studydesign_reordered, data = fulldata, FUN = length)

# Create evidence gap map (egm) 1
egm1 <- ggplot(df_count1, aes(x = outcomes_short_reordered, y = intervention_class, size = author_year, color = studydesign_reordered)) +
  geom_point(alpha = 0.7) +
  scale_y_discrete(limits=rev)+
  scale_size_continuous(range = c(3, 12), labels = function(x) round(x,0)) + 
  labs(x = "Outcomes", y = "Class of interventions", size = "Number of studies", color = "Study design",
       caption = "Notes on study design: 
       Between = comparative (between-subject comparison)
       Within = comparative (within-subject comparison/repeated measures design)
       Post only = posttest design (only a post measurement after the implementation of an intervention and the intervention is explicitly mentioned)
       Other = other designs") +
  theme(plot.caption = element_text(hjust=0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "right",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1))

egm1


# Save the plot as a TIFF file
ggsave("egm1.tiff", plot = egm1, width = 10, height = 8, dpi = 300)

# Save the plot as a JPEG file
ggsave("egm1.jpg", plot = egm1, width = 14, height = 8, dpi = 300, limitsize = FALSE)

## Separated from direct and proxy outcomes
library(ggplot2)
library(dplyr)

## Evidence gap map 2
# Aggregate
df_count2 <- aggregate(author_year ~ intervention_class + outcomes_class_reordered + studydesign_reordered + direct, data = fulldata, FUN = length)

# Relabel 'direct' for clarity in plots
df_count2$direct <- factor(df_count2$direct, levels = c(1, 0), labels = c("Direct Outcomes", "Proxy Outcomes"))

# Order outcomes_class within Proxy Outcomes based on the highest number
df_count2 <- df_count2 %>%
  arrange(direct, desc(author_year)) %>%
  mutate(outcomes_class_reordered = factor(outcomes_class_reordered, levels = unique(new_order_outcomes_class)))

# Create evidence gap map 2
egm2 <- ggplot(df_count2, aes(x = outcomes_class_reordered, y = intervention_class, size = author_year, color = studydesign_reordered)) +
  geom_point(alpha = 0.7) +
  scale_y_discrete(limits=rev)+
  facet_wrap(~direct, scales = "free_x") +
  scale_size_continuous(range = c(3, 12), labels = function(x) round(x, 0)) +
  labs(x = "Outcomes", y = "Class of interventions", size = "Number of studies", color = "Study design",
       caption = "Notes on study design: 
       Between = comparative (between-subject comparison)
       Within = comparative (within-subject comparison/repeated measures design)
       Post only = posttest design (only a post measurement after the implementation of an intervention and the intervention is explicitly mentioned)
       Other = other designs")+
  theme(plot.caption = element_text(hjust=0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "right",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1))

egm2

# Save the plot as a JPEG file
ggsave("egm2.jpg", plot = egm2, width = 16, height = 8, dpi = 300, limitsize = FALSE)

## Bar graph showing studies over time by academic fields

library(ggplot2)

# Create a bar graph 
ggplot(fulldata, aes(x = year, fill = academic_field_short)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3", name = "Academic field") +  # Using a palette with distinctive colors and setting legend title
  labs(x = "Year", y = "Number of studies", title = "Number of studies per academic field") +
  theme(axis.text.y = element_text(size = 12))  # Medium size y-label +
  theme_minimal()


library(dplyr)
library(tidyr)

# Create a summary table of interventions with counts of author_year
summary_interventions <- fulldata %>%
  group_by(intervention_class, intervention_specific) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2)) %>%
  rename(`Intervention Class` = intervention_class, 
         `Intervention Specific` = intervention_specific, 
         `n` = Count, 
         `%` = Percentage)

# Print the summary table
gt::gt(summary_interventions)

# Save the summary interventions as jpeg
library(gridExtra)

png("table_interventions.png", width=625,height=400,bg = "white")
grid.table(summary_interventions, row = NULL)
dev.off()

## List of outcomes
library(dplyr)

# Replace values in 'direct' column in the df data frame
fulldata <- fulldata %>%
  mutate(direct = ifelse(direct == 1, "Direct", "Proxy"))

# Create a summary table of outcomes list
summary_outcomes <- fulldata %>%
  group_by(direct, outcomes_class_reordered, outcomes_short_reordered) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2)) %>%
  rename(`Outcomes` = direct,
         `Outcomes Class` = outcomes_class_reordered, 
         `Outcomes Specific` = outcomes_short_reordered, 
         `n` = Count, 
         `%` = Percentage)

# Print the summary table
gt::gt(summary_outcomes)

# Save as jpeg
library(gridExtra)
png("table_outcomes.png", width=600,height=480,bg = "white")
grid.table(summary_outcomes, row = NULL)
dev.off()
