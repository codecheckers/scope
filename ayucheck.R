## Scoping Review data analysis

## data = scope

#installing packages
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

# clean column names but no need
janitor::clean_names(scope)
scope1 <- janitor::clean_names(scope)
View(scope1)

scope %>% 
  count(study_design)

# check duplicate but skip this as I've checked them in excel = countifs
install.packages("janitor")
library(janitor)
get_dupes(scope1)

head(scope)

scope |>
  count(intervention_class, outcomes_short)

#cleaning scope1 by removing all line breaks
scope[] <- lapply(scope, function(x) gsub("\n|\r", "", x))

#export it to excel for double check
install.packages("writexl")
library(writexl)
write_xlsx(scope_clean, "C:/Users/P018013/OneDrive - Amsterdam UMC/Scoping review WP2/scoping R/scope_clean.xlsx")


############################
## Evidence Map          ###
## I'll revise it later  ###
############################

scope_clean_ayu[is.na(scope_clean_ayu)] <- 0

head(scope_clean_ayu)

library(reshape2)
library(ggplot2)


## pivot

scope1 %>% pivot_wider(names_from = intervention_classification_specific, values_from = short_title)
pivot = melt(scope1, id = c("intervention_classification_group", "study_design"))

##double check for duplication
#intervention classification specific and proxy outcomes
df_check <- subset(scope1, select=c("short_title", "intervention_classification_specific", "proxy_outcomes"))

print(df_check)

View(df_check)
write_xlsx(df_check, "C:/Users/P018013/OneDrive - Amsterdam UMC/Scoping review WP2/df_check.xlsx")

get_dupes(df_check)
df_check_dupes <- get_dupes(df_check)
write_xlsx(df_check_dupes,"C:/Users/P018013/OneDrive - Amsterdam UMC/Scoping review WP2/df_check_dupes.xlsx" )

##Bibliometrix
install.packages("shiny")
install.packages("bibliometrix")

library(bibliometrix)
biblioshiny()

author <- read.csv("C:/Users/P018013/OneDrive - Amsterdam UMC/Scoping review WP2/export-data.csv", 
                   sep = ",")
View(author)
janitor::clean_names(author)
author1 <- janitor::clean_names(author)
View(author1)
author1[] <- lapply(author1, function(x) gsub("\n|\r", "", x))
write_xlsx(author1, "C:/Users/P018013/OneDrive - Amsterdam UMC/Scoping review WP2/author.xlsx")

#Beta version
install.packages("remotes")         

remotes::install_github("massimoaria/bibliometrix")

##file scope_effect ada tambahan effectiveness
table(scope_effect$author_stated_effect)
table(scope_effect$short_title, scope_effect$author_stated_effect)

####################
## Network graphs ##
####################

library(readxl)
copy_recode <- read_excel("C:/Users/P018013/OneDrive - Amsterdam UMC/Scoping review WP2/copy_recode.xlsx")
View(copy_recode)

library(igraph)
install.packages("networkD3")
library(networkD3)

network_scope <- tibble(from= c(copy_recode$exposure1),
                            to = c(copy_recode$outcome1))

#plot1
p1 <- simpleNetwork(network_scope, height = "NULL", width = "NULL",
                    fontSize = 14)

p1

#plot2 using Edge List method
p2 <- graph_from_data_frame(d=network_scope, directed=F)
plot(p2)

deg <- degree(p2, mode="all")
plot(p2, vertex.size=deg*0.7, vertex.label.dist = 0.5,
     vertex.color=rgb(0.1,0.7,0.8,0.5), layout = layout,
     main = "Evidence Map", cex = 1.2)


#work with clean data
scope_clean <- read_excel("C:/Users/P018013/OneDrive - Amsterdam UMC/Scoping review WP2/copy_scope_clean.xlsx", 
                            +     sheet = "duplication_deleted")
View(scope_clean)

table(scope_clean$intervention_classification_group, scope_clean$direct_outcomes)
table(scope_clean$intervention_classification_specific, scope_clean$proxy_outcomes)
table(scope_clean$author_stated_effect)

##buble plot but not nice
xx = ggplot(df1m, aes(x = c.scope_040324.intervention_specific., y = variable)) + 
  geom_point(aes(size = value, fill = variable), alpha = 0.75, shape = 21)
xx

## With the new dataset

# Upload the dataset

library(readxl)
fulldata <- read_excel("fulldata.xlsx", sheet = "Sheet1")
View(fulldata)

# Clean fulldata  by removing all line breaks
scope[] <- lapply(scope, function(x) gsub("\n|\r", "", x))

#library
library(ggplot2)

# Evidence gap map between intervention class x outcome short
# Count the number of author_year for each combination of intervention_class and outcomes_short
df_count <- aggregate(author_year ~ intervention_class + outcomes_short + study_design_short, data = fulldata, FUN = length)

# Create evidence gap map1
egm <- ggplot(df_count, aes(x = outcomes_short, y = intervention_class, size = author_year, color = study_design_short)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 12)) + # Adjust the size range for better visibility
  labs(x = "Outcomes Short", y = "Intervention Class", size = "Count of Author Year", color = "Study Design Short") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate X-axis labels
        legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) # Add and color panel border

egm

# Evidence gap map between intervention exact x outcome short
# Count the number of author_year for each combination of intervention_class and outcomes_short
df_count <- aggregate(author_year ~ intervention_exact + outcomes_short + study_design_short, data = fulldata, FUN = length)

# Create evidence gap map2
egm2 <- ggplot(df_count, aes(x = outcomes_short, y = intervention_exact, size = author_year, color = study_design_short)) +
  geom_point(alpha = 0.7) + 
  scale_size_continuous(range = c(3, 12)) + # Adjust the size range for better visibility
  labs(x = "Outcomes Short", y = "Intervention Exact", size = "Count of Author Year", color = "Study Design Short") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate X-axis labels
        legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) # Add and color panel border

egm2

library(ggplot2)

# Count the occurrences of combinations without using group_by
egm3 <- as.data.frame(table(fulldata$intervention_class, fulldata$outcomes_short, fulldata$study_design_short))
names(egm3) <- c("intervention_class", "outcomes_short", "study_design_short", "count")

# Create size category without using group_by
egm3$size_category <- cut(egm3$count, breaks = c(0, 3, 6, 9, Inf), labels = c("1-3", "4-6", "7-9", ">10"))

# Define size mapping based on categories
size_mapping <- c("1-3" = 3, "4-6" = 6, "7-9" = 9, ">10" = 12)

# Create the bubble plot
ggplot(egm3, aes(x = outcomes_short, y = intervention_class, size = size_category, fill = study_design_short)) +
  geom_point(shape = 21, alpha = 0.7) +
  scale_size_manual(values = size_mapping) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1)) +
  labs(x = "Outcomes Short", y = "Intervention Class", size = "Count of Author Year Category", fill = "Study Design Short")


##