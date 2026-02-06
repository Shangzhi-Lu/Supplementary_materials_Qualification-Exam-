################################################################################
# This script requires one data file: 
# "Sanky_Plot_Data.xlsx"
# Shangzhi Lu,@CUHKSZ  Feb 2026
################################################################################

#### 1. Preparation_____________________________________________________________
package_list <- c("tidyverse","readxl","svglite","ggsankeyfier","RColorBrewer","writexl")
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### 2. Import and extract data_________________________________________________
Plot_Data <- read_excel("Sanky_Plot_Data.xlsx", sheet = "Data", na = "---")
Title_Order <- Plot_Data %>%
  group_by(Title) %>%
  summarise(
    title_num = max(Number, na.rm = TRUE),  # ordering value per Title
    .groups = "drop"
  ) %>%
  arrange(desc(title_num), Title) %>%       # largest on top; add Title as tie-breaker
  mutate(
    title_rank = row_number(),
    Title_node = sprintf("%04d|%s", title_rank, Title) # internal ordered id (one per Title)
  )

# Join the ordered Title_node back to each row
Plot_Data <- Plot_Data %>%
  left_join(Title_Order %>% select(Title, Title_node), by = "Title") %>%
  mutate(
    row_order = row_number(),
    weight = 1
  )

#### 3. Prepare Sankey long data________________________________________________
Plot_Data_long <- bind_rows(
  # Stage 1 → 2: Title → Construct
  Plot_Data %>%
    mutate(edge_id = row_number() * 3 - 2) %>%
    transmute(edge_id, row_order,
              stage = "Title",
              node = Title_node,  # IMPORTANT: one node per Title (no splitting)
              node_label = Title,     # display label
              connector = "from",
              weight),
  
  Plot_Data %>%
    mutate(edge_id = row_number() * 3 - 2) %>%
    transmute(edge_id, row_order,
              stage = "Construct",
              node = Construct,
              node_label = Construct,
              connector = "to",
              weight),
  
  # Stage 2 → 3: Construct → Type & Method
  Plot_Data %>%
    mutate(edge_id = row_number() * 3 - 1) %>%
    transmute(edge_id, row_order,
              stage = "Construct",
              node = Construct,
              node_label = Construct,
              connector = "from",
              weight),
  
  Plot_Data %>%
    mutate(edge_id = row_number() * 3 - 1) %>%
    transmute(edge_id, row_order,
              stage = "Research Method",
              node = Type,
              node_label = Type,
              connector = "to",
              weight),
  
  # Stage 3 → 4: Type & Method → Topic
  Plot_Data %>%
    mutate(edge_id = row_number() * 3) %>%
    transmute(edge_id, row_order,
              stage = "Research Method",
              node = Type,
              node_label = Type,
              connector = "from",
              weight),
  
  Plot_Data %>%
    mutate(edge_id = row_number() * 3) %>%
    transmute(edge_id, row_order,
              stage = "Topic",
              node = Topic,
              node_label = Topic,
              connector = "to",
              weight)
) %>%
  mutate(stage = factor(stage, levels = c("Title","Construct","Research Method","Topic"))) %>%
  arrange(stage, row_order)


#### 5. Colors and positions____________________________________________________
unique_labels <- unique(Plot_Data_long$node_label)
n_colors <- length(unique_labels)

my_colors <- colorRampPalette(brewer.pal(12,"Set3"))(n_colors)
names(my_colors) <- unique_labels

pos <- position_sankey(order = "as_is", v_space = 0.05)
pos_text <- position_sankey(order = "as_is", v_space = 0.05, nudge_x = 0.1)

#### 6. Plot Sankey______________________________________________________________
Sanky_Plot <- ggplot(
  Plot_Data_long,
  aes(
    x = stage,
    y = weight,
    group = node,                 # group by internal node id (Title_node is shared per Title)
    connector = connector,
    edge_id = edge_id
  )
) +
  geom_sankeynode(aes(fill = node_label), position = pos, show.legend = FALSE) +
  geom_sankeyedge(aes(fill = node_label), position = pos, alpha = 0.6, show.legend = FALSE) +
  geom_text(aes(label = node_label),
            stat = "sankeynode",
            position = pos_text,
            hjust = 0,
            size = 13/.pt) +
  scale_x_discrete(expand = expansion(add = c(0.2,0.7)), position = "top") +
  scale_fill_manual(values = my_colors) +
  theme_void() +
  theme(
    plot.margin = margin(0.5,0.5,0.5,0.5, unit="cm"),
    axis.text.x = element_text(color="black", face="bold", size=15)
  )

Sanky_Plot

#### 7. Export__________________________________________________________________
ggsave("Figure1.pdf", Sanky_Plot, width = 19.5, height = 10)
ggsave("Figure1.svg", plot = Sanky_Plot, device = "svg",  width = 19.5, height = 10, units = "in")