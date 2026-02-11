df <- read.csv(file = 'policyreport/Results_PLE_policy_brief_table.csv')
df <- read.csv(file = 'policyreport/Results_CE_inputs_policy_brief_table.csv')

library(reshape2)
library(dplyr)
library(stringr)

install.packages("patchwork")
library(patchwork)

dff <- melt(df, id.vars = c('Variables', 'Category')) %>% rename(number = value) %>% 
  mutate(value = as.numeric(str_extract(number, "[-]?[0-9]*\\.?[0-9]+")),
         unit = str_extract(number, "p\\.p\\.|%"),
         term = as.factor(str_extract(variable, "Short.term|Medium.term|Long.term")),
         region = str_extract(variable, "EU|RoW|Z\\.1") %>% str_replace("Z\\.1", "EU")) %>%
  mutate(term = factor(term, levels = c("Short.term", "Medium.term", "Long.term")),
         Variables = sapply(Variables, replace_second_space))
levels(dff$term) <- rev(c('Long', 'Medium', 'Short'))

p1 <- dff %>% filter(Category == 'Economic' & Variables != 'Corporate Demand\nfor Loans' & Variables != 'Household Demand\nfor Loans') %>% 
  ggplot(aes(x = term, y = value, fill = value > 0)) + 
  geom_bar(stat = 'identity', position = 'dodge', show.legend = F) + facet_grid(Variables ~ region) +
  # scale_fill_manual(values = c("TRUE" = "darkolive", "FALSE" = "red")) +
  theme(strip.text.y.right = element_text(angle = 0), title = element_text(face = 'bold')) + 
  labs(title = 'Economic Dimension') + ylab('')
p11 <- dff %>% filter(Variables == 'Corporate Demand\nfor Loans' | Variables == 'Household Demand\nfor Loans') %>% 
  ggplot(aes(x = term, y = value, fill = value > 0)) + 
  geom_bar(stat = 'identity', position = 'dodge', show.legend = F) + facet_grid(Variables ~ region) +
  # scale_fill_manual(values = c("TRUE" = "darkolive", "FALSE" = "red")) +
  theme(strip.text.y.right = element_text(angle = 0), title = element_text(face = 'bold')) + 
  labs(title = 'Financial Dimension') + ylab('')
p2 <- dff %>% filter(Category == 'Social') %>% 
  ggplot(aes(x = term, y = value, fill = value > 0)) + 
  geom_bar(stat = 'identity', position = 'dodge', show.legend = F) + facet_grid(Variables ~ region) +
  # scale_fill_manual(values = c("TRUE" = "darkolive", "FALSE" = "red")) +
  theme(strip.text.y.right = element_text(angle = 0), title = element_text(face = 'bold')) + 
  labs(title = 'Social Dimension') + ylab('')
p3 <- dff %>% filter(Category == 'Ecological') %>% 
  ggplot(aes(x = term, y = value, fill = value < 0)) + 
  geom_bar(stat = 'identity', position = 'dodge', show.legend = F) + facet_grid(Variables ~ region) +
  # scale_fill_manual(values = c("TRUE" = "darkolive", "FALSE" = "red")) +
  theme(strip.text.y.right = element_text(angle = 0), title = element_text(face = 'bold')) + 
  labs(title = 'Ecological Dimension') + ylab('')
# gridExtra :: grid.arrange(p1, p2, p3, ncol = 1)
  


# Assuming you have three ggplot objects: p1, p2, p3

# Set the same y-axis limits (if applicable) for consistent scaling across all plots
y_limits <- c(min(c(min(p1$data$value), min(p2$data$value), min(p3$data$value))),
              max(c(max(p1$data$value), max(p2$data$value), max(p3$data$value))))

p1 <- p1 + ylim(y_limits)
p2 <- p2 + ylim(y_limits)
p3 <- p3 + ylim(y_limits)

aspect_ratio <- NULL  # Adjust this ratio as needed for your specific plot dimensions

p1 <- p1 + theme(aspect.ratio = aspect_ratio)
p11 <- p11 + theme(aspect.ratio = aspect_ratio)
p2 <- p2 + theme(aspect.ratio = aspect_ratio)
p3 <- p3 + theme(aspect.ratio = aspect_ratio)

# Combine plots using patchwork with specific plot layout settings
combined_plot2 <- ((p1 / p11 ) + plot_layout(heights = c(.7, .3)) | ( p2 / p3)) # + plot_layout(ncol = 2, heights = c(0.8, .2, .5, .5)) 
# p1 / p2 / p3 +
# combined_plot1 <- p1 / p2 / p3 + 
#   plot_layout(ncol = 1, heights = c(1.5, 0.75, .75)) &  # Ensure each plot gets equal height
#   theme(
#     strip.text.y = element_text(angle = 0),     # Ensure facet labels are not rotated
#     plot.margin = margin(1, 1, 1, 1)    # Ensure consistent margins around plots
#   )

# Display the combined plot
pdf('table1b.pdf', width = 8, height = 12)
combined_plot1
dev.off()

pdf('table1_policybrief1.pdf', width = 13, height = 11)
combined_plot2
dev.off()

# Combine plots using patchwork with specific plot layout settings
combined_plot <- p1 / p2 / p3 +
  plot_layout(ncol = 1, heights = rep(1, 3)) &  # Ensure each plot gets equal height
  theme(
    strip.text.y = element_text(angle = 0),     # Ensure facet labels are not rotated
    aspect.ratio = 1,                           # Set the aspect ratio to 1:1 for consistent facet heights
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)    # Ensure consistent margins around plots
  )

# Display the combined plot
combined_plot

         
# %>%
#  mutate())
dff <- filter(dff, Variables != 'Corporate Demand for Loans')

dff$Category <- factor(dff$Category, levels = c('Economic', 'Social', 'Ecological'))


library(ggplot2)
pdf('table1.pdf', width = 8, height = 12)
dff %>% ggplot(aes(x = term, y = value, fill = value)) + 
  geom_bar(stat = 'identity', position = 'dodge') + facet_grid(Category + Variables ~ region) +
  scale_fill_gradientn(colors = c("red", "green"), 
                       values = scales::rescale(c(min(dff$value), max(dff$value))),
                       limits = c(min(dff$value), max(dff$value))) +
  theme(strip.text.y.right = element_text(angle = 0))
dev.off()

# dff %>% 
#   ggplot(aes(x = term, y = value, fill = value)) + 
#   geom_bar(stat = 'identity', position = 'dodge') + 
#   facet_grid(Category ~ Variables + region, switch = "y") + 
#   theme(
#     strip.text.y = element_text(angle = 0),      # Set Variables text to no rotation
#     strip.text.x = element_text(angle = 0),      # Ensure the region labels are not rotated
#     strip.placement = "outside",                 # Place the category on the outside
#     strip.background = element_blank()           # Remove background from strips for clarity
#   )

replace_second_space <- function(x) {
  # Find the positions of spaces
  space_positions <- str_locate_all(x, " ")[[1]]
  
  # If there are at least two spaces, replace the second one
  if (nrow(space_positions) >= 2) {
    second_space_position <- space_positions[2, "start"]
    # Replace the second space with a line break
    x <- str_sub(x, 1, second_space_position - 1) %>% 
      str_c("\n", str_sub(x, second_space_position + 1, str_length(x)))
  }
  
  return(x)
}