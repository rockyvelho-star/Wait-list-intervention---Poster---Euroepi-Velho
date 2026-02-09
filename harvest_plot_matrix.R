# ============================================================================
# HARVEST PLOT MATRIX - R CODE
# Generate a matrix of harvest plots showing interventions
# Bar width = sample size, Bar height = quality, Bar color = effect direction
# ============================================================================

library(ggplot2)
library(gridExtra)
library(grid)

# ============================================================================
# DATA SETUP
# ============================================================================

# Create study data
studies <- data.frame(
  study_num = 1:23,
  study_name = c(
    "Study 1", "Study 2", "Study 3", "Study 4", "Study 5",
    "Study 6", "Study 7", "Study 8", "Study 9", "Study 10",
    "Study 11", "Study 12", "Study 13", "Study 14", "Study 15",
    "Study 16", "Study 17", "Study 18", "Ching et al.", "Dias et al.",
    "Dias et al.", "Subotic-Kerry et al.", "Fujita et al."
  ),
  year = c(
    2015, 2016, 2017, 2018, 2019, 2020, 2020, 2020, 2021, 2021,
    2022, 2022, 2022, 2023, 2023, 2023, 2023, 2023, 2023, 2024,
    2024, 2025, 2025
  ),
  sample_size = c(
    48, 67, 20, 40, 55, 52, 30, 32, 48, 52,
    55, 30, 67, 67, 20, 67, 24, 103, 156, 89,
    24, 375, 0
  ),
  quality = c(
    "Low", "Moderate", "Low", "Low-Moderate", "Low-Moderate",
    "Moderate", "Low", "Low-Moderate", "Low", "Low",
    "Moderate", "Low", "Moderate", "Moderate", "Low",
    "Moderate", "Moderate", "Low", "Moderate", "Low-Moderate",
    "Low-Moderate", "Moderate", "Moderate"
  ),
  effect = c(
    rep("Positive", 18), "Positive", "Positive",
    "Positive", "Descriptive", "No Data"
  ),
  intervention = c(
    "Psychoeducation Alone", "Psychoeducation + Parent Support",
    "Bibliotherapy/Self-help", "Psychoeducation Alone",
    "Coaching", "Psychoeducation + Parent Support",
    "Bibliotherapy/Self-help", "Psychoeducation Alone",
    "Bibliotherapy/Self-help", "Coaching",
    "Psychoeducation + Parent Support", "Bibliotherapy/Self-help",
    "Psychoeducation Alone", "Psychoeducation + Parent Support",
    "Bibliotherapy/Self-help", "Psychoeducation Alone",
    "Psychoeducation + Parent Support", "Psychoeducation Alone",
    "Digital Interventions", "Social Prescribing",
    "Social Prescribing", "Digital Interventions",
    "Digital Interventions"
  ),
  stringsAsFactors = FALSE
)

# Quality to numeric height
quality_levels <- c("Very Low" = 0.4, "Low" = 0.6, "Low-Moderate" = 0.8, 
                   "Moderate" = 1.0, "High" = 1.2)
studies$height <- quality_levels[studies$quality]

# Sample size to numeric width
sample_to_width <- function(n) {
  ifelse(n == 0, 0.35,
  ifelse(n <= 30, 0.45,
  ifelse(n <= 50, 0.55,
  ifelse(n <= 70, 0.65,
  ifelse(n <= 100, 0.80,
  ifelse(n <= 150, 0.95, 1.15))))))
}
studies$width <- sample_to_width(studies$sample_size)

# Effect to color
effect_colors <- c(
  "Positive" = "#2E7D32",
  "Mixed" = "#F57C00",
  "Negative" = "#C62828",
  "No Data" = "#757575",
  "Descriptive" = "#1976D2"
)
studies$color <- effect_colors[studies$effect]

# ============================================================================
# HELPER FUNCTION - CREATE ONE PANEL
# ============================================================================

create_panel <- function(data, intervention_name) {
  # Sort by year
  data <- data[order(data$year), ]
  
  # Calculate x positions (avoid overlaps)
  bar_spacing <- 0.35
  data$x_pos <- cumsum(c(0.9, data$width[-1] + bar_spacing))
  
  # Total sample size
  total_n <- sum(data$sample_size[data$sample_size > 0])
  n_studies <- nrow(data)
  
  # Create plot
  p <- ggplot(data) +
    # Draw bars
    geom_rect(aes(xmin = x_pos - width/2, xmax = x_pos + width/2,
                  ymin = 0, ymax = height),
              fill = data$color, color = "black", linewidth = 1.2) +
    
    # Study number (centered in bar, upper portion)
    geom_text(aes(x = x_pos, y = height * 0.60, label = study_num),
              size = 6, fontface = "bold", color = "white") +
    
    # Year in brackets (centered in bar, lower portion)
    geom_text(aes(x = x_pos, y = height * 0.28, 
                  label = paste0("(", year, ")")),
              size = 3.5, fontface = "bold", color = "white") +
    
    # Sample size below bar
    geom_text(aes(x = x_pos, y = -0.15, 
                  label = ifelse(sample_size > 0, 
                                paste0("n=", sample_size),
                                "n=0\n(terminated)")),
              size = 3.5, fontface = "italic", 
              color = ifelse(data$sample_size > 0, "#333333", "#C62828")) +
    
    # Axes and labels
    scale_y_continuous(
      name = "Study Quality",
      breaks = c(0, 0.4, 0.6, 0.8, 1.0, 1.2),
      labels = c("0", "V.Low", "Low", "Low-Mod", "Mod", "High"),
      limits = c(-0.32, 1.52)
    ) +
    scale_x_continuous(limits = c(0.2, max(data$x_pos) + 0.4)) +
    
    # Panel title
    ggtitle(paste0(intervention_name, "\n",
                   n_studies, " studies | Total n=", total_n)) +
    
    # Theme
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linetype = "dashed", 
                                        color = "gray70"),
      panel.border = element_rect(color = "#555555", fill = NA, 
                                  linewidth = 1.2),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12,
                               margin = margin(b = 10)),
      axis.title.y = element_text(face = "bold", size = 11)
    )
  
  return(p)
}

# ============================================================================
# CREATE ALL 6 PANELS
# ============================================================================

interventions_list <- c(
  "Psychoeducation Alone",
  "Psychoeducation + Parent Support",
  "Bibliotherapy/Self-help",
  "Coaching",
  "Digital Interventions",
  "Social Prescribing"
)

panels <- lapply(interventions_list, function(interv) {
  subset_data <- studies[studies$intervention == interv, ]
  create_panel(subset_data, interv)
})

# ============================================================================
# CREATE LEGEND
# ============================================================================

# Create legend grob
create_legend <- function() {
  grid.newpage()
  
  # Draw width legend
  grid.text("BAR WIDTH = Sample Size", x = 0.15, y = 0.85, 
           gp = gpar(fontsize = 14, fontface = "bold"))
  
  width_labels <- c("≤30", "31-50", "51-70", "71-100", "101-150", ">150")
  for(i in 1:6) {
    x_pos <- 0.05 + (i-1)*0.04
    grid.rect(x = x_pos, y = 0.65, width = 0.02, height = 0.1,
             gp = gpar(fill = "#9E9E9E", col = "black"))
    grid.text(paste0("n=", width_labels[i]), x = x_pos, y = 0.50,
             gp = gpar(fontsize = 10, fontface = "bold"))
  }
  
  # Draw height legend
  grid.text("BAR HEIGHT = Study Quality", x = 0.50, y = 0.85,
           gp = gpar(fontsize = 14, fontface = "bold"))
  
  height_labels <- c("Very\nLow", "Low", "Low-\nMod", "Mod", "High")
  heights <- c(0.4, 0.6, 0.8, 1.0, 1.2)
  for(i in 1:5) {
    x_pos <- 0.38 + (i-1)*0.047
    grid.rect(x = x_pos, y = 0.65, width = 0.015, 
             height = heights[i]/12,
             gp = gpar(fill = "#9E9E9E", col = "black"))
    grid.text(height_labels[i], x = x_pos, y = 0.50,
             gp = gpar(fontsize = 9, fontface = "bold"))
  }
  
  # Draw color legend
  grid.text("BAR COLOR = Effect Direction", x = 0.80, y = 0.85,
           gp = gpar(fontsize = 14, fontface = "bold"))
  
  color_labels <- c("Positive", "Mixed", "Negative", "No Data", "Descriptive")
  colors <- c("#2E7D32", "#F57C00", "#C62828", "#757575", "#1976D2")
  for(i in 1:5) {
    x_pos <- 0.68 + (i-1)*0.055
    grid.rect(x = x_pos, y = 0.65, width = 0.020, height = 0.10,
             gp = gpar(fill = colors[i], col = "black"))
    grid.text(color_labels[i], x = x_pos, y = 0.50,
             gp = gpar(fontsize = 10, fontface = "bold"))
  }
  
  # Notes
  grid.text(
    "Reading the bars: Study number (in circle) and year (in brackets) appear inside each bar | Sample size (n=X) shown below bar",
    x = 0.5, y = 0.15,
    gp = gpar(fontsize = 10, fontface = "italic")
  )
}

# ============================================================================
# ARRANGE FINAL PLOT - 3x2 GRID + LEGEND
# ============================================================================

# Open PNG device
png("/mnt/user-data/outputs/harvest_matrix_from_R.png", 
    width = 3200, height = 2000, res = 150)

# Create layout
layout_matrix <- rbind(
  c(1, 2, 3),
  c(4, 5, 6),
  c(7, 7, 7)
)

grid.arrange(
  panels[[1]], panels[[2]], panels[[3]],
  panels[[4]], panels[[5]], panels[[6]],
  layout_matrix = layout_matrix,
  heights = c(1, 1, 0.3)
)

# Add legend (requires grid.arrange with custom grob)
# Alternatively, add legend manually with grid functions

dev.off()

# ============================================================================
# ALTERNATIVE: SAVE INDIVIDUAL PANELS
# ============================================================================

# Save each panel separately if needed
for(i in 1:6) {
  ggsave(
    filename = paste0("/mnt/user-data/outputs/panel_", i, "_", 
                     gsub("[/ ]", "_", interventions_list[i]), ".png"),
    plot = panels[[i]],
    width = 8, height = 6, dpi = 300
  )
}

print("✅ Harvest plot matrix created from R!")
print("Main file: harvest_matrix_from_R.png")
print("Individual panels saved as: panel_1_*.png through panel_6_*.png")

# ============================================================================
# NOTES FOR CUSTOMIZATION
# ============================================================================

# To modify:
# 1. Change colors: Edit effect_colors vector
# 2. Adjust spacing: Change bar_spacing value in create_panel()
# 3. Modify quality heights: Edit quality_levels vector
# 4. Change sample size widths: Edit sample_to_width() function
# 5. Add more interventions: Add rows to studies dataframe
# 6. Adjust panel layout: Modify layout_matrix for different grid sizes
