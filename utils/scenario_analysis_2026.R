# R/scenario_analysis_2026.R

scenario_analysis_2026_plot <- function(
  shock_df,
  shock_title,
  variable.table,
  shock_table_1_filename = NULL,
  shock_table_2_filename = NULL,
  shock_subtitle = "Teal = positive change, red = negative change (relative to baseline)"
) {
  # Packages (assumed loaded, but safe)
  library(reshape2)
  library(dplyr)
  library(stringr)
  library(patchwork)

  # --- helpers ---
  replace_second_space <- function(x) {
    space_positions <- stringr::str_locate_all(x, " ")[[1]]
    if (nrow(space_positions) >= 2) {
      second_space_position <- space_positions[2, "start"]
      x <- stringr::str_sub(x, 1, second_space_position - 1) %>%
        stringr::str_c(
          "\n",
          stringr::str_sub(x, second_space_position + 1, stringr::str_length(x))
        )
    }
    x
  }

  # --- data prep ---
  dff <- shock_df %>%
    dplyr::rename(category = Name, variable = Variable) %>%
    reshape2::melt(
      id.vars = c("variable", "category"),
      variable.name = "region_term",
      value.name = "value"
    ) %>%
    dplyr::mutate(
      value = as.numeric(value),
      dimension = "aggregate",
      term = stringr::str_extract(
        region_term,
        "Immediate|Short\\.Term|Long\\.Term"
      ),
      region = stringr::str_extract(region_term, "Z1|Z2") %>%
        dplyr::recode(Z1 = "Core", Z2 = "Periphery"),
      term = factor(
        term,
        levels = c("Immediate", "Short.Term", "Long.Term"),
        labels = c("Short\nTerm", "Medium\nTerm", "Long\nTerm")
      )
    ) %>%
    dplyr::left_join(
      variable.table %>% dplyr::select(dimension, label, name, unit),
      by = c("dimension", "variable" = "label")
    ) %>%
    dplyr::mutate(
      display_name = paste0(
        sapply(name, replace_second_space),
        "\n(",
        variable,
        ", ",
        unit,
        ")"
      )
    )

  # --- plots ---
  p1 <- dff %>%
    dplyr::filter(
      category == "Macroeconomic",
      variable != "lf",
      variable != "lh"
    ) %>%
    ggplot(aes(x = term, y = value, fill = value > 0)) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    facet_grid(display_name ~ region, scales = "free_y") +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    scale_fill_manual(values = c("TRUE" = "#17B3B8", "FALSE" = "#F05A50")) +
    theme(
      strip.text.y.right = element_text(angle = 0),
      plot.title = element_text(face = "bold", size = 16)
    ) +
    labs(title = "Economic Dimension") +
    ylab("")

  p11 <- dff %>%
    dplyr::filter(variable %in% c("lf", "lh")) %>%
    ggplot(aes(x = term, y = value, fill = value > 0)) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    facet_grid(display_name ~ region, scales = "free_y") +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    scale_fill_manual(values = c("TRUE" = "#17B3B8", "FALSE" = "#F05A50")) +
    theme(
      strip.text.y.right = element_text(angle = 0),
      plot.title = element_text(face = "bold", size = 16)
    ) +
    labs(title = "Financial Dimension") +
    ylab("")

  p2 <- dff %>%
    dplyr::filter(category == "Social") %>%
    ggplot(aes(x = term, y = value, fill = value > 0)) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    facet_grid(display_name ~ region, scales = "free_y") +
    scale_fill_manual(values = c("TRUE" = "#17B3B8", "FALSE" = "#F05A50")) +
    theme(
      strip.text.y.right = element_text(angle = 0),
      plot.title = element_text(face = "bold", size = 16)
    ) +
    labs(title = "Social Dimension") +
    ylab("")

  p3 <- dff %>%
    dplyr::filter(category == "Ecological") %>%
    ggplot(aes(x = term, y = value, fill = value < 0)) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    facet_grid(display_name ~ region, scales = "free_y") +
    scale_fill_manual(values = c("TRUE" = "#17B3B8", "FALSE" = "#F05A50")) +
    theme(
      strip.text.y.right = element_text(angle = 0),
      plot.title = element_text(face = "bold", size = 16)
    ) +
    labs(title = "Ecological Dimension") +
    ylab("")

  combined_plot2 <- ((p1 / p11) +
    plot_layout(heights = c(0.7, 0.3)) |
    (p2 / p3)) +
    patchwork::plot_annotation(
      title = shock_title,
      subtitle = shock_subtitle,
      theme = theme(
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic")
      )
    )

  print(combined_plot2)

  # Optional PDF outputs if filenames provided
  if (!is.null(shock_table_2_filename) && nzchar(shock_table_2_filename)) {
    pdf(shock_table_2_filename, width = 13, height = 11)
    print(combined_plot2)
    dev.off()
  }

  # You had combined_plot1 logic; keeping it as optional if you later reintroduce it:
  # if (!is.null(shock_table_1_filename) && exists("combined_plot1")) { ... }

  invisible(combined_plot2)
}
