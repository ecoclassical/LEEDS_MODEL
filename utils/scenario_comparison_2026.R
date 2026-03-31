build_policy_comparison_plot <- function(
  df,
  variable.table,
  sc = NULL,
  workspace_dir = NULL,
  filename = "p_comparison.pdf",
  rho = 0.2,
  colors = NULL,
  region_filter = NULL,   # "EU", "RoW", or NULL (both)
  title = "Comparison of Circular Transition Scenarios",
  ncol = 1,
  var_name_order = NULL,  # optional character vector of `name` values in desired facet row order
  col_assignment = NULL   # integer vector length == n_categories: which column each category goes to (1 or 2)
) {
  # ---- helper ----
  replace_second_space <- function(x) {
    if (is.na(x) || !nzchar(x)) {
      return(x)
    }
    space_positions <- stringr::str_locate_all(x, " ")[[1]]
    if (nrow(space_positions) >= 2) {
      pos <- space_positions[2, "start"]
      x <- paste0(substr(x, 1, pos - 1), "\n", substr(x, pos + 1, nchar(x)))
    }
    x
  }

  # ---- reshape + enrich ----
  dff <- df %>%
    dplyr::rename(category = Name, variable = Variable) %>%
    reshape2::melt(
      id.vars = c("variable", "category", "scenario"),
      variable.name = "region_term",
      value.name = "value"
    ) %>%
    dplyr::mutate(
      value = as.numeric(value),
      term = stringr::str_extract(
        region_term,
        "Immediate|Short\\.Term|Long\\.Term"
      ),
      region = stringr::str_extract(region_term, "Z1|Z2") %>%
        dplyr::recode(Z1 = "EU", Z2 = "RoW"),
      dimension = "aggregate",
      shock = as.integer(stringr::str_extract(
        scenario,
        "(?<=Scenario\\s)\\d+"
      ))
    ) %>%
    dplyr::mutate(
      term = factor(
        term,
        levels = c("Immediate", "Short.Term", "Long.Term"),
        labels = c("Immediate", "Short\nTerm", "Long\nTerm")
      )
    ) %>%
    dplyr::left_join(
      variable.table %>% dplyr::select(dimension, label, name, unit),
      by = c("dimension", "variable" = "label")
    ) %>%
    dplyr::left_join(sc, by = "shock")

  # ---- optional region filter ----
  if (!is.null(region_filter)) {
    dff <- dff %>% dplyr::filter(region == region_filter)
  }

  # ---- display engineering ----
  dff <- dff %>%
    dplyr::mutate(
      display_name = ifelse(
        !is.na(name) & grepl("\n", name, fixed = TRUE),
        name,
        paste0(
          ifelse(is.na(name), variable, sapply(name, replace_second_space)),
          "\n(",
          variable,
          ifelse(!is.na(unit) & nzchar(unit), paste0(", ", unit), ""),
          ")"
        )
      ),
      display_scenario = stringr::str_replace(
        scenario,
        "( \\| .*?)( \\| )",
        "\\1\n"
      )
    ) %>%
    dplyr::filter(!is.na(value))

  # ---- optional facet row ordering ----
  if (!is.null(var_name_order)) {
    name_map <- dff %>% dplyr::distinct(name, display_name)
    ordered_levels <- vapply(var_name_order, function(n) {
      idx <- which(name_map$name == n)
      if (length(idx) > 0) name_map$display_name[idx[1]] else NA_character_
    }, character(1))
    ordered_levels <- ordered_levels[!is.na(ordered_levels)]
    # Prepend all other display_names not in var_name_order so they keep their order
    all_levels <- unique(as.character(dff$display_name))
    full_levels <- c(setdiff(all_levels, ordered_levels), ordered_levels)
    dff <- dff %>%
      dplyr::mutate(display_name = factor(display_name, levels = full_levels))
  }

  # ---- FIX NUMERIC ORDERING ----
  dff <- dff %>%
    dplyr::arrange(shock) %>%
    dplyr::mutate(
      display_scenario = factor(
        display_scenario,
        levels = unique(display_scenario[order(shock)])
      )
    )

  cats <- unique(dff$category)

  # Build color scale (per-scenario, keyed by shock integer if colors provided)
  if (!is.null(colors)) {
    level_map <- dff %>%
      dplyr::distinct(shock, display_scenario) %>%
      dplyr::mutate(color = colors[as.character(shock)])
    color_vec <- setNames(level_map$color, as.character(level_map$display_scenario))
    fill_scale <- ggplot2::scale_fill_manual(values = color_vec, drop = FALSE)
  } else {
    fill_scale <- ggplot2::scale_fill_discrete(drop = FALSE)
  }

  # ---- facet formula: drop region column when filtering to one region ----
  facet_formula <- if (is.null(region_filter)) {
    display_name ~ domain + region
  } else {
    display_name ~ domain
  }

  dodge <- ggplot2::position_dodge2(width = 0.8, preserve = "single")

  plots <- lapply(cats, function(cc) {
    ggplot2::ggplot(
      dplyr::filter(dff, category == cc),
      ggplot2::aes(term, value, fill = display_scenario)
    ) +
      ggplot2::geom_col(
        position = dodge,
        width = 0.7
      ) +
      fill_scale +
      ggplot2::facet_grid(facet_formula, scales = "free_y") +
      ggplot2::labs(title = cc, y = "% deviation from baseline", x = NULL, fill = "Scenario") +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "darkgray"
      )
  })

  # ---- subtitle ----
  subtitle_text <- paste0("Shock Parameter \u03c1 = ", rho)

  annotation <- patchwork::plot_annotation(
    title = title,
    subtitle = subtitle_text,
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(size = 24, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 17),
      plot.title.position = "plot"
    )
  )

  if (!is.null(col_assignment) && length(col_assignment) == length(plots)) {
    # Compute per-category variable counts for proportional heights
    cat_vars <- sapply(cats, function(cc)
      length(unique(dff$display_name[dff$category == cc])))

    # Left column: no legend, proportional heights
    left_idx   <- which(col_assignment == 1)
    left_plots <- lapply(plots[left_idx], function(p)
      p + ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(size = 17)
      ))
    col_left <- patchwork::wrap_plots(
      left_plots,
      ncol    = 1,
      heights = cat_vars[left_idx]
    )

    # Right column: single collected legend, proportional heights, spacing between entries
    right_idx   <- which(col_assignment == 2)
    right_plots <- lapply(plots[right_idx], function(p)
      p + ggplot2::theme(
        legend.position      = "right",
        plot.title           = ggplot2::element_text(size = 17),
        legend.title         = ggplot2::element_text(size = 13),
        legend.text          = ggplot2::element_text(size = 11),
        legend.key.height    = ggplot2::unit(0.5, "cm"),
        legend.key.spacing.y = ggplot2::unit(0.2, "cm")
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, byrow = TRUE)))
    col_right <- patchwork::wrap_plots(
      right_plots,
      ncol    = 1,
      heights = cat_vars[right_idx],
      guides  = "collect"
    )

    p.comparison <- (col_left | col_right) + annotation
  } else {
    p.comparison <- patchwork::wrap_plots(
      plots,
      ncol   = ncol,
      guides = "collect"
    ) &
      ggplot2::theme(
        legend.position = "right",
        plot.title = ggplot2::element_text(size = 17)
      ) &
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1))

    p.comparison <- p.comparison + annotation
  }

  # ---- optional save ----
  if (!is.null(workspace_dir)) {
    ggplot2::ggsave(
      filename = file.path(workspace_dir, filename),
      plot = p.comparison,
      width = 14,
      height = 18,
      units = "in"
    )
  }

  return(p.comparison)
}


build_policy_comparison_plot_avg_terms <- function(
  df,
  variable.table,
  sc = NULL,
  workspace_dir = NULL,
  filename = "p_comparison_avg_terms.pdf",
  avg_fun = function(x) mean(x, na.rm = TRUE),
  rho = 0.2,
  colors = NULL
) {
  # ---- helper ----
  replace_second_space <- function(x) {
    if (is.na(x) || !nzchar(x)) {
      return(x)
    }
    space_positions <- stringr::str_locate_all(x, " ")[[1]]
    if (nrow(space_positions) >= 2) {
      pos <- space_positions[2, "start"]
      x <- paste0(substr(x, 1, pos - 1), "\n", substr(x, pos + 1, nchar(x)))
    }
    x
  }

  # ---- reshape + enrich (long) ----
  dff <- df %>%
    dplyr::rename(category = Name, variable = Variable) %>%
    reshape2::melt(
      id.vars = c("variable", "category", "scenario"),
      variable.name = "region_term",
      value.name = "value"
    ) %>%
    dplyr::mutate(
      value = as.numeric(value),
      term = stringr::str_extract(
        region_term,
        "Immediate|Short\\.Term|Long\\.Term"
      ),
      region = stringr::str_extract(region_term, "Z1|Z2") %>%
        dplyr::recode(Z1 = "EU", Z2 = "RoW"),
      dimension = "aggregate",
      shock = as.integer(stringr::str_extract(scenario, "(?<=Scenario\\s)\\d+"))
    ) %>%
    dplyr::left_join(
      variable.table %>% dplyr::select(dimension, label, name, unit),
      by = c("dimension", "variable" = "label")
    )

  if (!is.null(sc)) {
    dff <- dff %>% dplyr::left_join(sc, by = "shock")
  }

  # ---- display engineering ----
  dff <- dff %>%
    dplyr::mutate(
      display_name = ifelse(
        !is.na(name) & grepl("\n", name, fixed = TRUE),
        name,
        paste0(
          ifelse(is.na(name), variable, sapply(name, replace_second_space)),
          "\n(",
          variable,
          ifelse(!is.na(unit) & nzchar(unit), paste0(", ", unit), ""),
          ")"
        )
      ),
      display_scenario = stringr::str_replace(
        scenario,
        "( \\| .*?)( \\| )",
        "\\1\n"
      )
    ) %>%
    dplyr::filter(!is.na(value))

  # ---- FIX NUMERIC ORDERING (scenario factor by shock) ----
  dff <- dff %>%
    dplyr::arrange(shock) %>%
    dplyr::mutate(
      display_scenario = factor(
        display_scenario,
        levels = unique(display_scenario)
      ),
      # keep full levels available across facets
      display_scenario = droplevels(display_scenario)
    )

  # ---- average over term (collapse Immediate/Short/Long) ----
  # Keep domain/region/etc so facets remain meaningful.
  dff_avg <- dff %>%
    dplyr::group_by(
      category,
      variable,
      display_name,
      display_scenario,
      shock,
      domain,
      region
    ) %>%
    dplyr::summarise(
      value = avg_fun(value),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # ensure same levels across panels
      display_scenario = factor(
        display_scenario,
        levels = rev(levels(dff$display_scenario))
      )
    )

  cats <- unique(dff_avg$category)

  plots <- lapply(cats, function(cc) {
    ggplot2::ggplot(
      dplyr::filter(dff_avg, category == cc),
      ggplot2::aes(x = value, y = display_scenario, fill = region)
    ) +
      ggplot2::geom_bar(width = 0.75, position = "dodge", stat = "identity") +
      ggplot2::facet_grid(~display_name, scales = 'free_x') +
      ggplot2::labs(title = cc, y = NULL, x = NULL) +
      ggplot2::geom_vline(
        xintercept = 0,
        linetype = "dashed",
        color = "darkgray"
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
      )
  })

  p.comparison.avg <- patchwork::wrap_plots(
    plots,
    ncol = 1,
    guides = "collect"
  ) &
    ggplot2::theme(legend.position = "top") &
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))

  p.comparison.avg <- p.comparison.avg +
    patchwork::plot_annotation(
      title = "Comparison of Circular Transition Scenarios",
      subtitle = paste0("Shock Parameter \u03c1 = ", rho)
    )

  # ---- optional save ----
  if (!is.null(workspace_dir)) {
    ggplot2::ggsave(
      filename = file.path(workspace_dir, filename),
      plot = p.comparison.avg,
      width = 10,
      height = 19,
      units = "in"
    )
  }

  p.comparison.avg
}
