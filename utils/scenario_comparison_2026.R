build_policy_comparison_plot <- function(
  df,
  variable.table,
  sc = NULL,
  workspace_dir = NULL,
  filename = "p_comparison.pdf"
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

  # ---- display engineering ----
  dff <- dff %>%
    dplyr::mutate(
      display_name = paste0(
        sapply(name, replace_second_space),
        "\n(",
        variable,
        ", ",
        unit,
        ")"
      ),
      display_scenario = stringr::str_replace(
        scenario,
        "( \\| .*?)( \\| )",
        "\\1\n"
      )
    ) %>%
    dplyr::filter(!is.na(value))

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
      ggplot2::scale_fill_discrete(drop = FALSE) +
      ggplot2::facet_grid(display_name ~ domain + region, scales = "free_y") +
      ggplot2::labs(title = cc, y = NULL, x = NULL, fill = "Scenario") +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "darkgray"
      )
  })

  p.comparison <- patchwork::wrap_plots(
    plots,
    ncol = 1,
    guides = "collect"
  ) &
    ggplot2::theme(legend.position = "top") &
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 5, byrow = TRUE)
    )

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
  avg_fun = function(x) mean(x, na.rm = TRUE)
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
      display_name = paste0(
        sapply(name, replace_second_space),
        "\n(",
        variable,
        ", ",
        unit,
        ")"
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
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
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
