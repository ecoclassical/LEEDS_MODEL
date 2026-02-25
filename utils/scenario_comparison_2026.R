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
        labels = c("Immediate", "Short term", "Long term")
      )
    ) %>%
    dplyr::left_join(
      variable.table %>% dplyr::select(dimension, label, name, unit),
      by = c("dimension", "variable" = "label")
    )

  # ---- optional scenario enrichment ----
  if (!is.null(sc)) {
    if (!"shock" %in% names(sc)) {
      stop("sc must contain a 'shock' column")
    }
    dff <- dplyr::left_join(dff, sc, by = "shock")
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

  plots <- lapply(cats, function(cc) {
    ggplot2::ggplot(
      dplyr::filter(dff, category == cc),
      ggplot2::aes(term, value, fill = display_scenario)
    ) +
      ggplot2::geom_col(
        position = ggplot2::position_dodge(width = 0.8),
        width = 0.7
      ) +
      ggplot2::facet_grid(display_name ~ region, scales = "free_y") +
      ggplot2::labs(title = cc, y = NULL, x = NULL, fill = "Scenario") +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "lightgray"
      )
  })

  p.comparison <- patchwork::wrap_plots(
    plots,
    ncol = 1,
    guides = "collect"
  ) &
    ggplot2::theme(legend.position = "right")

  # ---- optional save ----
  if (!is.null(workspace_dir)) {
    ggplot2::ggsave(
      filename = file.path(workspace_dir, filename),
      plot = p.comparison,
      width = 10,
      height = 14,
      units = "in"
    )
  }

  return(p.comparison)
}
