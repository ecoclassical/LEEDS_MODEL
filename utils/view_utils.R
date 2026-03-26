plot_target_fit <- function(
  target_result,
  title    = "Baseline Calibration",
  output   = "output/baseline_fit.png",
  width    = 12, height = 7, dpi = 150
) {
  var_labels <- c(
    c          = "Consumption",   id         = "Investment",
    g          = "Gov Spending",  rex        = "Exports",
    imp        = "Imports",       M_TOT_int  = "Material Use",
    fd         = "Final Demand",  va         = "Value Added",
    go         = "Gross Output",  gdef       = "Gov Deficit",
    debt_gdp   = "Debt / GDP",    b_s        = "Net Worth"
  )

  df <- as.data.frame(t(target_result$table), stringsAsFactors = FALSE)
  df$variable <- rownames(df)
  df$ratio    <- as.numeric(df$ratio)
  df$value    <- as.numeric(df$value)
  df$target_v <- as.numeric(df$target)
  df$region   <- ifelse(startsWith(df$variable, "Z1"), "EU (Z1)", "RoW (Z2)")
  df$var_name <- sub("^Z[12]_", "", df$variable)
  df$label    <- ifelse(
    !is.na(var_labels[df$var_name]),
    var_labels[df$var_name],
    df$var_name
  )

  # Cap extreme ratios (gdef ≈ ±27) for display; annotate them
  cap <- 2.5
  df$ratio_disp  <- pmax(pmin(df$ratio, cap), -cap)
  df$out_of_range <- abs(df$ratio) > cap
  df$annot       <- ifelse(df$out_of_range, sprintf("%.1f×", df$ratio), NA_character_)

  # Colour: green if within 5%, amber within 15%, red otherwise
  df$fit_band <- cut(
    abs(df$ratio - 1),
    breaks = c(0, 0.05, 0.15, Inf),
    labels = c("good (±5%)", "ok (±15%)", "poor (>15%)")
  )

  p <- ggplot(df, aes(
    x     = ratio_disp,
    y     = reorder(label, ratio_disp),
    color = fit_band,
    shape = region
  )) +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.6,
               color = "grey50") +
    geom_segment(aes(x = 1, xend = ratio_disp,
                     yend = reorder(label, ratio_disp)),
                 linewidth = 0.5, alpha = 0.4) +
    geom_point(size = 3.5) +
    geom_text(
      aes(label = annot), na.rm = TRUE,
      hjust = -0.25, size = 3, fontface = "italic"
    ) +
    facet_wrap(~region, ncol = 2) +
    scale_color_manual(
      values = c("good (±5%)" = "#2ca25f",
                 "ok (±15%)"  = "#fe9929",
                 "poor (>15%)"= "#d7191c"),
      name = "Calibration"
    ) +
    scale_shape_manual(values = c("EU (Z1)" = 16, "RoW (Z2)" = 17),
                       guide = "none") +
    scale_x_continuous(
      limits = c(NA, cap + 0.4),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    labs(
      title    = title,
      subtitle = sprintf(
        "Total fit: %.4f  |  Target fit: %.4f  (lower = better)",
        target_result$total.fit, target_result$fitness
      ),
      x = "Simulated / Target  (capped at ±2.5; labelled if beyond)",
      y = NULL
    ) +
    theme_leeds() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())

  ggsave(output, p, width = width, height = height, dpi = dpi)
  message("Saved: ", output)
  invisible(p)
}

theme_leeds <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_text(face = "bold"),
      strip.text.y = element_text(angle = 0),
      plot.subtitle = element_text(face = "italic", size = 10)
    )
}

plot.selected.vars <- function(res, selected.list, chunk_size = 12) {
  vars_all <- unique(unlist(selected.list, use.names = FALSE))

  chunks <- split(vars_all, ceiling(seq_along(vars_all) / chunk_size))

  for (k in seq_along(chunks)) {
    cli::cli_h2(sprintf("Selected vars (page %d / %d)", k, length(chunks)))
    plot.vars(res, chunks[[k]])
  }

  invisible(vars_all)
}

plot.selected.by_group <- function(res, selected.list) {
  for (nm in names(selected.list)) {
    cli::cli_h2(nm)
    plot.vars(res, unique(selected.list[[nm]]))
  }
  invisible(NULL)
}

plot.vars <- function(res, vars) {
  dt <- reshape2::melt(res$sim, varnames = c('var', 'time')) %>%
    filter(grepl('Z1', var) | grepl('Z2', var)) %>%
    filter(!grepl('\\-', var))
  dt$region <- substr(dt$var, 1, 2)
  dt$area <- res$initial$countries[as.numeric(substr(dt$var, 2, 2))]
  dt$variable <- substring(dt$var, 4)
  dt <- dt %>% filter(variable %in% vars)
  dt$unit <- setNames(variable.table$unit, variable.table$label)[dt$variable]
  dt$name <- ifelse(
    dt$unit == '',
    paste0(
      setNames(variable.table$name, variable.table$label)[dt$variable],
      '\n(',
      dt$variable,
      ')'
    ),
    paste0(
      setNames(variable.table$name, variable.table$label)[dt$variable],
      '\n(',
      dt$variable,
      ', in ',
      dt$unit,
      ')'
    )
  )
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]

  print(
    dt %>%
      ggplot(aes(x = time, y = value, color = area)) +
      geom_hline(
        data = filter(target.set, var.label %in% vars),
        mapping = aes(yintercept = value),
        linetype = 'dashed',
        linewidth = .5
      ) +
      facet_wrap(~name, scales = 'free') +
      geom_line() +
      theme_leeds()
  )
}


view.shock <- function(
  res,
  shock.vars,
  shock.sectors,
  shock.title,
  t0 = 1,
  tf = res$last_period %||% ncol(res$simulation)
) {
  # --- helpers ---
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  stopifnot(!is.null(res$simulation), !is.null(res$initial))

  # Build labels: e.g. "Z1_beta-31", "Z2_beta-31", ...
  shock.labels <- paste0(rep(zlabs, each = length(shock.vars)), "_", shock.vars)

  # Bilateral MRIO fallback: "Z1_beta-31" -> "Z1_beta_Z1-31" etc.
  missing <- setdiff(shock.labels, rownames(res$simulation))
  if (length(missing) > 0) {
    shock.labels <- gsub(
      "_(beta|sigma|iota_g|iota)-",
      "_\\1_Z1-",
      shock.labels
    )
    still.missing <- setdiff(shock.labels, rownames(res$simulation))
    if (length(still.missing) > 0) {
      stop(
        "view.shock(): missing labels in res$simulation: ",
        paste(still.missing, collapse = ", ")
      )
    }
  }

  df <- res$simulation[shock.labels, , drop = FALSE] %>%
    melt(varnames = c("var", "time"))

  df$region <- substr(df$var, 1, 2)
  df$area <- res$initial$countries[as.numeric(substr(df$var, 2, 2))]
  df$variable <- substring(df$var, 4)

  # Name mapping from the *scenario's* initial, not global `initial`
  name_map <- setNames(res$initial$vars$name, res$initial$vars$label)
  df$name <- name_map[as.character(df$var)]

  # Industry mapping (if shock.vars encodes sector ids like beta-31 etc.)
  df$industry <- setNames(shock.sectors, shock.vars)[df$variable]

  df$full.name <- sapply(df$name, insert.line.break)
  idx <- !is.na(df$industry)
  if (any(idx)) {
    df$full.name[idx] <- paste0(
      df$name[idx],
      ",\n",
      res$initial$sectors[df$industry[idx]]
    )
  }
  df$full.name <- paste0(df$full.name, "\n(", df$variable, ")")

  # Shock time from this scenario
  t_shock <- res$initial$pars["t.shock", "value"]

  df %>%
    dplyr::filter(time >= t0 & time <= tf) %>%
    ggplot(aes(x = time, y = value, color = area)) +
    facet_grid(full.name ~ ., scales = "free") +
    geom_line() +
    geom_vline(
      xintercept = t_shock,
      linetype = "dashed",
      linewidth = .4
    ) +
    theme(strip.text.y.right = element_text(angle = 0)) +
    labs(
      title = shock.title,
      subtitle = "Shocked Variables. Vertical dashed line indicates shock time"
    ) +
    theme_leeds()
}

view.A <- function(
  res,
  pairs,
  title = "IO coefficients (A) over time",
  t0 = 1,
  tf = res$last_period %||% dim(res$A.matrix)[3],
  sector_list = NULL, # NEW: data.frame with sector_code + label
  label_rows = TRUE,
  label_cols = TRUE,
  max_label_chars = 45 # NEW: safety truncation
) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  stopifnot(!is.null(res$A.matrix))
  A <- res$A.matrix
  stopifnot(length(dim(A)) == 3)

  # pairs: data.frame/list with columns row, col (1-based indices)
  if (is.matrix(pairs)) {
    pairs <- as.data.frame(pairs)
  }
  if (is.list(pairs) && !is.data.frame(pairs)) {
    pairs <- as.data.frame(pairs)
  }
  stopifnot(all(c("row", "col") %in% names(pairs)))

  Tn <- dim(A)[3]
  tf <- min(tf, Tn)
  tt <- t0:tf

  # --- label lookup ---
  sector_label <- function(idx) {
    # idx is 1..K and corresponds to sector_code in your mapping
    if (
      !is.null(sector_list) &&
        all(c("sector_code", "label") %in% names(sector_list))
    ) {
      hit <- sector_list$label[match(idx, sector_list$sector_code)]
      if (!is.na(hit) && nzchar(hit)) return(hit)
    }

    # fallback: original long sector names if present
    if (
      !is.null(res$initial$sectors) &&
        length(res$initial$sectors) >= idx &&
        !is.na(res$initial$sectors[idx]) &&
        nzchar(res$initial$sectors[idx])
    ) {
      return(res$initial$sectors[idx])
    }

    # final fallback
    paste0("Sector ", idx)
  }

  trunc <- function(x) {
    if (is.na(x) || !nzchar(x)) {
      return(x)
    }
    if (nchar(x) <= max_label_chars) {
      return(x)
    }
    paste0(substr(x, 1, max_label_chars - 1), "\u2026")
  }

  row_lab <- function(r) {
    if (isTRUE(label_rows)) {
      paste0("row ", r, ": ", trunc(sector_label(r)))
    } else {
      paste0("row ", r)
    }
  }
  col_lab <- function(c) {
    if (isTRUE(label_cols)) {
      paste0("col ", c, ": ", trunc(sector_label(c)))
    } else {
      paste0("col ", c)
    }
  }

  out <- lapply(seq_len(nrow(pairs)), function(i) {
    r <- pairs$row[i]
    c <- pairs$col[i]

    if (r < 1 || r > dim(A)[1] || c < 1 || c > dim(A)[2]) {
      stop(
        "view.A(): (row,col)=(",
        r,
        ",",
        c,
        ") out of bounds for A[",
        dim(A)[1],
        "x",
        dim(A)[2],
        "]."
      )
    }

    data.frame(
      time = tt,
      value = A[r, c, tt],
      row = r,
      col = c,
      coef = paste0("A[", r, ",", c, "]"),
      label = paste0(row_lab(r), "\n\u2192\n", col_lab(c))
    )
  }) |>
    dplyr::bind_rows()

  t_shock <- res$initial$pars["t.shock", "value"]

  out |>
    ggplot2::ggplot(ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(label ~ ., scales = "free_y") +
    ggplot2::geom_vline(
      xintercept = t_shock,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    ggplot2::theme(strip.text.y.right = ggplot2::element_text(angle = 0)) +
    ggplot2::labs(
      title = title,
      subtitle = "Selected A[row,col] entries. Dashed line = shock time.",
      y = "Coefficient value",
      x = "time"
    ) +
    theme_leeds()
}

view.A_row_heatmap <- function(
  res,
  rows,
  t0 = 1,
  tf = res$last_period %||% dim(res$A.matrix)[3],
  title = "A row heatmap over time"
) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  A <- res$A.matrix
  tf <- min(tf, dim(A)[3])
  tt <- t0:tf

  df <- lapply(rows, function(r) {
    mat <- A[r, , tt, drop = FALSE]
    # mat is 1 x ncol x T
    expand.grid(
      time = tt,
      col = seq_len(dim(A)[2])
    ) %>%
      dplyr::mutate(
        value = as.vector(mat[1, , ]),
        row = r
      )
  }) %>%
    dplyr::bind_rows()

  ggplot(df, aes(x = time, y = col, fill = value)) +
    geom_tile() +
    facet_grid(row ~ ., scales = "free_y") +
    labs(title = title, x = "time", y = "column", fill = "A") +
    theme_leeds()
}

# Plot Variables in Shocks
view.vars <- function(data, viz.vars, var.label, shock.title) {
  df <- data %>% filter(time >= t0 & time <= tf)
  df$ref.value <- rep(
    filter(df, time == (t.shock - 1)) %>% select(value),
    times = tf - t0 + 1
  ) %>%
    unlist
  df$norm.value <- df$value / df$ref.value

  filter(df, variable %in% viz.vars) %>%
    ggplot(aes(x = time, y = value, color = area, linetype = scenario)) +
    ylab('Normalized Value') +
    facet_grid(name ~ area, scales = 'free_y') +
    geom_line() +
    theme(strip.text.y.right = element_text(angle = 0)) +
    geom_vline(xintercept = t.shock, linetype = 'dashed', linewidth = .4) +
    labs(
      title = shock.title,
      subtitle = paste(
        'Selected',
        var.label,
        'Indicators. Vertical dashed line indicates shock time'
      )
    ) +
    coord_cartesian(xlim = c(t0, tf)) +
    theme_leeds()
}

view.scaled.vars <- function(data, viz.vars, var.label, shock.title) {
  df <- data %>% filter(time >= t0 & time <= tf)
  df$ref.value <- rep(
    filter(df, time == (t.shock - 1)) %>% select(value),
    times = tf - t0 + 1
  ) %>%
    unlist
  df$norm.value <- df$value / df$ref.value

  filter(df, variable %in% viz.vars) %>%
    ggplot(aes(x = time, y = norm.value, color = area, linetype = scenario)) +
    ylab('Normalized Value') +
    facet_grid(name ~ area) +
    geom_line() +
    theme(strip.text.y.right = element_text(angle = 0)) +
    geom_vline(xintercept = t.shock, linetype = 'dashed', linewidth = .4) +
    labs(
      title = shock.title,
      subtitle = paste(
        'Selected',
        var.label,
        'Indicators. Vertical dashed line indicates shock time'
      )
    ) +
    coord_cartesian(xlim = c(t0, tf)) +
    theme_leeds()
}

plot_selected_vars <- function(
  baseline,
  scenario,
  selected.list,
  shock_title,
  selected = c('n', 'c', 'shw', 'va', 'cab', 'nf', 'gdef', 'tb', 'go'),
  t_shock,
  print_blocks = TRUE
) {
  # Build long df once
  df <- shock.long.new(baseline, scenario)

  # Optionally print block plots (scaled unless gdef is present)
  if (isTRUE(print_blocks) && length(selected.list) > 0) {
    for (i in seq_along(selected.list)) {
      vars_i <- selected.list[[i]]
      label_i <- names(selected.list)[i]

      if ('gdef' %in% vars_i) {
        print(view.vars(
          data = df,
          viz.vars = vars_i,
          var.label = label_i,
          shock.title = shock_title
        ))
      } else {
        print(view.scaled.vars(
          data = df,
          viz.vars = vars_i,
          var.label = label_i,
          shock.title = shock_title
        ))
      }
    }
  }

  # Return the "selected indicators" plot
  p.selected.vars <- df |>
    dplyr::filter(.data$variable %in% selected) |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$time,
      y = .data$value,
      color = .data$area,
      linetype = .data$scenario
    )) +
    ggplot2::facet_wrap(~ .data$name, scales = "free") +
    ggplot2::geom_line() +
    ggplot2::geom_vline(
      xintercept = t_shock,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    ggplot2::labs(
      title = shock_title,
      subtitle = "Selected Indicators. Vertical dashed line indicates shock time"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(face = "italic", size = 10),
      legend.title = ggplot2::element_text(face = "bold")
    ) +
    theme_leeds()

  return(p.selected.vars)
}
