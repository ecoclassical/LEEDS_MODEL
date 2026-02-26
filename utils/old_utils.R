# variable.table <<- read.csv('flexible_/data/Variable_Definitions.csv')
# variable.table <<- read.csv(paste0(directory, 'data/Variable_Definitions.csv'))
# scenario.table <- read.csv(paste0(directory, 'data/Scenario_List.csv'), row.names = 1)
# sectors <- c('Manufacturing', 'Agriculture', 'Services', 'Waste', 'Recycling')
# countries <- c('EU', 'RoW')

#### initialization ####
# source(paste0(directory, 'functions/auxiliary_july3.R'))
# identif <- paste0(directory, 'data/newvalues1.xlsx')
# initial <- load.new.init(identif)

theme_leeds <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_text(face = "bold"),
      strip.text.y = element_text(angle = 0),
      plot.subtitle = element_text(face = "italic", size = 10)
    )
}

#### Auxiliary Functions to Label Variables ####
z.lab <- function(variable) paste0(zlabs, '_', variable) # e.g. z.lab('c') returns c('Z1_c', 'Z2_c')
zk.lab <- function(variable) {
  unlist(lapply(zlabs, function(z) paste0(z, '_', variable, '-', 1:K)))
} # e.g. returns industry-level variables
zk.sum <- function(vec) {
  sapply(seq_len(N), function(i) {
    sum(vec[
      seq(1, by = K, length.out = N)[i]:(seq(1, by = K, length.out = N) +
        K -
        1)[i]
    ])
  })
}
# zk.sum <- function (vec) array(sapply(mapply(seq, cumsum(c(0, K[-length(K)])) + 1, cumsum(K), SIMPLIFY = FALSE), function (x) sum(vec[x])), dim = N, dimnames = list(zlabs)) # sums industry-level variables into country-level (i.e. from KN to N)
# zk.mean <- function (vec) array(sapply(mapply(seq, cumsum(c(0, K[-length(K)])) + 1, cumsum(K), SIMPLIFY = FALSE), function (x) mean(vec[x])), dim = N, dimnames = list(zlabs)) # sums industry-level variables into country-level (i.e. from KN to N)
zk.mean <- function(vec) {
  sapply(seq_len(N), function(i) {
    mean(vec[
      (seq(1, by = K, length.out = N))[i]:(seq(1, by = K, length.out = N) +
        K -
        1)[i]
    ])
  })
} # sums industry-level variables into country-level (i.e. from KN to N)
rev.zk.lab <- function(variable) {
  unlist(lapply(rev(zlabs), function(z) paste0(z, '_', variable, '-', 1:K)))
} # e.g. returns reverse industry-level variables
# rev.zk.lab <- function (variable) unlist(lapply(rev(zlabs), function (z) paste0(z, '_', variable, '-', 1 : K[z]))) # e.g. returns reverse industry-level variables

#### Packages ####
library(kableExtra)
library(reshape2)
library(dplyr)
library(ggplot2)
library(beepr)
library(cli)

#### function that plots variables ####
# DISPLAY THE PLOT OF A PARTICULAR CATEGORY, AGGREGATE-LEVEL
# ARGUMENTS results and category, e.g. plot.type(results, 'govt.central.bank')
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

#### SHOCK LONG ####

shock.long.new <- function(baseline, scenario) {
  # Baseline
  res <- baseline
  dt <- reshape2::melt(res$simulation, varnames = c('var', 'time')) %>%
    filter(grepl('Z1', var) | grepl('Z2', var)) %>%
    filter(!grepl('\\-', var))
  dt$region <- substr(dt$var, 1, 2)
  dt$area <- res$initial$countries[as.numeric(substr(dt$var, 2, 2))]
  dt$variable <- substring(dt$var, 4)
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
  dt$scenario <- 'baseline'
  dt$shock <- 0
  df <- dt

  # Shock
  res <- scenario
  dt <- reshape2::melt(res$simulation, varnames = c('var', 'time')) %>%
    filter(grepl('Z1', var) | grepl('Z2', var)) %>%
    filter(!grepl('\\-', var))
  dt$region <- substr(dt$var, 1, 2)
  dt$area <- res$initial$countries[as.numeric(substr(dt$var, 2, 2))]
  dt$variable <- substring(dt$var, 4)
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
  dt$scenario <- 'shock'
  dt$shock <- 1

  return(rbind(df, dt))
}

#### LOAD TARGET VALUES IN WIDE AND LONG FORMAT ####
load.target <- function(target.file, variable.table) {
  target.table.0 <- openxlsx::read.xlsx(
    target.file,
    sheet = 'Target',
    rowNames = TRUE
  ) /
    10000 # WIDE FORMAT
  target.set <- data.frame(
    area = rep(c('Z1', 'Z2'), times = dim(target.table.0)[2]),
    var.label = rep(
      c(
        'c',
        'id',
        'g',
        'rex',
        'imp',
        'M_TOT_int',
        'fd',
        'va',
        'go',
        'gdef',
        'debt_gdp',
        'b_s'
      ),
      each = 2
    ),
    value = unlist(target.table.0)
  )
  target.set$var <- paste0(target.set$area, '_', target.set$var)
  target.set$type <- setNames(variable.table$type, variable.table$label)[
    target.set$var.label
  ]
  target.set$variable <- setNames(variable.table$name, variable.table$label)[
    target.set$var.label
  ]
  target.set$unit <- setNames(variable.table$unit, variable.table$label)[
    target.set$var.label
  ]
  target.set$name <- ifelse(
    target.set$unit == '',
    paste0(target.set$variable, '\n(', target.set$var.label, ')'),
    paste0(
      target.set$variable,
      '\n(',
      target.set$var.label,
      ', in ',
      target.set$unit,
      ')'
    )
  )
  # LONG FORMAT
  rownames(target.set) <- target.set$var

  return(list(wide = target.table.0, long = target.set))
}

# Auxiliary Function that returns Instrument Values
return.instr.vals <- function(res) {
  foo <- dplyr::bind_rows(
    list(
      agg.vars = data.frame(
        label = sapply(instruments$var.agg, z.lab) %>% as.vector,
        value = round(
          res$simulation[sapply(instruments$var.agg, z.lab) %>% as.vector, 1],
          4
        ),
        row.names = NULL
      ),
      ind.vars = data.frame(
        label = sapply(instruments$var.ind, z.lab) %>% as.vector,
        value = round(
          sapply(instruments$var.ind, function(x) {
            zk.mean(res$simulation[zk.lab(x), 1])
          }) %>%
            as.vector,
          4
        ),
        row.names = NULL
      ),
      # ind.vars = data.frame(label = sapply(instruments$var.ind, zk.lab) %>% as.vector,
      #                       value = res$simulation[sapply(instruments$var.ind, zk.lab) %>% as.vector, 1],
      #                       row.names = NULL),
      parameter = data.frame(
        label = sapply(instruments$par, z.lab) %>% as.vector,
        value = round(
          res$initial$pars[
            sapply(instruments$par, z.lab) %>% as.vector,
            'value'
          ],
          4
        ),
        row.names = NULL
      )
    ),
    .id = 'item'
  )

  return(foo)
}

# Compute Target Table and Values
compute.target <- function(sim, select.target.vars = target.vars) {
  # select.target.vars <- c('imp', 'M_TOT_int')
  select.target.vars <- apply(
    expand.grid(zlabs, select.target.vars),
    1,
    function(x) paste0(x, collapse = "_")
  )
  df <- target.set[, c('var', 'value')]
  df$target <- sim$simulation[
    target.set$var,
    sim$initial$pars['nPeriods', 'value']
  ]
  df$ratio <- df$target / df$value

  total.fitness <- sum((1 - df$ratio)^2, na.rm = T) / sum(!is.na(df$ratio))
  fitness <- sum(
    (1 - df[df$var %in% select.target.vars, 'ratio'])^2,
    na.rm = T
  ) /
    sum(!is.na(df[df$var %in% select.target.vars, 'ratio']))

  return(list(
    table = t(df[, -1]),
    total.fit = total.fitness,
    fitness = fitness
  ))
}

#### function loads new initial values ####
load.init <- function(identif) {
  #identif <- 'flexible_/data/initial_values_july31_jbf_corrected_aug5.xlsx'
  initial <- list(
    variables = list(
      global = openxlsx::read.xlsx(identif, sheet = 'global.vars'),
      aggregate = openxlsx::read.xlsx(identif, sheet = 'aggregate.vars'),
      industry = openxlsx::read.xlsx(identif, sheet = 'industry.vars')
    ),
    parameters = list(
      global = openxlsx::read.xlsx(identif, sheet = 'global.pars'),
      aggregate = openxlsx::read.xlsx(identif, sheet = 'aggregate.pars')
    ),
    A.matrix = openxlsx::read.xlsx(
      identif,
      sheet = 'A.matrix',
      rowNames = TRUE
    ),
    B.matrix = openxlsx::read.xlsx(identif, sheet = 'B.matrix', rowNames = TRUE)
  )

  # Create Parameter Table
  init.par <- initial$parameters
  init.agg.long <- reshape2::melt(init.par$aggregate, variable.name = 'area')
  pars <- data.frame(
    label = c(
      init.par$global$label,
      paste0(init.agg.long$area, '_', init.agg.long$label)
    ),
    value = c(init.par$global$value, init.agg.long$value),
    type = c(rep(NA, times = nrow(init.par$global)), init.agg.long$type)
  )
  rownames(pars) <- pars$label
  initial$pars <- pars

  # Industry and Country Labels
  if (!sum(!grepl('\\.', dimnames(initial$A.matrix)[[1]]))) {
    foo <- stringr::str_extract(dimnames(initial$A.matrix)[[1]], "^[^.]+")
    initial$countries <- unique(foo)
    initial$sectors <- unique(stringr::str_extract(
      dimnames(initial$A.matrix)[[1]],
      "(?<=\\.).+"
    ))
    initial$N <- length(unique(foo))
    initial$K <- dim(initial$A.matrix)[1] / initial$N
  }

  # Create Variable Table
  init.var <- initial$variables
  init.agg.long <- reshape2::melt(
    init.var$aggregate,
    variable.name = 'area',
    id.vars = c('variable', 'name', 'industry', 'dynamic', 'type')
  )
  init.ind.long <- reshape2::melt(
    init.var$industry,
    variable.name = 'industry',
    id.vars = c('variable', 'name', 'area', 'dynamic', 'type')
  )
  vars <- data.frame(
    label = c(
      init.var$global$variable,
      paste0(init.agg.long$area, '_', init.agg.long$variable),
      paste0(
        init.ind.long$area,
        '_',
        init.ind.long$variable,
        '-',
        init.ind.long$industry
      )
    ),
    name = c(init.var$global$name, init.agg.long$name, init.ind.long$name), # paste0(init.ind.long$name, ', ', initial$sectors[init.ind.long$industry]))
    value = c(init.var$global$t1, init.agg.long$value, init.ind.long$value),
    dynamic = c(
      rep('endogenous', length(init.var$global$t1)),
      init.agg.long$dynamic,
      init.ind.long$dynamic
    ),
    type = c(
      rep(NA, length(init.var$global$t1)),
      init.agg.long$type,
      init.ind.long$type
    )
  )
  rownames(vars) <- vars$label
  initial$vars <- vars
  initial$identif <- identif

  # Industry and Country Labels
  zlabs <<- initial$zlabs <- paste0('Z', 1:initial$N) # zlabs correspond to the labels for each country using Marco's notation
  N <<- initial$N
  K <<- initial$K # K is the variable number of industries per country, in this case they are the same
  # vars <<- initial$vars # initial state vector

  return(initial)
}

### Save Initial Values to xlsx File
save.init <- function(res, filename) {
  initial <- res$initial
  initial$parameters$global$value <- initial$pars[
    !grepl('Z', initial$pars$label),
    'value'
  ]
  initial$parameters$aggregate[, zlabs] <- initial$pars[
    grepl('Z', initial$pars$label),
    'value'
  ]
  initial$variables$global$t1 <- initial$vars[
    !grepl('Z', initial$vars$label),
    'value'
  ]
  initial$variables$aggregate[, zlabs] <- initial$vars[
    grepl('Z', initial$vars$label) & !grepl('-', initial$vars$label),
    'value'
  ]
  initial$variables$industry[, 5 + (1:unique(initial$K))] <- initial$vars[
    grepl('Z', initial$vars$label) & grepl('-', initial$vars$label),
    'value'
  ]

  foo <- list(
    global.pars = initial$parameters$global,
    aggregate.pars = initial$parameters$aggregate,
    global.vars = initial$variables$global,
    aggregate.vars = initial$variables$aggregate,
    industry.vars = initial$variables$industry
  )

  wb <- openxlsx::createWorkbook()
  for (sheet_name in names(foo)) {
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet = sheet_name, x = foo[[sheet_name]])
  }

  openxlsx::addWorksheet(wb, "A.matrix")
  openxlsx::writeData(
    wb,
    sheet = "A.matrix",
    x = initial$A.matrix,
    rowNames = TRUE
  )
  openxlsx::addWorksheet(wb, "B.matrix")
  openxlsx::writeData(
    wb,
    sheet = "B.matrix",
    x = initial$B.matrix,
    rowNames = TRUE
  )
  openxlsx::saveWorkbook(wb, file = filename, overwrite = TRUE)
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

  missing <- setdiff(shock.labels, rownames(res$simulation))
  if (length(missing) > 0) {
    stop(
      "view.shock(): missing labels in res$simulation: ",
      paste(missing, collapse = ", ")
    )
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

# View Summary Table
shock.summary <- function(baseline, shock.run, t_, t_names) {
  df <- do.call(
    rbind,
    lapply(names(selected.list), function(name) {
      data.frame(
        Name = name,
        Variable = selected.list[[name]],
        stringsAsFactors = FALSE
      )
    })
  )

  for (z in zlabs) {
    for (tmp in 1:length(t_names)) {
      df[, paste0(t_names[tmp], '.', z)] <- shock.run$simulation[
        paste0(z, '_', df$Variable),
        t_[tmp]
      ] /
        baseline$simulation[paste0(z, '_', df$Variable), t_[tmp]] -
        1
    }
  }
  df[, -c(1:2)] <- round(100 * df[, -c(1:2)], digits = 2)

  return(df)
}

insert.line.break <- function(x) {
  if (nchar(x) > 50) {
    # Find the approximate middle of the string
    middle <- nchar(x) %/% 2
    # Find the nearest space to the middle
    space_position <- regexpr(" ", x, fixed = TRUE)[1]
    if (space_position < 0) {
      return(x)
    } # Return the original string if no space is found

    # Find the closest space to the middle of the string
    closest_space <- which.min(abs(gregexpr(" ", x)[[1]] - middle))
    space_index <- gregexpr(" ", x)[[1]][closest_space]

    # Insert line break at the space closest to the middle
    x <- sub(paste0("^(.{", space_index - 1, "})( )"), "\\1\n", x)
  }
  return(x)
}


run_or_load_shock <- function(n_shock, initial, model_fun, force = FALSE) {
  shock_file <- get_shock_filename(n_shock)

  if (file.exists(shock_file) && !force) {
    message("✔ Loading cached shock ", n_shock)
    return(readRDS(shock_file))
  }

  sc_row <- sc[sc$shock == n_shock, , drop = FALSE]
  if (nrow(sc_row) != 1) {
    stop("run_or_load_shock: shock id not unique or not found: ", n_shock)
  }

  rho_val <- suppressWarnings(as.numeric(sc_row$rho[[1]]))

  # Try your common column names, but don’t break if missing
  z1_val <- if ("Z1_ce" %in% names(sc_row)) {
    as.character(sc_row$Z1_ce[[1]])
  } else {
    ""
  }
  z2_val <- if ("Z2_ce" %in% names(sc_row)) {
    as.character(sc_row$Z2_ce[[1]])
  } else {
    ""
  }

  ctx <- paste0(
    "[shock:",
    n_shock,
    " rho:",
    ifelse(is.na(rho_val), "NA", format(rho_val, digits = 3)),
    ifelse(nzchar(z1_val), paste0(" Z1:", z1_val), ""),
    ifelse(nzchar(z2_val), paste0(" Z2:", z2_val), ""),
    "]"
  )

  message("▶ Running shock ", n_shock)

  res <- run.model(
    initial,
    model_fun,
    sc = sc,
    log_file = get_log_filename(n_shock),
    log_append = FALSE,
    print_final_state = FALSE,
    log_context = ctx
  )

  saveRDS(res, shock_file)
  return(res)
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
