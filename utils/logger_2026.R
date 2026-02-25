create_logger <- function(
  log_file = NULL,
  log_append = TRUE,
  width = 110,
  context = NULL
) {
  if (is.null(log_file)) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base_dir <- file.path(getwd(), "logs")
    dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)
    log_file <- file.path(base_dir, paste0("LEEDS_MODEL_run_", ts, ".log"))
  }

  log_con <- file(
    log_file,
    open = if (isTRUE(log_append)) "a" else "w",
    encoding = "UTF-8"
  )

  .log_line <- function(txt) {
    txt <- paste0(.ctx(), txt)
    cat(txt, "\n", file = stderr())
    writeLines(txt, log_con, sep = "\n", useBytes = TRUE)
    invisible(NULL)
  }

  .rule <- function(title = NULL, ch = "─") {
    if (is.null(title) || !nzchar(title)) {
      .log_line(paste(rep(ch, width), collapse = ""))
    } else {
      pad <- max(1, width - nchar(title) - 2)
      left <- floor(pad / 2)
      right <- pad - left
      .log_line(paste0(
        paste(rep(ch, left), collapse = ""),
        " ",
        title,
        " ",
        paste(rep(ch, right), collapse = "")
      ))
    }
  }

  .section <- function(title) {
    .log_line("")
    .log_line(paste0("── ", title, " ──"))
  }

  .ctx <- function() {
    if (is.null(context) || !nzchar(context)) "" else paste0(context, " ")
  }

  .info <- function(txt) .log_line(paste0("ℹ ", txt))
  .ok <- function(txt) .log_line(paste0("✔ ", txt))
  .warn <- function(txt) .log_line(paste0("⚠ ", txt))
  .fail <- function(txt) .log_line(paste0("✖ ", txt))

  list(
    rule = .rule,
    section = .section,
    info = .info,
    ok = .ok,
    warn = .warn,
    fail = .fail,
    close = function() close(log_con),
    file = log_file
  )
}
