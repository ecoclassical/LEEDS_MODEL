# R/tfm_wide_table.R
# Build the interactive “TFM Formulas — Wide Table” widget as a reusable function.
# Usage (in Quarto / revealjs):
#   source(here::here("R", "tfm_wide_table.R"))
#   make_tfm_wide_table()

make_tfm_wide_table <- function(
  xlsx_path = NULL,
  sheet = "TFM",
  drop_cols = c("Firms_Capital"),
  page_length = 25,
  scroll_y = "600px"
) {
  # ---- locate file ----
  if (is.null(xlsx_path)) {
    if (requireNamespace("here", quietly = TRUE)) {
      xlsx_path <- "tfm_formulas.xlsx"
    } else {
      xlsx_path <- file.path("tfm_formulas.xlsx")
    }
  }

  if (!file.exists(xlsx_path)) {
    stop(
      "Missing file: ", xlsx_path, "\n",
      "Place tfm_formulas.xlsx at data/tfm/tfm_formulas.xlsx or pass `xlsx_path=`."
    )
  }

  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' is required.")
  if (!requireNamespace("DT", quietly = TRUE)) stop("Package 'DT' is required.")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("Package 'htmltools' is required.")

  # ---- read ----
  wide <- readxl::read_excel(xlsx_path, sheet = sheet, .name_repair = "minimal")
  if (!"transaction" %in% names(wide)) names(wide)[1] <- "transaction"

  # ---- helpers ----
  escape_html <- function(x) {
    x <- ifelse(is.na(x), "", x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }

  slugify <- function(x) {
    x <- tolower(trimws(x))
    x <- gsub("[^a-z0-9]+", "-", x)
    x <- gsub("(^-+|-+$)", "", x)
    x
  }

  sna_code_group <- function(code) {
    code <- toupper(trimws(code))
    m <- regexec("^([A-Z])\\.([0-9]+).*?$", code, perl = TRUE)
    g <- regmatches(code, m)[[1]]
    if (length(g) >= 3) return(paste0(g[1], g[2], g[3])) # e.g., "P.13" -> "P13"
    "OTHER"
  }

  sna_sector_group <- function(sec) {
    sec <- toupper(trimws(sec))
    m <- regexec("^S\\.([0-9]{1,5}).*$", sec, perl = TRUE)
    g <- regmatches(sec, m)[[1]]
    if (length(g) >= 2) return(paste0("S", g[2])) # e.g., "S.13" -> "S13"
    "SOTHER"
  }

  sna_flag_group <- function(flag) {
    flag <- toupper(trimws(flag))
    if (flag %in% c("U/A", "UA", "USES")) return("UA")
    if (flag %in% c("R/P", "RP", "RESOURCES")) return("RP")
    "OTHER"
  }

  io_ic_group <- function(tag) {
    tag <- toupper(trimws(tag))
    if (grepl("^IO[1-5]$", tag)) return(paste0("io-p", sub("^IO", "", tag)))
    if (grepl("^IC[1-5]$", tag)) return(paste0("ic-p", sub("^IC", "", tag)))
    "pos-other"
  }

  make_chip <- function(text, cls, title = NULL) {
    if (is.na(text) || trimws(text) == "") return("")
    t <- escape_html(text)
    tt <- if (!is.null(title)) escape_html(title) else t
    paste0("<span class='chip ", cls, "' title='", tt, "'>", t, "</span>")
  }

  # Parse tokens in a single cell formula string and emit colored “chips”.
  tokenize_formula <- function(x) {
    if (is.na(x) || trimws(x) == "") return("")
    x <- trimws(as.character(x))

    # allow multiple expressions per cell separated by ";"
    parts <- strsplit(x, "\\s*;\\s*")[[1]]
    parts <- parts[nzchar(parts)]

    render_one <- function(expr) {
      expr <- trimws(expr)

      # pattern: "(src) a, b, c" OR "(src) a" etc.
      # examples seen in your table:
      #  - "(sna) P.13, S.13, R/P"
      #  - "(tfm) PublicServices, FirmsCurrent"
      #  - "(io) IO3" or "(ic) IC2" (positional chips)
      m <- regexec("^\\(([^)]+)\\)\\s*(.*)$", expr, perl = TRUE)
      g <- regmatches(expr, m)[[1]]
      if (length(g) < 3) {
        # not matching "(src) ...": just show raw
        return(make_chip(expr, "raw"))
      }

      src <- tolower(trimws(g[2]))
      rest <- trimws(g[3])
      # split by comma; keep non-empty
      rest <- strsplit(rest, "\\s*,\\s*")[[1]]
      rest <- rest[nzchar(rest)]

      chips <- character(0)

      if (src == "sna") {
        # expected: code, sector, flag
        if (length(rest) >= 1) {
          code <- rest[1]
          chips <- c(chips, make_chip(code, paste0("sna code-", slugify(sna_code_group(code)))))
        }
        if (length(rest) >= 2) {
          sec <- rest[2]
          chips <- c(chips, make_chip(sec, paste0("sna sec-", slugify(sna_sector_group(sec)))))
        }
        if (length(rest) >= 3) {
          flag <- rest[3]
          chips <- c(chips, make_chip(flag, paste0("sna flag-", slugify(sna_flag_group(flag)))))
        }
        if (length(rest) > 3) {
          extra <- rest[4:length(rest)]
          chips <- c(chips, vapply(extra, function(z) make_chip(z, "sna extra"), character(1)))
        }
        return(paste(chips, collapse = " "))
      }

      if (src == "tfm") {
        # expected: transaction name + sector/tag pieces
        chips <- c(chips, make_chip("tfm", "src-tfm", title = "tfm"))
        if (length(rest) >= 1) {
          chips <- c(chips, make_chip(rest[1], "tfm-trx", title = rest[1]))
        }
        if (length(rest) >= 2) {
          chips <- c(chips, make_chip(rest[2], "tfm-sec", title = rest[2]))
        }
        if (length(rest) >= 3) {
          extra <- rest[3:length(rest)]
          chips <- c(chips, vapply(extra, function(z) make_chip(z, "tfm-attr", title = z), character(1)))
        }
        return(paste(chips, collapse = " "))
      }

      if (src %in% c("io", "ic")) {
        # positional tags like IO1..IO5 / IC1..IC5
        chips <- c(chips, make_chip(src, paste0("src-", src), title = src))
        if (length(rest) >= 1) {
          # allow multiple positional tags in one expression
          chips <- c(chips, vapply(rest, function(z) make_chip(z, paste0("chip ", io_ic_group(z))), character(1)))
        }
        return(paste(chips, collapse = " "))
      }

      # default: show src + any tokens
      chips <- c(chips, make_chip(src, paste0("src-", slugify(src)), title = src))
      if (length(rest) >= 1) {
        chips <- c(chips, vapply(rest, function(z) make_chip(z, "tok"), character(1)))
      }
      paste(chips, collapse = " ")
    }

    out <- vapply(parts, render_one, character(1))
    paste(out, collapse = "<br/>")
  }

  # ---- transform cells into chip HTML ----
  cols <- setdiff(names(wide), "transaction")
  for (nm in cols) {
    wide[[nm]] <- vapply(wide[[nm]], tokenize_formula, character(1))
  }

  # ---- drop columns if requested ----
  if (length(drop_cols) > 0) {
    wide <- wide[, !names(wide) %in% drop_cols, drop = FALSE]
  }

  # ---- CSS (as in your QMD) ----
  css <- "
:root{
  --bg:#f6f7f7;
  --hdr:#204A31;
  --hdrText:#ffffff;
  --firstColBg:#f1f1f1;

  --tfmBg:#E6935C;
  --tfmText:#1f1f1f;

  --snaBg:#2D5BD1;
  --snaText:#ffffff;

  --cgD:#6A5ACD;
  --cgP:#2E8B57;
  --cgB:#B8860B;
  --cgF:#8B4B00;
  --cgR:#B22222;

  --secBg:#1F7A3B;
  --secText:#ffffff;

  --flagUABg:#e6e6e6;
  --flagRPBg:#e6e6e6;

  --ioPBg:#DCEFE7;
  --icPBg:#EAD7C6;
}

.chip{
  display:inline-block;
  padding:2px 8px;
  margin:2px 4px 2px 0;
  border-radius:999px;
  font-size:12px;
  line-height:1.35;
  white-space:nowrap;
  border:1px solid rgba(0,0,0,0.08);
  box-shadow:0 1px 0 rgba(0,0,0,0.03);
}

.chip.src-tfm{ background:var(--tfmBg); color:var(--tfmText); font-weight:800; }
.chip.tfm-trx{ background:#f2e5dc; color:#111; font-weight:700; }
.chip.tfm-sec{ background:#f2e5dc; color:#111; font-weight:700; }
.chip.tfm-attr{ background:#f2e5dc; color:#111; font-weight:650; }

.chip.sna{ background:var(--snaBg); color:var(--snaText); font-weight:800; }
.chip.sna.extra{ background:#e8eefc; color:#111; font-weight:650; }

.chip.code-p13{ background:var(--cgP); color:#fff; font-weight:820; }
.chip.code-p11{ background:var(--cgP); color:#fff; font-weight:820; }
.chip.code-p3{  background:var(--cgP); color:#fff; font-weight:820; }
.chip.code-p5b{ background:var(--cgP); color:#fff; font-weight:820; }

.chip.code-d71{ background:var(--cgD); color:#fff; font-weight:820; }
.chip.code-d72{ background:var(--cgD); color:#fff; font-weight:820; }

.chip.sec-s13{ background:var(--secBg); color:var(--secText); font-weight:820; }
.chip.sec-s12{ background:var(--secBg); color:var(--secText); font-weight:820; }
.chip.sec-s121{ background:var(--secBg); color:var(--secText); font-weight:820; }

.chip.flag-ua{ background:var(--flagUABg); color:#111; font-weight:820; }
.chip.flag-rp{ background:var(--flagRPBg); color:#111; font-weight:820; }

/* ---------- IO / IC positional chips ---------- */
.chip.io-p1,.chip.io-p2,.chip.io-p3,.chip.io-p4,.chip.io-p5{
  background: var(--ioPBg);
  color:#111111;
  font-weight: 820;
}
.chip.ic-p1,.chip.ic-p2,.chip.ic-p3,.chip.ic-p4,.chip.ic-p5{
  background: var(--icPBg);
  color:#111111;
  font-weight: 820;
}

/* DataTable header + first column */
table.dataTable thead th{
  background: var(--hdr) !important;
  color: var(--hdrText) !important;
  border-bottom: none !important;
}
table.dataTable tbody td:first-child{
  font-weight: 800;
  background: var(--firstColBg);
}

div.dataTables_wrapper { width: 100%; }
"

  style_tag <- htmltools::tags$style(htmltools::HTML(css))

  # ---- DT ----
  dt <- DT::datatable(
    wide,
    rownames = FALSE,
    escape = FALSE,
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      scrollY = scroll_y,
      scrollCollapse = TRUE,
      autoWidth = FALSE,
      dom = "ftip"
    ),
    callback = DT::JS(
      "
      var api = table.api();

      function adjust() {
        api.columns.adjust();
      }

      // after initial layout
      setTimeout(adjust, 0);

      // after redraws (filtering/paging)
      api.on('draw', function(){ adjust(); });

      // on window resize
      $(window).on('resize', function(){ adjust(); });
      "
    ),
    width = "100%",
    class = "stripe hover"
  )

  # return CSS + widget together (works in Quarto + revealjs)
  htmltools::tagList(style_tag, dt)
}