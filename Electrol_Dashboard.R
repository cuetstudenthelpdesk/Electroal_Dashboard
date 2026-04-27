# ============================================================
#  ASHOKA ANALYTICS — Electoral Dashboard (R Shiny)
#  DATA: UP Booth-Wise (AE/LE 2014-2024)
#  Slicers: 1.Year  2.Region  3.District  4.Assembly
#           5.Tehsil  6.Block  7.AE/LE  8.Polling Booth
#
#  HOW TO USE:
#   STEP 1 — Run prepare_data.R ONCE to convert Excel → RDS
#   STEP 2 — shiny::runApp('Electrol_Dashboard.R')
#   STEP 3 — rsconnect::deployApp()  (RDS + this file, no Excel needed)
# ============================================================

options(shiny.port = 3838)
options(shiny.host = "127.0.0.1")

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(shinyWidgets)

# ============================================================
# 1. COLOUR PALETTES
# ============================================================
PARTY_COLORS <- c(
  BJP    = "#FF6600", SP     = "#1B7A42", BSP    = "#7A2AB8",
  INC    = "#19AAED", SBSP   = "#FF3333", AIMIM  = "#006400",
  NISHAD = "#FF69B4", JAP    = "#8B4513", MKD    = "#708090",
  VSIP   = "#20B2AA", CPI    = "#CC0000", BMU    = "#DAA520",
  QED    = "#9400D3", NOTA   = "#808080", IND    = "#A0A0A0"
)

DIFF_COLORS <- c(
  `Extreme Difficult` = "#8B0000",
  Difficult           = "#E74C3C",
  Moderate            = "#F1C40F",
  Easy                = "#82E0AA",
  `Very Easy`         = "#1A5C2A"
)

# ============================================================
# 2. LOAD DATA  (RDS — fast, low-memory, no readxl needed)
# ============================================================
.find_data <- function() {
  app_dir <- tryCatch(
    dirname(rstudioapi::getSourceEditorContext()$path),
    error = function(e) getwd()
  )
  rds <- list.files(app_dir, pattern = "\\.rds$",
                    full.names = TRUE, ignore.case = TRUE)
  if (length(rds) > 0) return(rds[1])
  
  # Fallback: look for Excel and auto-convert
  xlsx <- list.files(app_dir, pattern = "\\.xlsx$",
                     full.names = TRUE, ignore.case = TRUE)
  if (length(xlsx) > 0) {
    message("No RDS found — converting Excel to RDS (one-time, ~30s)…")
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Please install readxl: install.packages('readxl')")
    raw <- readxl::read_excel(xlsx[1], sheet = 1)
    out <- file.path(app_dir, "booth_data.rds")
    saveRDS(raw, out, compress = "xz")
    message("Saved: ", out)
    return(out)
  }
  stop("No .rds or .xlsx file found in: ", app_dir,
       "\nPlace booth_data.rds (or the Excel) alongside this script.")
}

message("Loading data…")
raw_all <- readRDS(.find_data())

# ── Column mapping (60 cols in file; first 4 are junk/index cols) ──
col_names_all <- c(
  "junk1","junk2","junk3","junk4",
  "srno","region","district","tehsil","block",
  "assembly","election_type","year",
  "addr","lat","lng",
  "reg","male_votes","female_votes","other_votes","voters",
  "epic_voters","tendered",
  "SP","BJP","BSP","INC","JAP","AIMIM","MKD","VSIP","SBSP","CPI","BMU",
  "NISHAD","NOTA","IND","QED",
  "winner_raw","BJP_pct","diff_excel",
  "SP_pct","INC_pct","BSP_pct","BMU_pct","NISHAD_pct","NOTA_pct",
  "SBSP_pct","MKD_pct","AIMIM_pct","JAP_pct","VSIP_pct","CPI_pct",
  "QED_pct","IND_pct",
  "winPct","margin","runner","r1Pct","r2","r2Pct"
)
n_rename <- min(length(col_names_all), ncol(raw_all))
colnames(raw_all)[1:n_rename] <- col_names_all[1:n_rename]
# Drop junk index columns
raw_all <- raw_all[, !grepl("^junk", names(raw_all))]

# ── Remove embedded header rows ────────────────────────────
raw_all <- raw_all[
  !grepl("^Constituency|^Vidhan|^Assembly|^Const",
         as.character(raw_all$assembly), ignore.case = TRUE) &
    !is.na(raw_all$assembly) &
    grepl("^[0-9]{4}$", as.character(raw_all$year)), ]

# ── Numeric columns ────────────────────────────────────────
NUM_COLS <- c(
  "lat","lng","reg","male_votes","female_votes","other_votes","voters",
  "epic_voters","tendered",
  "SP","BJP","BSP","INC","JAP","AIMIM","MKD","VSIP","SBSP","CPI","BMU",
  "NISHAD","NOTA","IND","QED",
  "BJP_pct","SP_pct","INC_pct","BSP_pct","BMU_pct","NISHAD_pct",
  "NOTA_pct","SBSP_pct","MKD_pct","AIMIM_pct","JAP_pct","VSIP_pct",
  "CPI_pct","QED_pct","IND_pct","winPct","margin","r1Pct","r2Pct"
)

# ── Build booths_data ──────────────────────────────────────
booths_data <- raw_all %>%
  mutate(
    winner        = trimws(as.character(winner_raw)),
    addr          = trimws(as.character(addr)),
    region        = trimws(as.character(region)),
    district      = trimws(as.character(district)),
    assembly      = trimws(as.character(assembly)),
    tehsil        = trimws(as.character(tehsil)),
    block         = trimws(as.character(block)),
    election_type = trimws(toupper(as.character(election_type))),
    diff_excel    = trimws(as.character(diff_excel)),
    across(all_of(intersect(NUM_COLS, names(raw_all))),
           ~ suppressWarnings(as.numeric(.)))
  ) %>%
  mutate(
    constituency = district,
    lat     = ifelse(is.na(lat)  | lat  == 0, 26.85, lat),
    lng     = ifelse(is.na(lng)  | lng  == 0, 80.91, lng),
    turnout = round(ifelse(reg > 0, voters / reg * 100, NA_real_), 1),
    diff    = case_when(
      !is.na(diff_excel) & diff_excel != "" & diff_excel != "NA" ~ diff_excel,
      margin >= 15 ~ "Very Easy",
      margin >= 10 ~ "Easy",
      margin >=  5 ~ "Moderate",
      margin >=  2 ~ "Difficult",
      TRUE         ~ "Extreme Difficult"
    )
  ) %>%
  filter(!is.na(winner), winner != "", !is.na(voters), voters > 0) %>%
  # Keep only columns we actually need (cuts memory ~40%)
  select(region, district, constituency, tehsil, block, assembly,
         election_type, year, addr, lat, lng,
         reg, voters, turnout,
         SP, BJP, BSP, INC, JAP, AIMIM, MKD, VSIP, SBSP, CPI, BMU,
         NISHAD, NOTA, IND, QED,
         BJP_pct, SP_pct, INC_pct, BSP_pct, BMU_pct, NISHAD_pct,
         NOTA_pct, SBSP_pct, MKD_pct, AIMIM_pct, JAP_pct, VSIP_pct,
         CPI_pct, QED_pct, IND_pct,
         winPct, margin, winner, runner, r1Pct, r2, r2Pct, diff)

# ── Fix accidental lat/lng swaps ──────────────────────────
in_india <- function(la, ln)
  is.finite(la) & is.finite(ln) & la >= 6 & la <= 38 & ln >= 68 & ln <= 98

swap_idx <- !in_india(booths_data$lat, booths_data$lng) &
  in_india(booths_data$lng, booths_data$lat)
if (any(swap_idx, na.rm = TRUE)) {
  tmp <- booths_data$lat[swap_idx]
  booths_data$lat[swap_idx] <- booths_data$lng[swap_idx]
  booths_data$lng[swap_idx] <- tmp
}

HAS_TEHSIL <- any(!is.na(booths_data$tehsil) & booths_data$tehsil != "" &
                    booths_data$tehsil != "NA")
HAS_BLOCK  <- any(!is.na(booths_data$block)  & booths_data$block  != "" &
                    booths_data$block  != "NA")

# ── Constituency summary ──────────────────────────────────
constituencies <- booths_data %>%
  group_by(constituency) %>%
  summarise(
    lat    = mean(lat,    na.rm = TRUE),
    lng    = mean(lng,    na.rm = TRUE),
    voters = sum(voters,  na.rm = TRUE),
    reg    = sum(reg,     na.rm = TRUE),
    booths = n(),
    margin = round(mean(margin, na.rm = TRUE), 1),
    winner = names(sort(table(winner), decreasing = TRUE))[1],
    winPct = round(mean(winPct, na.rm = TRUE), 1),
    runner = names(sort(table(runner), decreasing = TRUE))[1],
    r1Pct  = round(mean(r1Pct,  na.rm = TRUE), 1),
    r2     = names(sort(table(r2), decreasing = TRUE))[1],
    r2Pct  = round(mean(r2Pct,  na.rm = TRUE), 1),
    .groups = "drop"
  ) %>% as.data.frame()

# ── Year-wise party vote share ────────────────────────────
year_data <- booths_data %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(
    BJP    = round(mean(BJP_pct,    na.rm = TRUE), 1),
    SP     = round(mean(SP_pct,     na.rm = TRUE), 1),
    BSP    = round(mean(BSP_pct,    na.rm = TRUE), 1),
    INC    = round(mean(INC_pct,    na.rm = TRUE), 1),
    SBSP   = round(mean(SBSP_pct,   na.rm = TRUE), 1),
    AIMIM  = round(mean(AIMIM_pct,  na.rm = TRUE), 1),
    NISHAD = round(mean(NISHAD_pct, na.rm = TRUE), 1),
    NOTA   = round(mean(NOTA_pct,   na.rm = TRUE), 1),
    IND    = round(mean(IND_pct,    na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "party", values_to = "pct") %>%
  filter(!is.na(pct)) %>% as.data.frame()

elect_types <- sort(unique(booths_data$election_type[
  !is.na(booths_data$election_type) & booths_data$election_type != ""]))

# Free raw data — not needed after processing
rm(raw_all); gc()

message("Data ready: ", nrow(booths_data), " booths loaded.")

# ============================================================
# 3. UI HELPERS
# ============================================================
lbl    <- function(txt) tags$label(txt,
                                   style = "font-weight:700;font-size:12px;")
lbl_sm <- function(txt) tags$label(txt,
                                   style = "font-weight:700;font-size:11px;margin-top:5px;display:block;")

naPickerUI <- function(id, txt) tagList(
  tags$label(txt, style = "font-weight:700;font-size:12px;color:#bbb;"),
  pickerInput(id, label = NULL, choices = c("— N/A —" = ""), selected = "",
              options = list(style = "btn-default btn-sm")))

naPickerUI_sm <- function(id, txt) tagList(
  tags$label(txt,
             style = "font-weight:700;font-size:11px;color:#bbb;margin-top:5px;display:block;"),
  pickerInput(id, label = NULL, choices = c("— N/A —" = ""), selected = "",
              options = list(style = "btn-default btn-sm")))

picker <- function(id, lbl_txt, choices, sm = FALSE) {
  tagList(
    if (sm) lbl_sm(lbl_txt) else lbl(lbl_txt),
    pickerInput(id, label = NULL, choices = choices, selected = "",
                options = list(style = "btn-default btn-sm", `live-search` = TRUE))
  )
}

# ============================================================
# 4. UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$span(
      tags$b("Ashoka Analytics"),
      tags$small(" | Electoral", style = "font-size:11px;opacity:.8;")
    ),
    titleWidth = 240
  ),
  
  dashboardSidebar(
    width = 220,
    tags$div(
      style = paste0("background:linear-gradient(175deg,#F57C20,#D06010);",
                     "height:100%;padding-top:6px;"),
      tags$div(
        style = "padding:10px 14px 8px;color:rgba(255,255,255,.75);font-size:11px;",
        tags$i(class = "fa fa-user-circle"), " admin  |  Administrator"
      ),
      tags$hr(style = "border-color:rgba(255,255,255,.2);margin:4px 0;"),
      sidebarMenu(id = "tabs",
                  menuItem("Dashboard",     tabName = "dashboard",    icon = icon("th-large")),
                  menuItem("Electoral Map", tabName = "electoralmap", icon = icon("map-marked-alt"))
      )
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-blue .main-header .logo,
      .skin-blue .main-header .navbar { background:#F57C20 !important; border-bottom:none; }
      .skin-blue .main-header .logo:hover { background:#D96A10 !important; }
      .skin-blue .main-sidebar {
        background:linear-gradient(175deg,#F57C20,#D06010) !important; }
      .skin-blue .sidebar-menu > li > a { color:rgba(255,255,255,.85) !important; }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li:hover > a {
        background:rgba(255,255,255,.2) !important;
        border-left:3px solid #fff !important; color:#fff !important; }
      .content-wrapper, .right-side { background:#F4F6FA !important; }
      .box { border-radius:10px !important;
             box-shadow:0 2px 8px rgba(0,0,0,.07) !important; }
      .box-header { border-radius:10px 10px 0 0 !important; }
      .box.box-warning { border-top-color:#F57C20 !important; }
      .small-box { border-radius:10px !important; }
      .slicer-box { background:#fff; border-radius:10px;
                    padding:14px 18px; margin-bottom:16px;
                    box-shadow:0 2px 7px rgba(0,0,0,.06); }
      table.dataTable thead th {
        background:#F57C20 !important; color:#fff !important;
        font-weight:600 !important; }
      table.dataTable tbody tr:hover { background:#FFF4EC !important; }
      .map-legend { display:flex; gap:10px; flex-wrap:wrap;
                    padding:6px 4px; font-size:11px; }
      .legend-item { display:flex; align-items:center; gap:4px; }
      .legend-dot { width:10px; height:10px; border-radius:50%;
                    display:inline-block; }
      ::-webkit-scrollbar { width:5px; }
      ::-webkit-scrollbar-thumb { background:#F57C20; border-radius:3px; }
      /* Zoom widget */
      :root { --dash-zoom-percent: 100%; }
      .content-wrapper, .right-side, .main-footer { zoom: var(--dash-zoom-percent); }
      .global-zoom-widget {
        position:fixed; right:16px; bottom:12px; z-index:2000;
        width:205px; background:#F6F6F6; border:1px solid #CFCFCF;
        border-radius:16px; box-shadow:0 2px 8px rgba(0,0,0,.12);
        padding:7px 10px 5px; }
      .global-zoom-title { font-size:11px; font-weight:700; color:#555; margin-bottom:1px; }
      .global-zoom-value { text-align:right; font-size:11px; color:#F57C20;
                           font-weight:700; margin-top:-5px; }
      .global-zoom-widget .form-group { margin-bottom:1px; }
      .global-zoom-widget .irs-grid { display:none; }
      .zoom-row { display:flex; align-items:center; gap:5px; }
      .zoom-icon { color:#636363; font-size:12px; line-height:1; width:12px; text-align:center; }
      .zoom-slider-wrap { flex:1; }
      .global-zoom-widget .irs--shiny .irs-line {
        background:#D6D6D6; border-color:#BBBBBB; height:6px; top:24px; }
      .global-zoom-widget .irs--shiny .irs-bar {
        background:#AFAFAF; border-top:1px solid #9A9A9A;
        border-bottom:1px solid #9A9A9A; top:24px; height:6px; }
      .global-zoom-widget .irs--shiny .irs-handle {
        width:15px; height:15px; top:20px; border:1px solid #8E8E8E;
        background:#ECECEC; box-shadow:inset 0 1px 0 rgba(255,255,255,.95); }
      .global-zoom-widget .irs--shiny .irs-handle i { background:#7D7D7D; width:1px; }
      .global-zoom-widget .irs--shiny .irs-min,
      .global-zoom-widget .irs--shiny .irs-max {
        color:#6B6B6B; font-size:10px; background:transparent; }
      .global-zoom-widget .irs--shiny .irs-single { display:none; }
    "))),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        if ($('.global-zoom-widget').parent()[0] !== document.body)
          $('body').append($('.global-zoom-widget'));
        var z0 = parseInt($('#page_zoom').val() || '100', 10);
        document.documentElement.style.setProperty('--dash-zoom-percent', z0 + '%');
        $('#page_zoom_value').text(z0 + '%');
      });
      $(document).on('input change', '#page_zoom', function() {
        var z = parseInt(this.value || '100', 10);
        document.documentElement.style.setProperty('--dash-zoom-percent', z + '%');
        $('#page_zoom_value').text(z + '%');
      });
    ")),
    
    # Zoom widget (fixed bottom-right)
    tags$div(class = "global-zoom-widget",
             tags$div("Page Size", class = "global-zoom-title"),
             tags$div(class = "zoom-row",
                      tags$i(class = "fa fa-search-minus zoom-icon"),
                      tags$div(class = "zoom-slider-wrap",
                               sliderInput("page_zoom", label = NULL, min = 70, max = 130,
                                           value = 100, step = 5, ticks = FALSE, width = "100%")),
                      tags$i(class = "fa fa-search-plus zoom-icon")),
             tags$div("100%", id = "page_zoom_value", class = "global-zoom-value")
    ),
    
    tabItems(
      
      # ══════════════════════════════════════════════════════
      # TAB 1 : DASHBOARD
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "dashboard",
              
              div(class = "slicer-box",
                  fluidRow(
                    column(4, picker("sel_yr",  "1. Year",
                                     c("All" = "", sort(unique(booths_data$year))), sm = FALSE)),
                    column(4, picker("sel_reg", "2. Region",
                                     c("All" = "", sort(unique(
                                       booths_data$region[!is.na(booths_data$region) &
                                                            booths_data$region != ""]))), sm = FALSE)),
                    column(4, picker("sel_con", "3. District",
                                     c("All" = "", sort(unique(booths_data$constituency))), sm = FALSE))
                  ),
                  fluidRow(
                    column(4, picker("sel_asm", "4. Constituency Assembly",
                                     c("All" = ""), sm = FALSE)),
                    column(4,
                           if (HAS_TEHSIL) picker("sel_teh", "5. Tehsil", c("All" = ""), sm = FALSE)
                           else naPickerUI("sel_teh", "5. Tehsil")),
                    column(4,
                           if (HAS_BLOCK)  picker("sel_blk", "6. Block", c("All" = ""), sm = FALSE)
                           else naPickerUI("sel_blk", "6. Block"))
                  ),
                  fluidRow(
                    column(4, picker("sel_ele", "7. AE / LE",
                                     c("All" = "", elect_types), sm = FALSE)),
                    column(4, picker("sel_bth", "8. Polling Booth Address",
                                     c("All" = ""), sm = FALSE)),
                    column(4,
                           tags$label("\u00a0", style = "display:block;"),
                           actionButton("reset_btn", "\u21ba  Reset All",
                                        class = "btn btn-warning btn-sm",
                                        style = "font-weight:700;font-size:13px;width:100%;margin-top:3px;"))
                  )
              ),
              
              # KPI row
              fluidRow(
                valueBoxOutput("kpi_turnout", width = 3),
                valueBoxOutput("kpi_reg",     width = 3),
                valueBoxOutput("kpi_booths",  width = 3),
                valueBoxOutput("kpi_margin",  width = 3)
              ),
              
              # Charts row 1
              fluidRow(
                box(title = tagList(icon("chart-line"), " % Vote Share Over Years"),
                    width = 8, solidHeader = TRUE, status = "warning",
                    plotlyOutput("lineChart",  height = "190px")),
                box(title = tagList(icon("chart-pie"), " Vote Share by Party"),
                    width = 4, solidHeader = TRUE, status = "warning",
                    plotlyOutput("donutChart", height = "190px"))
              ),
              
              # Charts row 2
              fluidRow(
                box(title = tagList(icon("chart-bar"),
                                    " Winner / 1st Runner / 2nd Runner %"),
                    width = 6, solidHeader = TRUE, status = "warning",
                    plotlyOutput("clustChart",  height = "190px")),
                box(title = tagList(icon("align-left"),
                                    " Booth — Registered vs Turnout"),
                    width = 6, solidHeader = TRUE, status = "warning",
                    plotlyOutput("horizChart",  height = "190px"))
              ),
              
              # Charts row 3
              fluidRow(
                box(title = tagList(icon("chart-bar"), " Winner % vs BJP % by Booth"),
                    width = 4, solidHeader = TRUE, status = "warning",
                    plotlyOutput("winnerBjpChart", height = "190px")),
                box(title = tagList(icon("chart-line"),
                                    " BJP % / Winner % / Margin % / Turnout — by Block"),
                    width = 4, solidHeader = TRUE, status = "warning",
                    plotlyOutput("blockLineChart", height = "190px")),
                box(title = tagList(icon("th"),
                                    " Booth Priority Quadrant (Difficulty \u00d7 Turnout)"),
                    width = 4, solidHeader = TRUE, status = "warning",
                    plotlyOutput("quadrantChart", height = "190px"))
              ),
              
              # Table + Map row
              fluidRow(
                box(title = tagList(icon("table"), " Booth Details"),
                    width = 7, solidHeader = TRUE, status = "warning",
                    DTOutput("boothTable")),
                box(title = tagList(icon("map"), " QGIS Electoral Map"),
                    width = 5, solidHeader = TRUE, status = "warning",
                    div(style = "display:flex;gap:6px;margin-bottom:8px;",
                        actionButton("dash_sat", "\U0001F6F0\uFE0F Satellite",
                                     class = "btn btn-sm btn-dark",
                                     style = "font-size:10px;font-weight:700;"),
                        actionButton("dash_osm", "\U0001F5FA\uFE0F OpenStreetMap",
                                     class = "btn btn-sm btn-info",
                                     style = "font-size:10px;font-weight:700;")),
                    leafletOutput("dashMap", height = "300px"),
                    div(class = "map-legend", style = "margin-top:6px;",
                        lapply(names(DIFF_COLORS), function(d)
                          tags$span(class = "legend-item",
                                    tags$span(class = "legend-dot",
                                              style = paste0("background:", DIFF_COLORS[d], ";")), d)))
                )
              )
      ), # end dashboard
      
      # ══════════════════════════════════════════════════════
      # TAB 2 : ELECTORAL MAP
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "electoralmap",
              fluidRow(
                column(width = 2,
                       box(width = NULL, solidHeader = TRUE, status = "warning",
                           title = tagList(icon("filter"), " Map Filters"),
                           picker("em_yr",  "1. Year",   c("All" = "", sort(unique(booths_data$year))), sm = TRUE),
                           picker("em_reg", "2. Region",
                                  c("All" = "", sort(unique(
                                    booths_data$region[!is.na(booths_data$region) &
                                                         booths_data$region != ""]))), sm = TRUE),
                           picker("em_con", "3. District",
                                  c("All" = "", sort(unique(booths_data$constituency))), sm = TRUE),
                           picker("em_asm", "4. Constituency Assembly", c("All" = ""), sm = TRUE),
                           if (HAS_TEHSIL) picker("em_teh", "5. Tehsil", c("All" = ""), sm = TRUE)
                           else naPickerUI_sm("em_teh", "5. Tehsil"),
                           if (HAS_BLOCK)  picker("em_blk", "6. Block",  c("All" = ""), sm = TRUE)
                           else naPickerUI_sm("em_blk", "6. Block"),
                           picker("em_ele", "7. AE / LE", c("All" = "", elect_types), sm = TRUE),
                           picker("em_bth", "8. Polling Booth", c("All" = ""), sm = TRUE),
                           actionButton("em_reset", "\u21ba Reset All",
                                        class = "btn btn-warning btn-sm",
                                        style = "width:100%;font-weight:700;margin-top:8px;"),
                           tags$hr(),
                           tags$b("Map Layer", style = "font-size:11px;"),
                           div(style = "display:flex;flex-direction:column;gap:4px;margin-top:4px;",
                               actionButton("em_sat", "\U0001F6F0\uFE0F Satellite",
                                            class = "btn btn-sm btn-dark",
                                            style = "width:100%;font-size:10px;font-weight:700;"),
                               actionButton("em_osm", "\U0001F5FA\uFE0F OpenStreetMap",
                                            class = "btn btn-sm btn-info",
                                            style = "width:100%;font-size:10px;font-weight:700;")),
                           tags$hr(),
                           tags$b("Quick Stats", style = "font-size:11px;"),
                           tableOutput("em_stats"),
                           tags$hr(),
                           tags$b("Difficulty", style = "font-size:11px;"),
                           div(style = "font-size:10px;margin-top:4px;",
                               lapply(names(DIFF_COLORS), function(d)
                                 div(style = "display:flex;align-items:center;gap:5px;margin:3px 0;",
                                     tags$span(style = paste0(
                                       "width:10px;height:10px;border-radius:50%;",
                                       "background:", DIFF_COLORS[d], ";display:inline-block;")), d)))
                       )
                ),
                column(width = 10,
                       box(width = NULL, solidHeader = TRUE, status = "warning",
                           title = tagList(icon("map-marked-alt"),
                                           " Electoral Map — Polling Station View"),
                           leafletOutput("fullMap", height = "580px"),
                           div(class = "map-legend", style = "margin-top:8px;",
                               tags$b("Party: ",
                                      style = "font-size:10px;color:#888;margin-right:4px;"),
                               lapply(names(PARTY_COLORS), function(p)
                                 tags$span(class = "legend-item",
                                           tags$span(class = "legend-dot",
                                                     style = paste0("background:", PARTY_COLORS[p], ";")), p)),
                               tags$span(style = "margin-left:14px;"),
                               tags$b("Difficulty: ",
                                      style = "font-size:10px;color:#888;margin-right:4px;"),
                               lapply(names(DIFF_COLORS), function(d)
                                 tags$span(class = "legend-item",
                                           tags$span(class = "legend-dot",
                                                     style = paste0("background:", DIFF_COLORS[d], ";")), d)))
                       )
                )
              )
      ) # end electoralmap
      
    ) # end tabItems
  ) # end dashboardBody
) # end ui

# ============================================================
# 5. SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ── Master filter ────────────────────────────────────────
  apply_filters <- function(yr, reg, con, asm, teh, blk, ele, bth) {
    d <- booths_data
    if (!is.null(yr)  && yr  != "") d <- d[d$year == as.integer(yr), ]
    if (!is.null(reg) && reg != "") d <- d[!is.na(d$region) & d$region == reg, ]
    if (!is.null(con) && con != "") d <- d[d$constituency == con, ]
    if (!is.null(asm) && asm != "") d <- d[d$assembly == asm, ]
    if (!is.null(teh) && teh != "" && HAS_TEHSIL)
      d <- d[!is.na(d$tehsil) & d$tehsil == teh, ]
    if (!is.null(blk) && blk != "" && HAS_BLOCK)
      d <- d[!is.na(d$block) & d$block == blk, ]
    if (!is.null(ele) && ele != "")
      d <- d[!is.na(d$election_type) & d$election_type == ele, ]
    if (!is.null(bth) && bth != "") d <- d[d$addr == bth, ]
    d
  }
  up_to <- function(yr="",reg="",con="",asm="",teh="",blk="",ele="")
    apply_filters(yr, reg, con, asm, teh, blk, ele, "")
  
  filtered    <- reactive({
    apply_filters(input$sel_yr, input$sel_reg, input$sel_con, input$sel_asm,
                  input$sel_teh, input$sel_blk, input$sel_ele, input$sel_bth)
  })
  em_filtered <- reactive({
    apply_filters(input$em_yr, input$em_reg, input$em_con, input$em_asm,
                  input$em_teh, input$em_blk, input$em_ele, input$em_bth)
  })
  
  valid_geo <- function(df)
    df %>% filter(is.finite(lat), is.finite(lng),
                  lat >= 6, lat <= 38, lng >= 68, lng <= 98)
  valid_geo_loose <- function(df)
    df %>% filter(is.finite(lat), is.finite(lng),
                  lat >= -90, lat <= 90, lng >= -180, lng <= 180)
  
  # ── Cascade — Dashboard ──────────────────────────────────
  observeEvent(input$sel_yr, ignoreInit = TRUE, {
    d <- up_to(yr = input$sel_yr)
    updatePickerInput(session, "sel_reg",
                      choices = c("All"="", sort(unique(d$region[!is.na(d$region) & d$region!=""]))),
                      selected = "")
    for (id in c("sel_con","sel_asm","sel_teh","sel_blk","sel_bth"))
      updatePickerInput(session, id, choices = c("All"=""), selected = "")
  })
  observeEvent(input$sel_reg, ignoreInit = TRUE, {
    d <- up_to(yr = input$sel_yr, reg = input$sel_reg)
    updatePickerInput(session, "sel_con",
                      choices = c("All"="", sort(unique(d$constituency))), selected = "")
    for (id in c("sel_asm","sel_teh","sel_blk","sel_bth"))
      updatePickerInput(session, id, choices = c("All"=""), selected = "")
  })
  observeEvent(input$sel_con, ignoreInit = TRUE, {
    d <- up_to(yr = input$sel_yr, reg = input$sel_reg, con = input$sel_con)
    updatePickerInput(session, "sel_asm",
                      choices = c("All"="", sort(unique(
                        d$assembly[!is.na(d$assembly) & d$assembly!=""]))), selected = "")
    for (id in c("sel_teh","sel_blk","sel_bth"))
      updatePickerInput(session, id, choices = c("All"=""), selected = "")
  })
  observeEvent(input$sel_asm, ignoreInit = TRUE, {
    d <- up_to(yr=input$sel_yr,reg=input$sel_reg,con=input$sel_con,asm=input$sel_asm)
    if (HAS_TEHSIL && input$sel_asm != "")
      updatePickerInput(session, "sel_teh",
                        choices = c("All"="", sort(unique(
                          d$tehsil[!is.na(d$tehsil) & d$tehsil!=""]))), selected = "")
    else
      updatePickerInput(session, "sel_teh", choices = c("All"=""), selected = "")
    updatePickerInput(session, "sel_blk", choices = c("All"=""), selected = "")
    if (!HAS_TEHSIL && !HAS_BLOCK && input$sel_asm != "")
      updatePickerInput(session, "sel_bth",
                        choices = c("All"="", sort(unique(
                          d$addr[!is.na(d$addr) & d$addr!=""]))), selected = "")
    else
      updatePickerInput(session, "sel_bth", choices = c("All"=""), selected = "")
  })
  observeEvent(input$sel_teh, ignoreInit = TRUE, {
    d <- up_to(yr=input$sel_yr,reg=input$sel_reg,con=input$sel_con,
               asm=input$sel_asm,teh=input$sel_teh)
    if (HAS_BLOCK && input$sel_teh != "")
      updatePickerInput(session, "sel_blk",
                        choices = c("All"="", sort(unique(
                          d$block[!is.na(d$block) & d$block!=""]))), selected = "")
    else
      updatePickerInput(session, "sel_blk", choices = c("All"=""), selected = "")
    updatePickerInput(session, "sel_bth", choices = c("All"=""), selected = "")
  })
  observeEvent(input$sel_blk, ignoreInit = TRUE, {
    d <- up_to(yr=input$sel_yr,reg=input$sel_reg,con=input$sel_con,
               asm=input$sel_asm,teh=input$sel_teh,blk=input$sel_blk)
    ets <- sort(unique(d$election_type[!is.na(d$election_type)&d$election_type!=""]))
    updatePickerInput(session, "sel_ele", choices = c("All"="", ets), selected = "")
    updatePickerInput(session, "sel_bth", choices = c("All"=""), selected = "")
  })
  observeEvent(input$sel_ele, ignoreInit = TRUE, {
    d <- up_to(yr=input$sel_yr,reg=input$sel_reg,con=input$sel_con,
               asm=input$sel_asm,teh=input$sel_teh,blk=input$sel_blk,ele=input$sel_ele)
    updatePickerInput(session, "sel_bth",
                      choices = if (nrow(d)>0)
                        c("All"="", sort(unique(d$addr[!is.na(d$addr) & d$addr!=""])))
                      else c("All"=""), selected = "")
  })
  observeEvent(input$reset_btn, {
    updatePickerInput(session, "sel_yr",
                      choices = c("All"="", sort(unique(booths_data$year))), selected = "")
    updatePickerInput(session, "sel_reg",
                      choices = c("All"="", sort(unique(
                        booths_data$region[!is.na(booths_data$region)&booths_data$region!=""]))),
                      selected = "")
    updatePickerInput(session, "sel_con",
                      choices = c("All"="", sort(unique(booths_data$constituency))), selected = "")
    for (id in c("sel_asm","sel_teh","sel_blk"))
      updatePickerInput(session, id, choices = c("All"=""), selected = "")
    updatePickerInput(session, "sel_ele",
                      choices = c("All"="", elect_types), selected = "")
    updatePickerInput(session, "sel_bth", choices = c("All"=""), selected = "")
  })
  
  # ── Cascade — Electoral Map ──────────────────────────────
  observeEvent(input$em_yr, ignoreInit = TRUE, {
    d <- up_to(yr = input$em_yr)
    updatePickerInput(session, "em_reg",
                      choices = c("All"="", sort(unique(d$region[!is.na(d$region)&d$region!=""]))),
                      selected = "")
    for (id in c("em_con","em_asm","em_teh","em_blk","em_bth"))
      updatePickerInput(session, id, choices = c("All"=""), selected = "")
  })
  observeEvent(input$em_reg, ignoreInit = TRUE, {
    d <- up_to(yr = input$em_yr, reg = input$em_reg)
    updatePickerInput(session, "em_con",
                      choices = c("All"="", sort(unique(d$constituency))), selected = "")
    for (id in c("em_asm","em_teh","em_blk","em_bth"))
      updatePickerInput(session, id, choices = c("All"=""), selected = "")
  })
  observeEvent(input$em_con, ignoreInit = TRUE, {
    d <- up_to(yr=input$em_yr,reg=input$em_reg,con=input$em_con)
    updatePickerInput(session, "em_asm",
                      choices = c("All"="", sort(unique(
                        d$assembly[!is.na(d$assembly)&d$assembly!=""]))), selected = "")
    for (id in c("em_teh","em_blk","em_bth"))
      updatePickerInput(session, id, choices = c("All"=""), selected = "")
  })
  observeEvent(input$em_asm, ignoreInit = TRUE, {
    d <- up_to(yr=input$em_yr,reg=input$em_reg,con=input$em_con,asm=input$em_asm)
    if (HAS_TEHSIL && input$em_asm != "")
      updatePickerInput(session, "em_teh",
                        choices = c("All"="", sort(unique(
                          d$tehsil[!is.na(d$tehsil)&d$tehsil!=""]))), selected = "")
    else
      updatePickerInput(session, "em_teh", choices = c("All"=""), selected = "")
    updatePickerInput(session, "em_blk", choices = c("All"=""), selected = "")
    if (!HAS_TEHSIL && !HAS_BLOCK && input$em_asm != "")
      updatePickerInput(session, "em_bth",
                        choices = c("All"="", sort(unique(
                          d$addr[!is.na(d$addr)&d$addr!=""]))), selected = "")
    else
      updatePickerInput(session, "em_bth", choices = c("All"=""), selected = "")
  })
  observeEvent(input$em_teh, ignoreInit = TRUE, {
    d <- up_to(yr=input$em_yr,reg=input$em_reg,con=input$em_con,
               asm=input$em_asm,teh=input$em_teh)
    if (HAS_BLOCK && input$em_teh != "")
      updatePickerInput(session, "em_blk",
                        choices = c("All"="", sort(unique(
                          d$block[!is.na(d$block)&d$block!=""]))), selected = "")
    else
      updatePickerInput(session, "em_blk", choices = c("All"=""), selected = "")
    updatePickerInput(session, "em_bth", choices = c("All"=""), selected = "")
  })
  observeEvent(input$em_blk, ignoreInit = TRUE, {
    d <- up_to(yr=input$em_yr,reg=input$em_reg,con=input$em_con,
               asm=input$em_asm,teh=input$em_teh,blk=input$em_blk)
    ets <- sort(unique(d$election_type[!is.na(d$election_type)&d$election_type!=""]))
    updatePickerInput(session, "em_ele", choices = c("All"="", ets), selected = "")
    updatePickerInput(session, "em_bth", choices = c("All"=""), selected = "")
  })
  observeEvent(input$em_ele, ignoreInit = TRUE, {
    d <- up_to(yr=input$em_yr,reg=input$em_reg,con=input$em_con,
               asm=input$em_asm,teh=input$em_teh,blk=input$em_blk,ele=input$em_ele)
    updatePickerInput(session, "em_bth",
                      choices = if (nrow(d)>0)
                        c("All"="", sort(unique(d$addr[!is.na(d$addr)&d$addr!=""])))
                      else c("All"=""), selected = "")
  })
  observeEvent(input$em_reset, {
    updatePickerInput(session, "em_yr",
                      choices = c("All"="", sort(unique(booths_data$year))), selected = "")
    updatePickerInput(session, "em_reg",
                      choices = c("All"="", sort(unique(
                        booths_data$region[!is.na(booths_data$region)&booths_data$region!=""]))),
                      selected = "")
    updatePickerInput(session, "em_con",
                      choices = c("All"="", sort(unique(booths_data$constituency))), selected = "")
    for (id in c("em_asm","em_teh","em_blk"))
      updatePickerInput(session, id, choices = c("All"=""), selected = "")
    updatePickerInput(session, "em_ele",
                      choices = c("All"="", elect_types), selected = "")
    updatePickerInput(session, "em_bth", choices = c("All"=""), selected = "")
  })
  
  # ══════════════════════════════════════════════════════════
  # KPI CARDS
  # ══════════════════════════════════════════════════════════
  output$kpi_turnout <- renderValueBox(
    valueBox(format(sum(filtered()$voters, na.rm=TRUE), big.mark=","),
             "Total Voter Turnout", icon=icon("vote-yea"), color="orange"))
  output$kpi_reg <- renderValueBox(
    valueBox(format(sum(filtered()$reg, na.rm=TRUE), big.mark=","),
             "Registered Voters", icon=icon("clipboard-list"), color="blue"))
  output$kpi_booths <- renderValueBox(
    valueBox(format(nrow(filtered()), big.mark=","), "Polling Booths",
             icon=icon("map-pin"), color="green"))
  output$kpi_margin <- renderValueBox(
    valueBox(paste0(round(mean(filtered()$BJP_pct, na.rm=TRUE), 1), "%"),
             "BJP Avg Vote %", icon=icon("percent"), color="red"))
  
  # ══════════════════════════════════════════════════════════
  # CHARTS
  # ══════════════════════════════════════════════════════════
  empty_plot <- function(msg = "No data for this selection")
    plotly_empty() %>%
    layout(title = list(text = msg, font = list(size = 12)))
  
  output$lineChart <- renderPlotly({
    yd   <- year_data
    cols <- PARTY_COLORS[unique(yd$party)]
    cols[is.na(cols)] <- "#AAAAAA"
    plot_ly(yd, x=~year, y=~pct, color=~party, colors=cols,
            type="scatter", mode="lines+markers",
            line=list(width=2.5), marker=list(size=6)) %>%
      layout(xaxis  = list(title="", tickfont=list(size=10)),
             yaxis  = list(title="Vote %", tickfont=list(size=10), ticksuffix="%"),
             legend = list(orientation="h", y=1.12, font=list(size=10)),
             margin = list(t=10, b=30, l=45, r=10),
             plot_bgcolor="transparent", paper_bgcolor="transparent",
             hovermode="x unified")
  })
  
  output$donutChart <- renderPlotly({
    d <- filtered()
    if (nrow(d) == 0) return(empty_plot())
    pct_cols <- paste0(c("BJP","SP","BSP","INC","SBSP","AIMIM",
                         "NISHAD","JAP","MKD","VSIP","CPI","BMU",
                         "QED","NOTA","IND"), "_pct")
    parties  <- sub("_pct$", "", pct_cols)
    pv <- data.frame(party = parties,
                     pct = sapply(pct_cols, function(col)
                       if (col %in% names(d)) round(mean(d[[col]], na.rm=TRUE), 2) else 0)) %>%
      filter(!is.na(pct), pct > 0) %>% arrange(desc(pct))
    cols_vec <- unname(PARTY_COLORS[pv$party])
    cols_vec[is.na(cols_vec)] <- "#CCCCCC"
    plot_ly(pv, labels=~party, values=~pct, type="pie", hole=0.62,
            marker=list(colors=cols_vec, line=list(color="#fff", width=2)),
            textinfo="label+percent", textfont=list(size=10)) %>%
      layout(showlegend=TRUE, legend=list(orientation="v", font=list(size=9)),
             margin=list(t=10,b=10,l=10,r=10), paper_bgcolor="transparent")
  })
  
  output$clustChart <- renderPlotly({
    d <- filtered()
    if (nrow(d) == 0) return(empty_plot())
    if (input$sel_bth != "" || input$sel_asm != "") {
      grp  <- d %>% group_by(addr) %>%
        summarise(w=mean(winPct,na.rm=TRUE), r1=mean(r1Pct,na.rm=TRUE),
                  r2=mean(r2Pct,na.rm=TRUE), .groups="drop") %>% head(15)
      xlab <- substr(grp$addr, 1, 20)
    } else if (input$sel_con != "") {
      grp  <- d %>% group_by(assembly) %>%
        summarise(w=mean(winPct,na.rm=TRUE), r1=mean(r1Pct,na.rm=TRUE),
                  r2=mean(r2Pct,na.rm=TRUE), .groups="drop")
      xlab <- grp$assembly
    } else {
      grp  <- constituencies %>% rename(w=winPct, r1=r1Pct, r2=r2Pct) %>% head(20)
      xlab <- grp$constituency
    }
    plot_ly(x=xlab, y=grp$w, name="Winner %", type="bar",
            marker=list(color="#FF6600")) %>%
      add_trace(y=grp$r1, name="1st Runner %", marker=list(color="#19AAED")) %>%
      add_trace(y=grp$r2, name="2nd Runner %", marker=list(color="#1B7A42")) %>%
      layout(barmode="group",
             xaxis=list(title="", tickfont=list(size=9)),
             yaxis=list(title="%", ticksuffix="%", tickfont=list(size=9)),
             legend=list(orientation="h", y=1.12, font=list(size=9)),
             margin=list(t=10,b=55,l=40,r=10),
             plot_bgcolor="transparent", paper_bgcolor="transparent",
             hovermode="x unified")
  })
  
  output$winnerBjpChart <- renderPlotly({
    d <- filtered()
    if (nrow(d) == 0) return(empty_plot())
    grp <- d %>% group_by(addr) %>%
      summarise(winner_pct=round(mean(winPct,na.rm=TRUE),1),
                bjp_pct=round(mean(BJP_pct,na.rm=TRUE),1), .groups="drop") %>%
      arrange(desc(winner_pct)) %>% head(60)
    plot_ly(x=substr(grp$addr,1,18), y=grp$winner_pct,
            name="Winner %", type="bar", marker=list(color="#19AAED")) %>%
      add_trace(y=grp$bjp_pct, name="BJP %", marker=list(color="#003087")) %>%
      layout(barmode="group",
             xaxis=list(title="", tickfont=list(size=8), tickangle=-45),
             yaxis=list(title="%", ticksuffix="%", tickfont=list(size=9)),
             legend=list(orientation="h", y=1.08, font=list(size=10)),
             margin=list(t=10,b=90,l=45,r=10),
             plot_bgcolor="transparent", paper_bgcolor="transparent",
             hovermode="x unified")
  })
  
  get_block_data <- function(d, sel_con, sel_teh, sel_blk) {
    grp_col <- if (sel_con == "") "constituency"
    else if (!HAS_BLOCK || sel_blk != "") "assembly"
    else "block"
    if (grp_col == "block")
      d <- d %>% filter(!is.na(block) & block != "" & block != "NA")
    d %>%
      group_by(grp_label = .data[[grp_col]]) %>%
      summarise(
        BJP_pct  = round(mean(BJP_pct, na.rm=TRUE), 1),
        winPct   = round(mean(winPct,  na.rm=TRUE), 1),
        margin   = round(mean(margin,  na.rm=TRUE), 1),
        turnout  = round(mean(turnout, na.rm=TRUE), 1),
        n_booths = n(),
        diff     = names(sort(table(diff), decreasing=TRUE))[1],
        .groups  = "drop"
      )
  }
  
  output$blockLineChart <- renderPlotly({
    d  <- filtered()
    if (nrow(d) == 0) return(empty_plot())
    bd <- get_block_data(d, input$sel_con, input$sel_teh, input$sel_blk)
    if (nrow(bd) == 0) return(empty_plot("No block data available"))
    xlab <- bd$grp_label
    plot_ly(x=xlab, y=bd$BJP_pct, name="BJP %",
            type="scatter", mode="lines+markers",
            line=list(color="#FF6600",width=2.5),
            marker=list(color="#FF6600",size=7)) %>%
      add_trace(y=bd$winPct, name="Winner %",
                line=list(color="#19AAED",width=2.5),
                marker=list(color="#19AAED",size=7)) %>%
      add_trace(y=bd$margin, name="Margin %",
                line=list(color="#E74C3C",width=2,dash="dot"),
                marker=list(color="#E74C3C",size=6)) %>%
      add_trace(y=bd$turnout, name="Turnout %",
                line=list(color="#1A5C2A",width=2,dash="dash"),
                marker=list(color="#1A5C2A",size=6)) %>%
      layout(xaxis=list(title="",tickfont=list(size=9),tickangle=-40,
                        categoryorder="array",categoryarray=xlab),
             yaxis=list(title="%",ticksuffix="%",tickfont=list(size=9)),
             legend=list(orientation="h",y=1.12,font=list(size=10)),
             margin=list(t=10,b=80,l=45,r=10),
             plot_bgcolor="transparent", paper_bgcolor="transparent",
             hovermode="x unified")
  })
  
  output$quadrantChart <- renderPlotly({
    d  <- filtered()
    if (nrow(d) == 0) return(empty_plot())
    bd <- get_block_data(d, input$sel_con, input$sel_teh, input$sel_blk)
    if (nrow(bd) == 0) return(empty_plot("No block data available"))
    diff_score <- c(`Extreme Difficult`=1,Difficult=2,Moderate=3,Easy=4,`Very Easy`=5)
    bd$diff_num <- unname(diff_score[bd$diff])
    bd$diff_num[is.na(bd$diff_num)] <- 3
    med_t <- median(bd$turnout, na.rm=TRUE)
    med_d <- median(bd$diff_num, na.rm=TRUE)
    dot_cols <- unname(DIFF_COLORS[bd$diff])
    dot_cols[is.na(dot_cols)] <- "#AAAAAA"
    shapes <- list(
      list(type="rect",fillcolor="rgba(139,0,0,.07)",line=list(color="transparent"),
           x0=0,x1=med_t,y0=med_d,y1=5.5,xref="x",yref="y"),
      list(type="rect",fillcolor="rgba(231,76,60,.06)",line=list(color="transparent"),
           x0=med_t,x1=100,y0=med_d,y1=5.5,xref="x",yref="y"),
      list(type="rect",fillcolor="rgba(241,196,15,.07)",line=list(color="transparent"),
           x0=0,x1=med_t,y0=0.5,y1=med_d,xref="x",yref="y"),
      list(type="rect",fillcolor="rgba(26,92,42,.08)",line=list(color="transparent"),
           x0=med_t,x1=100,y0=0.5,y1=med_d,xref="x",yref="y"))
    annotations <- list(
      list(x=med_t/2, y=5.35, text="<b>URGENT</b>",
           font=list(size=11,color="#8B0000"), showarrow=FALSE),
      list(x=(med_t+100)/2, y=5.35, text="<b>WATCH</b>",
           font=list(size=11,color="#E74C3C"), showarrow=FALSE),
      list(x=med_t/2, y=0.65, text="<b>MOBILISE</b>",
           font=list(size=11,color="#BA7517"), showarrow=FALSE),
      list(x=(med_t+100)/2, y=0.65, text="<b>STRONG</b>",
           font=list(size=11,color="#1A5C2A"), showarrow=FALSE))
    plot_ly(bd, x=~turnout, y=~diff_num, type="scatter",
            mode="markers+text", text=~grp_label,
            textposition="top center", textfont=list(size=9),
            marker=list(color=dot_cols, size=~sqrt(n_booths)*4,
                        sizemode="diameter", opacity=0.85,
                        line=list(color="white",width=1)),
            hovertemplate=paste0(
              "<b>%{text}</b><br>Turnout: %{x:.1f}%<br>",
              "BJP %: %{customdata[0]:.1f}%<br>",
              "Margin: %{customdata[1]:.1f}%<br>",
              "Booths: %{customdata[2]}<br>",
              "Difficulty: %{customdata[3]}<extra></extra>"),
            customdata=~cbind(BJP_pct,margin,n_booths,diff)) %>%
      layout(shapes=shapes, annotations=annotations,
             xaxis=list(title="Voter Turnout %",ticksuffix="%",tickfont=list(size=10),
                        range=c(max(0,min(bd$turnout,na.rm=TRUE)-5),
                                min(100,max(bd$turnout,na.rm=TRUE)+5))),
             yaxis=list(title="Difficulty →  Hard to Easy",
                        tickvals=c(1,2,3,4,5),
                        ticktext=c("Extreme Diff","Difficult",
                                   "Moderate","Easy","Very Easy"),
                        tickfont=list(size=9), range=c(0.5,5.5)),
             legend=list(orientation="h",y=1.08,font=list(size=9)),
             margin=list(t=40,b=50,l=90,r=10),
             plot_bgcolor="transparent", paper_bgcolor="transparent",
             hovermode="closest") %>%
      add_annotations(x=med_t, y=3,
                      text=paste0("Median\nturnout\n",round(med_t,1),"%"),
                      showarrow=FALSE, font=list(size=8,color="#888888"),
                      xanchor="left", xshift=4)
  })
  
  output$horizChart <- renderPlotly({
    d <- filtered()
    if (nrow(d) == 0) return(empty_plot())
    if (nrow(d) > 10) d <- head(d, 10)
    plot_ly(d, y=~substr(addr,1,22), x=~reg,
            name="Reg. Voters", type="bar", orientation="h",
            marker=list(color="rgba(41,128,185,.8)")) %>%
      add_trace(x=~voters, name="Voter Turnout",
                marker=list(color="rgba(245,124,32,.85)")) %>%
      layout(barmode="group",
             xaxis=list(title="Count",tickfont=list(size=9)),
             yaxis=list(title="",tickfont=list(size=8)),
             legend=list(orientation="h",y=1.12,font=list(size=9)),
             margin=list(t=10,b=30,l=165,r=10),
             plot_bgcolor="transparent", paper_bgcolor="transparent")
  })
  
  output$boothTable <- renderDT({
    d <- filtered() %>%
      transmute(
        Year       = year,  `AE/LE` = election_type,
        Region     = region, District = constituency,
        Tehsil     = tehsil, Block = block,
        Assembly   = assembly,
        Address    = substr(addr, 1, 35),
        Difficulty = diff,
        `BJP %`    = paste0(round(BJP_pct,1), "%"),
        Winner     = winner,
        `Win %`    = paste0(round(winPct,1),  "%"),
        `Runner 1` = runner,
        `Run1 %`   = paste0(round(r1Pct,1),   "%"),
        `Runner 2` = r2,
        `Run2 %`   = paste0(round(r2Pct,1),   "%"),
        Turnout    = paste0(turnout, "%"),
        Margin     = paste0(round(margin,1),   "%"),
        Voters     = format(voters, big.mark=","),
        Registered = format(reg,    big.mark=","),
        SP=SP, BJP=BJP, BSP=BSP, INC=INC,
        SBSP=SBSP, AIMIM=AIMIM, NISHAD=NISHAD,
        JAP=JAP, MKD=MKD, VSIP=VSIP, CPI=CPI,
        BMU=BMU, QED=QED, NOTA=NOTA, IND=IND
      )
    if (!HAS_TEHSIL) d <- d[, names(d) != "Tehsil"]
    if (!HAS_BLOCK)  d <- d[, names(d) != "Block"]
    datatable(d,
              options = list(pageLength=8, scrollX=TRUE, dom="frtip",
                             columnDefs = list(
                               list(className="dt-center", targets=8:18))),
              rownames = FALSE, class = "compact stripe hover") %>%
      formatStyle("Difficulty",
                  backgroundColor = styleEqual(names(DIFF_COLORS), paste0(DIFF_COLORS,"33")),
                  color = styleEqual(names(DIFF_COLORS), DIFF_COLORS),
                  fontWeight = "bold") %>%
      formatStyle("Winner",
                  color = styleEqual(names(PARTY_COLORS), PARTY_COLORS),
                  fontWeight = "bold")
  })
  
  # ── Maps ─────────────────────────────────────────────────
  map_popup <- function(d) paste0(
    "<b style='color:#F57C20'>", d$addr, "</b><br>",
    "<b>Year:</b> ", d$year, " | <b>Type:</b> ", d$election_type, "<br>",
    "<b>Difficulty:</b> <span style='color:", unname(DIFF_COLORS[d$diff]),
    "'>", d$diff, "</span><br>",
    "<b>BJP %:</b> ", round(d$BJP_pct,1), "%<br>",
    "<b>Registered:</b> ", format(d$reg, big.mark=","), "<br>",
    "<b>Turnout:</b> ", d$turnout, "%<br>",
    "<b>Winner:</b> ", d$winner, " (", d$winPct, "%)<br>",
    "<b>Runner Up:</b> ", d$runner, " (", d$r1Pct, "%)<br>",
    "<b>Margin:</b> ", d$margin, "%")
  
  base_map <- function(output_id, tile_id) {
    renderLeaflet({
      leaflet() %>%
        addProviderTiles("OpenStreetMap", layerId = tile_id) %>%
        setView(lng = 80.91, lat = 26.85, zoom = 7)
    })
  }
  output$dashMap <- base_map("dashMap", "dtile")
  output$fullMap <- base_map("fullMap", "etile")
  
  observeEvent(input$dash_sat,
               leafletProxy("dashMap") %>% addProviderTiles("Esri.WorldImagery", layerId="dtile"))
  observeEvent(input$dash_osm,
               leafletProxy("dashMap") %>% addProviderTiles("OpenStreetMap",     layerId="dtile"))
  observeEvent(input$em_sat,
               leafletProxy("fullMap") %>% addProviderTiles("Esri.WorldImagery", layerId="etile"))
  observeEvent(input$em_osm,
               leafletProxy("fullMap") %>% addProviderTiles("OpenStreetMap",     layerId="etile"))
  
  update_map <- function(proxy, d_geo, sel_con, sel_bth, sel_asm) {
    proxy <- proxy %>% clearMarkers()
    if (nrow(d_geo) == 0) return()
    if (sel_bth != "" || sel_asm != "") {
      cols <- unname(DIFF_COLORS[d_geo$diff])
      proxy %>%
        addCircleMarkers(data=d_geo, lat=~lat, lng=~lng, radius=9,
                         fillColor=cols, color="white", weight=1.5,
                         fillOpacity=0.92,
                         popup=map_popup(d_geo),
                         label=~paste0(substr(addr,1,25)," [",diff,"]"))
      if (nrow(d_geo) == 1)
        proxy %>% setView(lng=d_geo$lng[1], lat=d_geo$lat[1], zoom=14)
      else
        proxy %>% fitBounds(min(d_geo$lng,na.rm=TRUE)-.01,
                            min(d_geo$lat,na.rm=TRUE)-.01,
                            max(d_geo$lng,na.rm=TRUE)+.01,
                            max(d_geo$lat,na.rm=TRUE)+.01)
    } else if (sel_con != "") {
      asms <- d_geo %>% group_by(assembly) %>%
        summarise(lat=mean(lat,na.rm=TRUE), lng=mean(lng,na.rm=TRUE),
                  winner=names(sort(table(winner),decreasing=TRUE))[1],
                  voters=sum(voters),
                  winPct=round(mean(winPct)), .groups="drop")
      proxy %>%
        addCircleMarkers(data=asms, lat=~lat, lng=~lng, radius=11,
                         fillColor=unname(PARTY_COLORS[asms$winner]),
                         color="white", weight=2, fillOpacity=0.9,
                         popup=paste0("<b>",asms$assembly,"</b><br>",
                                      asms$winner," (",asms$winPct,"%)<br>",
                                      "Voters: ",format(asms$voters,big.mark=",")),
                         label=~assembly) %>%
        fitBounds(min(asms$lng)-.04, min(asms$lat)-.04,
                  max(asms$lng)+.04, max(asms$lat)+.04)
    } else {
      proxy %>%
        addCircleMarkers(data=constituencies, lat=~lat, lng=~lng, radius=12,
                         fillColor=unname(PARTY_COLORS[constituencies$winner]),
                         color="white", weight=2, fillOpacity=0.9,
                         popup=paste0("<b>",constituencies$constituency,"</b><br>",
                                      constituencies$winner," (",constituencies$winPct,"%)<br>",
                                      "Reg: ",format(constituencies$reg,big.mark=",")),
                         label=~constituency) %>%
        fitBounds(77.0, 23.5, 85.0, 30.5)
    }
  }
  
  observe({
    d     <- filtered()
    d_geo <- valid_geo(d)
    if (nrow(d_geo) == 0) d_geo <- valid_geo_loose(d)
    update_map(leafletProxy("dashMap"), d_geo,
               input$sel_con, input$sel_bth, input$sel_asm)
  })
  observe({
    d     <- em_filtered()
    d_geo <- valid_geo(d)
    if (nrow(d_geo) == 0) d_geo <- valid_geo_loose(d)
    update_map(leafletProxy("fullMap"), d_geo,
               input$em_con, input$em_bth, input$em_asm)
  })
  
  output$em_stats <- renderTable({
    d <- em_filtered()
    data.frame(
      Metric = c("Booths","Avg Turnout","Reg. Voters","Top Party"),
      Value  = c(
        format(nrow(d), big.mark=","),
        paste0(round(mean(d$turnout, na.rm=TRUE), 1), "%"),
        format(sum(d$reg, na.rm=TRUE), big.mark=","),
        if (nrow(d)>0) names(sort(table(d$winner),decreasing=TRUE))[1] else "—"
      )
    )
  }, striped=FALSE, bordered=FALSE, spacing="xs",
  colnames=FALSE, width="100%", rownames=FALSE)
  
} # end server

# ============================================================
# 6. RUN
# ============================================================
shinyApp(ui = ui, server = server)