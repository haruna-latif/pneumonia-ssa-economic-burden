# Economic burden of pneumonia in sub-Saharan Africa: systematic review
# Step 1: convert every extracted cost to 2025 US dollars
#
# HOW TO RUN
#   Run from the top folder of the repository (see README.md), or use run_all.R.
#   Input : data/costs_input.csv and the two World Bank zip files in
#           data/worldbank/ (consumer price index and official exchange rate).
#   Output: tables/costs_2025USD.csv
#
# METHOD (protocol formula)
#   C_2025_USD = C_t_LCU x (CPI_2025 / CPI_t) / XR_2025
#   C_t_LCU : cost in local currency at the study's price year t
#   CPI     : World Bank consumer price index of the study country
#             (FP.CPI.TOTL, 2010 = 100)
#   XR      : official exchange rate, local currency per US$, period average
#             (PA.NUS.FCRF)
#   A cost reported in US$ is first turned back into local currency with the
#   rate the study used (if stated) or the World Bank rate for the year the
#   authors converted it.
#
# MISSING 2025 VALUES
#   When a country's 2025 CPI or 2025 exchange rate is not yet published, the
#   cost is converted at the latest year Y with BOTH values available, then
#   brought from Y to 2025 with US inflation:
#   C_2025_USD = C_t_LCU x (CPI_Y / CPI_t) / XR_Y x (USCPI_2025 / USCPI_Y)
#   When Y = 2025 the extra factor is 1, so this is the protocol formula.
#   The World Bank file has no 2025 US CPI yet, so the 2025 value is added
#   from the US Bureau of Labor Statistics CPI-U annual averages
#   (2024 = 313.689, 2025 = 321.943; BLS "Summary of annual and semi-annual
#   indexes", bls.gov). Every row that uses this route is flagged.

target_year <- 2025

# unpack any World Bank zip files in the folder
dir.create("working_files/worldbank", recursive = TRUE, showWarnings = FALSE)
for (z in list.files("data/worldbank", pattern = "^API_.*\\.zip$", full.names = TRUE))
  unzip(z, exdir = "working_files/worldbank", overwrite = TRUE)

read_wdi <- function(pattern) {
  f <- list.files("working_files/worldbank", pattern = pattern, full.names = TRUE)
  f <- f[!grepl("Metadata", f) & grepl("\\.csv$", f)]
  if (length(f) == 0)
    stop("World Bank file not found: ", pattern,
         "\nWorking directory is: ", getwd(),
         "\nRun this script from the top folder of the repository.")
  d <- read.csv(f[1], skip = 4, check.names = FALSE)
  d <- d[, c("Country Code", grep("^[0-9]{4}$", names(d), value = TRUE))]
  long <- reshape(d, direction = "long", varying = names(d)[-1],
                  v.names = "value", timevar = "year",
                  times = as.integer(names(d)[-1]), idvar = "Country Code")
  names(long)[1] <- "iso3"
  long[!is.na(long$value), c("iso3", "year", "value")]
}

cpi <- read_wdi("^API_FP.CPI.TOTL")
xr  <- read_wdi("^API_PA.NUS.FCRF")

# add the 2025 US CPI (BLS ratio applied to the World Bank 2024 value)
us2024 <- cpi$value[cpi$iso3 == "USA" & cpi$year == 2024]
if (!any(cpi$iso3 == "USA" & cpi$year == 2025))
  cpi <- rbind(cpi, data.frame(iso3 = "USA", year = 2025,
                               value = us2024 * 321.943 / 313.689))

lookup <- function(tab, iso, yr) {
  v <- tab$value[tab$iso3 == iso & tab$year == yr]
  if (length(v) == 0) NA_real_ else v[1]
}
# latest year (up to 2025) with BOTH CPI and exchange rate for the country
ref_year <- function(iso) {
  both <- intersect(cpi$year[cpi$iso3 == iso], xr$year[xr$iso3 == iso])
  both <- both[both <= target_year]
  if (length(both) == 0) NA_integer_ else max(both)
}

costs <- read.csv("data/costs_input.csv", stringsAsFactors = FALSE)
costs$partial <- as.logical(toupper(costs$partial))

out <- do.call(rbind, lapply(seq_len(nrow(costs)), function(i) {
  r <- costs[i, ]
  # A: cost in local currency at price year t
  if (r$currency == "USD") {
    xr_t <- if (!is.na(r$study_xr) && r$study_xr != "") as.numeric(r$study_xr)
            else lookup(xr, r$iso3, r$xr_year)
    lcu_t <- r$value * xr_t
  } else {
    xr_t  <- NA_real_
    lcu_t <- r$value
  }
  # B: inflate with local CPI to the reference year Y
  Y       <- ref_year(r$iso3)
  cpi_t   <- lookup(cpi, r$iso3, r$cpi_year)
  cpi_Y   <- lookup(cpi, r$iso3, Y)
  # C: convert to US$ at year Y, then US inflation from Y to 2025
  xr_Y    <- lookup(xr, r$iso3, Y)
  us_adj  <- lookup(cpi, "USA", target_year) / lookup(cpi, "USA", Y)
  usd     <- lcu_t * cpi_Y / cpi_t / xr_Y * us_adj
  # Sensitivity check: cost in US$ at year t, inflated with US CPI only
  # (avoids local hyperinflation/devaluation effects, e.g. South Sudan)
  usd_t <- if (r$currency == "USD") r$value
           else r$value / lookup(xr, r$iso3, r$cpi_year)
  usd_alt <- usd_t * lookup(cpi, "USA", target_year) / lookup(cpi, "USA", r$cpi_year)
  data.frame(r, xr_t = xr_t, cpi_t = cpi_t, ref_year = Y, cpi_Y = cpi_Y,
             xr_Y = xr_Y, us_cpi_factor = round(us_adj, 4),
             cost_2025_USD = round(usd, 2),
             cost_2025_USD_US_inflation_only = round(usd_alt, 2),
             flag = ifelse(Y < target_year,
                           paste0("converted at ", Y, ", then US CPI to 2025"),
                           ""))
}))

dir.create("tables", showWarnings = FALSE)
write.csv(out, "tables/costs_2025USD.csv", row.names = FALSE)
cat("\nDone. Results written to", file.path(getwd(), "tables", "costs_2025USD.csv"), "\n")
print(out[, c("record_no", "country", "setting", "perspective", "component",
              "value", "currency", "cost_2025_USD", "flag")], row.names = FALSE)
