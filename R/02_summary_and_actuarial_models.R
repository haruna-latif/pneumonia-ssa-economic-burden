# Economic burden of pneumonia in sub-Saharan Africa: systematic review
# Step 2: summary tables and supplementary actuarial models
#
# Run AFTER 01_convert_to_2025_USD.R, from the top folder of the repository.
# Needs only base R and the MASS package (installed with R).
#
# Outputs (tables in "tables", figures in "figures"):
#   Table_S1_by_setting_perspective.csv, Table_S2_by_country.csv,
#   Table_S3_distribution_fits.csv, Table_S4_credibility.csv,
#   Table_S5_aggregate_loss.csv, Table_S5b_severity_sensitivity.csv, Table_S6_scenarios.csv,
#   Table_S7_conversion_sensitivity.csv and Figures 1 to 8 (PNG)
#
# IMPORTANT for interpretation: each data point is a study-level mean or
# median cost for one setting, not an individual patient's cost. The fitted
# distributions therefore describe how average costs vary ACROSS settings
# and studies in sub-Saharan Africa (between-setting heterogeneity).

library(MASS)
set.seed(2025)
dir.create("tables", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

d <- read.csv("tables/costs_2025USD.csv", stringsAsFactors = FALSE)
d$partial <- as.logical(toupper(d$partial))
d$cost <- d$cost_2025_USD

# ---------------------------------------------------------------------------
# 1. Classify every figure by setting and perspective
# ---------------------------------------------------------------------------
d$setting_group <- with(d, ifelse(grepl("Inpatient|admission|PICU|Treated pneumonia", setting), "Inpatient",
                           ifelse(grepl("Community", setting), "Community (iCCM)",
                           ifelse(grepl("Outpatient|Home|Facility-based|Emergency", setting), "Outpatient",
                                  "All episodes"))))
d$perspective_group <- with(d, ifelse(grepl("Household", perspective), "Household",
                               ifelse(grepl("Societal", perspective), "Societal",
                                      "Provider / health system")))

# ---------------------------------------------------------------------------
# 2. Analysis sets for the models (one full cost per study setting)
#    Provider = provider/government/health-sector total or direct medical
#    cost per case. Partial costs and private fee-for-service tariffs are
#    excluded; for record 84 the fuller top-down estimate is used; for
#    record 1016 the standard iCCM arm (not the mHealth intervention) is used.
# ---------------------------------------------------------------------------
prov <- subset(d, perspective_group == "Provider / health system" & !partial &
                 !grepl("fee-for-service|PICU|bottom-up|mHealth", paste(setting, component)))
out_set <- subset(prov, setting_group %in% c("Outpatient", "Community (iCCM)") &
                    !grepl("Emergency", setting))
inp_set <- subset(prov, setting_group == "Inpatient")
write.csv(rbind(cbind(set = "Outpatient/community", out_set),
                cbind(set = "Inpatient", inp_set)),
          "tables/analysis_sets_used.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. Summary tables
# ---------------------------------------------------------------------------
summ <- function(x) c(n = length(x), median = median(x),
                      Q1 = unname(quantile(x, 0.25)), Q3 = unname(quantile(x, 0.75)),
                      min = min(x), max = max(x), mean = mean(x))
d$component_group <- with(d, ifelse(grepl("^Direct medical", component), "Direct medical",
                            ifelse(grepl("^Direct \\(medical", component), "Direct (medical + non-medical)",
                            ifelse(grepl("^Direct non-medical", component), "Direct non-medical",
                            ifelse(grepl("^Indirect", component), "Indirect",
                            ifelse(grepl("Out-of-pocket", component), "Out-of-pocket", "Total"))))))
# one figure per study setting: drop bottom-up duplicates of record 84 and the mHealth arm of 1016
full <- subset(d, !partial & !grepl("bottom-up|mHealth", paste(setting, component)))
t1 <- aggregate(cost ~ setting_group + perspective_group + component_group, data = full, FUN = summ)
t1 <- do.call(data.frame, t1); names(t1) <- sub("cost\\.", "", names(t1))
t1[, -(1:3)] <- round(t1[, -(1:3)], 2)
write.csv(t1, "tables/Table_S1_by_setting_perspective.csv", row.names = FALSE)

t2 <- aggregate(cost ~ country + setting_group, data = rbind(out_set, inp_set), FUN = summ)
t2 <- do.call(data.frame, t2); names(t2) <- sub("cost\\.", "", names(t2))
t2[, -(1:2)] <- round(t2[, -(1:2)], 2)
t2 <- t2[order(t2$setting_group, -t2$median), ]
write.csv(t2, "tables/Table_S2_by_country.csv", row.names = FALSE)

# Descriptive statistics of the two modelling sets (moment-based)
#   skewness = m3 / m2^(3/2), excess kurtosis = m4 / m2^2 - 3,
#   coefficient of variation = SD / mean
desc <- function(x, label) {
  m <- mean(x); m2 <- mean((x - m)^2); m3 <- mean((x - m)^3); m4 <- mean((x - m)^4)
  data.frame(set = label, n = length(x), mean = m, sd = sd(x), cv = sd(x) / m,
             median = median(x), Q1 = unname(quantile(x, .25)), Q3 = unname(quantile(x, .75)),
             min = min(x), max = max(x), skewness = m3 / m2^1.5, excess_kurtosis = m4 / m2^2 - 3)
}
t0 <- rbind(desc(out_set$cost, "Outpatient/community, provider (per case)"),
            desc(inp_set$cost, "Inpatient, provider (per admission)"),
            desc(subset(full, perspective_group == "Household" & component_group %in% c("Total", "Out-of-pocket") &
                          setting_group == "Inpatient")$cost, "Inpatient, household total or OOP"),
            desc(subset(full, perspective_group == "Household" & component_group %in% c("Total", "Out-of-pocket") &
                          setting_group %in% c("Outpatient", "Community (iCCM)"))$cost, "Outpatient/community, household total or OOP"))
t0[, -(1:2)] <- round(t0[, -(1:2)], 2)
write.csv(t0, "tables/Table_S0_descriptive_statistics.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# 4. Distribution fitting by maximum likelihood (MLE)
#    Candidates: exponential, gamma, lognormal
#    Comparison: log-likelihood, AIC, Kolmogorov-Smirnov test, and a
#    chi-square goodness-of-fit test where n allows 5 classes of equal
#    expected frequency (expected count >= 5 needs n >= 25)
# ---------------------------------------------------------------------------
fit_one <- function(x, label) {
  s <- 100                                   # rescale for numerical stability
  y <- x / s
  fe <- fitdistr(y, "exponential")
  fg <- fitdistr(y, "gamma", start = list(shape = mean(y)^2 / var(y), rate = mean(y) / var(y)),
                 lower = c(1e-4, 1e-6))
  fl <- fitdistr(y, "lognormal")
  pars <- list(
    exponential = c(rate = unname(fe$estimate["rate"]) / s),
    gamma       = c(shape = unname(fg$estimate["shape"]), rate = unname(fg$estimate["rate"]) / s),
    lognormal   = c(meanlog = unname(fl$estimate["meanlog"]) + log(s), sdlog = unname(fl$estimate["sdlog"])))
  ll <- c(exponential = sum(dexp(x, pars$exponential, log = TRUE)),
          gamma = sum(dgamma(x, pars$gamma[1], pars$gamma[2], log = TRUE)),
          lognormal = sum(dlnorm(x, pars$lognormal[1], pars$lognormal[2], log = TRUE)))
  k <- c(exponential = 1, gamma = 2, lognormal = 2)
  cdf <- list(exponential = function(q) pexp(q, pars$exponential),
              gamma = function(q) pgamma(q, pars$gamma[1], pars$gamma[2]),
              lognormal = function(q) plnorm(q, pars$lognormal[1], pars$lognormal[2]))
  ks_p <- sapply(names(cdf), function(m) suppressWarnings(ks.test(x, cdf[[m]])$p.value))
  chi <- sapply(names(cdf), function(m) {
    n <- length(x); if (n < 25) return(c(stat = NA, p = NA))
    br <- quantile(x, probs = seq(0, 1, length.out = 6)); br[1] <- 0; br[6] <- Inf
    obs <- table(cut(x, br, include.lowest = TRUE))
    ex  <- n * diff(cdf[[m]](br))
    st  <- sum((obs - ex)^2 / ex)
    df  <- 5 - 1 - k[m]
    c(stat = st, p = pchisq(st, df, lower.tail = FALSE))
  })
  means <- c(exponential = 1 / pars$exponential,
             gamma = pars$gamma[1] / pars$gamma[2],
             lognormal = exp(pars$lognormal[1] + pars$lognormal[2]^2 / 2))
  res <- data.frame(set = label, row.names = NULL, model = names(ll), n = length(x),
                    param1 = sapply(pars, `[`, 1), param2 = sapply(pars, function(p) if (length(p) > 1) p[2] else NA),
                    fitted_mean = means, sample_mean = mean(x),
                    logLik = ll, AIC = 2 * k - 2 * ll,
                    KS_p = ks_p, chisq = chi["stat", ], chisq_p = chi["p", ])
  res$best <- res$AIC == min(res$AIC)
  list(table = res, pars = pars, x = x)
}
f_out <- fit_one(out_set$cost, "Outpatient/community, provider")
f_inp <- fit_one(inp_set$cost, "Inpatient, provider")
t3 <- rbind(f_out$table, f_inp$table)
t3[, 4:12] <- signif(t3[, 4:12], 5)
write.csv(t3, "tables/Table_S3_distribution_fits.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# 5. Bühlmann credibility by country (outpatient/community provider cost)
#    Z_i = n_i / (n_i + k), k = EPV / VHM
#    Credibility estimate = Z_i * (country mean) + (1 - Z_i) * (overall mean)
#    EPV (expected process variance) and VHM (variance of hypothetical means)
#    are estimated nonparametrically from the data (unequal n_i).
#    Run on the cost scale and, because costs are right-skewed, on the log
#    scale (estimate back-transformed with exp()).
# ---------------------------------------------------------------------------
buhl <- function(v, grp) {
  g <- split(v, grp); ni <- sapply(g, length); xi <- sapply(g, mean); n <- sum(ni)
  mu <- sum(ni * xi) / n
  epv <- sum(sapply(g[ni > 1], function(z) sum((z - mean(z))^2))) / sum(ni[ni > 1] - 1)
  vhm <- (sum(ni * (xi - mu)^2) - (length(g) - 1) * epv) / (n - sum(ni^2) / n)
  k <- if (vhm > 0) epv / vhm else Inf
  Z <- if (is.finite(k)) ni / (ni + k) else rep(0, length(ni))
  list(ni = ni, xi = xi, mu = mu, epv = epv, vhm = vhm, k = k, Z = Z, est = Z * xi + (1 - Z) * mu)
}
b_raw <- buhl(out_set$cost, out_set$country)
b_log <- buhl(log(out_set$cost), out_set$country)
mu <- b_raw$mu
t4 <- data.frame(country = names(b_raw$ni), n = b_raw$ni,
                 country_mean = round(b_raw$xi, 2),
                 Z_cost_scale = round(b_raw$Z, 3), estimate_cost_scale = round(b_raw$est, 2),
                 Z_log_scale = round(b_log$Z, 3), estimate_log_scale = round(exp(b_log$est), 2))
t4 <- t4[order(-t4$estimate_log_scale), ]
par_rows <- data.frame(country = c("Overall mean (cost scale)", "EPV / VHM / k (cost scale)",
                                   "Overall geometric mean (log scale)", "EPV / VHM / k (log scale)"),
                       n = sum(b_raw$ni), country_mean = NA, Z_cost_scale = NA,
                       estimate_cost_scale = c(round(b_raw$mu, 2), NA, NA, NA),
                       Z_log_scale = NA, estimate_log_scale = c(NA, NA, round(exp(b_log$mu), 2), NA))
par_rows$note <- c("", sprintf("EPV=%.3f; VHM=%.3f; k=%s", b_raw$epv, b_raw$vhm, format(round(b_raw$k, 2))),
                   "", sprintf("EPV=%.4f; VHM=%.4f; k=%.2f", b_log$epv, b_log$vhm, b_log$k))
t4$note <- ""
write.csv(rbind(t4, par_rows), "tables/Table_S4_credibility.csv", row.names = FALSE)
cat(sprintf("Credibility (cost scale): EPV=%.2f VHM=%.3f; (log scale): EPV=%.4f VHM=%.4f k=%.2f\n",
            b_raw$epv, b_raw$vhm, b_log$epv, b_log$vhm, b_log$k))

# ---------------------------------------------------------------------------
# 6. Collective risk model and Monte Carlo aggregate loss
#    Illustrative cohort: 100,000 children under five, one year
#    Frequency: N ~ Poisson(lambda), lambda = cohort x incidence
#      incidence 0.29 episodes per child-year in developing countries and
#      7-13% of episodes severe enough to need hospital care
#      (Rudan et al., 2008, Bull World Health Organ 86:408-416); base case
#      uses the 10% midpoint.
#    Severity: best-AIC distribution fitted in step 4 for each setting.
#    S = sum of outpatient/community costs + sum of inpatient costs
#    Analytical check (compound Poisson): E[S] = lambda E[X],
#    Var[S] = lambda E[X^2]
# ---------------------------------------------------------------------------
cohort <- 100000; inc <- 0.29; p_hosp <- 0.10; sims <- 20000
best_model <- function(f) f$table$model[which.min(f$table$AIC)]
rsev <- function(f, m, mult = 1) {
  p <- f$pars[[m]]
  switch(m,
    exponential = function(k) rexp(k, p["rate"] / mult),
    gamma       = function(k) rgamma(k, p["shape"], p["rate"] / mult),
    lognormal   = function(k) rlnorm(k, p["meanlog"] + log(mult), p["sdlog"]))
}
moments <- function(f, m, mult = 1) {
  p <- f$pars[[m]]
  switch(m,
    exponential = c(EX = unname(mult / p["rate"]), EX2 = unname(2 * (mult / p["rate"])^2)),
    gamma = c(EX = unname(mult * p["shape"] / p["rate"]),
              EX2 = unname(mult^2 * p["shape"] * (p["shape"] + 1) / p["rate"]^2)),
    lognormal = c(EX = unname(mult * exp(p["meanlog"] + p["sdlog"]^2 / 2)),
                  EX2 = unname(mult^2 * exp(2 * p["meanlog"] + 2 * p["sdlog"]^2))))
}
# Sum of N iid severities: exact for gamma (sum is gamma(N*shape)), exponential
# (gamma(N,rate)); for lognormal, simulated draw by draw.
sum_sev <- function(N, f, m, mult = 1) {
  p <- f$pars[[m]]
  if (m == "gamma") return(rgamma(length(N), N * p["shape"], p["rate"] / mult))
  if (m == "exponential") return(rgamma(length(N), N, p["rate"] / mult))
  sapply(N, function(k) sum(rlnorm(k, p["meanlog"] + log(mult), p["sdlog"])))
}
run_sim <- function(freq_mult = 1, sev_mult = 1, sims_n = sims, mods = NULL) {
  lam_out <- cohort * inc * (1 - p_hosp) * freq_mult
  lam_inp <- cohort * inc * p_hosp * freq_mult
  m_o <- if (is.null(mods)) best_model(f_out) else mods[1]
  m_i <- if (is.null(mods)) best_model(f_inp) else mods[2]
  N_o <- rpois(sims_n, lam_out); N_i <- rpois(sims_n, lam_inp)
  S <- sum_sev(N_o, f_out, m_o, sev_mult) + sum_sev(N_i, f_inp, m_i, sev_mult)
  mo <- moments(f_out, m_o, sev_mult); mi <- moments(f_inp, m_i, sev_mult)
  ES <- lam_out * mo["EX"] + lam_inp * mi["EX"]
  VS <- lam_out * mo["EX2"] + lam_inp * mi["EX2"]
  v95 <- unname(quantile(S, 0.95)); v99 <- unname(quantile(S, 0.99))
  list(S = S, row = data.frame(
    lambda_outpatient = lam_out, lambda_inpatient = lam_inp,
    severity_outpatient = m_o, severity_inpatient = m_i,
    mean_S_simulated = mean(S), mean_S_analytical = unname(ES),
    sd_S_simulated = sd(S), sd_S_analytical = sqrt(unname(VS)),
    VaR95 = v95, TVaR95 = mean(S[S >= v95]), VaR99 = v99,
    expected_cost_per_child = mean(S) / cohort,
    expected_cost_per_episode = mean(S) / (lam_out + lam_inp)))
}
base <- run_sim()
t5 <- base$row; t5[sapply(t5, is.numeric)] <- round(t5[sapply(t5, is.numeric)], 2)
write.csv(t5, "tables/Table_S5_aggregate_loss.csv", row.names = FALSE)

# Scenario analysis: +/-20% on frequency and on severity
sc <- data.frame(scenario = c("Base case", "Frequency -20%", "Frequency +20%",
                              "Severity -20%", "Severity +20%",
                              "Both -20%", "Both +20%"),
                 fm = c(1, .8, 1.2, 1, 1, .8, 1.2), sm = c(1, 1, 1, .8, 1.2, .8, 1.2))
t6 <- do.call(rbind, lapply(seq_len(nrow(sc)), function(i) {
  r <- run_sim(sc$fm[i], sc$sm[i], sims_n = 5000)$row
  data.frame(scenario = sc$scenario[i], mean_S = round(r$mean_S_simulated, 0),
             VaR95 = round(r$VaR95, 0), TVaR95 = round(r$TVaR95, 0),
             cost_per_child = round(r$expected_cost_per_child, 2))
}))
t6$change_vs_base_pct <- round(100 * (t6$mean_S / t6$mean_S[1] - 1), 1)
write.csv(t6, "tables/Table_S6_scenarios.csv", row.names = FALSE)

# Sensitivity to the choice of severity distribution: the AIC values of the
# three distributions are close, so the aggregate model is repeated with the
# lognormal in both settings (same cohort, 20,000 simulated years, own seed).
set.seed(20261107)
ln <- run_sim(mods = c("lognormal", "lognormal"))$row
sev_sens <- rbind(
  data.frame(severity = "Gamma (base case)", base$row[, c("mean_S_simulated", "mean_S_analytical", "sd_S_simulated", "sd_S_analytical", "VaR95", "TVaR95", "expected_cost_per_child")]),
  data.frame(severity = "Lognormal", ln[, c("mean_S_simulated", "mean_S_analytical", "sd_S_simulated", "sd_S_analytical", "VaR95", "TVaR95", "expected_cost_per_child")]))
sev_sens[sapply(sev_sens, is.numeric)] <- round(sev_sens[sapply(sev_sens, is.numeric)], 2)
write.csv(sev_sens, "tables/Table_S5b_severity_sensitivity.csv", row.names = FALSE)
cat("\nSeverity sensitivity (gamma vs lognormal):\n"); print(sev_sens)

# Conversion-method sensitivity (protocol method vs US-inflation-only)
t7 <- d[, c("record_no", "country", "setting", "component", "cost_2025_USD",
            "cost_2025_USD_US_inflation_only")]
t7$ratio <- round(t7$cost_2025_USD / t7$cost_2025_USD_US_inflation_only, 2)
write.csv(t7, "tables/Table_S7_conversion_sensitivity.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# 7. Figures (base R graphics, PNG, 300 dpi)
# ---------------------------------------------------------------------------
png_open <- function(f, w = 8, h = 5.5) {
  png(file.path("figures", f), width = w, height = h, units = "in", res = 300)
  par(family = "sans", bty = "l", las = 1)
}
col_set <- c("Inpatient" = "#1F4E79", "Outpatient" = "#2E75B6", "Community (iCCM)" = "#9DC3E6",
             "All episodes" = "#7F7F7F")

# Figure 1: provider cost per case by country and setting (log scale)
png_open("Figure2_cost_by_country.png", 8, 7)
pd <- rbind(out_set, inp_set); pd <- pd[order(pd$setting_group, pd$cost), ]
par(mar = c(4.5, 11, 2, 1))
plot(pd$cost, seq_len(nrow(pd)), log = "x", pch = 19, col = col_set[pd$setting_group],
     yaxt = "n", ylab = "", xlab = "Provider cost per case, 2025 US$ (log scale)",
     main = "Figure 2. Provider cost per case by country and setting", cex.main = 0.95)
axis(2, at = seq_len(nrow(pd)), labels = paste0(pd$country, " (", pd$record_no, ")"), las = 1, cex.axis = 0.6)
legend("bottomright", legend = c("Inpatient", "Outpatient", "Community (iCCM)"),
       col = col_set[c("Inpatient", "Outpatient", "Community (iCCM)")], pch = 19, bty = "n", cex = 0.8)
dev.off()

# Figure 2: box plots by setting and perspective
png_open("Figure3_boxplot_setting_perspective.png")
full <- subset(full, component_group %in% c("Total", "Direct medical"))  # whole-episode costs only
full$grp <- factor(paste(full$setting_group, full$perspective_group, sep = "\n"))
par(mar = c(7, 5, 2, 1))
boxplot(cost ~ grp, data = full, log = "y", las = 2, cex.axis = 0.55, col = "grey90",
        ylab = "Cost per episode, 2025 US$ (log scale)", xlab = "",
        main = "Figure 3. Total cost per pneumonia episode by setting and perspective")
stripchart(cost ~ grp, data = full, vertical = TRUE, add = TRUE, method = "jitter", pch = 19, cex = 0.5, col = "#2166AC")
dev.off()

# Figures 3 and 4: histograms with fitted densities
dens_plot <- function(f, file, title) {
  png_open(file)
  x <- f$x; xs <- seq(min(x) * 0.5, max(x) * 1.2, length.out = 400)
  hist(x, breaks = "FD", freq = FALSE, col = "grey85", border = "white",
       main = title, xlab = "Cost per case, 2025 US$", ylim = c(0, max(dgamma(xs, f$pars$gamma[1], f$pars$gamma[2]),
                                                                      dlnorm(xs, f$pars$lognormal[1], f$pars$lognormal[2]),
                                                                      hist(x, breaks = "FD", plot = FALSE)$density) * 1.1))
  lines(xs, dgamma(xs, f$pars$gamma[1], f$pars$gamma[2]), lwd = 2, col = "#1F4E79")
  lines(xs, dlnorm(xs, f$pars$lognormal[1], f$pars$lognormal[2]), lwd = 2, col = "#2166AC", lty = 2)
  lines(xs, dexp(xs, f$pars$exponential), lwd = 2, col = "#9DC3E6", lty = 3)
  rug(x)
  legend("topright", c("Gamma", "Lognormal", "Exponential"), col = c("#1F4E79", "#2166AC", "#9DC3E6"),
         lty = 1:3, lwd = 2, bty = "n")
  dev.off()
}
dens_plot(f_out, "Figure4_fit_outpatient.png", "Figure 4. Fitted distributions: outpatient and community provider costs")
dens_plot(f_inp, "Figure5_fit_inpatient.png", "Figure 5. Fitted distributions: inpatient provider costs")

# Figure 5: Q-Q plots for gamma and lognormal (both settings)
png_open("Figure6_QQ_plots.png", 8, 7)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1), oma = c(0, 0, 2.5, 0))
qq <- function(f, m, title) {
  x <- sort(f$x); pp <- ppoints(length(x))
  q <- if (m == "gamma") qgamma(pp, f$pars$gamma[1], f$pars$gamma[2]) else qlnorm(pp, f$pars$lognormal[1], f$pars$lognormal[2])
  plot(q, x, pch = 19, col = "#2166AC", xlab = paste("Theoretical", m, "quantiles"), ylab = "Observed cost", main = title)
  abline(0, 1, lty = 2)
}
qq(f_out, "gamma", "Outpatient: gamma"); qq(f_out, "lognormal", "Outpatient: lognormal")
qq(f_inp, "gamma", "Inpatient: gamma");  qq(f_inp, "lognormal", "Inpatient: lognormal")
mtext("Figure 6. Q-Q plots of fitted gamma and lognormal distributions", outer = TRUE, line = 0.8, font = 2, cex = 1.1)
dev.off()

# Figure 6: credibility estimates vs raw country means
png_open("Figure7_credibility.png")
tt <- t4[order(t4$estimate_log_scale), ]; tt$credibility_estimate <- tt$estimate_log_scale; mu <- exp(b_log$mu)
par(mar = c(4.5, 8, 2, 1))
plot(tt$country_mean, seq_len(nrow(tt)), pch = 1, xlim = range(c(tt$country_mean, tt$credibility_estimate, mu)),
     yaxt = "n", ylab = "", xlab = "Outpatient/community provider cost per case, 2025 US$",
     main = "Figure 7. B\u00fchlmann credibility estimates by country (log scale)")
points(tt$credibility_estimate, seq_len(nrow(tt)), pch = 19, col = "#1F4E79")
segments(tt$country_mean, seq_len(nrow(tt)), tt$credibility_estimate, seq_len(nrow(tt)), col = "grey60")
abline(v = mu, lty = 2)
axis(2, at = seq_len(nrow(tt)), labels = paste0(tt$country, " (n=", tt$n, ")"), las = 1, cex.axis = 0.7)
legend("bottomright", c("Country mean", "Credibility estimate", "Overall geometric mean"),
       pch = c(1, 19, NA), lty = c(NA, NA, 2), col = c("black", "#1F4E79", "black"), bty = "n", cex = 0.8)
dev.off()

# Figure 7: simulated aggregate loss distribution
png_open("Figure8_aggregate_loss.png")
S <- base$S / 1e6
hist(S, breaks = 60, col = "grey85", border = "white", freq = FALSE,
     main = "Figure 8. Simulated annual provider cost of pneumonia\n(100,000 children under five)",
     xlab = "Aggregate cost, million 2025 US$")
abline(v = c(mean(S), quantile(S, .95), mean(S[S >= quantile(S, .95)])),
       col = c("black", "#1F4E79", "#2166AC"), lty = c(1, 2, 3), lwd = 2)
legend("topright", c("Mean", "VaR 95%", "TVaR 95%"), col = c("black", "#1F4E79", "#2166AC"),
       lty = 1:3, lwd = 2, bty = "n")
dev.off()

# Figure 8: scenario analysis (change in expected aggregate cost)
png_open("Figure9_scenarios.png")
s2 <- t6[-1, ]
par(mar = c(4.5, 9, 2, 1))
barplot(s2$change_vs_base_pct, names.arg = s2$scenario, horiz = TRUE, las = 1, cex.names = 0.8,
        col = ifelse(s2$change_vs_base_pct > 0, "#1F4E79", "#2166AC"),
        xlab = "Change in expected annual cost vs base case (%)", main = "Figure 9. Scenario analysis (\u00b120%)")
abline(v = 0)
dev.off()

cat("\nAll tables (tables/) and figures (figures/) written under", getwd(), "\n")
print(t3[, c("set", "model", "fitted_mean", "sample_mean", "AIC", "KS_p", "chisq_p", "best")])
print(t5)
print(t6)
