# Economic burden of pneumonia in sub-Saharan Africa: systematic review
# Step 3: PRISMA 2020 flow diagram (Figure 1), drawn in base R
#
# All counts are typed below, so the figure can be redrawn if a number
# changes. Sources of each count:
#   database counts      = the four search exports (PubMed, Europe PMC,
#                          Cochrane Library, Global Index Medicus)
#   citation searching   = reference lists of the included reports
#   duplicates           = Zotero merge + script check, incl. one trial in two registries (1,545 - 1,263)
#   screening decisions  = the title and abstract and full-text screening records
#                          (counts typed below)
#   study type / region  = Pneumonia_SR_extraction_workbook.xlsx (Studies)

n_pubmed <- 1117; n_epmc <- 265; n_cochrane <- 105; n_gim <- 58
n_total  <- n_pubmed + n_epmc + n_cochrane + n_gim       # 1,545
n_dup    <- 282
n_screen <- n_total - n_dup                              # 1,263
n_ex_ta  <- 1200
n_sought <- n_screen - n_ex_ta                           # 63
n_notret <- 4
n_assess <- n_sought - n_notret                          # 59
n_ex_ft_e04 <- 9; n_ex_ft_e07 <- 1
n_ex_ft  <- n_ex_ft_e04 + n_ex_ft_e07                    # 10
n_reports_db <- n_assess - n_ex_ft                       # 49 reports, 48 studies
# identification via other methods: citation searching of the included reports
n_cit <- 12; n_cit_notret <- 1
n_cit_assess <- n_cit - n_cit_notret                     # 11
n_cit_ex <- c(E05 = 3, E01 = 2, E04 = 2, E02 = 1, E03 = 1, E07 = 1)
n_cit_inc <- n_cit_assess - sum(n_cit_ex)                # 1
n_reports <- n_reports_db + n_cit_inc                    # 50
n_studies <- 49
n_coi <- 21; n_ee <- 28
n_cost_synth <- 22; n_models <- 14
reg <- c(East = 19, Southern = 15, West = 10, Multi = 5)
stopifnot(n_coi + n_ee == n_studies, sum(reg) == n_studies, n_cit_inc == 1)

fmt <- function(x) format(x, big.mark = ",")

# colours and helpers ---------------------------------------------------------
fill <- "#DCE9F7"; border <- "#8DB3E2"; arrowc <- "#5B9BD5"; side <- "#4A86C8"
rbox <- function(x0, y0, x1, y1, r = 0.8, col = fill, bdr = border, lwd = 1.5) {
  t <- seq(0, pi / 2, length.out = 12)
  xs <- c(x1 - r + r * cos(t), x0 + r - r * sin(t), x0 + r - r * cos(t), x1 - r + r * sin(t))
  ys <- c(y1 - r + r * sin(t), y1 - r + r * cos(t), y0 + r - r * sin(t), y0 + r - r * cos(t))
  polygon(xs, ys, col = col, border = bdr, lwd = lwd)
}
box <- function(x0, y0, x1, y1, lab, cex = 0.95, adj = 0) {
  rect(x0, y0, x1, y1, col = fill, border = border, lwd = 1.5)
  if (adj == 0) text(x0 + 1.2, (y0 + y1) / 2, lab, adj = c(0, 0.5), cex = cex, family = "serif")
  else text((x0 + x1) / 2, (y0 + y1) / 2, lab, cex = cex, family = "serif")
}
arr <- function(x0, y0, x1, y1) arrows(x0, y0, x1, y1, length = 0.1, lwd = 3, col = arrowc)
sidebar <- function(y0, y1, lab) {
  rbox(1, y0, 7, y1, r = 1.2, col = side, bdr = "#3A6FA8")
  text(4, (y0 + y1) / 2, lab, srt = 270, col = "white", font = 2, cex = 1.05, family = "serif")
}

draw_prisma <- function() {
  par(mar = c(0.5, 0.5, 0.5, 0.5), family = "serif")
  plot(NA, xlim = c(0, 100), ylim = c(0, 130), axes = FALSE, xlab = "", ylab = "", asp = NA)
  text(50, 128, "PREFERRED REPORTING ITEMS FOR SYSTEMATIC REVIEWS AND", font = 2, cex = 1.05)
  text(50, 124.5, "META-ANALYSES (PRISMA 2020) FLOW DIAGRAM", font = 2, cex = 1.05)
  segments(c(12, 25), c(126.6, 123.1), c(88, 75), c(126.6, 123.1), lwd = 1)
  cx <- 0.78   # text size inside boxes

  # column headings
  box(9, 118.5, 64, 121.5, "Identification of studies via databases", cex = 0.85, adj = 0.5)
  box(67, 118.5, 99, 121.5, "Identification via other methods", cex = 0.85, adj = 0.5)

  # IDENTIFICATION
  sidebar(97, 117, "IDENTIFICATION")
  box(9, 97, 37, 117, paste0("Records identified from\ndatabases (n = ", fmt(n_total), "):\n",
      "PubMed/MEDLINE (n = ", fmt(n_pubmed), ")\n",
      "Europe PMC (n = ", fmt(n_epmc), ")\n",
      "Cochrane Library (n = ", fmt(n_cochrane), ")\n",
      "Global Index Medicus (n = ", fmt(n_gim), ")"), cex = cx)
  box(41, 101, 64, 113, paste0("Records removed before\nscreening:\nDuplicates (n = ", fmt(n_dup), ")"), cex = cx)
  arr(37.5, 107, 40.5, 107)
  box(67, 101, 99, 113, paste0("Records identified from\ncitation searching of the\nincluded reports (n = ", n_cit, ")"), cex = cx)

  # SCREENING
  sidebar(50, 94, "SCREENING")
  arr(23, 96.5, 23, 93.5)
  box(9, 87, 37, 93.5, paste0("Records screened\n(n = ", fmt(n_screen), ")"), cex = cx)
  box(41, 87, 64, 93.5, paste0("Records excluded\n(n = ", fmt(n_ex_ta), ")"), cex = cx)
  arr(37.5, 90.25, 40.5, 90.25)
  arr(23, 86.5, 23, 83.5)
  box(9, 77, 37, 83.5, paste0("Reports sought for\nretrieval (n = ", n_sought, ")"), cex = cx)
  box(41, 77, 64, 83.5, paste0("Reports not retrieved\n(n = ", n_notret, ")"), cex = cx)
  arr(37.5, 80.25, 40.5, 80.25)
  arr(83, 100.5, 83, 83.5)
  box(67, 77, 99, 83.5, paste0("Reports sought for retrieval (n = ", n_cit, ")\nReports not retrieved (n = ", n_cit_notret, ")"), cex = cx)
  arr(23, 76.5, 23, 73.5)
  box(9, 67, 37, 73.5, paste0("Reports assessed for\neligibility (n = ", n_assess, ")"), cex = cx)
  box(41, 52, 64, 73.5, paste0("Reports excluded (n = ", n_ex_ft, "):\n",
      "Pneumonia costs not\nreported separately\n(n = ", n_ex_ft_e04, ")\n",
      "Overlapping analysis of\nthe same model (n = ", n_ex_ft_e07, ")"), cex = cx)
  arr(37.5, 70.25, 40.5, 70.25)
  arr(83, 76.5, 83, 73.5)
  box(67, 52, 99, 73.5, paste0("Reports assessed (n = ", n_cit_assess, ")\n",
      "Reports excluded (n = ", sum(n_cit_ex), "):\n",
      "Reviews or ineligible design (n = ", n_cit_ex["E05"], ")\n",
      "Outside sub-Saharan Africa\nor setting not stated (n = ", n_cit_ex["E01"], ")\n",
      "Pneumonia not separable (n = ", n_cit_ex["E04"], ")\n",
      "Not pneumonia (n = ", n_cit_ex["E02"], ")\n",
      "No economic outcome (n = ", n_cit_ex["E03"], ")\n",
      "Overlapping data (n = ", n_cit_ex["E07"], ")"), cex = cx)

  # INCLUDED
  sidebar(2, 47, "INCLUDED")
  arr(23, 66.5, 23, 47.5)
  arr(83, 51.5, 83, 47.5); segments(83, 47.5, 70, 47.5, lwd = 3, col = arrowc)
  box(9, 37, 70, 47, paste0("Studies included in the review (n = ", n_studies, "; reports n = ", n_reports, ")\n",
      "Studies with measured costs in the cost synthesis (n = ", n_cost_synth, ")\n",
      "Studies used in the actuarial models (n = ", n_models, ")"), cex = 0.85)
  # branch to study types
  arr(40, 36.5, 40, 33.5)
  segments(25, 33.5, 75, 33.5, lwd = 3, col = arrowc)
  arr(25, 33.5, 25, 30.5); arr(75, 33.5, 75, 30.5)
  box(10, 24, 46, 30.5, paste0("Cost-of-illness and costing\nstudies (n = ", n_coi, ")"), adj = 0.5, cex = 0.85)
  box(54, 24, 96, 30.5, paste0("Economic evaluations (cost-effectiveness,\ncost-utility, cost-benefit) (n = ", n_ee, ")"), adj = 0.5, cex = 0.85)
  # branch to regions
  arr(53, 23.5, 53, 20.5)
  segments(18, 20.5, 88, 20.5, lwd = 3, col = arrowc)
  xs <- c(18, 41.3, 64.6, 88)
  labs <- c(paste0("East Africa\n(n = ", reg["East"], ")"),
            paste0("Southern Africa\n(n = ", reg["Southern"], ")"),
            paste0("West Africa\n(n = ", reg["West"], ")"),
            paste0("Multi-country, regional\nor global (n = ", reg["Multi"], ")"))
  for (i in 1:4) {
    arr(xs[i], 20.5, xs[i], 16.5)
    box(xs[i] - 10.5, 6, xs[i] + 10.5, 16.5, labs[i], adj = 0.5, cex = 0.82)
  }
}

dir.create("figures", showWarnings = FALSE)
png("figures/Figure1_PRISMA_flow.png", width = 8.5, height = 11, units = "in", res = 300)
draw_prisma(); dev.off()
pdf("figures/Figure1_PRISMA_flow.pdf", width = 8.5, height = 11, family = "Times")
draw_prisma(); dev.off()
cat("PRISMA diagram written to figures/Figure1_PRISMA_flow.png and .pdf\n")
