# Runs the three scripts in order, each in a fresh R session, and saves what R
# printed for each script in r_results/.
# Open R with this folder as the working directory, then: source("run_all.R")
dir.create("r_results", showWarnings = FALSE)
scripts <- c("01_convert_to_2025_USD", "02_summary_and_actuarial_models", "03_prisma_diagram")
rscript <- file.path(R.home("bin"), "Rscript")
for (s in scripts) {
  cat("Running", s, "...\n")
  status <- system2(rscript, file.path("R", paste0(s, ".R")),
                    stdout = file.path("r_results", paste0(s, ".txt")),
                    stderr = file.path("r_results", paste0(s, ".txt")))
  if (status != 0) stop("Script failed: ", s, " (see r_results/", s, ".txt)")
}
cat("Done. Figures are in figures/, tables in tables/, printed output in r_results/.\n")
