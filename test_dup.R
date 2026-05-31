library(DBI)
library(RSQLite)

source("shared/db_persistence.R")
source("shared/designplot/sqlite_persistence.R")
source("shared/designplot/constants.R")

db <- "data/field_book.sqlite"
con <- connectDesignplotDb(db)
initDesignplotDb(con)

# Pick a field
epr <- DBI::dbGetQuery(con,
  "SELECT DISTINCT plant_table_name FROM experiment_plant_runs LIMIT 1")
if (nrow(epr) == 0) { cat("SKIP\n"); quit("no") }
pt <- as.character(epr$plant_table_name[1])
cat("Field:", pt, "\n\n")

# Read experiment_plant_runs to see what experiments are on this field
epr2 <- DBI::dbGetQuery(con,
  "SELECT experiment_id, plant_table_name, plan_id FROM experiment_plant_runs WHERE plant_table_name = ?",
  params = list(pt))
cat("=== experiment_plant_runs ===\n")
print(epr2)
cat("\n")

# For each plan_id, count plant_assignments
for (pid in epr2$plan_id) {
  pa <- DBI::dbGetQuery(con,
    "SELECT plan_id, COUNT(*) AS n FROM plant_assignments WHERE plan_id = ?", params = list(pid))
  cat(sprintf("  plan_id=%s -> %d assignments\n", substr(pid, 1, 40), pa$n))
}

DBI::dbDisconnect(con)
cat("\n--- layout_info would have one entry per connected box per experiment ---\n")
cat("If experiments have non-contiguous cells, splitConnectedBoxes creates multiple boxes\n")
cat("But all boxes share the same name, so legend dedup by name keeps one entry.\n")
