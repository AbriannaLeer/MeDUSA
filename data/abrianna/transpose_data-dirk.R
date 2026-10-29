library(data.table)

# # POSITIVE
# 
# dt <- fread("data/abrianna/positive_GCTAp.csv")
# 
# # Grab sample names and mz values explicitly
# sample_names <- as.character(dt[[1]])   # first column = sample names
# mz_values    <- names(dt)[-1]           # column headers = mz values
# 
# # Transpose only the intensity matrix
# dt_t <- transpose(dt[, -1])
# 
# # Assign sample names as column names, mz values as first column
# setnames(dt_t, sample_names)
# dt_t[, mz := mz_values]
# setcolorder(dt_t, c("mz", sample_names))
# 
# fwrite(dt_t, "data/abrianna/positive_dirk.csv")
# 
# dt <- fread("data/abrianna/positive_dirk.csv")
# 
# # Drop the bad mz column, rename Labels to mz
# dt[, mz := NULL]
# setnames(dt, "Labels", "mz")
# 
# # Move mz to the front
# setcolorder(dt, c("mz", setdiff(names(dt), "mz")))
# 
# fwrite(dt, "data/abrianna/positive_dirk.csv")

# NEGATIVE

# # Fast read
# dt <- fread("data/abrianna/negative_GCTAp.csv")
# 
# # Get sample names from first column
# sample_col <- names(dt)[1]
# samples <- dt[[sample_col]]
# 
# # Transpose (excluding the sample column)
# dt_t <- transpose(dt[, -1])
# setnames(dt_t, old = names(dt_t), new = as.character(samples))
# 
# # Add mz column at the front
# dt_t[, mz := names(dt)[-1]]
# setcolorder(dt_t, c("mz", as.character(samples)))
# 
# # Fast write
# fwrite(dt_t, "data/abrianna/negative_dirk.csv")
# 
# dt <- fread("data/abrianna/negative_dirk.csv")
# 
# # Drop the bad mz column, rename Labels to mz
# dt[, mz := NULL]
# setnames(dt, "Labels", "mz")
# 
# # Move mz to the front
# setcolorder(dt, c("mz", setdiff(names(dt), "mz")))
# 
# fwrite(dt, "data/abrianna/negative_dirk.csv")

# change wide to long format with correct column names

dt <- fread("data/abrianna/positive_dirk.csv")

# Melt from wide to long
dt_long <- melt(dt, 
                id.vars = "mz", 
                variable.name = "sample_name", 
                value.name = "intensity")

fwrite(dt_long, "data/abrianna/positive_dirk.csv")
