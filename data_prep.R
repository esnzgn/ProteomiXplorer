# Load required packages
library("readxl")
library("tidyverse")
library("janitor")

# Read in the expression matrix (Sheet 2 of your file)
raw_data <- read_xlsx("mmc2.xlsx", sheet = 2)

# Clean column names to avoid hidden characters or spaces
raw_data <- janitor::clean_names(raw_data)

# Identify expression columns (matching "TENPX" pattern)
expr_cols <- grep("tenpx\\d{2}$", names(raw_data), value = TRUE)

# Filter columns: keep Gene_Symbol, Protein_Id, and expression values
data_expr <- raw_data %>%
  select(gene_symbol, protein_id, all_of(expr_cols))

# Pivot to long format
data_long <- data_expr %>%
  pivot_longer(cols = all_of(expr_cols), names_to = "sample", values_to = "expression")

# Extract metadata: Cell Line, Tissue, TenPlex
data_tidy <- data_long %>%
  mutate(
    sample = str_replace_all(sample, "_", "__"),  # Temporarily escape underscores
    cell_line = str_extract(sample, "^[^_]+"),
    tissue = str_extract(sample, "(?<=__)[^_]+"),
    tenplex = str_extract(sample, "tenpx\\d{2}$"),
    sample = str_replace_all(sample, "__", "_")  # Restore underscores
  ) %>%
  select(gene_symbol, protein_id, cell_line, tissue, tenplex, expression)

# Optional: remove NA or zero expression values
data_tidy <- data_tidy %>%
  filter(!is.na(expression), expression != 0)

# Save as RDS to speed up Shiny app loading
saveRDS(data_tidy, "data_tidy.rds")
