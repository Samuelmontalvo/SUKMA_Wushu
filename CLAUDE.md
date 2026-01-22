# CLAUDE.md - AI Assistant Guide for SUKMA Wushu Project

## Project Overview

This is an **R-based sports biomechanics research project** analyzing vertical jump performance data from Malaysian Wushu athletes competing in SUKMA (Southeast Asian Games). The project focuses on generating normative data, analyzing inter-limb asymmetries, and evaluating performance metrics for athletic assessment.

**Project Type:** Statistical Analysis & Research (R/R Markdown)
**Primary Author:** Samuel Montalvo, Ph.D. (smontal@stanford.edu)
**Domain:** Sports Science, Biomechanics, Performance Analysis

---

## Repository Structure

```
SUKMA_Wushu/
├── CLAUDE.md                    # AI assistant guide (this document)
├── SUKMA_Wushu.Rproj           # R Project configuration
├── .gitignore                   # Git ignore rules
│
├── data/                        # All data files
│   ├── SUKMA_Wushu_Data.csv                    # Processed dataset (100 KB)
│   ├── Athletes details SUKMA XX.xlsx           # Athlete metadata (18 KB)
│   ├── Project-Session-09_13_22-...-Countermovement_Jump.csv  # Raw measurements (346 KB)
│   └── SUKMA.xlsx                              # Additional data (277 KB)
│
├── scripts/                     # R analysis scripts
│   ├── Script.R                                # Main analysis script (231 lines)
│   ├── Script.Rmd                              # Secondary R Markdown analysis (159 lines)
│   └── Normative Jump Data Malaysia.Rmd        # Comprehensive analysis (2,742 lines)
│
├── outputs/                     # Generated reports and documents
│   ├── Normative-Jump-Data-Malaysia.html       # Main HTML report (6 MB)
│   ├── Normative-Jump-Data-Malaysia.docx       # Word document (117 KB)
│   ├── Table_Jump_Height.docx                  # Exported table
│   └── NSCA_abstract                           # Conference abstract image
│
└── figures/                     # Plots and visualizations
    ├── Table1groups.jpeg                       # Summary table
    ├── Table1groupsbygender.jpeg               # Gender-stratified table
    ├── Vertical_jump_height.png                # Key visualization (181 KB)
    └── Normative-Jump-Data-Malaysia_files/     # Generated figures folder
        └── figure-docx/                        # 13 PNG images
```

---

## Technology Stack

### Core Technologies
- **Language:** R (statistical computing)
- **IDE:** RStudio (recommended)
- **Document Format:** R Markdown (.Rmd)
- **Output Formats:** HTML, DOCX, PNG, CSV

### Key R Packages

**Data Processing:**
- `readr` - Fast CSV reading
- `dplyr` - Data manipulation
- `readxl` - Excel file import
- `janitor` - Data cleaning (column name standardization)
- `caret` - Data partitioning

**Visualization:**
- `ggplot2` - Core plotting library
- `ggprism` - Prism-style themes
- `ggpubr` - Publication-ready plots
- `ggridges` - Density ridge plots
- `plotly` - Interactive visualizations
- `ggcorrplot` - Correlation matrices

**Statistical Analysis:**
- `PupillometryR` - Statistical utilities
- `interlimb` - Asymmetry calculations
- `factoextra` - Multivariate analysis

**Table Generation:**
- `flextable` - Flexible table formatting
- `officer` - Word document creation
- `table1` - Descriptive statistics tables
- `kableExtra` - Enhanced table styling

**Output & Rendering:**
- `knitr` - R Markdown compilation
- `htmlwidgets` - Interactive HTML elements
- `webshot` - HTML to image conversion

---

## Key Concepts & Domain Knowledge

### Biomechanical Metrics Analyzed

1. **Jump Height (m)** - Primary performance indicator
2. **Peak Propulsive Force (N, N/kg)** - Maximum force during takeoff
3. **Peak Braking Force** - Eccentric phase loading
4. **Propulsive Net Impulse** - Force-time integral
5. **mRSI (Modified Reactive Strength Index)** - Efficiency metric
6. **Landing Force Characteristics** - Impact absorption
7. **Inter-limb Asymmetry** - Left/right imbalance at peak force
8. **Time to Takeoff** - Movement velocity
9. **Stabilization Time** - Landing control

### Performance Classification

The project uses a 9-level performance categorization system:
- Excellent
- Very Good
- Good
- Above Average
- Average
- Below Average
- Poor
- Very Poor
- Extremely Poor

Performance levels are color-coded in visualizations (green = excellent, red = poor).

### Data Pipeline Stages

1. **Load** raw jump test data (CSV format)
2. **Filter** for average trials only
3. **Merge** with athlete demographic data (Excel)
4. **Clean** column names (convert spaces to underscores)
5. **Remove** missing values
6. **Calculate** derived metrics (relative forces, asymmetry indices)
7. **Normalize** using Z-scores and T-scores
8. **Visualize** with multiple plot types
9. **Generate** normative tables by sex and performance level
10. **Export** to HTML, DOCX, and PNG formats

---

## Development Workflows

### Working with R Markdown Files

**To render/compile an .Rmd file:**
```r
# In R console or RStudio (from project root)
rmarkdown::render("scripts/Normative Jump Data Malaysia.Rmd")

# Or use RStudio's "Knit" button (when file is open)
```

**To generate Word documents:**
```r
# Within .Rmd, use flextable and officer
save_as_docx(
  flex_table_object,
  path = "output_filename.docx"
)
```

**To export plots:**
```r
ggsave(
  filename = "plot_name.png",
  plot = plot_object,
  width = 10,
  height = 8,
  dpi = 300
)
```

### Modifying Analysis

**Adding new metrics:**
1. Locate the data processing section in .Rmd file
2. Use `dplyr::mutate()` to add calculated columns
3. Update visualization and table generation functions
4. Re-render the document

**Changing visualizations:**
1. Find the relevant `ggplot()` code block
2. Modify aesthetic mappings or geoms
3. Update theme or color schemes if needed
4. Re-render to see changes

**Updating normative tables:**
1. Locate `generate_normative_tables()` function
2. Modify range breaks or performance categories
3. Update the function call with new parameters
4. Re-render document

### Data Updates

**To add new athlete data:**
1. Update `data/Athletes details SUKMA XX.xlsx` with new rows
2. Ensure column names match existing structure
3. Re-run the analysis pipeline
4. Verify merge succeeded (check row counts)

**To incorporate new jump test data:**
1. Export raw data from force plate system
2. Place CSV in `data/` directory
3. Update file path in .Rmd `read_csv()` call (use relative path from scripts folder)
4. Ensure column names match expected format
5. Re-render analysis

**File Path Convention:**
- All R scripts are in `scripts/` directory
- All data files are in `data/` directory
- All outputs go to `outputs/` directory
- All figures go to `figures/` directory
- Use relative paths: `../data/filename.csv` when reading from scripts

---

## Coding Conventions

### Naming Conventions

**Variables:**
- DataFrames: `Df`, `Df_male`, `Df_female`, `Df_filtered`
- Column names: `snake_case` (standardized via `janitor::clean_names()`)
- Functions: `PascalCase` (e.g., `generate_normative_tables()`)

**Files:**
- R scripts: `Script.R`
- R Markdown: `Descriptive Name.Rmd`
- Data: `descriptive_name.csv` or `.xlsx`
- Outputs: `Descriptive-Name-With-Hyphens.html`

### Code Style

**Pipeline Pattern:**
```r
Df <- read_csv("data.csv") %>%
  filter(condition) %>%
  mutate(new_column = calculation) %>%
  select(relevant_columns) %>%
  arrange(sort_column)
```

**Visualization Pattern:**
```r
ggplot(data, aes(x = variable1, y = variable2, fill = group)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme_prism() +
  scale_fill_manual(values = color_vector) +
  labs(
    title = "Descriptive Title",
    x = "X Label",
    y = "Y Label"
  )
```

**Function Definition:**
```r
generate_custom_table <- function(data, grouping_var, metric) {
  # Function body with clear steps
  result <- data %>%
    group_by({{grouping_var}}) %>%
    summarize(
      mean = mean({{metric}}, na.rm = TRUE),
      sd = sd({{metric}}, na.rm = TRUE)
    )

  return(result)
}
```

### Best Practices

1. **Always clean column names** after loading data: `janitor::clean_names()`
2. **Check for missing values** before analysis: `sum(is.na(data))`
3. **Stratify by sex** when analyzing performance metrics
4. **Use relative metrics** (normalized by body weight) for fair comparison
5. **Document unusual decisions** with comments in code
6. **Save intermediate data** for debugging: `write_csv(Df, "checkpoint.csv")`
7. **Version output files** if making iterative changes

---

## Common Tasks & Examples

### Task 1: Adding a New Visualization

```r
# Create a scatter plot with regression line
ggplot(Df, aes(x = jump_height_m, y = peak_propulsive_force_n_kg)) +
  geom_point(aes(color = sex), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_prism() +
  scale_color_manual(values = c("Male" = "#0073C2FF", "Female" = "#EFC000FF")) +
  labs(
    title = "Relationship Between Jump Height and Peak Force",
    x = "Jump Height (m)",
    y = "Peak Propulsive Force (N/kg)",
    color = "Sex"
  )
```

### Task 2: Creating a Summary Table

```r
# Generate descriptive statistics by group
summary_table <- Df %>%
  group_by(sex, event) %>%
  summarize(
    N = n(),
    Mean_Height = mean(jump_height_m, na.rm = TRUE),
    SD_Height = sd(jump_height_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  flextable() %>%
  theme_vanilla() %>%
  autofit()

# Export to Word
save_as_docx(summary_table, path = "summary_by_group.docx")
```

### Task 3: Calculating Inter-limb Asymmetry

```r
# Calculate asymmetry index
Df <- Df %>%
  mutate(
    asymmetry_index = abs(left_force - right_force) / (left_force + right_force) * 100
  )

# Categorize asymmetry levels
Df <- Df %>%
  mutate(
    asymmetry_level = case_when(
      asymmetry_index < 10 ~ "Low",
      asymmetry_index < 15 ~ "Moderate",
      TRUE ~ "High"
    )
  )
```

### Task 4: Filtering and Subsetting Data

```r
# Filter for specific conditions
Df_CQ_males <- Df %>%
  filter(
    sex == "Male",
    event == "CQ",
    !is.na(jump_height_m)
  )

# Select only relevant columns
Df_analysis <- Df %>%
  select(
    athlete_name,
    sex,
    age,
    jump_height_m,
    peak_propulsive_force_n_kg,
    asymmetry_index
  )
```

---

## Git Workflow

### Current Branch Strategy

- **Active Development Branch:** `claude/add-claude-documentation-ECXoX`
- **Main Branch:** (not specified in current config)

### Committing Changes

```bash
# Stage changes
git add <file_name>

# Commit with descriptive message
git commit -m "Add new visualization for landing force analysis"

# Push to remote (with retry logic for network issues)
git push -u origin claude/add-claude-documentation-ECXoX
```

### Best Practices for Git

1. **Commit rendered outputs separately** from source code
2. **Use descriptive commit messages** that explain what changed and why
3. **Don't commit large data files** unless necessary (consider .gitignore)
4. **Push to the designated Claude branch** only
5. **Use git status** before committing to review changes

### Files to Ignore

Already configured in `.gitignore`:
- `.Rproj.user/` - RStudio user-specific files
- `.Rhistory` - Command history
- `.RData` - Workspace data
- `.Ruserdata` - User data
- `.httr-oauth` - OAuth tokens

**Consider adding:**
- `*.html` - Large rendered reports (unless needed)
- `*_cache/` - Knit cache directories
- `*_files/` - Generated figure directories

---

## Troubleshooting

### Common Issues

**Issue 1: Package Not Found**
```r
# Solution: Install missing package
install.packages("package_name")

# For multiple packages
install.packages(c("ggplot2", "dplyr", "readr"))
```

**Issue 2: Column Name Mismatch**
```r
# Solution: Use janitor to standardize
library(janitor)
Df <- Df %>% clean_names()

# Check column names
names(Df)
```

**Issue 3: Knit/Render Fails**
- Check that all required packages are installed
- Verify file paths are correct (use absolute paths if needed)
- Look for errors in code chunks
- Try rendering with `error = TRUE` in chunk options to see detailed errors

**Issue 4: Missing Data in Merge**
```r
# Solution: Check join columns before merging
anti_join(df1, df2, by = "join_column")  # Shows unmatched rows

# Use left_join to preserve all rows from primary dataset
Df_merged <- left_join(df_primary, df_secondary, by = "athlete_id")
```

**Issue 5: Plot Not Displaying**
- Ensure the plot object is explicitly printed in .Rmd chunks
- Check that data has no NA values in required columns
- Verify aesthetic mappings match actual column names

---

## Testing & Validation

Since this is a research project, validation occurs through:

1. **Visual Inspection** - Review all plots for outliers or anomalies
2. **Statistical Summaries** - Check means, SDs, and ranges for plausibility
3. **Cross-validation** - Compare results with literature norms
4. **Manual Review** - Verify exported tables match in-code summaries
5. **Reproducibility** - Re-render documents to ensure consistency

### Validation Checklist

- [ ] All data files load without errors
- [ ] No unexpected missing values (check with `summary()`)
- [ ] Sample sizes match expected N
- [ ] Visualizations render correctly
- [ ] Tables export to Word/HTML properly
- [ ] Performance categories align with established norms
- [ ] Sex-stratified analyses show expected differences
- [ ] Statistical tests run without warnings
- [ ] Output files are generated in expected locations

---

## AI Assistant Guidelines

### When Working on This Project

1. **Read Before Modifying**: Always read existing .R or .Rmd files before making changes
2. **Understand Data Structure**: Review `SUKMA_Wushu_Data.csv` structure before adding metrics
3. **Preserve Scientific Accuracy**: Do not modify statistical methods without domain justification
4. **Test Locally**: Suggest rendering/running code to verify changes work
5. **Document Changes**: Add comments explaining new calculations or modifications
6. **Respect APA Style**: Maintain academic formatting in outputs (this is for publication)

### What to Prioritize

- **Data integrity** over speed
- **Reproducibility** over convenience
- **Clear documentation** over clever code
- **Standard practices** in sports science and statistics
- **Publication quality** outputs

### What to Avoid

- Don't delete or overwrite original raw data files
- Don't change statistical methods without discussion
- Don't add packages without checking dependencies
- Don't create new files unless explicitly requested
- Don't modify .Rproj settings without reason
- Don't push to branches other than designated Claude branches

### Helpful Commands

```r
# View dataset structure
str(Df)
glimpse(Df)  # dplyr version

# Quick summary statistics
summary(Df)
skimr::skim(Df)  # More detailed summary

# Check for duplicates
Df %>% get_dupes(athlete_name)

# View first/last rows
head(Df, n = 10)
tail(Df, n = 10)

# Count observations by group
Df %>% count(sex, event)

# Find unique values
unique(Df$event)
```

---

## Project History

**Initial Commit:** 2022-11-11 (27c84c9) - "1st changes"
**Recent Updates:** 2025-05-19
- 90967ad - "w"
- b0b977d - "a"
- e453d7a - "u[date"

**Current Phase:** Documentation enhancement for AI assistant collaboration

---

## Contact & Resources

**Principal Investigator:** Samuel Montalvo, Ph.D.
**Email:** smontal@stanford.edu

**Related Publications:**
- Conference abstract included in repository (NSCA_abstract)

**External Resources:**
- R Documentation: https://www.rdocumentation.org/
- R Markdown Guide: https://rmarkdown.rstudio.com/
- ggplot2 Documentation: https://ggplot2.tidyverse.org/
- Sports Biomechanics Literature: Search for "vertical jump normative data" and "inter-limb asymmetry"

---

## Quick Start for AI Assistants

1. **Familiarize** yourself with R/R Markdown if not already proficient
2. **Read** `scripts/Normative Jump Data Malaysia.Rmd` to understand the full analysis pipeline
3. **Check** the current branch: `git branch` (should be `claude/add-claude-documentation-ECXoX`)
4. **Review** existing visualizations in `outputs/Normative-Jump-Data-Malaysia.html`
5. **Understand** the data structure by examining `data/SUKMA_Wushu_Data.csv`
6. **Ask clarifying questions** before making significant changes
7. **Test changes** by rendering the .Rmd file
8. **Commit and push** to the designated Claude branch when complete

---

## Version History

- **v1.1** (2026-01-22) - Reorganized repository into professional folder structure (data, scripts, outputs, figures)
- **v1.0** (2026-01-21) - Initial CLAUDE.md creation with comprehensive project documentation

---

*This document is designed to help AI assistants understand and work effectively with the SUKMA Wushu project. Keep it updated as the project evolves.*
