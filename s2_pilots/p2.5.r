library(gt)

gt_theme = function(data, ...){
  data %>%
    tab_options(
    table.font.size = px(12),
    data_row.padding = px(0),
    row_group.padding = px(0),
    column_labels.padding = px(0)
  )
}

p2 = read_rds("s2/p2/data/data_lavaan.rds") %>% select(-bl_income, -bl_education)
p3 = read_rds("s2/p3/data/lavaan_data.rds")%>% select(-bl_income, -bl_education)

data.z = bind_rows(p2,p3)

data.z = data.z %>% filter(condition != "drinking", condition != "work")
# some lavaan
library(lavaan)

model = "
sc_self ~ standard10 + self_minutes
self_minutes ~ standard10 + fb*friend_minutes + eb*exemplar_minutes
standard10 ~ fs*friend_minutes + es*exemplar_minutes
friend_minutes~~exemplar_minutes"

fit_constrain = sem(model, data = data.z, group = "condition", group.equal = c("regressions"))
fit = sem(model, data = data.z, group = "condition")

fit %>% summary(standardized = T, fit.measures = T)
fit_constrain %>% summary(standardized = T, fit.measures = T)

fitmeasures(fit) %>% enframe %>% rename(unconstrained = value) %>%
left_join(fitmeasures(fit_constrain) %>% enframe %>% rename(constrained = value), by = "name") %>% 
filter(name %in% c("tli", "cfi", "rmsea", "srmr", "chisq", "df", "pvalue")) %>%
  gt()  %>% 
  tab_spanner_delim("_") %>%
  fmt_number(decimals =  3) %>%
  gt_theme()

# compare models
anova(fit, fit_constrain)

# Wald test
test_result <- lavTestWald(fit, constraints = "fs - es == 0")
test_result

test_result <- lavTestWald(fit, constraints = "fb - eb == 0")
test_result

unconstrained = parameterEstimates(fit, standardized = T) %>% 
  filter(op %in% c("~", "~~")) %>% 
  select(lhs, rhs, est, std.all, pvalue, group) %>% 
  as_tibble() %>%
  mutate(group = case_match(
    group,
    1 ~ "punctuality",
    2~ "social media",
    3~"exercise" ,
    4 ~ "bedtime",
    5 ~ "saving",
    6 ~ "healthy eating"
  )) %>%
  filter(lhs != rhs) %>%
  pivot_wider(names_from = group, values_from = c(est, std.all, pvalue), names_glue = "{group}_{.value}") %>%
  select(lhs, rhs, starts_with("exercise"), starts_with("punctuality"), starts_with("drinking"), starts_with("social"),starts_with("bedtime"), starts_with("healthy"), starts_with("work"), starts_with("saving"))

unconstrained %>%
  gt()  %>% 
  tab_spanner_delim("_") %>%
  fmt_number() %>%
  gt_theme()

constrained = parameterEstimates(fit_constrain, standardized = T) %>% 
  filter(op %in% c("~", "~~")) %>% 
  select(lhs, rhs, est, std.all, pvalue, group) %>% 
  as_tibble() %>%
  mutate(group = case_match(
    group,
    1 ~ "all paths",
  )) %>%
  filter(lhs != rhs, group == 'all paths') %>%
  pivot_wider(names_from = group, values_from = c(est, std.all, pvalue), names_glue = "{group}_{.value}") %>%
  select(lhs, rhs, starts_with("all"), starts_with("punctuality"), starts_with("drinking"), starts_with("social"),starts_with("bedtime"), starts_with("healthy"), starts_with("work"), starts_with("saving")) 
  
constrained = gt()  %>% 
  tab_spanner_delim("_") %>%
  fmt_number() %>%
  gt_theme()

left_join(constrained, unconstrained, by = c("lhs", "rhs")) %>%
  gt()  %>% 
  tab_spanner_delim("_") %>%
  fmt_number() %>%
  gt_theme()

library(stargazer)


save_stargazer_as_image <- function(stargazer_output, output_path) {
  # Check if required packages are installed and load them
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The 'magick' package is not installed. Please install it and try again.")
  }
  library(magick)
  
  # Create a basic LaTeX document structure
  latex_document <- c(
    "\\documentclass{article}",
    "\\begin{document}",
    stargazer_output,
    "\\end{document}"
  )
  
  # Write the LaTeX document to a temporary .tex file
  tex_file <- tempfile(fileext = ".tex")
  writeLines(latex_document, tex_file)
  
  # Define the path for the .pdf output
  pdf_file <- sub(".tex$", ".pdf", tex_file)
  
  # Call pdflatex directly to compile the .tex file
  exit_code <- system(paste("pdflatex", shQuote(tex_file)))
  
  # If PDF exists, convert it to an image
  if (file.exists(pdf_file) && exit_code == 0) {
    # Convert .pdf to .png
    image <- magick::image_read_pdf(pdf_file)
  
    # Save the image to the specified path
    magick::image_write(image, output_path)
  
    cat(sprintf("Image saved to: %s", output_path))
  } else {
    cat("PDF generation failed.")
  }
}

# Example usage:
stargazer_output <- stargazer::stargazer(mtcars, type="latex")
save_stargazer_as_image(stargazer_output, "output.png")

