# sumtab

**A minimalistic, fast, pipe-friendly, robust, and opinionated statistical summary table generator.**

## Overview

`sumtab` is a flexible R package designed to generate customizable summary tables for peer-reviewed scientific articles, offering various options for parametric/non-parametric statistical tests, descriptive statistics, and multivariate analysis.

## Features

- Supports parametric (e.g., t-test) and non-parametric (e.g., Wilcoxon test) statistical tests.
- Automatically selects the appropriate test based on the data's characteristics.
- Pipe-friendly design for easy integration with `%>%`.
- Option for grouping data and displaying multivariate analysis.
- Customizable formatting options for generating publication-ready tables.

## Installation

You can install the package directly from GitHub using `devtools`:
```r
# Install devtools if you haven't already
install.packages("devtools")

# Install sumtab from GitHub
devtools::install_github("advafaeian/sumtab")
```

## Usage

Here’s a basic example of how to use the package:
```r
library(sumtab)

# Summarize your data
sumtab(data = my_data)

# Or using the pipe-friendly syntax:
my_data %>% sumtab()

my_data %>% sumtab(by = "my_group", reporting_type = "parametric", analysis = TRUE)
```
