

## library() calls go here
library(conflicted)
library(dotenv)
library(targets)
library(tarchetypes)

## data analysis
library(tidyverse)
library(cmprsk)
library(recipes)
library(riskRegression)
library(Hmisc)
library(MASS)

## formatting
library(glue)
library(table.glue)

# tabulation
library(gtsummary)
library(flextable)
library(table.glue)

conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("all_numeric", "gtsummary")
conflict_prefer("select", "dplyr")
