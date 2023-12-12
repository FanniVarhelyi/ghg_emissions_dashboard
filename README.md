### Visualizing global Greenhouse gas emissions, 2015-2021

This repository contains the scripts for the data and visualization behind my GHG emissions dashboard, available here:

https://fanni-v.shinyapps.io/ghg_emissions_dashboard/

The repo is structured as follows:

data/

  This folder contains the processed datasets the Shiny app uses. The original datasets are available at:
  - Climate Trace GHG emissions per sector and per country, 2015-2021: https://climatetrace.org
  - Gapminder population data per country, 2015-2021 (V7): https://www.gapminder.org/data/documentation/gd003/

src/

  This folder contains two scripts:
  - preprocessing.r details all the steps taken to clean, filter, and merge the datasets
  - app.R is the codebook behind the Shiny dashboard

documentation/

  This folder contains the documentation summarizing all steps taken to acquire and preprocess the data, as well as additional considerations such as bias or privacy.
