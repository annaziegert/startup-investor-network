# Startup-Investor Network Analysis ShinyApp

Welcome to our project repository for the Startup-Investor Network Analysis application. This project was carried out under the guidance of Professor Rodrigo Belo as part of our Network Analytics course at Nova School of Business and Economics.

## Project Overview

This ShinyApp provides a user-friendly interface for exploring and analyzing relationships between startups and investors. It is specifically designed to help startups find potential investors, and inversely for investors to discover exciting startup opportunities. By studying the nearest neighbours in this network, users can gain insights on investment patterns, and identify the investors who have invested in startups similar to theirs.

## Data

The data used for this project is adapted from the Kaggle dataset [Unicorn, Decacorn, Hectocorn in 2021](https://www.kaggle.com/datasets/prasertk/unicorn-decacorn-hectocron-in-2021?resource=download). It provides a comprehensive list of startups and their investors, which forms the backbone of our network analysis.

## Methodology

We performed network analytics on the startup-investor relationship data. Each startup and investor forms a node in our network, with edges representing investment relationships. The nearest neighbours of a node provide valuable insights into potential connections a startup or investor could make.

Our ShinyApp is designed to make this analysis accessible to users. By inputting a startup name, the app will return the startup's nearest neighbours and their respective investors. Similarly, entering an investor's name will reveal startups that they might be interested in, based on similar investment patterns.

## Code and Files

The repository contains the following files:
- app.R: This R script includes the code for the ShinyApp, containing both the user interface (UI) and server logic.
- deploy.R: This file contains the script to deploy the ShinyApp on a Shiny Server.
- setup.R: This file contains the instructions to install all the necessary R packages to run your ShinyApp
- data-cleaning.ipynb: This notebook contains the code for cleaning the dataset.
- descriptive-analysis.Rmd: This file includes the code for performing a descriptive analysis on the clean dataset.
- README.md: The file you're currently reading.

## Contributions
This project is a result of a collective effort from our dedicated team. We welcome constructive feedback and suggestions for improving our app. If you have any queries or need further clarification on any aspect of the project, feel free to raise an issue in this repository.

We hope our ShinyApp will prove beneficial in connecting startups with potential investors, and vice versa!

## Acknowledgments

We extend our deepest gratitude to Professor Rodrigo Belo for his continuous guidance and support throughout the development of this project.

