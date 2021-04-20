# Case Based Reasoning Application
This repository contains a case-based reasoning model Shiny App, with a UI-interface. The model predicts the sentence-type (detention or non-detention) of a new lawsuit case. The model uses open data from the Cook County Government (https://datacatalog.cookcountyil.gov/Courts/Sentencing). 

# How to run the app
## Run the app in your browser
A version of this app is also hosted via shinyapps.io and can be viewed at the following URL: https://sietske97.shinyapps.io/casebased-reasoning-application/

# How to run the app locally
Run the app from Rstudio, without cloning this repository, by running the following code:
```[run app local, echo = TRUE]
runGitHub("casebased-reasoning-application", "sietske97")
```

# Contents of the folders
## app.R
This file contains the code of the application. First, the three scripts are sourced in the R folder. The user interface is then defined under the ui section. The 'server' part contains the responsive code to generate the user interface.

## data
The data folder contains two files:

**data.csv**is the dataset used to run the cbr model. It is the database in which similar cases are searched.

**query.csv**is an example file that a user can edit and upload via the application.

## R
The R folder consists of three files.
**01_libraries.R** is a script that loads libraries

**02_functions.R** is a script with two user-defined functions that the model uses

**03_defining_values.R** is a script that defines some values that the app needs. This way there is no unnecessary code in the app itself.

## rsconnect
Data used for uploading the application to the shinyapps.io server

## www
The www folder contains additional data that the app needs, such as images, html files and a CSS format file. The .jpg and .png files are images that are loaded into the app. The workflow.html file shows the workflow of the targets, and the mycss.css contains additional formatting for the placement of the lab logo.

## Blog
This map contains the markdown for the blog that I wrote on how to make a CBR-system in R. 