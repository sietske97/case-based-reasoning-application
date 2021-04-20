# Case Based Reasoning Application
Deze repository bevat een  case-based reasoning model SHiny APP, met een user interface. Het model voorspelt de sentence-type (detention of non-detention) van een nieuwe veroordeling. Het model gebruikt dat avan de [Cook County Government](https://datacatalog.cookcountyil.gov/Courts/Sentencing). 

# Hoe kan de app gerund worden?
## Run de app in je browser
Een versie van deze app is gehost via shinyapps.io en kan bekeken worden via de volgende URL: https://sietske97.shinyapps.io/casebased-reasoning-application/

# Run de app lokaal vanuit R
Om de app vanuit Rstudio te runnen, zonder dat je deze repository hoeft te clonen, run je de volgende code:

```{run app local, echo = TRUE}
runGitHub("casebased-reasoning-application", "sietske97")
```

# Inhoud van de mappen
## app.R
Dit bestand bevat de code van de applicatie. De drie scripts zijn afkomstig uit de R-map. De gebruikersinterface wordt dan gedefinieerd onder de ui-sectie. Het 'server'-gedeelte bevat de responsieve code om de gebruikersinterface te genereren.

## data
De datafolder bevat twee documenten:

**data.csv**is de dataset die gebruikt wordt om het CBR-model te runnen. Het is de database waarin het systeem opzoek gaat naar vergelijkbare casussen. 

**query.csv**is een voorbeeld query casus die de gebruiker kan uploaden.

## R
De R-map bestaat uit drie scripts
**01_libraries.R** is een script welke packages laadt.

**02_functions.R** is a script met twee user-defined functies die het model gebruikt/

**03_defining_values.R** is een script waarin enkele waarden worden gedefineerd die de app gebruikt. Dit voorkomt onnodige code in de app zelf. 

## www
Deze map bevat extra data die de app nodig heeft zoals afbeeldingen en html-bestanden. 

## Blog
Deze map bevat een markdown-file wat bestaat uit een blog met meer informatie over het CBR-model en hoe deze in R gebouwd kan worden. 