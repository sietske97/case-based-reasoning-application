# Case Based Reasoning Applicatie
Dit is een shiny app, met daarin een CBR-prototype gebaseerd op open data van de Couk County Government. De applicatie kan gerund worden door deze repository te clonen en dan in app.R op 'run' te klikken. Daarnaast is een versie van deze app ook gehost via shinyapps.io en te bekijken op de volgende URL:  https://sietske97.shinyapps.io/case_based_reasoning_application/ 

# Inhoud van de mappen
## app.R
Dit bestand bevat de code van de applicatie. Eerst worden de drie scripts in de map R gesourced. Daarna wordt onder het deel ui de user interface gedefinieerd. Het deel 'server' bevat de responsive code om de user interface te genereren. 

## data
De data map bevat twee bestanden:
**data.csv** is de dataset die gebruikt wordt om het cbr-model te runnen. Het is de database waarin gezocht wordt naar soortgelijke casussen.
**query.csv** is een voorbeeld bestand dat een gebruiker zelf kan aanpassen en kan uploaden via de applicatie. 

## R
De map R bestaat uit drie bestanden. 
**01_libraries.R** is een script die libraries laadt
**02_functions.R** is een script met twee user-defined functies die het model gebruikt
**03_defining_values.R** is een script die enkele values definieert die de app nodig heeft. Op deze manier staat er geen overbodige code in de app zelf. 

## rsconnect
Data die gebruikt is voor het uploaden van de applicatie naar de shinyapps.io server

## www
De www map bevat extra gegevens die de app nodig heeft, zoals afbeeldingen, html-bestanden en een css-opmaak bestand. De .jpg en .png bestanden zijn afbeeldingen die in de app geladen worden. Het workflow.html bestand laat de workflow van de targets zien, en het mycss.css bevat extra opmaak voor de plaatsing van het lablogo. 