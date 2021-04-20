#------------------------------------------#
## sourcen van libraries, functies en defining values
## die gebruikt worden in de app ##
source("R/01_libraries.R")
source("R/02_functions.R")
source("R/03_defining_values.R")

#------------------------------------------#
## interface van de app ##
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  column(
    width = 12,
    div(style = "height:50px")
  ),
  # Titel van de applicatie
  # Moet in column omdat anders de titel niet dikgedrukt kan
  column(
    width = 12,
    style = "font-weight: bold",
    titlePanel(
      title = "Case-Based Reasoning webapplicatie om 'sentence' te voorspellen"
    )
  ),

  # Wachtbalk als de applicatie aan het laden is
  tags$head(tags$style(type = "text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #F5F5F5;
               z-index: 105;
             }
          ")),
  conditionalPanel(
    condition = "$('html').hasClass('shiny-busy')",
    tags$div("Loading...", id = "loadmessage")
  ),
  tabsetPanel(
    # Horizontale tabs
    tabPanel(
      # Eerste horizontale tab met algemene informatie
      title = tags$h4(tags$strong("Algemene informatie")),
      navlistPanel(
        # Verticale tabs op pagina 'algemen informatie'
        tabPanel(
          # Eerste verticale tab op algemene informatie pagina
          # Introductie tab met algemene informatie over de applicatie
          title = tags$h4("Introductie"),
          tags$h2(tags$strong("Introductie")),
          fluidRow(
            column(
              width = 6,
              tags$h4(tags$strong("Wat is het doel van deze applicatie?")),
              tags$p("Het doel van deze applicatie is om de werking - en bijbehorende waarde -
                  van een Case-Based Reasoning (CBR) systeem te tonen."),
              tags$h4(tags$strong("Wat voorspelt het model?")),
              tags$p("Het model voorspelt (classificeert) voor nieuwe zaken het type veroordeling 
                 dat de verdachte van de nieuwe zaak waarschijnlijk krijgt: detention of non-detention. 
                 Detention zijn voornamelijk gevangenisstraffen en non-detention
                 zijn veroordelingen zoals een taakstraf. Er wordt gebruik gemaakt van een openbare dataset met
                 daarin alle veroordelingen in een bepaald district in Amerika. Als output genereert het model
                 een klasse (detention of non-detention) en bijbehorende probability, en een lijst met meest-vergelijkbare
                 oude veroordelingen en een similarity score tussen de query casus en de oude casus.")
            ),
            column(
              width = 6,
              tags$h4(tags$strong("Wat zijn de functionaliteiten van de app?")),
              tags$p("Deze applicatie bestaat uit twee hoofdonderdelen: een tab met 'algemene informatie' en een tab met 'genereer query'"),
              tags$p(""),
              tags$p("Onder de 'algemene informatie' tab is informatie over CBR, de gebruikte testdata en het gebruikte CBR-model te vinden. Onder het kopje
                  'wat is CBR' wordt algemene uitleg gegeven over een CBR systeem. Onder het kopje 'gebruikte testset'
                  wordt de gebruikte testset beschreven, en het kopje 'gebruikte CBR model' weergeeft hoe het model tot
                  stand is gekomen en bevat wat algemene gegevens (accuracy e.d.) van het model."),
              tags$p(""),
              tags$p("Onder de genereer query tab kun je zelf het CBR-model uitproberen. Je hebt hier drie mogelijkheden:
                 je kunt hier zelf waarden invoeren voor alle variabelen, je kunt het systeem een random observatie
                  laten genereren of je kunt een .csv bestand uploaden met daarin een nieuwe observatie. De resulaten
                 van deze query verschijnen ook op dit tabblad.")
            )
          )
        ),

        tabPanel(
          # Tweede verticale tab op algemene informatie pagina
          # Tab met informatie over wat CBR is
          title = tags$h4("Wat is Case Based Reasoning?"),
          fluidRow(
            column(
              width = 8,
              tags$h2(tags$strong("Wat is Case-Based Reasoning?")),
              tags$p("Case-Based Reasoning is een methode waarbij nieuwe problemen opgelost worden op basis van oplossingen van
                 soortgelijke problemen uit het verleden. Op basis van de gelijksoortigheid tussen een nieuw 'probleem' en
                 een oude oplossing kan er een oplossing gevonden worden voor het nieuwe probleem. Het CBR-model in deze 
                 webapplicatie zoekt een oplossing voor het probleem 'Wat voor type detentie krijgt een veroordeelde?'. 
                 Er zijn twee mogelijke oplossingen: detention en non-detention. Op basis van oude problemen en bijbehorende
                 oplossingen (dit zijn dus oude veroordelingen) kan er een oplossing voor het nieuwe probleem aangedragen
                 worden; oftewel: welk type detentie krijgt een veroordeelde."),
              tags$p(""),
              tags$h4(tags$strong("Welk algoritme gebruikt het CBR model?")),
              tags$p("Onderliggend het CBR model wordt het K-nearest-neighbours (KNN) algoritme gebruikt.
                 KNN is een supervised machine learning algoritme en de werking is relatief simpel.
                 Een supervised algoritme heeft gelabelde data nodig als input, om de output van een label te
                 voorzien. Het algoritme kan voor classificatie problemen en regressie problemen toegepast worden.
                 "),
              tags$p(""),
              tags$h4(tags$strong("Hoe werkt het K-nearest-neigbhours algoritme?")),
              tags$p("KNN werkt met een database en een nieuwe case. Op basis van een formule wordt de afstand tussen
            de nieuwe case (ook wel de query genoemd) en de casussen in de database berekend. Dit gebeurt op 
            basis van de variabelen van de data. Omdat KNN de absolute afstand berekent, wordt de data eerst 
            genormalizeerd. Nadat de afstand is berekend worden er een zelf gekozen aantal casussen in
            de database geselecteerd die het dichtste bij de nieuwe case liggen. Dit zelf gekozen aantal
            is de 'k' in de nearest neighbours algoritme. Vervolgens wordt, in het geval van classificatie,
            van de geselecteerde casussen in de database gekeken welke klasse ze hebben. Vervolgens
            wordt er heel simpel 'gestemd': de klasse die de meeste van de geselecteerde casussen hebben
            wordt toegewezen aan de nieuwe casus."),
              tags$p(""),
              tags$h4(tags$strong("Voordelen van Case-Based Reasoning")),
              tags$p("Case-Based Reasoning gaat uit van de waarde van oplossingen voor oude problemen. Deze oplossingen kunnen
                  namelijk ook hergebruikt worden voor nieuwe problemen. Dit maakt CBR anders dan andersoortige classificatie
                  methoden zoals logistische regressie en random-forest modellen. Deze modellen moeten op voorhand getraind worden,
                  en genereren niet pas een oplossing als er nieuwe data beschikbaar is. Een CBR-model wordt ook op voorhand getraind,
                  maar alleen om de ideale k te bepalen. Een CBR-model zoekt, als er nieuwe data beschikbaar is, in de case-base naar soortgelijke casussen. Vervolgens
                  wordt de 'oplossing' van het meest soortgelijke probleem, of problemen, aangedragen als oplossing voor de nieuwe casus."),
              tags$h4(tags$strong("Fases van een CBR-systeem")),
              img(src = "cbr_image.jpg"),
              tags$p("Een CBR-systeem bestaat uit vier verschillende fases, die op bovenstaande afbeelding 
                 afgebeeld zijn. De verschillende fases worden hieronder omschreven."),
              tags$strong("1. Retrieve fase: in het systeem"),
              tags$p("De cyclus van CBR begint met een nieuw probleem, de 'current problem'. Vervolgens wordt in de Case Base, waarin alle
                 oude casussen staan gezocht naar gelijke casussen. Dit wordt gedaan met behulp van een algoritme dat 
                 met een distance measure de afstand tussen de nieuwe en oude case berekent. Vervolgens worden de meest
                 gelijk casussen opgezocht, en mogelijk weergegeven aan de gebruiker."),
              tags$strong("2. Reuse fase: in en buiten het systeem"),
              tags$p("Op basis van de meest gelijke gevallen wordt er een oplossing aangedragen. Dit kan de oplossing van de meest gelijke
                 casus zijn, maar kan ook een combinatie van verschillende oplossingen zijn. Ook kan een bestaande oplossing
                 aangepast worden aan een nieuwe situatie. De oplossing wordt vervolgens, buiten het systeem, toegepast op de nieuwe casus."),
              tags$strong("3. Revise fase: buiten het systeem"),
              tags$p("Deze stap is niet verplicht, maar nadat de oplossing is toegepast kan na verloop van tijd gezegd worden
                 in hoeverre de oplossing werkte. In het geval dat de oplossing niet werkte, kan de casus opnieuw door het
                 CBR systeem behandeld worden, en kan er een revised solution aangedragen worden. Deze kan vervolgens
                 ook weer toegepast worden op de casus"),
              tags$strong("4. Retain fase: in het systeem"),
              tags$p("Als laatste stap in het CBR systeem kan de nieuwe casus, met bijbehorende solution, weer toegevoegd
                 worden aan de case base als dit waarde oplevert voor het systeem. Als de nieuwe casus wordt toegevoegd
                 kunnen zaken die in de toekomst een solution zoeken, ook leren van de solution van de nieuwe case,
                 die dan wel succesvol was of niet. Op deze manier kan een CBR-systeem dus leren van zijn eigen advies.
                 Het is wel belangrijk om ervoor te zorgen dat het systeem niet zijn eigen bias creëert door het opslaan
                 van zijn eigen zaken, dus of dit gebeurt verschilt per systeem")
            ),
            column(
              width = 4,
              column(
                width = 12,
                div(style = "height:50px")
              ),
              column(
                width = 2
              ),
              column(
                width = 10,
                style = "border-style: ridge;width:400px;",
                tags$h4(tags$strong("Wat is het verschil tussen classificatie en regressie?")),
                tags$p("Bij classificatie moet het model een klasse voorspellen en bij regressie moet het model een
                   getal voorspellen."),
                tags$strong("Voorbeelden van een classificatieprobleem:"),
                tags$li("Het identificeren van spam-mails. Twee labels: spam en niet-spam"),
                tags$li("Het identificeren van een ziekte. Twee labels: ziek en niet-ziek"),
                tags$li("Het identificeren van bepaalde groepen klanten. Twee labels: koopt wel en koopt niet na reclame"),
                tags$p(""),
                tags$strong("Voorbeelden van een regressieprobleem:"),
                tags$li("Het voorspellen van de omzet van een bedrijf valuta"),
                tags$li("Het voorspellen van de ijsverkoop in een bepaald jaar: kwantiteit in valuta"),
                tags$li("Het voorspellen van de temperatuur in een bepaalde maand: kwantiteit in temperatuur"),
                tags$p("")
              ),
            )
          )
        ),
        tabPanel(
          # Derde verticale tab op algemene informatie pagina
          # Tab met daarin informatie over de dataset die gebruikt is
          tags$h4("De 'sentence' dataset"),
          column(
            width = 11,
            tags$h2(tags$strong("De 'sentence' dataset")),
            tags$p("Voor dit CBR model is een open dataset gebruik van de Cook County Government.
                 In deze dataset staan ruim 250.000 veroordelingen van duizenden mensen die in Cook County
                 zijn veroordeeld voor verschillende type delicten."),
            tags$a(href = "https://datacatalog.cookcountyil.gov/Courts/Sentencing", "De dataset is te downloaden via deze link"),
            tags$h4(tags$strong("Aanpassingen aan data")),
            tags$p("Om de data te kunnen gebruiken voor dit CBR-systeem zijn er verschillende aanpassingen gedaan. 
                 De aanpassingen zijn toegepast in de functies read_and_clean, te vinden in het functions.R bestand,
                 maar de meest belangrijke aanpassingen worden hieronder vermeld:"),
            tags$li("De originele kolom SENTENCE_TYPE bevatte verschillende soorten detentie. Deze zijn aangepast, zoals vermeld in
                  de uitleg van de data op de website van Cook County, naar twee categorieën: detention en non-detention"),
            tags$li("De database bevatte veel dubbele observaties, zowel dubbele charge_id als person_ids. Daarom
                  zijn deze observaties verwijderd uit de dataset."),
            tags$li("Er zijn verschillende variabelen uit de dataset verwijderd. Uiteindelijk worden er acht
                  voorspellende variabelen meegenomen in het model, en zijn er drie variabelen die meer info
                  geven over de zaak. Deze variabelen worden weergegeven bij het tab 'gebruikte CBR-model'."),
            tags$li("Er is een steekproef van 1/5 van de gehele database genomen, omdat een groot aantal observaties
                  het CBR-model langzamer kan maken. Er is voor gezorgd dat alle groepen evenredig vertegenwoordigd
                  zijn in het model."),
            tags$li("Deze steekproef is vervolgens opgesplitst in een testset en in een train set. De trainset wordt gebruikt
                  als case-base, en de testset is alleen gebruikt om de werking van het model te testen."),
            tags$h4(tags$strong("Impressie van de sentence dataset")),
            tags$p("Hieronder zie je de eerste tien observaties van de trainset. De totale trainset bestaat
                 uit 2791 observaties van 12 verschillende variabelen."),
            DT::dataTableOutput("head_trainset")
          )
        ),
        tabPanel(
          # Vierde verticale tab op algemene informatie pagina
          # Tab met informatie over het ontwikkelde CBR-model
          tags$h4("Het ontwikkelde CBR-model"),
          column(
            width = 11,
            tags$h2(tags$strong("Het ontwikkelde CBR-model")),
            tags$p("Het model is gebouwd in R, en er is geen geüpdatet Case-Based Reasoning package
            beschikbaar in deze programmeertaal. Daarom heb ik zelf, op basis van een bestaande KNN-package,
            code geschreven voor een CBR-model. Hieronder leg ik kort uit hoe ik het FNN-package heb gebruikt
            om een CBR-model te kunnen bouwen. Daarna ga ik kort in op hoe de meest ideale k voor de sentence
            data is gevonden met behulp van een test en train set."),
            tags$h4(tags$strong("Project workflow op basis van targets")),
            tags$p("Dit project heb ik met behulp van het targets package gemaakt. Dit is een package
                   waarmee je makkelijk een duidelijke project-workflow kan maken. Dit houdt in dat je je 
                   code in functies schrijft, waardoor het voor anderen duidelijk is wat er in je code gebeurt.
                   Daarnaast heeft het package als voordeel dat het erg tijdbesparend is: als je maar één stap
                   van je workflow hebt veranderd wordt alleen de aangepaste stap uitgevoerd."),
            tags$p("De targets package wordt ook gebruikt om een duidelijke workflow van je code te genereren,
                   de workflow van mijn project is hieronder te vinden. Klik op de stappen van het proces
                   om een korte omschrijving van de stap te lezen. Het uiteindelijke CBR-model is te vinden
                   onder 'results_cbr' en de gegenereerde query is de stap 'random_query'"),
            includeHTML("www/workflow.html"),
            tags$h4(tags$strong("Van een KNN-package naar CBR-Model")),
            tags$p("In R zijn vele KNN-packages beschikbaar, echter geen CBR-package. Daarom heb ik het package
                   FNN (Fast Nearest Neighbours) gebruikt om een CBR-model te bouwen. Het FNN package heeft
                   verschillende opties voor KNN, en genereert naast de voorspelde klasse/waarde ook een
                   distance matrix en een indextabel, met daarin de nearest-neighbours. Ik heb deze twee
                   tabellen gebruikt om het CBR-systeem te bouwen."),
            tags$p("Een distance matrix is een tabel met daarin de afstand tussen de nieuwe case en de casussen
                   in de database. Bij KNN worden de k-nearest neighbours van de nieuwe case geselecteerd: dit
                   zijn dus de casussen met de laagste afstand. Voor een CBR-model wil je geen afstand genereren,
                   maar een similarity percentage. In de code zit een stuk verwerkt die de maximale distance
                   o.b.v. een dataset kan genereren. Deze berekent de afstand tussen twee casussen: een van de 
                   casussen heeft allemaal nullen als waarneming, en een andere casus heeft allemaal enen als waarneming.
                   Het resultaat hiervan is de maximale afstand tussen twee punten in een dataset. Om vervolgens het similarity
                   percentage tussen de query en de oude casussen te berekenen wordt de volgende formule gebruikt:"),
            tags$code("similarity = 1 - (afstand tussen twee punten / maximiale afstand)"),
            tags$p(""),
            tags$p("Vervolgens heb ik de indextabel gebruikt om een tabel te genereren met de k-nearest neighbours.
                   Het model weergeeft simpelweg de k-nearest neighbours op basis van het similarity percentage.
                   De klasse van de query-case wordt op basis van het stemprincipe gegenereert: de voorspelde klasse
                   is de klasse die de meeste van de nearest-neighbours hebben. Hierbij wordt echter wel de 
                   probability van het stemmen getoond. Dit is het percentage van de nearest neighbours die ook deze
                   klasse heeft."),
            tags$h4(tags$strong("KNN-methode en ideale K bepalen op basis van test- en train set")),
            tags$p("Het KNN-algoritme van het FNN-package heeft twee belangrijke instellingen: het aantal nearest neighbours
                   dat geselecteerd wordt en de methode van de KNN: 'kd-tree', 'cover_tree', of 'brute'. Daarnaast
                   kunnen er verschillende voorspellende variabelen gebruikt worden: alle voorspellende
                   variabelen of alleen voorspellende variabelen die een bepaald significantieniveau hebben. Deze
                   significantie kan berekend worden met behulp van logistische regressie."),
            tags$p("De ideale K, methode van KNN en welke voorspellende variabelen meegenomen moeten worden kan
                 berekend worden door voor elk van deze mogelijkheden (de drie methodes met de twee soorten
                 voorspellende variabelen) het KNN-algoritme te runnen op een test set. De data wordt hierbij 
                 opgesplitst in een trainset van 70% en een testset van 30%. Vervolgens wordt het KNN-algoritme
                 toegepast op de testset, met een k van 1 tot en met 300. Zo wordt voor elke case in de testset
                 de klasse voorspeld. Vervolgens kan deze voorspelde klasse vergeleken worden met de echte klasse.
                 Dit resulteert in een 'accuracy level'. Dit is het percentage klasse van de testset dat
                 het model goed voorspeld heeft."),
            tags$p("De onderstaande grafiek toont voor elke van de combinaties, tot en met k=300, de accuracy 
                 van het model. Zoals te zien is presteert het model beter wanneer alle predictors worden
                 meegenomen en dit is ook verwerkt in het model. De hoogste accuracy, 64.9%, genereert het model met een
                 k-value van 15 en met methode de brute. Dit zijn dan ook de standaard instellingen
                 voor het CBR model. De accuracy is niet heel erg hoog, maar dat komt ook doordat dit model
                 is gebouwd om de waarde van de CBR-methode te illustreren met een test database en niet een zo
                 hoog mogelijke accuracy te genereren. Wel moet opgemerkt worden dat het percentage veroordelingen
                 in de database met een 'sentence' slechts 45.1% is. Dit betekent dat het model beter in staat 
                 is om de veroordelingen te voorspellen dan als alle voorspellingen gelabeled 
                 worden met 'detention'. In dat geval zal de accuracy maar 45.1% zijn.")
          ),
          column(
            width = 10,
            offset = 1,
            img(
              src = "plot_knn.png",
              style = "display: block; margin-left: auto; margin-right: auto;"
            )
          )
        )
      )
    ),
    tabPanel(
      # Tweede horizontale tab met het CBR model
      title = tags$h4(tags$strong("CBR Model")),
      navlistPanel(
        tabPanel(
          # Eerste verticale tab op CBR model horizontale tab
          # Tab om een query te genereren die door het CBR model gerund wordt
          title = tags$h4("Genereer query"),
          fluidPage(
            column(
              width = 12,
              style = "text-align: center",
              tags$h2(tags$strong("Genereer query"))
            ),
            column(
              width = 10,
              offset = 1,
              tags$p("Kies hier een methode om een nieuwe query voor het CBR-model te genereren.
                   Er zijn drie opties: upload een .csv bestand in het juiste format, genereer een
                   random query, of voer handmatig een nieuwe query in. Onder elke methode staat een aparte
                     submit knop: upload query voor de eerste methode, genereer random query voor de tweede
                     methode, en submit query voor de derde methode. De gegenereerde query verschijnt
                     enkele seconden nadat je op de knop hebt gedrukt onder aan de pagina.
                     Wanneer je tevreden bent met de query klik je op 'accept' query. De query wordt nu 
                     gerund. De output van het model kun je vinden onder het tabje 'resultaten CBR-model'")
            ),
            column(
              width = 1
            ),
            column(
              width = 12,
              #  hr(style = "border-top: 1px solid #606060;")
            ),
            column(
              width = 3,
              style = ("text-align: center;height: 950px"),
              tags$h3(tags$strong("1. Upload bestand")),
              fileInput("csv_cbr",
                label = "Upload .csv bestand"
              ),
              actionButton(
                inputId = "upload_file",
                label = "Upload query"
              )
            ),
            column(
              width = 3,
              offset = 0.5,
              style = ("text-align: center;height: 950px"),
              tags$h3(tags$strong("2. Genereer random query")),
              column(
                width = 12,
                div(style = "height:72px;"),
                actionButton(
                  inputId = "random_query",
                  label = "Genereer random query"
                ),
                tags$p(""),
                tags$strong(textOutput("random_query_submit"))
              )
            ),
            column(
              width = 6,
              tags$h3(tags$strong("3. Handmatige invoer query")),
              style = ("text-align: center;height: 950px"),
              column(
                width = 6,
                style = "text-align: center",
                inputPanel(
                  tags$strong("Niet-voorspellende variabelen"),
                  helpText("Dit zijn variabelen die niet door het model meegenomen worden als voorspellende variabelen,
                      maar puur als additionele informatie weer worden gegeven."),
                  numericInput(
                    inputId = "een",
                    label = "case_id",
                    value = NULL,
                    min = min_value[[1]],
                    max = max_value[[1]]
                  ),
                  selectInput(
                    inputId = "zeven",
                    label = "gender",
                    choices = levels(database[[7]])
                  )
                )
              ),
              column(
                width = 6,
                style = "text-align: center",
                inputPanel(
                  tags$strong("Voorspellende variabelen"),
                  helpText("Dit zijn voorspellende variabelen die door het model gebruikt worden om de class van de query
                   case te voorspellen."),
                  numericInput(
                    inputId = "twee",
                    label = "charge count",
                    value = NULL,
                    min = min_value[[2]],
                    max = max_value[[2]]
                  ),
                  selectInput("drie",
                    label = "charge disposition",
                    choices = levels(database[[3]])
                  ),
                  selectInput(
                    inputId = "vier",
                    label = "sentence court name",
                    choices = levels(database[[4]])
                  ),
                  numericInput(
                    inputId = "vijf",
                    label = "length of case in days",
                    value = NULL,
                    min = min_value[[5]],
                    max = max_value[[5]]
                  ),
                  numericInput(
                    inputId = "zes",
                    label = "age at incident",
                    value = NULL,
                    min = min_value[[6]],
                    max = max_value[[6]]
                  ),
                  selectInput(
                    inputId = "acht",
                    label = "offense category",
                    choices = levels(database[[8]])
                  ),
                  numericInput(
                    inputId = "negen",
                    label = "days incident arrest",
                    value = NULL,
                    min = min_value[[9]],
                    max = max_value[[9]]
                  ),
                  selectInput(
                    inputId = "elf",
                    label = "convicted chicago",
                    choices = levels(database[[11]])
                  ),
                  actionButton(
                    inputId = "submit",
                    label = "Submit query"
                  ),
                  tags$p(""),
                  tags$strong(textOutput("submit_message"))
                )
              )
            ),
            column(
              width = 12,
              style = "text-align: center",
              uiOutput("gegenereerde_query")
            )
          )
        ),
        tabPanel(
          # Tweede verticale tab op CBR model horizontale tab
          # Tab om het CBR model te runnen met de gegenereerde query
          title = tags$h4("Run CBR-model"),
          column(
            width = 12,
            tags$h2(tags$strong("Run CBR-model")),
            style = "text-align: center",
            column(
              width = 10,
              style = "text-align: left",
              offset = 1,
              tags$p("Het KNN-algoritme, welke onder het CBR-model draait, heeft twee additionele instellingen: 
              het aantal nearest neighbours dat geselecteerd wordt (K) en de methode waarop deze gevonden
                worden. De instellingen die de hoogste accuracy opleveren zijn geselecteerd, maar deze
                     instellingen kunnen hier aangepast worden.")
            ),
            column(
              width = 1
            ),
            column(
              width = 8,
              style = "text-align: center",
              inputPanel(
                numericInput("k",
                  label = "Aantal voor K",
                  value = 15
                ),
                selectInput("cbr_method",
                  label = "CBR-methode",
                  choices = c("kd_tree", "cover_tree", "brute"),
                  selected = "brute"
                )
              )
            ),
            column(
              width = 4,
              inputPanel(
                column(
                  width = 12,
                  style = "height: 10px"
                ),
                column(
                  width = 12,
                  actionButton(
                    inputId = "run_cbr_model",
                    label = "Run CBR-model"
                  )
                ),
                column(
                  width = 12,
                  style = "height: 25px"
                )
              )
            )
          ),
          column(
            width = 12,
            tags$h2(tags$strong(textOutput("resultaten_cbr_model"))),
            style = "text-align: center",
            column(
              width = 6,
              tags$h3(tags$strong(textOutput("voorspelde_klasse_text"))),
              style = "text-align: right",
            ),
            column(
              width = 6,
              h3(strong(textOutput("voorspelde_klasse_query"))),
              style = "text-align: left"
            )
          ),
          column(
            width = 12,
            column(
              width = 6,
              tags$h3(tags$strong(textOutput("percentage_neighbours_text")),
                style = "text-align: right"
              )
            ),
            column(
              width = 6,
              h3(strong(textOutput("percentage_neighbours")),
                style = "text-align: left"
              ),
            )
          ),
          column(
            width = 12,
            uiOutput("streep")
          ),
          column(
            width = 10,
            offset = 1,
            tags$h3(tags$strong(textOutput("nearest_neighbours_query")),
              style = "text-align: center"
            )
          ),
          column(width = 1),
          column(
            width = 12,
            DT::dataTableOutput("query_cbr_results")
          ),
          column(
            width = 12,
            uiOutput("accepting_query")
          ),
          column(
            width = 12,
            uiOutput("nieuwe_case_base"),
          ),
          column(
            width = 12,
            dataTableOutput("update_casebase")
          )
        )
      )
    )
  )
)

#------------------------------------------#
# server = code om content in de UI te genereren
server <- function(input, output) {
  # reactive values voor de knoppen 'random_query' en 'manual_query'
  reactive <- reactiveValues(value = "leeg")
  observe({
    input$random_query
    reactive$value <- "random_query"
  })
  observe({
    input$submit
    reactive$value <- "manual_query"
  })
  observe({
    input$upload_file
    reactive$value <- "upload_file"
  })

  # output train set op 'de sentence database' tab
  output$head_trainset <- DT::renderDataTable(
    {
      database %>%
        relocate(sentence, gender, case_id)
    },
    options = list(scrollX = TRUE)
  )

  # submit messages voor knoppen op 'genereer query' tab
  output$accept_query_message <- renderText({
    if (input$run_cbr_model > 0) {
      return("")
    } else if (input$accept_query > 0) {
      return("De query is geaccepteerd")
    } else {
      return("")
    }
  })

  # submit messages voor genereren van query
  output$submit_message <- renderText({
    if (input$run_cbr_model > 0) {
      return("")
    } else if (input$submit == 1) {
      return("De ingevoerde query is gegenereerd")
    } else if (input$submit > 1) {
      return("De nieuw ingevoerde query is gegenereerd")
    }
    else {
      return("")
    }
  })

  output$random_query_submit <- renderText({
    if (input$run_cbr_model > 0) {
      return("")
    } else if (input$random_query == 1) {
      return("De random query is gegenereerd")
    } else if (input$random_query > 1) {
      return("De nieuwe random query is gegenereerd")
    }
    else {
      return("")
    }
  })

  # renderen van UI voor het accepteren van de query
  # op de eerste horizontale tab van de pagina cbr-model
  output$gegenereerde_query <- renderUI({
    if (reactive$value == "random_query") {
      display_random_query()
      taglist <- list(fluidPage(
        column(
          width = 12,
          tags$h2(tags$strong("De gegenereerde query")),
          DT::DTOutput("query_display", width = "100%"),
          DT::DTOutput("random_query_display"),
          hr(style = "border-top: 1px solid #606060;"),
          tags$h2(tags$strong("Accepteren van query")),
          style = "text-align: center",
          column(
            width = 12,
            style = "height: 20px"
          )
        ),
        column(
          width = 8,
          style = "text-align: left",
          tags$p("Klik op 'accept query' om de query te accepteren. Ga vervolgens naar de tab
                       'run CBR-model', waar je de query kunt runnen.")
        ),
        column(
          width = 4,
          actionButton(
            inputId = "accept_query",
            label = "Accept query"
          ),
          tags$p(""),
          tags$strong(textOutput("accept_query_message"))
        )
      ))
    }
    else if (reactive$value == "manual_query") {
      display_query()
      list(fluidPage(
        column(
          width = 12,
          tags$h2(tags$strong("De gegenereerde query")),
          DT::DTOutput("query_display", width = "100%"),
          DT::DTOutput("random_query_display"),
          hr(style = "border-top: 1px solid #606060;"),
          tags$h2(tags$strong("Accepteren van query")),
          style = "text-align: center",
          column(
            width = 12,
            style = "height: 20px"
          )
        ),
        column(
          width = 8,
          style = "text-align: left",
          tags$p("Klik op 'accept query' om de query te accepteren. Ga vervolgens naar de tab
                       'run CBR-model', waar je de query kunt runnen.")
        ),
        column(
          width = 4,
          actionButton(
            inputId = "accept_query",
            label = "Accept query"
          ),
          tags$p(""),
          tags$strong(textOutput("accept_query_message"))
        )
      ))
    }
    else if (reactive$value == "upload_file") {
      display_query_manual()
      list(fluidPage(
        column(
          width = 12,
          tags$h2(tags$strong("De gegenereerde query")),
          DT::DTOutput("query_display", width = "100%"),
          DT::DTOutput("random_query_display"),
          hr(style = "border-top: 1px solid #606060;"),
          tags$h2(tags$strong("Accepteren van query")),
          style = "text-align: center",
          column(
            width = 12,
            style = "height: 20px"
          )
        ),
        column(
          width = 8,
          style = "text-align: left",
          tags$p("Klik op 'accept query' om de query te accepteren. Ga vervolgens naar de tab
                       'run CBR-model', waar je de query kunt runnen.")
        ),
        column(
          width = 4,
          actionButton(
            inputId = "accept_query",
            label = "Accept query"
          ),
          tags$p(""),
          tags$strong(textOutput("accept_query_message"))
        )
      ))
    }
  })

  ## Queries genereren ##
  # event reactive voor de twee soorten queries #
  display_query <- eventReactive(input$submit, {
    query <- c(
      input$een, input$twee, input$drie, input$vier, input$vijf, input$zes, input$zeven, input$acht,
      input$negen, "", input$elf
    )
    query_data <- database %>%
      rbind(query) %>%
      tail(1) %>%
      mutate_if(is.character, as.numeric)
  })

  display_query_manual <- eventReactive(input$upload_file, {
    query <- read_delim(input$csv_cbr$datapath,
      ";",
      escape_double = FALSE, trim_ws = TRUE
    )
    query_data <- database %>%
      rbind(query) %>%
      tail(1) %>%
      mutate_if(is.character, as.numeric)
    return(query_data)
  })
  display_random_query <- eventReactive(input$random_query, {
    min_value <- NULL
    max_value <- NULL
    levels <- NULL
    random_level <- NULL
    value <- NULL
    for (i in 1:ncol(database)) {
      if (!(is.numeric(database[[i]]))) {
        min_value[[i]] <- NA
        max_value[[i]] <- NA
        levels[[i]] <- levels(database[[i]])
      } else {
        min_value[[i]] <- min(database[[i]])
        max_value[[i]] <- max(database[[i]])
        levels[[i]] <- levels(database[[i]])
      }
    }
    for (i in 1:ncol(database)) {
      if (is.na(min_value[i])) {
        all_level <- levels[[i]]
        random_level[[i]] <- sample(all_level, 1)
      } else {
        a <- c(min_value[[i]], max_value[[i]])
        random_level[[i]] <- sample(a, 1)
      }
    }
    query_data <- database %>%
      rbind(random_level) %>%
      tail(1) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(sentence = "")
    return(query_data)
  })

  # weergeven van de gegenereerde random queries
  output$random_query_display <- DT::renderDataTable(
    {
      if (reactive$value == "random_query") {
        query <- display_random_query()
        query <- query %>%
          relocate(sentence, gender, case_id)
        return(query)
      }
      if (reactive$value == "manual_query") {
        query <- display_query()
        query <- query %>%
          relocate(sentence, gender, case_id)
        return(query)
      }
      if (reactive$value == "upload_file") {
        query <- display_query_manual()
        query <- query %>%
          relocate(sentence, gender, case_id)
        return(query)
      }
    },
    options = list(scrollX = TRUE)
  )

  ## Runnen en resultaten van CBR-model ##
  # Event reactive voor het runnen van het cbr_model
  run_cbr_model <- eventReactive(input$run_cbr_model, {
    if (reactive$value == "random_query") {
      random_query_data <- display_random_query()
      display_the_results <- cbr_model(
        database = database,
        query = random_query_data,
        methode = input$cbr_method,
        k = input$k
      )
      return(display_the_results)
      break()
    }
    if (reactive$value == "manual_query") {
      custom_query_data <- display_query()
      display_the_results <- cbr_model(
        database = database,
        query = custom_query_data,
        methode = input$cbr_method,
        k = input$k
      )
      return(display_the_results)
      break()
    }
    if (reactive$value == "upload_file") {
      manual_query_data <- display_query_manual()
      display_the_results <- cbr_model(
        database = database,
        query = manual_query_data,
        methode = input$cbr_method,
        k = input$k
      )
      return(display_the_results)
      break()
    }
  })

  # voorspelde solution en % nearest neighbours
  output$voorspelde_klasse_text <- renderText({
    run_cbr_model()
    return("De voorspelde klasse, proposed solution, van de query-case is:")
  })
  output$voorspelde_klasse_query <- renderText({
    results <- run_cbr_model()
    return(results$class)
  })
  output$percentage_neighbours_text <- renderText({
    run_cbr_model()
    return("Dit is door het volgende percentage van de nearest neighbours aangedragen:")
  })
  output$percentage_neighbours <- renderText({
    results <- run_cbr_model()
    text <- round((results$probability * 100), digits = 2)
    text <- paste(text, "%")
    return(text)
  })

  # Extra text en lay-out voor resultaten CBR model
  output$resultaten_cbr_model <- renderText({
    run_cbr_model()
    return("Resultaten CBR model")
  })
  output$nearest_neighbours_query <- renderText({
    run_cbr_model()
    return("Nearest neighbours van de query case:")
  })
  output$streep <- renderUI({
    run_cbr_model()
    hr(style = "border-top: 1px solid #606060;")
  })

  # datatable met nearest neighbours van query case
  output$query_cbr_results <- DT::renderDataTable(
    {
      results <- run_cbr_model()
      data <- results$results_dataframe
      data <- data %>%
        mutate(similarity = round(similarity, digits = 4)) %>%
        mutate(similarity = paste(similarity * 100, "%", sep = ""))
    },
    options = list(
      scrollX = TRUE,
      digits = 3
    )
  )

  ## Query en solution toevoegen aan de case base ##
  # Event reactive voor de knop
  add_query_to_casebase <- eventReactive(input$toevoegen_casebase, {
    if (reactive$value == "random_query") {
      add_row <- display_random_query()
      results <- run_cbr_model()
      add_row <- add_row %>%
        mutate(sentence = results$class)
      database <- database %>%
        full_join(add_row)
      return(database)
      break()
    }
    if (reactive$value == "manual_query") {
      add_row <- display_query()
      results <- run_cbr_model()
      add_row <- add_row %>%
        mutate(sentence = results$class)
      database <- database %>%
        full_join(add_row)
      return(database)
      break()
    }
    if (reactive$value == "upload_file") {
      add_row <- display_query_manual()
      results <- run_cbr_model()
      add_row <- add_row %>%
        mutate(sentence = results$class)
      database <- database %>%
        full_join(add_row)
      return(database)
      break()
    }
  })

  # Rendereen van UI om query toe te voegen
  # aan de case-base
  output$accepting_query <- renderUI({
    run_cbr_model()
    tagList(column(
      width = 12,
      hr(style = "border-top: 1px solid #606060;"),
      tags$h2(tags$strong("Toevoegen aan case-base?")),
      style = "text-align: center",
      column(
        width = 12,
        style = "height: 20px"
      ),
      column(
        width = 8,
        style = "text-align: left",
        tags$p("Voeg de query met de proposed solution toe aan de case-base?")
      ),
      column(
        width = 4,
        actionButton("toevoegen_casebase",
          label = "voeg toe aan case base"
        ),
        tags$p("")
      )
    ))
  })


  # Renderen van nieuwe kolom onder pagina met daarin nieuwe case base
  output$nieuwe_case_base <- renderUI({
    add_query_to_casebase()
    tagList(column(
      width = 12,
      style = "text-align: center",
      p(""),
      hr(style = "border-top: 1px solid #606060;"),
      tags$h2(tags$strong("Nieuwe case-base")),
      p("Dit is de nieuwe case-base met de gerunde query case")
    ))
  })

  # Datatable met nieuwe case base
  output$update_casebase <- renderDataTable(
    {
      data <- add_query_to_casebase()
      data <- data %>%
        relocate(sentence, gender, case_id)
      return(data)
    },
    options = list(
      scrollX = TRUE,
      pageLength = 10
    )
  )
}

#------------------------------------------#
## Runnen van applicatie ##
shinyApp(ui = ui, server = server)
