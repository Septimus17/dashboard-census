#### load libraries ####
library(readr)
library(rlang)
library(dplyr)
library(VIM)
library(MASS)
library(ggplot2)
library(gridExtra)
library(scales)
library(shinythemes)
library(plotly)
library(DT)
library(knitr)
library(markdown)
library(vcd)
library(shinydashboard)
library(shinyjs)
library(extrafont)
library(shinydashboardPlus)
library(shinyWidgets)
library(apputils)

#### load data and scripts ####
source("global.R")

#### User interface ####
shinyUI(
  tagList(
    #### Page ####
    dashboardPagePlus(
      enable_preloader = T,
      skin = "purple",
      #### Header ####
      dashboardHeaderPlus(
        title = tagList(
          span(class = "logo-lg", "Dashboard CensUS"), 
          img(id = "logo",
              src = "https://image.flaticon.com/icons/png/512/1209/1209043.png"))
      ),
      #### Sidebar ####
      dashboardSidebar(
        sidebarMenu(
          menuItem("Data Understanding", tabName = "page1", icon = icon("search")),
          menuItem("Data Preparation", tabName = "page2", icon = icon("cog")),
          menuItem("Data Table", tabName = "page3", icon = icon("table")),
          menuItem("Plot", tabName = "page4", icon = icon("bar-chart-o"), 
                   startExpanded = F,
                   menuSubItem("One Variable", tabName = "subpage1"),
                   menuSubItem("Two Variables", tabName = "subpage2"),
                   menuSubItem("Three Variables", tabName = "subpage3"))
        )
      ),
      #### Body ####
      dashboardBody(
        tags$head(includeCSS('www/style.css')),
        tabItems(
          tabItem(
            #### Page1 ####
            tabName = "page1",
            #uiOutput('und')
            h2("Data Understanding"),
            p("La seguente analisi è orientata all'elaborazione e allo sviluppo di tecniche
            di Machine Learning, sui dati del censimento degli Stati Uniti del 1996. Il 
            dataset è formato da 32.561 osservazioni, l'obiettivo è quello di classificare 
            gli individui in due macro-categorie del reddito: minore o uguale di 50K 
            dollari annui, maggiore di 50K dollari annui. Di seguito vedremo tutte le 
            variabili presenti nel dataset."),
            fluidRow(
              box(
                p(code("age"), 
                  ": variabile quantitativa, indica l'età dell'individuo."
                ),
                width = 4
              ),
              box(
                plotlyOutput("age.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p(code("workclass"), 
                  ": variabile qualitativa, indica il settore in cui l'individuo lavora:"),
                tags$ul(
                  tags$li("`?`: Dato mancante,"),
                  tags$li("`Federal-gov`: Pubblico federale,"),
                  tags$li("`Local-gov`: Pubblico locale,"),
                  tags$li("`Never-worked`: Mai lavorato,"),
                  tags$li("`Private`: Privato,"),
                  tags$li("`Self-emp-inc`: Lavoratore autonomo con reddito,"),
                  tags$li("`Self-emp-no-inc`: Lavoratore autonomo senza reddito,"),
                  tags$li("`State-gov`: Pubblico statale,"),
                  tags$li("`Without-pay`: Senza paga.")
                ),
                width = 4
              ),
              box(
                plotlyOutput("work.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p(code("fnlwgt"), 
                  ": variabile quantitativa, indica i pesi relativi ad ogni individuo, essi 
                sono controllati per stime indipendenti della popolazione di civili non
                istituzionali degli Stati Uniti. Questi sono preparati mensilmente dalla
                Population Division al Census Bureau. Sono stati usati 3 set di controlli:"),
                tags$ul(
                  tags$li("Una stima di una singola cella della popolazione dai 16 anni in
                        su per ogni stato;"),
                  tags$li("Controlli per origine ispanica per età e sesso;"),
                  tags$li("Controlli per etnia, età e sesso.")
                ),
                p("Persone con caratteristiche demografiche simili dovrebbero avere pesi 
                simili."),
                width = 4
              ),
              box(
                plotlyOutput("fnlw.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p(code("education"), 
                  ": variabile qualitativa ordinale, indica il grado di istruzione
                dell'individuo, viene semplificata nella variabile", 
                  code("education.num")," dove si da un valore numerico (da 1 a 16) ad ogni 
                livello di istruzione avendo così un'ordinamento ben preciso tra i vari 
                livelli."),
                width = 4
              ),
              box(
                plotlyOutput("edu.plot"),
                width = 4,
                status = "primary"
              ),
              box(
                plotlyOutput("edu.num.plot"),
                width = 4,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p(code("marital.status"), 
                  ": variabile qualitativa, indica lo stato civile dell'individuo:"),
                tags$ul(
                  tags$li("`Divorced`: Divorziato,"),
                  tags$li("`Married-AF-spouse`: Prima volta sposato,"),
                  tags$li("`Married-civ-spouse`: Sposato civilmente,"),
                  tags$li("`Married-spouse-absent`: Sposato con coniuge assente,"),
                  tags$li("`Never-married`: Mai sposato,"),
                  tags$li("`Separated`: Separato,"),
                  tags$li("`Widowed`: Vedovo.")
                ),
                width = 4
              ),
              box(
                plotlyOutput("mar.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p(code("occupation"), 
                  ": variabile qualitativa, indica la tipologia di lavoro che svolge 
                l'individuo:"),
                tags$ul(
                  tags$li("`?`: Dato mancante,"),
                  tags$li("`Adm-clerical`: Amministrativo clericale,"),
                  tags$li("`Armed-Forces`: Forze armate,"),
                  tags$li("`Craft-repair`: Riparazioni/artigianato,"),
                  tags$li("`Exec-managerial`: Esecutivo/manageriale,"),
                  tags$li("`Farming-fishing`: Agricoltura/pesca,"),
                  tags$li("`Handlers-cleaners`: Gestori/addetti alle pulizie,"),
                  tags$li("`Machine-op-inspct`: Operai,"),
                  tags$li("`Priv-house-serv`: Servizi di casa,"),
                  tags$li("`Prof-specialty`: Professionale/specializzato,"),
                  tags$li("`Protective-serv`: Servizi di protezione,"),
                  tags$li("`Sales`: Vendite,"),
                  tags$li("`Tech-support`: Tecnologia/supporto,"),
                  tags$li("`Transport-moving`: Trasporti.")
                ),
                width = 4
              ),
              box(
                plotlyOutput("occ.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p(code("relationship"), 
                  ": variabile qualitativa, indica lo stato familiare dell'individuo:"),
                tags$ul(
                  tags$li("`Husband`: Marito,"),
                  tags$li("`Not-in-family`: Non in famiglia,"),
                  tags$li("`Other-relative`: Altri parenti,"),
                  tags$li("`Own-child`: Figlio,"),
                  tags$li("`Unmarried`: Non sposato,"),
                  tags$li("`Wife`: Moglie.")
                ),
                width = 4
              ),
              box(
                plotlyOutput("rel.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p(code("race"), 
                  ": variabile qualitativa, indica l'etnia dell'individuo:"),
                tags$ul(
                  tags$li("`Amer-Indian-Eskimo`: Nativo americano o eskimese,"),
                  tags$li("`Asian-Pac-Islander`: Asiatico o proveniente dalle isole del 
                        Pacifico (Filippine, Australia, Nuova Zelanda, etc.),"),
                  tags$li("`Black`: Nero o di colore,"),
                  tags$li("`Other`: Altra etnia,"),
                  tags$li("`White`: Bianco o occidentale.")
                ),
                width = 4
              ),
              box(
                plotlyOutput("race.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p(code("sex"), 
                  ": variabile qualitativa dicotomica, indica il sesso dell'individuo
                (maschio o femmina)."),
                width = 4
              ),
              box(
                plotlyOutput("sex.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p(code("capital.gain"), "e", code("capital.loss"),
                  ": variabili quantitative, indicano rispettivamente l'ammontare di
                capitale guadagnato e perso, durante l'anno, dall'individuo."),
                width = 4
              ),
              box(
                plotlyOutput("gain.plot"),
                width = 4,
                status = "primary"
              ),
              box(
                plotlyOutput("loss.plot"),
                width = 4,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p(code("hours.per.week"), 
                  ": variabile quantitativa, indica l'ammontare di ore lavorative, durante 
                la settimana, dell'individuo."),
                width = 4
              ),
              box(
                plotlyOutput("hours.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p(code("native.country"), 
                  ": variabile qualitativa, indica la nazione di provenienza dell'individuo."),
                width = 4
              ),
              box(
                plotlyOutput("country.plot"),
                width = 8,
                status = "primary"
              )
            )
          ),
          tabItem(
            #### Page2 ####
            tabName = "page2",
            #uiOutput('prep')
            h2("Data Preparation"),
            p("Alcune delle variabili, elencate nella pagina precedente, presentano dei 
            problemi strutturali. In questo paragrafo verranno esaminate e, se necessario,
            modificate."),
            fluidRow(
              box(
                p("Per quanto riguarda la variabile",code("workclass"), 
                  ", sono state raggruppate le categorie `Never-worked` e `Without-pay` 
                in un'unica nuova categoria `No-Work`, vista la poca numerosità (minore 
                dell'1%) delle stesse e il significato simile delle due categorie."
                ),
                width = 4
              ),
              box(
                plotlyOutput("new.work.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p("Per la variabile", code("marital.status"), ", visto il significato simile 
                delle categorie `Married-civ-spouse`, `Married-AF-spouse` e 
                `Married-spouse-absent`, esse sono state raggruppate sotto la nuova 
                categoria `Married` e vista anche la bassa numerosità (minore dell'1%) 
                delle ultime due categorie."
                ),
                width = 4
              ),
              box(
                plotlyOutput("new.mar.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p("Invece, per la variabile", code("occupation"), "vista la bassa 
                numerosità (minore dell'1%) delle categorie `Armed-Forces` e `Priv-house-serv`, 
                esse sono state aggregate alla categoria `Other-service`."
                ),
                width = 4
              ),
              box(
                plotlyOutput("new.occ.plot"),
                width = 8,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p("Un altro problema strutturale che risalta, riguarda le distribuzioni di", 
                  code("capital.gain"), "e", code("capital.loss"),". Entrambe infatti hanno 
                una forte concentrazione di osservazioni intorno allo zero, circa il 90% 
                per ", 
                  code("capital.gain"), ", e circa il 95% per ", 
                  code("capital.loss"), " e pertanto si è deciso di creare due nuove 
                variabili categorizzando le vecchie in due sole categorie:"
                ),
                tags$ul(
                  tags$li(code("gain"), ": `No` se ", 
                          code("capital.gain"), " è 0 e `Yes` se è maggiore di 0;"),
                  tags$li(code("loss"), ": `No` se ", 
                          code("capital.loss"), " è 0 e `Yes` se è maggiore di 0;")
                ),
                width = 4
              ),
              box(
                plotlyOutput("new.gain.plot"),
                width = 4,
                status = "primary"
              ),
              box(
                plotlyOutput("new.loss.plot"),
                width = 4,
                status = "primary"
              )
            ),
            fluidRow(
              box(
                p("Infine, considerando la variabile", code("native.country"), 
                  "vista la poca numerosità (minore dell'1%) di quasi tutti gli stati, essi 
                sono stati raggruppati sotto la categoria `Other-countries`, eccetto
                `Mexico` (circa il 2%), gli `United-States` (circa il 90%) e la categoria
                `?` (circa il 2%) che verrà successivamente analizzata come dato mancante 
                (NA)."
                ),
                width = 4
              ),
              box(
                plotlyOutput("new.country.plot"),
                width = 8,
                status = "primary"
              )
            )
          ),
          tabItem(
            #### Page3 ####
            tabName = "page3",
            fluidRow(
              column(
                4,
                selectInput(
                  "work",
                  "Workclass:",
                  c("All",
                    unique(as.character(new_adult_data$workclass))))
              ),
              column(
                4,
                selectInput("race",
                            "Race:",
                            c("All",
                              unique(as.character(new_adult_data$race))))
              ),
              column(
                4,
                selectInput("rel",
                            "Relationship:",
                            c("All",
                              unique(as.character(new_adult_data$relationship))))
              )
            ),
            fluidRow(
              # Create a new row for the table.
              box(width = 12,
                  status = "primary",
                  div(style = 'overflow-x: scroll', 
                      DT::dataTableOutput('table')
                  )
              )
            )
          ),
          tabItem(
            #### Subpage1 ####
            tabName = "subpage1",
            fluidPage(
              sidebarPanel(
                width = 4,
                useShinyjs(),
                selectInput(
                  "Var.dens", 
                  "Scegli la variabile:", 
                  choices = names(new_adult_data),
                  selected = 1),
                prettyToggle(
                  inputId = "checktest1",
                  label_on = "Test Data",
                  label_off = "Training Data",
                  icon_on = icon("check"),
                  icon_off = icon("remove")
                ),
                prettyToggle(
                  inputId = "checkimp1",
                  label_on = "Imputed Data",
                  label_off = "NA Data",
                  icon_on = icon("check"),
                  icon_off = icon("remove")
                ),
                shinyjs::hidden(
                  prettyToggle(
                    inputId = "checkrate",
                    label_on = "Rate",
                    label_off = "Count",
                    icon_on = icon("check"),
                    icon_off = icon("remove")
                  )
                )
              ),
              box(plotlyOutput("density.plot"),
                  width = 8,
                  status = "primary")
            )
          ),
          tabItem(
            #### Subpage2 ####
            tabName = "subpage2",
            fluidRow(
              column(
                width = 4,
                sidebarPanel(
                  width = 12,
                  selectInput(
                    "Var1",
                    "Scegli la variabile 1:", 
                    choices=names(new_adult_data)),
                  selectInput(
                    "Var2", 
                    "Scegli la variabile 2:", 
                    choices = names(new_adult_data)),
                  prettyToggle(
                    inputId = "checktest2",
                    label_on = "Test Data",
                    label_off = "Training Data",
                    icon_on = icon("check"),
                    icon_off = icon("remove")
                  ),
                  prettyToggle(
                    inputId = "checkimp2",
                    label_on = "Imputed Data",
                    label_off = "NA Data",
                    icon_on = icon("check"),
                    icon_off = icon("remove")
                  )
                ),
                uiOutput("test2")
              ),
              box(plotlyOutput("two.plot"),
                  width = 8,
                  status = "primary")
            )
          ),
          tabItem(
            #### Subpage3 ####
            tabName = "subpage3",
            fluidRow(
              sidebarPanel(
                width = 3,
                selectInput(
                  "Var3.1",
                  "Scegli la variabile quantitativa (y):", 
                  choices=names(select_if(new_adult_data,is.integer))),
                selectInput(
                  "Var3.2", 
                  "Scegli la variabile esplicativa 1:", 
                  choices = names(new_adult_data)),
                selectInput(
                  "Var3.3", 
                  "Scegli la variabile esplicativa 2:", 
                  choices = names(new_adult_data)),
                prettyToggle(
                  inputId = "checktest3",
                  label_on = "Test Data",
                  label_off = "Training Data",
                  icon_on = icon("check"),
                  icon_off = icon("remove")
                ),
                prettyToggle(
                  inputId = "checkimp3",
                  label_on = "Imputed Data",
                  label_off = "NA Data",
                  icon_on = icon("check"),
                  icon_off = icon("remove")
                )
              ),
              box(plotlyOutput("three.plot"),
                  width = 9,
                  status = "primary")
            )
          )
        )
      )
    ),
    #### Footer ####
    tags$footer(
      tags$ul(
        id="foot",
        tags$li(tags$i("Powered by:")),
        tags$li(tags$a(href = "https://shiny.rstudio.com/",
                       tags$img(src = "http://hexb.in/hexagons/shiny.png"))),
        tags$li(tags$a(href = "https://ggplot2.tidyverse.org/",
                       tags$img(src = "http://hexb.in/hexagons/ggplot2.png"))),
        tags$li(tags$a(href = "https://dplyr.tidyverse.org/",
                       tags$img(src = "http://hexb.in/hexagons/dplyr.png"))),
        tags$li(tags$a(href = "https://magrittr.tidyverse.org/",
                       tags$img(src = "http://hexb.in/hexagons/pipe.png"))),
        tags$li(tags$a(href = "https://www.rstudio.com/",
                       tags$img(src = "http://hexb.in/hexagons/rstudio.png"))),
        tags$li(tags$a(href = "https://readr.tidyverse.org/",
                       tags$img(src = "http://hexb.in/hexagons/readr.png"))),
      align = "center"
    )
  )
)
)



