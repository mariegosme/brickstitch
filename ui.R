ui <- tabsetPanel(
  tabPanel("Automatic pattern from image",   sidebarLayout(
    sidebarPanel(
      fileInput(
        "image",
        "Choisir une image (PNG ou JPG)",
        accept = c("image/png", "image/jpeg")
      ),
      
      sliderInput(
        "n_rangs",
        "Nombre de rangs",
        min = 10,
        max = 200,
        value = 50
      ),
      
      checkboxInput(
        "use_kmeans",
        "Réduire le nombre de couleurs",
        value = FALSE
      ),
      
      conditionalPanel(
        condition = "input.use_kmeans == true",
        numericInput(
          "k_colors",
          "Nombre de couleurs",
          value = 10,
          min = 2,
          max = 50
        )
      ),
      
      actionButton("go", "Générer le pattern"),
      downloadButton("savePattern1", "Enregistrer le pattern")
    ),
    
    mainPanel(
      plotOutput("pattern_plot", height = "600px"),
      verbatimTextOutput("info")
    )
  )), #end tabpanel image
  tabPanel("Improve pattern manually", fluidPage(
    fluidRow(
      column(
        width = 2,
        h4("Options"),
        radioButtons(
          "gridType", "Type de grille",
          choices = c("Alignée" = "aligned", "Décalée" = "offset")
        ),
        numericInput("nr", "Nombre de rangs", 10, min = 1),
        numericInput("nc", "Nombre de colonnes", 10, min = 1),
        actionButton("newGrid", "Nouvelle grille"),
        hr(),
        downloadButton("savePattern", "Enregistrer (.txt)"),
        fileInput("loadPattern", "Charger un pattern")
      ),
      
      column(
        width = 7,
        h3("sélectionnez des cellules puis cliquez sur une couleur, ou sélectionnez une couleur puis cliquez sur des cellules"),
        h4("(pour déselectionner, cliquez juste en dehors de la grille)"),
        plotOutput(
          "gridPlot",
          click = "gridClick",
          brush = brushOpts(
            id = "gridBrush",
            resetOnNew = TRUE,
            direction = "xy", 
            delay = 5000
          ),
          dblclick = "gridDblClick"
        ),
        actionButton("clearBrush", "nettoyer"),
        textOutput(outputId="selectedcells")
      ),
      
      column(
        width = 3,
        plotOutput(
          "legendPlot",
          click = "legendClick",
          dblclick = "legendDblClick"
        ),
        actionButton("addColor", "Ajouter une couleur"),
        textOutput(outputId="colorList"),
        textOutput(outputId="selectedColor")
      )
    )
  )
)#end tabpanel manuel
)


