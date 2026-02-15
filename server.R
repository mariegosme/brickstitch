library(shiny)

function(input, output, session) {
  #### tab1 ####
  
  # lecture de l'image
  img <- reactive({
    req(input$image)
    ext <- tools::file_ext(input$image$name)
    
    if (ext == "png") {
      png::readPNG(input$image$datapath)
    } else {
      jpeg::readJPEG(input$image$datapath)
    }
  })
  
  # génération du pattern
  pattern1 <- eventReactive(input$go, {
    
    centers <- NULL
    if (input$use_kmeans) {
      centers <- input$k_colors
    }
    
    brick_stitch_horizontal(
      img = img(),
      n_rangs = input$n_rangs,
      centers = centers
    )
  })
  
  # visualisation
  output$pattern_plot <- renderPlot({
    req(pattern1())
    plot_brick_to_png(pattern1()$cols)
  })
  
  output$info <- renderPrint({
    req(pattern1())
    if (!is.null(pattern1()$R2)) {
      cat("Qualité du clustering (R²) :", round(pattern1()$R2, 3))
    }
  })
  
  output$savePattern1<-downloadHandler(
    filename = function() {
      paste0("my_data_", Sys.Date(), ".txt")
    },
    content = function(file) {
    req(pattern1())
    toto<-pattern1()
    print(str(toto))
    fact<-factor(toto$cols)
    listecol<-levels(fact)
    pat<-matrix(as.integer(fact), nrow=nrow(toto$cols), ncol=ncol(toto$cols))
    write.table(as.data.frame(matrix(listecol, nrow=1)), file, row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t")
    write.table(as.data.frame(matrix(listecol, nrow=1)), file, append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t")
    write.table(pat, file, append=TRUE, row.names=FALSE,col.names=FALSE,sep="\t")
  })
  
  #### tab2 ####
  rv <- reactiveValues(
    pattern = NULL,           # matrice nr x nc (indices couleurs)
    colorList = c("#FFFFFF"), # index 1 = blanc par défaut
    colorLabels = c("#FFFFFF"),
    selectedCells = matrix(ncol = 2, nrow = 0),
    selectedColor = NULL,
    editLabelIdx=NULL,
    editColIdx = NULL,
    brushTrigger = 0,
    history = list()          # pour CTRL+Z
  )
  
  observeEvent(input$newGrid, {
    rv$pattern <- matrix(
      1,
      nrow = input$nr,
      ncol = input$nc
    )
    rv$selectedCells <- matrix(ncol = 2, nrow = 0)
    rv$selectedColor <- NULL
  })
  
  output$gridPlot <- renderPlot({
    rv$brushTrigger   # dépendance forcée
    req(rv$pattern)
    plotGrid(
      rv$pattern,
      rv$colorList,
      rv$selectedCells,
      input$gridType
    )
  })
  
  output$selectedcells<-renderText(paste("selected=,", paste(t(rv$selectedCells), collapse=",")))
  
  
  observeEvent(input$gridClick, {
    req(rv$pattern)
    
    cell <- coordsToCell(
      input$gridClick$x,
      input$gridClick$y,
      nrow(rv$pattern),
      ncol(rv$pattern),
      input$gridType
    )
    
    if (is.null(cell)) {
      rv$selectedCells <- matrix(ncol = 2, nrow = 0)
      return()
    }
    
    idx <- which(
      rv$selectedCells[,1] == cell[1] &
        rv$selectedCells[,2] == cell[2]
    )
    
    if(length(rv$selectedColor)==0){ #selection/deselection
      if (length(idx)) {
        rv$selectedCells <- rv$selectedCells[-idx, , drop = FALSE]
      } else {
        rv$selectedCells <- rbind(rv$selectedCells, cell)
      }
    } else { #attribution couleur
      rv$pattern[cell[1],cell[2]]<-rv$selectedColor
    }
    
  })
  
  
  brush <- NULL
  makeReactiveBinding("brush")
  
  observeEvent(input$gridBrush, {
    brush <<- input$gridBrush
  })
  
  observeEvent(input$clearBrush, {
    print("clear brush traces")
    session$resetBrush("gridBrush")
    rv$brushTrigger <- rv$brushTrigger + 1
  })
  
  #brushReactive <- reactive(input$gridBrush) |> debounce(150)
  #observeEvent(input$gridBrush, {
  #observeEvent(brushReactive(), {
  observeEvent(brush, {
    req(rv$pattern)
    #b <- input$gridBrush
    b<-brush
    if (is.null(b)) return()
    
    nr <- nrow(rv$pattern)
    
    cells <- expand.grid(
      i = seq_len(nr),
      j = seq_len(ncol(rv$pattern))
    )
    
    keep <- with(cells, {
      if (identical(rv$gridType , "offset")) dx <-  ifelse(i %% 2 == 0, 0.5,0) else dx <- 0
      x <- j - 0.5 + dx
      y <- nr - i + 0.5
      x >= b$xmin & x <= b$xmax &
        y >= b$ymin & y <= b$ymax
    })
    
    if(length(rv$selectedColor)==0){ #selection/deselection
      newCells <- as.matrix(cells[keep, ])
      # Inversion cumulative
      rv$selectedCells <- unique(inverse_selection(
        rv$selectedCells,
        newCells
      ))
    } else { #attribution couleur
      toto <- cells[keep, ]
      for(i in 1:nrow(toto)){
        x<-toto[i,1]
        y<-toto[i,2]
        rv$pattern[x,y]<-rv$selectedColor
      }
    }
    
    session$onFlushed(function() {
      session$resetBrush("gridBrush")
    }, once = TRUE)
    
  })
  
  output$legendPlot <- renderPlot({
    plotLegend(
      colors=rv$colorList, 
      labels=rv$colorLabels, 
      selectedColor=rv$selectedColor)
  })
  
  observeEvent(input$legendClick, {
    y <- input$legendClick$y
    idx <- floor(max(seq_along(rv$colorList)) - y) + 1
    
    if (idx < 1 || idx > length(rv$colorList)) {
      rv$selectedColor <- NULL
      return()
    }
    
    if (nrow(rv$selectedCells) > 0) { #si des cellules sont selectionnees, on leur attribue la couleur puis on les deselectionne
      rv$pattern[rv$selectedCells] <- idx
      rv$selectedCells<-matrix(ncol = 2, nrow = 0)
    } else { #si pas de cellule selectonnee, on selectionne/deselectionne la legende
      #si deja selectionne, deselectionne, sinon selectionne
      sel <- !is.null(rv$selectedColor)
      if (sel) sel<- rv$selectedColor==idx
      rv$selectedColor <- if (sel) NULL else idx
    }
  })
  
  observeEvent(input$legendDblClick, {
    y <- input$legendDblClick$y
    x <-  input$legendDblClick$x
    n <- length(rv$colorList)
    idx <- n - floor(y)
    
    if (x<1){ #clique sur couleur => change couleur
      rv$editColIdx <- idx
      showModal(modalDialog(
        colourInput("editCol","Changer de couleur",
                    value = rv$colorList[idx]),
        footer = tagList(
          actionButton("confirmEdit","OK")
        )
      ))
    } else { #click nom legende
      rv$editLabelIdx <- idx 
      showModal(modalDialog(
        textInput("editLab","Nommer cette zone",
                  value = rv$colorLabels[idx]),
        footer = tagList(
          actionButton("confirmEditLab","OK")
        )
      ))
      
    }
  })

observeEvent(input$confirmEdit, {
  req(rv$editColIdx)
  idx<-rv$editColIdx
  print(paste("editing color", idx))
  rv$colorList[idx] <- input$editCol
  rv$editColIdx <- NULL
  removeModal()
})
  
  observeEvent(input$confirmEditLab, {
    req(rv$editLabelIdx)
    idx <- rv$editLabelIdx
    print(paste("editing color name", idx, "qui vaut", rv$colorLabels[idx]))
    toto<-rv$colorLabels
    toto[idx]<-input$editLab
    print(toto)
    rv$colorLabels <- toto
    rv$editLabelIdx <- NULL
    removeModal()
  })
  
  observeEvent(input$addColor, {
    showModal(
      modalDialog(
        colourInput("newCol", "Choisir une couleur"),
        easyClose = TRUE,
        footer = actionButton("confirmAddColor", "Ajouter")
      )
    )
  })
  
  observeEvent(input$confirmAddColor, {
    rv$colorList <- c(rv$colorList, input$newCol)
    rv$colorLabels <- c(rv$colorLabels, input$newCol)
    removeModal()
  })
  
  output$colorList<-renderText(paste(paste(rv$colorLabels, rv$colorList, sep="="), collapse=","))
  output$selectedColor<-renderText(paste("selected=,", paste(rv$selectedColor, collapse=",")))
  
  
  output$savePattern<-downloadHandler(
    filename = function() {
      paste0("pattern_", Sys.time(), ".txt")
    },
    content = function(file) {
      print("save pattern onglet 2")
      req(rv$pattern)
      req(rv$colorList)
      req(rv$colorLabels)
      pat<-rv$pattern
      listecol<-rv$colorList
      names(listecol)<-rv$colorLabels
      print(listecol)
      write.table(as.data.frame(matrix(names(listecol), nrow=1)), file, row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t")
      write.table(as.data.frame(matrix(listecol, nrow=1)), file, append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t")
      write.table(pat, file, append=TRUE, row.names=FALSE,col.names=FALSE,sep="\t")
    })

  observeEvent(input$loadPattern, {
    
    f <- input$loadPattern$datapath
    lines <- readLines(f, n=2)
    labels<-unlist(strsplit(lines[1], split="\t"))
    colors<-unlist(strsplit(lines[2], split="\t"))
    pattern<-as.matrix(read.table(f, sep="\t", skip=2))
    ncol <- length(strsplit(lines[1]," ")[[1]])
    rv$pattern<-pattern
    rv$colorList <- colors
    rv$colorLabels <- labels
    rv$selectedCells <- matrix(ncol = 2, nrow = 0)
    rv$selectedColor <- NULL
    rv$history <- list()          # pour CTRL+Z
    
  })
  
  
}