library(colourpicker)

#' Title
#'
#' @param img an image as output by readPNG or readJPG
#' @param n_rangs number of rows of the pattern
#' @param centers if NULL, no clustering of data ; if a single number, the number of colors k to use ; if a vector of colors, the centers of the clusters (similar as centers arguents of kmeans)
#'
#' @returns a list with 2 elements: cols=matrix of n-rangs rows (and number of columns depending on the aspect ratio of the image), and R2=the ratio of between_SS / total_SS (if !is.null(centers))
#' @export
#'
#' @examples
brick_stitch_horizontal <- function(img, n_rangs, centers=NULL) {
  
  H <- dim(img)[1]
  W <- dim(img)[2]
  ratio <- W / H
  n_cols <- round(n_rangs * ratio)
  
  cell_h <- H / n_rangs
  cell_w <- W / n_cols
  
  mean_rgb <- function(r1, r2, c1, c2) {
    block <- img[
      max(1, r1):min(H, r2),
      max(1, c1):min(W, c2),
      ,
      drop = FALSE
    ]
    apply(block, 3, mean)
  }
  
  cols <- matrix(NA, n_rangs, n_cols)
  
  for (i in 1:n_rangs) {
    shift <- ifelse(i %% 2 == 0, 0.5 * cell_w, 0)
    
    for (j in 1:n_cols) {
      r1 <- floor((i - 1) * cell_h) + 1
      r2 <- floor(i * cell_h)
      c1 <- floor((j - 1) * cell_w + shift) + 1
      c2 <- floor(j * cell_w + shift)
      
      m <- mean_rgb(r1, r2, c1, c2)
      cols[i, j] <- rgb(m[1], m[2], m[3])
    }
  }
  if(!is.null(centers)){
    print("clustering")
    matrice<-col2rgb(cols)
    (cl <- kmeans(t(matrice), centers))
    couleurs<-cl$centers
    couleurs<-rgb(couleurs[,1],couleurs[,2],couleurs[,3], maxColorValue=255)
    vecteur<-cl$cluster
    vecteur<-couleurs[vecteur]
    cols<-matrix(vecteur, nrow=n_rangs)
  } else{ cl<-list(betweenss=NA,totss=NA)}
  result<-list(cols=cols, R2=cl$betweenss/cl$totss)
  return(result)
}

plot_brick_to_png <- function(cols, file=NULL, bead_size = 20) {
  
  nr <- nrow(cols)
  nc <- ncol(cols)
  
  width  <- (nc + 1) * bead_size
  height <- nr * bead_size
  
  functionplot<-function(){
    par(mar = c(0, 0, 0, 0))
    
    plot(
      NA, NA,
      xlim = c(-0.5, nc + 0.5),
      ylim = c(0, nr),
      asp = 1,
      axes = FALSE,
      xlab = "", ylab = ""
    )
    
    for (i in 1:nr) {
      dx <- ifelse(i %% 2 == 0, 0.5, 0)
      
      for (j in 1:nc) {
        rect(
          j - 1 + dx,
          nr - i,
          j + dx,
          nr - i + 1,
          col = cols[i, j],
          border = "black"
        )
      }
    }
  }
  
  if(!is.null(file)) {
    png(file, width = width, height = height, bg = "white")
    functionplot()
    dev.off()
  } else {
    functionplot()
  }
}


plotGrid <- function(pattern, couleurs, selectedCells, gridType) {
  nr <- nrow(pattern)
  nc <- ncol(pattern)
  
  par(mar = c(1, 1, 1, 1))
  plot(
    NA, NA,
    xlim = c(-0.5, nc + 1),
    ylim = c(0, nr),
    asp = 1,
    axes = FALSE
  )
  
  for (i in seq_len(nr)) {
    dx <- if (gridType == "offset" && i %% 2 == 0) 0.5 else 0
    
    for (j in seq_len(nc)) {
      sel <- any(
        selectedCells[,1] == i &
          selectedCells[,2] == j
      )
      
      rect(
        j - 1 + dx,
        nr - i,
        j + dx,
        nr - i + 1,
        col = couleurs[pattern[i, j]],
        border = if (sel) "yellow" else "black",
        lwd = if (sel) 2 else 1
      )
    }
  }
}

coordsToCell <- function(x, y, nr, nc, gridType) {
  
  # ---- 1) Calcul rangée ----
  i <- floor(nr - y) + 1
  
  if (i < 1 || i > nr) return(NULL)
  
  # ---- 2) Décalage brickstitch ----
  dx <- 0
  if (gridType == "offset") {
    dx <- ifelse(i %% 2 == 0, 0.5, 0)
  }
  
  # ---- 3) Correction coordonnée x ----
  x_adj <- x - dx
  
  # ---- 4) Colonne ----
  j <- floor(x_adj) + 1
  
  if (j < 1 || j > nc) return(NULL)
  
  c(i, j)
}

inverse_selection <- function(old, new) {
  
  if (is.null(old) || nrow(old) == 0)
    return(unique(new))
  
  if (is.null(new) || nrow(new) == 0)
    return(old)
  
  old_key <- paste(old[,1], old[,2])
  new_key <- paste(new[,1], new[,2])
  
  # Intersection
  inter_key <- intersect(old_key, new_key)
  
  # XOR
  res_key <- setdiff(c(old_key, new_key), inter_key)
  
  res <- do.call(
    rbind,
    strsplit(res_key, " ")
  )
  
  matrix(as.numeric(res), ncol = 2)
}


plotLegend <- function(colors, labels, selectedColor) {
  n <- length(colors)
  par(mar = c(1, 1, 1, 6))
  plot(
    NA, NA,
    xlim = c(0, 2),
    ylim = c(0, n),
    axes = FALSE
  )
  
  for (i in seq_len(n)) {
    sel <- !is.null(selectedColor)
    if (sel) sel<- selectedColor==i
    rect(
      0, n - i,
      1, n - i + 1,
      col = colors[i],
      border = if (sel) "yellow" else "black",
      lwd = if (sel) 2 else 1
    )
    text(1.1, n - i + 0.5, labels[i], adj = 0)
  }
}

