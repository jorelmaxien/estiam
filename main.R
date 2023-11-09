library(ggplot2)
library(reshape2)

trouver_solutions <- function(x, y) {
  solutions <- 0
  all_solutions <- list()
  
  est_valide <- function(placement, colonne, ligne) {
    i <- 1
    while (i < colonne) {
      if (placement[i] == ligne || 
          placement[i] - i == ligne - colonne || 
          placement[i] + i == ligne + colonne ||
          abs(placement[i] - ligne) == abs(i - colonne)) { # Menace diagonale
        return(FALSE)
      }
      i <- i + 1
    }
    return(TRUE)
  }
  
  placer_dames <- function(placement, colonne = 1) {
    if (colonne > x) {
      solutions <<- solutions + 1
      all_solutions[[solutions]] <<- placement
    } else {
      ligne <- 1
      while (ligne <= y) {
        if (est_valide(placement, colonne, ligne)) {
          placement[colonne] <- ligne
          placer_dames(placement, colonne + 1)
        }
        ligne <- ligne + 1
      }
    }
  }
  
  placement_initial <- integer(x)
  placer_dames(placement_initial)
  
  cat("Nombre de solutions trouvées:", solutions, "\n")
  return(all_solutions)
}

# Utilisation de la fonction avec le nombre de lignes (x) et de colonnes (y) de votre choix
resultats <- trouver_solutions(8, 8)  # Par exemple, 8 lignes et 8 colonnes

# Création d'une heatmap avec ggplot2 (utiliser les résultats obtenus)
heatmap_data <- matrix(0, nrow = 8, ncol = 8)
i <- 1
while (i <= length(resultats)) {
  positions <- resultats[[i]]
  j <- 1
  while (j <= length(positions)) {
    heatmap_data[j, positions[j]] <- heatmap_data[j, positions[j]] + 1
    j <- j + 1
  }
  i <- i + 1
}

heatmap_data_melted <- melt(heatmap_data)

ggplot(heatmap_data_melted, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap des cases les plus touchées par les dames",
       x = "Colonnes",
       y = "Lignes")

