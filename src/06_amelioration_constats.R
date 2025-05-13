# Constat sur l'inflation de la variance

library(car)

vif(lm(FC ~ ff100_1 + ff100_2 + ff100_3 + ff100_4 + ff100_5 + ff100_6 +
         ff100_7 + ff100_8 + ff100_9 + ff100_10, data = df_input))

# Matrice de corrélation linéaire

cor_matrix <- cor(df_input[, c("ff100_1", "ff100_2", "ff100_3", "ff100_4", "ff100_5",
                               "ff100_6", "ff100_7", "ff100_8", "ff100_9", "ff100_10", "time_std")])
print(cor_matrix)

# Affichage de la matrice sous forme de heatmap

library(reshape2)

cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(Var1, Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(mid = "white", low = "blue", high = "red", midpoint = 0) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Matrice de Corrélation des Variables")
