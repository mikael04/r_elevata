##Isso aqui tudo é pra pegar o top 5 (antes top10, mas aparecia uma categoria que não queríamos), substituir os que não estão por -Outros e depois refazer a média
top5_fat <- head.matrix(pr_top5_fat, n=5)
top5_fat_ij_cat <- inner_join(top5_fat, categoria, by = c("produto_categoria_id"="categoria_id"))
top5_fat_ij_cat <- top5_fat_ij_cat[, -1]
top5_fat_ij_cat <- top5_fat_ij_cat[, -2:-4]
top5_fat_ij_cat <- as.data.frame(top5_fat_ij_cat)
