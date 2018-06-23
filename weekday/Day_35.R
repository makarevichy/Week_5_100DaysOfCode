# Exercise 1
df <- data.frame(id = 1:2, q1 = c("A", "B"), q2 = c("C", "A"), stringsAsFactors = FALSE)
df
library(data.table)
library(reshape2)
df_melt <- melt(df, id = 'id', variable.name = 'question')
# Exercise 2
dcast(df_melt, id ~ question)
# Exercise 3
df_cast <- dcast(df_melt, question ~ id)
names(df_cast)[2:3] <- paste('id', c(1, 2), sep = '_')
# Exercise 4
df2 <- data.frame(
  A = c("A1", "A12", "A31", "A4"), 
  B = c("B4", "C7", "C3", "B9"), 
  C = c("C3", "B16", "B3", "C4")
)
setDT(df2)
df2_melt <- melt(df2[, id := .I], id.vars = "id")
dcast(df2_melt, id ~ substr(value, 1, 1))[, -c("id")]


# Exercise 5
df3 <- data.frame(
  Join_ID = rep(1:3, each = 2), 
  Type    = rep(c("a", "b"), 3), 
  v2      = c(8, 9, 7, 6, 5, 4)*10
)
dcast(df3, Join_ID ~ paste0(Type, '_v2'))

# Exercise 6
data('Fertility', package = 'AER')
head(Fertility)
Fertility$mother_id <- 1:nrow(Fertility)
df_ferl <- melt(Fertility, measure.vars = paste0("gender", 1:2), value.name = "gender", variable.name = "order")
df_ferl$order <- gsub("[a-z]", "", df_ferl$order)

# Exercise 7
d1 = data.frame(
  ID=c(1,1,1,2,2,4,1,2), 
  medication=c(1,2,3,1,2,7,2,8)
)
d1
setDT(d1)
d1[, .(medications = paste0(medication, collapse = ", ")), by = .(ID)]
# Exercise 8
dfs <- data.frame(
  Name = c(rep("name1",3),rep("name2",2)),
  MedName = c("atenolol 25mg","aspirin 81mg","sildenafil 100mg", "atenolol 50mg","enalapril 20mg")
)

setDT(dfs)
dfs[, medn := paste0("medication_", 1:.N), by = Name]
dfs
# Exercise 9
df7 <- data.frame(
  v1 = c("name1, name2", "name3", "name4, name5"),
  v2 = c("1, 2", "3", "4, 5"),
  v3 = c(1, 2, 3)
)
df7
setDT(df7)
df7[, lapply(.SD, tstrsplit, ", "), by = v3][, .(v1,v2,v3)]

# Exercise 10
df <- data.frame(
  Method = c("10.fold.CV Lasso", "10.fold.CV.1SE", "BIC", "Modified.BIC"),
  n      = c(30, 30, 50, 50, 50, 50, 100, 100),
  lambda = c(1, 3, 1, 2, 2, 0, 1, 2),
  df     = c(21, 17, 29, 26, 25, 32, 34, 32)
)

dcast(df, Method ~ n, fill = "")