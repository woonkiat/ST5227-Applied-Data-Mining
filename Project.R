data("TitanicSurvival")
head(TitanicSurvival,3)
df <- na.omit(TitanicSurvival)
nrow(df)

df$is_male = (df$sex == "male") + 0
df$is_firstclass = (df$passengerClass == "1st") + 0
df$is_secondclass = (df$passengerClass == "2nd") + 0

test_index = seq(1,1045,5)
df2 = df[,c("survived","age","is_male","is_firstclass","is_secondclass")]


library(tree)
modelcart = tree(survived~. , df2[-test_index,])
plot(modelcart)
text(modelcart)

predcart = predict(modelcart, df2[test_index,], type="class")
errorcart = 1 - sum((df2[test_index,"survived"]==predcart) + 0)/length(df2[test_index,"survived"])
errorcart





library(kknn)

check_error = function(k) {
  
  errork <- rep(0,(nrow(df2[-test_index,]))) 
  
  for (i in 1:(nrow(df2[-test_index,]))){
    
    modelknn = kknn(survived~., df2[-test_index,][-i,], df2[-test_index,][i,], kernel="gaussian", distance=2, k = k)
    predknn = predict(modelknn)
    errork[i] = (df2[-test_index,"survived"][i] != predknn) + 0
  }
  
  meanerrork = mean(errork)
  return(meanerrork)
}


er = rep(0,300)
for (k in 1:300){
  er[k] = check_error(k)
}
plot(er, main = "CV error versus k", ylab = "CV error", xlab = "k" )
min(er)
best_k = which(er == min(er))[1]


modelknn = kknn(survived~., df2[-test_index,], df2[test_index,], kernel="gaussian", distance=2, k = best_k)
predknn = predict(modelknn)
errorknn = 1 - sum((df2[test_index,"survived"]==predknn) + 0)/length(df2[test_index,"survived"])
