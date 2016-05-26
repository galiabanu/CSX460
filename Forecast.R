data("iris")
fit <- train(Spesies~., data=iris, method ="rf")
fit$finalModel  %>% plot()
varImp(fit)
varImp(fit)  %>% plot()
varImp(fit$finalModel)
randomForest::varUsed(fit$finalModel)

install.packages("fma")
library(fma)
plot(hsales)
data("hsales")
hsales %>% matrix
forecast(hsales)
forecast(hsales)  %>% plot()
forecast(hsales, 100)  %>% plot()
acf(hsales)
forecast(hsales)  %>% summary()




