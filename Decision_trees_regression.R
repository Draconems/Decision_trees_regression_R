#Limpia el entorno de todas las variables

rm(list=ls()) 

#Inicio del programa

library(MASS)
library(tree)

data("Boston") #Housing Values in Suburbs of Boston.
attach(Boston)
summary(Boston)
head(Boston)

set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)  #Genera un vector de 253 elementos con valores entre el 6:504. 
test=-train

training_data=Boston[train,]
testing_data=Boston[test,]
testing_medv=medv[test]   #median value of owner-occupied homes in \$1000s.


#Ajuste del arbol en funcion de los datos de entrenamiento. 
tree_model=tree(medv~., training_data)
tree_model
plot(tree_model)
text(tree_model,pretty=0)

#Validar el modelo con la informacion de prueba. 
tree_pred=predict(tree_model, testing_data)
mean((tree_pred-testing_medv)^2)
#table(tree_pred, testing_medv)

# Cross validation for pruning the tree

cv_tree=cv.tree(tree_model)
plot(cv_tree$size, cv_tree$dev,type="b",xlab="Tree Size", ylab="MSE")

which.min(cv_tree$dev)
cv_tree$size[1]


#prune the tree to size 4

pruned_model=prune.tree(tree_model, best=4)
plot(pruned_model)
text(pruned_model,pretty=0)

#Check the accuracy of the model using testing data

tree_pred1=predict(pruned_model, testing_data)
mean((tree_pred1-testing_medv)^2)
