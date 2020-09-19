#James Baker
#11/18/17
#Challenge Problem 1 Model Script

#Here's a function to parse the path data for the n best sensors
#Creates linear regression for x and y, lm.fitx for x and lm.fity for y
xyfit_from_path = function(path, n) {

	#n must be in range [1,3]
	#Up to 3 since thats the minimum number of sensors on in each row in
	#given training data
	if(n > 3) {
		print("Not useful n for training model, so not included")
		return
	} else if(n < 1) {
		print("Not a valid n, or some data rows have no sensor data")
		return
	} else {
		sMatrix = matrix(nrow = nrow(path), ncol = (n*2))
		sensors = as.matrix(path[,2:14])
	
		#Traverse through every row of the data
		for(row in 1:nrow(path)) {
			#Convert sensor data row from factor to numeric
			sList = suppressWarnings(as.numeric(sensors[row,]))
			#Find the n closest sensor indices
			minN.index = order(sList)[1:n]
			#Find the n closest sensors using the indices
			minN = as.numeric(sList[minN.index])
			
			#Place n smallest values and their 
			for(i in 1:n) {
				#For each row, remember the n closest sensor distances
				sMatrix[row,i] = minN[i]
					
				#Remember the number of each sensor
				sIndex = minN.index[i]
		
				#Store string of the n closest sensors as well
				sMatrix[row,i+n] = (dimnames(sensors)[[2]])[sIndex]
			}
		}
		#Make a data frame with just x and y 
		path.reg = path[,15:16]
	
		#Add the closest n predictors as well as their indicies to path.reg
		#Then do a linear regression for x and y
		#Must convert both the close sensors and their labels from character class
		if(n == 1) {
			path.reg$close1 = as.numeric(sMatrix[,1])
			path.reg$cLabel1 = factor(sMatrix[,2])
		}else if(n == 2) {
			path.reg$close1 = as.numeric(sMatrix[,1])
			path.reg$close2 = as.numeric(sMatrix[,2])
			path.reg$cLabel1 = factor(sMatrix[,3])
			path.reg$cLabel2 = factor(sMatrix[,4])
		}else {
			path.reg$close1 = as.numeric(sMatrix[,1])
			path.reg$close2 = as.numeric(sMatrix[,2])
			path.reg$close3 = as.numeric(sMatrix[,3])
			path.reg$cLabel1 = factor(sMatrix[,4])
			path.reg$cLabel2 = factor(sMatrix[,5])
			path.reg$cLabel3 = factor(sMatrix[,6])
		}
		#Return regression data frame
		return(path.reg)
	}	
}

#Load given data, assuming we are using the proper working directory
path1 = read.csv("path1_data.csv", header = T, na.strings="?")
path2 = read.csv("path2_data.csv", header = T, na.strings="?")
path3 = read.csv("path3_data.csv", header = T, na.strings="?")
path4 = read.csv("path4_data.csv", header = T, na.strings="?")
path5 = read.csv("path5_data.csv", header = T, na.strings="?")
path6 = read.csv("path6_data.csv", header = T, na.strings="?")
path7 = read.csv("path7_data.csv", header = T, na.strings="?")

#Concatenate the 7 training paths
myPaths = rbind(path1, path2, path3, path4, path5, path6, path7)

#For k-fold cross validation
library(boot)

#Though n = 2 does not produce the best Adj R^2 it gives the best tradeoff
#Of number of coefficients in the model and Adj R^2 
n = 2

myPaths.reg = xyfit_from_path(myPaths,n)

#Regress on the n closest sensors interacting with their respective labels
if(n == 1) {
	#For R^2 values in summary function
	lm.fitx = lm(x ~ close1+cLabel1, data=myPaths.reg)
	lm.fity = lm(y ~ close1+cLabel1, data=myPaths.reg)
	#For cv.glm
	glm.fitx = glm(x ~ close1+cLabel1, data=myPaths.reg)
	glm.fity = glm(y ~ close1+cLabel1, data=myPaths.reg)
} else if(n == 2) {
	lm.fitx = lm(x ~ close1+close2+cLabel1+cLabel2, data=myPaths.reg)
	lm.fity = lm(y ~ close1+close2+cLabel1+cLabel2, data=myPaths.reg)
	glm.fitx = glm(x ~ close1+close2+cLabel1+cLabel2, data=myPaths.reg)
	glm.fity = glm(y ~ close1+close2+cLabel1+cLabel2, data=myPaths.reg)
} else if(n == 3){
	lm.fitx = lm(x ~ close1++close2+close3+cLabel1+cLabel2+cLabel3, data=myPaths.reg)
	lm.fity = lm(y ~ close1++close2+close3+cLabel1+cLabel2+cLabel3, data=myPaths.reg)
	glm.fitx = glm(x ~ close1++close2+close3+cLabel1+cLabel2+cLabel3, data=myPaths.reg)
	glm.fity = glm(y ~ close1++close2+close3+cLabel1+cLabel2+cLabel3, data=myPaths.reg)
}

#Get summary with R^2 values
summary(lm.fitx)
summary(lm.fity)

#Get Test MSE for subset
cv.errors.x = cv.glm(myPaths.reg,glm.fitx,K=10)
cv.errors.x$delta
cv.errors.y = cv.glm(myPaths.reg,glm.fity,K=10)
cv.errors.y$delta
