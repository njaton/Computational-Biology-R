#UPGMA program 
#Nick Jaton 

#-------------------------------------------------------------------------------------

#Using the phangorn package (skip to line 18 for longer way of doing UPGMA)

#Package Location -> https://cran.r-project.org/web/packages/phangorn/index.html
#library(phangorn)
#New_Set <- matrix( c(9,3,7,10,2,9,1,9,4,6,5,5,1,10,3), nrow=5, ncol=3, byrow=TRUE )
#rownames(New_Set) <- c("A","B","C","D","E")
#test <- dist(New_Set)
#x <- upgma(test)
#plot(x)

#-------------------------------------------------------------------------------------

#Not using the phangorn package

#The first 2 sets merged will become MERGE
#The next merge will become MERGE2 etc

New_Set <- matrix( c(0,1,7,10,4,0,0,6,3,9,0,0,0,5,12,0,0,0,0,11,0,0,0,0,0), nrow=5, ncol=5, byrow=TRUE )
rownames(New_Set) <- c("A","B","C","D", "E")
colnames(New_Set) <- c("A","B","C","D", "E")

k = 0 #used to get letter
i = 0 #row val
j = 0 #col val

LETLIST <- list()
SC1 = matrix()

#-------------------------------------------------------------------------------------------

SC1 <- which(New_Set == min(New_Set [New_Set >= 1]), arr.ind = TRUE)
#This basically just allows us to choose the first "lowest value" not all.
print(SC1)

i <- SC1[1]
j <- SC1[2]

print(i)
print(j)

#At this point I am getting the letter associated by each row
k1 <- arrayInd(i, dim(New_Set))
R1 <-rownames(New_Set)[k1[,1]]
k2 <- arrayInd(j, dim(New_Set))
R2 <-rownames(New_Set)[k2[,1]]
print(R1)
print(R2)

#Create a list to start organizing row names 
TMP <- list(R1,R2)
LETLIST[[i]] <- TMP

# we need to find how to combine each cell that is is i & j and create a new column from it. 
New_Set <- cbind(New_Set, MERGE1 = 0)
New_Set <- rbind(New_Set, MERGE1 = (New_Set[i,]+New_Set[j,])/2)
print(New_Set)

#remove used rows
New_Set <- New_Set[-i:-j,-i:-j]
print(New_Set)

#Second Round
#--------------------------------------------------------------------------------------------
SC1 <- which(New_Set == min(New_Set [New_Set >= 1]), arr.ind = TRUE)
#This basically just allows us to choose the first "lowest value" not all.
print(SC1)

i2 <- SC1[1]
j2 <- SC1[2]

print(i2)
print(j2)

#At this point I am getting the letter associated by each row
k1 <- arrayInd(i2, dim(New_Set))
R1 <-rownames(New_Set)[k1[,1]]
k2 <- arrayInd(j2, dim(New_Set))
R2 <-rownames(New_Set)[k2[,1]]

print(R1)
print(R2)

#Create a list to start organizing row names 
TMP <- list(R1,R2)
LETLIST[[i2]] <- TMP

# we need to find how to combine each cell that is is i & j and create a new column from it. 
New_Set <- cbind(New_Set, MERGE2 = 0)
New_Set <- rbind(New_Set, MERGE2 = (New_Set[i2,]+New_Set[j2,])/2)
print(New_Set)

#remove used rows
New_Set <- New_Set[-i2:-j2,-i2:-j2]
print(New_Set)
#Round 3
#-------------------------------------------------------------------------------------------
SC1 <- which(New_Set == min(New_Set [New_Set >= 1]), arr.ind = TRUE)
#This basically just allows us to choose the first "lowest value" not all.
print(SC1)

i3 <- SC1[1]
j3 <- SC1[2]

print(i3)
print(j3)

#At this point I am getting the letter associated by each row
k1 <- arrayInd(i3, dim(New_Set))
R1 <-rownames(New_Set)[k1[,1]]
k2 <- arrayInd(j3, dim(New_Set))
R2 <-rownames(New_Set)[k2[,1]]

print(R1)
print(R2)

#Create a list to start organizing row names 
TMP <- list(R1,R2)
LETLIST[[i3]] <- TMP

# we need to find how to combine each cell that is is i & j and create a new column from it. 
New_Set <- cbind(New_Set, MERGE3 = 0)
New_Set <- rbind(New_Set, MERGE3 = (New_Set[i3,]+New_Set[j3,])/2)
print(New_Set)

#remove used rows
New_Set <- New_Set[-i3,-i3]
New_Set <- New_Set[-j3,-j3]
print(New_Set)
#-------------------------------------------------------------------------------------------
print(LETLIST)
