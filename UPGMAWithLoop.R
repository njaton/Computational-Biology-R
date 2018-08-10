#Nick Jaton
#UPGMA w/Loop
#copyright @ 2018 Nicholas Jaton

#The first 2 sets merged will become MERGE
#The next merge will become MERGE2 etc

New_Set <- matrix( c(0,1,7,10,4,0,0,6,3,9,0,0,0,5,12,0,0,0,0,11,0,0,0,0,0), nrow=5, ncol=5, byrow=TRUE )
rownames(New_Set) <- c("A","B","C","D", "E")
colnames(New_Set) <- c("A","B","C","D", "E")

numrow <- 5

k = 0 #used to get letter
i = 0 #row val
j = 0 #col val

LVal <- numrow - 3
LETLIST <- list()
SC1 = matrix()

#-------------------------------------------------------------------------------------------
while(numrow > LVal) 
{
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

  numrow <- numrow - 1
} 
#--------------------------------------------------------------------------------------------
#Print final list, the list will print merge1, merge 2, etc not the initial values.
print(LETLIST)
