#Quick t.test on ALL_AML_FS file for Computational Biology Course 
#This creates 1 column of t.test scores to be sorted in either R, Excel, etc.
#4/13/2018
#copyright @ 2018 Nicholas Jaton

Mydata <- as.data.frame(ALL_AML_FS_Annot) 

NewTable <- matrix(nrow = 2114, ncol = 1)
TTestScore = 0

#For loop to go through our test data
for(R in 1:2114){
  
  #Fill in NewTable with our t.test scores
  NewTable[R] <- t.test(as.numeric(Mydata[R,4:13]), as.numeric(Mydata[R,14:23]), paired = FALSE, var.equal = TRUE, conf.level = .95)$p.value
  
  #Check to see if we have the right score amount
  if (NewTable[R] <= .05)
    
  TTestScore = TTestScore + 1
  print(TTestScore)
  
}

#Create a txt file to be moved into Excel
write(NewTable, file = "CTABLE",
      ncolumns = 1,
      append = FALSE, sep = " ")
