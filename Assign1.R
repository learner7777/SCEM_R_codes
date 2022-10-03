#question 8 with git control 
install.packages("usethis")
library(usethis)
use_git_config(user.name="learner7777",user.email="sraghav2002@gmail.com")

#question 1 and 3 Creating a dataframe
#creating both the vectors first

animals <-c("snake","ostrich","cat","spider")
mode(animals)
num_legs <- c(0,2,4,8)
mode(num_legs)
animals_df=data.frame(animals,num_legs)


#question 2 Checking the environment and removing variables

ls() #prints all the variables which are present on the environment
rm(num_legs) #removes elements from environment mentioned as the parameter
rm(animals,animals_df)

#question 5 Matrix Opertaions

x_vect = seq(12,2,-2)
x=matrix(x_vect,2,3)
y=matrix(seq(4),2,2)
z=matrix(seq(4,10,2),2,2)
yt=t(y)
zt=t(z)

#Matrix addition
sum_yz=y+z
sum_zy=z+y

#matrix Multiplication
mul_yz=y %*% zt
mul_zy=z %*% yt

#matrix element wise multiplication
mul_ele_yz=y*z
mul_ele_zy=z*y

#matrix inv 
?solve
y_inv=solve(y)
mul_y_inv_y=y_inv %*% yt
mul_y_inv_x=y_inv %*% x


#Question 6 only numbers with divisiblity of 2,7,both

numAdder = function(number){
  num_by_2=seq(2,number-1,2)
  num_by_7=seq(7,number-1,7)
  master=unique(c(num_by_2,num_by_7))
  sumF=sum(master)
  return(sumF)
}
ans=numAdder(1000)
