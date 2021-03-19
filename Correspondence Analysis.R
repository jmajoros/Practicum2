
# METHOD FOR CORRESPONDENCE PLOTS
# model the contingency tables like this
P2$y<-factor(P2$y)
P2$y.n<-unclass(P2$y)-1
m.ct = glm(y.n ~ job+education, family=poisson, 
         data = P2)  
#this is best for things with multiple factors 
#where we think some things might be correlated.

z = xtabs(residuals(m.ct,type="pearson")~job+education,
          data = P2)  
z #this shows us the residuals for each value.  Clearly, these variables are 
#NOT independent since we have residuals that are extremely high.  This means 
#that education and job are highly dependent for yeses.

num = 2  #here we are capturing the single value decomposition matrix
X = svd(z, nu = num, nv = num)
leftsv = X$u %*% diag(sqrt(X$d[1:num]))
rightsv = X$v %*% diag(sqrt(X$d[1:num]))
X$d #this is the diagonal, we want most of the 
#info to be captured in the first two terms for 
#this to be a valid plot
#meaning, we want the first two numbers to be 
#higher than the others generally, since this accounts
#for the info caputred by them.  Here we have 
# 157 and 12, then 6.7 and a very mall number.  
#so the plot should be fine.

# plot
plot(rbind(leftsv,rightsv),
     xlim=c(-3,3), ylim = c(-3, 3), #these will work well for things that are close to being modeled well
     #but will need to be adjusted otherwise.
     xlab="SV1",ylab="SV2",type = "n")
abline(h=0,v=0)
text(leftsv,dimnames(z)[[1]], col="blue")
text(rightsv,dimnames(z)[[2]], col = "red")

# Correspondence analysis key observations:
# (Recall the locations are based on the SVD of pearson residuals)
# Comparison can be made on one or both axis. Horizontal axis is more relevant
# 1. Location of one or more factor levels of a single factor (regressor)
# 1a. Factor levels near the origin (in either axis) follow the overall pattern of the data
# 1b. Levels far from the origin do not follow overall pattern of the data
# 1c. Two levels located near each other (both coordinates) may be merged together
# 2. Relative location of a factor level of one factor versus
#    factor level of the other factor
# 2a. If they are close to each other and away from the origin, positively associated
# 2b. If they are on opposite locations (away from the origin), negatively associated

#plot w/adjusted axes
plot(rbind(leftsv,rightsv),
     xlim=c(-12,12), ylim = c(-12, 12), #these will work well for things that are close to being modeled well
     #but will need to be adjusted otherwise.
     xlab="SV1",ylab="SV2",type = "n")
abline(h=0,v=0)
text(leftsv,dimnames(z)[[1]], col="blue")
text(rightsv,dimnames(z)[[2]], col = "red")

#SV1 captures MOST of the info.  So we focus there.  
#Not a lot that is useful here.  
m.ct = glm(y.n ~ job+marital, family=poisson, 
           data = P2)  
#this is best for things with multiple factors 
#where we think some things might be correlated.

z = xtabs(residuals(m.ct,type="pearson")~job+marital,
          data = P2)  
z #this shows us the residuals for each value.  Clearly, these variables are 
#NOT independent since we have residuals that are extremely high.  This means 
#that education and job are highly dependent for yeses.

num = 2  #here we are capturing the single value decomposition matrix
X = svd(z, nu = num, nv = num)
leftsv = X$u %*% diag(sqrt(X$d[1:num]))
rightsv = X$v %*% diag(sqrt(X$d[1:num]))
X$d #this is the diagonal, we want most of the 
#info to be captured in the first two terms for 
#this to be a valid plot
#meaning, we want the first two numbers to be 
#higher than the others generally, since this accounts
#for the info caputred by them.  Here we have 
# 157 and 12, then 6.7 and a very mall number.  
#so the plot should be fine.

# plot
plot(rbind(leftsv,rightsv),
     xlim=c(-10,10), ylim = c(-10, 10), #these will work well for things that are close to being modeled well
     #but will need to be adjusted otherwise.
     xlab="SV1",ylab="SV2",type = "n")
abline(h=0,v=0)
text(leftsv,dimnames(z)[[1]], col="blue")
text(rightsv,dimnames(z)[[2]], col = "red")

#So for instance here, we see that being married and blue-collar/self-employed 
#has a very big negative correlation for our yeses.  
#Not super helpful, but something.  It reinforces the findings about being 
#single being important.
