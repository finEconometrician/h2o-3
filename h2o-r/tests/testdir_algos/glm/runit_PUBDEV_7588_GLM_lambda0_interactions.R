setwd(normalizePath(dirname(R.utils::commandArgs(asValues=TRUE)$"f")))
source("../../../scripts/h2o-r-test-setup.R")

test.glm.interactions.lambda0 <- function() {
   data <- h2o.importFile("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/adult.csv")
   temp <- data["hours-per-week"] / data["age"]
   names(temp) <- "y"
   dataCombined <- h2o.cbind(data, temp)
   dataR <- as.data.frame(dataCombined)

   glmH2O <- h2o.glm(x=c("income", "gender"), y="y", training_frame=dataCombined, interactions=c("income", "gender"), lambda=0)
   glmR <- lm(y ~income*gender+income+gender, data=dataR)
   expect_true(length(glmR$coefficients)==length(glmH2O@model$coefficients), "number of coeffcients between R and H2O are different")
}

doTest("Testing model interactions for GLM when Lambda=0.0", test.glm.interactions.lambda0)
