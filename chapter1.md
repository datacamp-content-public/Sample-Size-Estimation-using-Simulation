---
title: Template Chapter 1
description: >-
  This is a template chapter.


---
## test.exercise1

```yaml
type: NormalExercise
lang: r
xp: 100
skills: 1
key: 6ee7538ff3
```

Do some data science.

`@instructions`


`@hint`



`@sample_code`
```{r}
# 1) Set Parameters

  sample.size <- c(50, 100, 500, dim(Sylvan)[1])
  effect.size <- c(.1, .2, .3)*7.2 # multiply by the SD (7.2%) to 
                                   # get percent absence differences 
  n.itterations <- 500 # number of times the simulation will run
  prop.treat <- .5 # a single value representing the proportion of students in the
                   # sample who are to recieve the treatment 

  total.n <- dim(Sylvan)[1] # total number of students

# 2) Create Results Matrix
# create empty matrix to store results in. Initially, the data frame has
# one row, with a column for each of the data points we are going to output.  
# In this case, the final results matrix will have one row for each 
# itteration of the simulation, and one column for each parameter, 
# and one column for each output value from the simulation.
  


# 3) Run Simulation
# For each parameter that varies, we include a new loop that loops
# over the number of options for that parameter. Because the values 
# of each parameter are defined as a vector, we can use the length() 
# function calculate the number of values for each parameter to make 
# our simulation more flexible. 

for(k in 1:length(sample.size)){
  for(j in 1:length(effect.size)){
    for(i in 1:n.itterations){
      
      # a) Select a subset of student to participate (based on 
      #    sample size)
      
  
      
      # b) Assign students to treatment (Create "T.Status" variable)

     
      # c) assign Outcome to treated cases 
      #   (create psudo outcome variable based on prior year data)
      #   The "outcome" for students in the control group is the middle school
      #   % absent
      #   The "outcome for the students in the treatmetn group is the middle school
      #   % absent + effect size
  
      
      # d) Run Model outcome~treatment assignment
 
      
      # e) Extract key variables (Treatment coefficient, Treatment SE,
      #   degrees of freedom, p value, N with data, 
      #   and whether the model made a type II error)
   
      
      # f) store results for ONLY this itteration of the simulation (single row)
        
     
        
      # g) append results from THIS SIMULATION to the RESULTS MATRIX

      
      
      
      # h) clean up
      #   remove generated variables to avoid accidentally putting 
      #   incorrect data in the results matrix in future itterations
      


# 4) Summarize results for each combination of parameters
# Average the number of type II errors made over the 
# input values for proportion treated, n size, and effect size
  

# 5) Calculate the power for each combination of parameters
  


# view summary of results

 


# 6) Create Power Plot
```
`@solution`
```{r}
# 1) Set Parameters

  sample.size <- c(50, 100, 500, dim(Sylvan)[1])
  effect.size <- c(.1, .2, .3)*7.2 # multiply by the SD (7.2%) to 
                                   # get percent absence differences 
  n.itterations <- 500 # number of times the simulation will run
  prop.treat <- .5 # a single value representing the proportion of students in the
                   # sample who are to recieve the treatment 

  total.n <- dim(Sylvan)[1] # total number of students

# 2) Create Results Matrix
# create empty matrix to store results in. Initially, the data frame has
# one row, with a column for each of the data points we are going to output.  
# In this case, the final results matrix will have one row for each 
# itteration of the simulation, and one column for each parameter, 
# and one column for each output value from the simulation.
  
  results <- data.frame(itteration = NA, sample.size = NA, 
                      effect.size = NA, prop.treat = NA, T.coef = NA , 
                      T.se = NA , T.pval = NA , df = NA , typeIIerror = NA , N = NA)

# 3) Run Simulation
# For each parameter that varies, we include a new loop that loops
# over the number of options for that parameter. Because the values 
# of each parameter are defined as a vector, we can use the length() 
# function calculate the number of values for each parameter to make 
# our simulation more flexible. 

for(k in 1:length(sample.size)){
  for(j in 1:length(effect.size)){
    for(i in 1:n.itterations){
      
      # a) Select a subset of student to participate (based on 
      #    sample size)
      
         S.sample <- Sylvan[sample(1:total.n, sample.size[k]),]
      
      # b) Assign students to treatment (Create "T.Status" variable)
      
        n.treated <- round(sample.size[k]*prop.treat, 0) # Number of treated cases
      
        S.sample$T.status <- 0 
        S.sample$T.status[sample(1:sample.size[k], n.treated)] <- 1
      
     
      # c) assign Outcome to treated cases 
      #   (create psudo outcome variable based on prior year data)
      #   The "outcome" for students in the control group is the middle school
      #   % absent
      #   The "outcome for the students in the treatmetn group is the middle school
      #   % absent + effect size
        S.sample$outcome <-S.sample$pct_absent_in_ms
        S.sample$outcome[S.sample$T.status == 1] <-    
                S.sample$pct_absent_in_ms[S.sample$T.status == 1] +
                effect.size[j]
      
      # d) Run Model outcome~treatment assignment
        out.mod <- lm(outcome ~ T.status, data = S.sample) # - Linear Model 
      
      # e) Extract key variables (Treatment coefficient, Treatment SE,
      #   degrees of freedom, p value, N with data, 
      #   and whether the model made a type II error)
        T.coef <- summary(out.mod)$coef["T.status" , "Estimate"]
        T.se   <- summary(out.mod)$coef["T.status" ,  "Std. Error"]
        df <- out.mod$df.residual
        T.pval <- 2*(pt(-abs(T.coef / T.se) , df = df)) # cdf t for 2 tailed pvalue
        N <- sum(complete.cases(S.sample[, c("outcome" , "T.status")]))
        typeIIerror <- T.pval >= 0.05
      
      # f) store results for ONLY this itteration of the simulation (single row)
        
        this.sim <- c(itteration = i, sample.size = sample.size[k] ,          
                    effect.size = effect.size[j] , prop.treat = prop.treat , 
                    T.coef = T.coef , 
                    T.se = T.se , T.pval = T.pval , df = df , 
                    typeIIerror = typeIIerror , N = N)
        
      # g) append results from THIS SIMULATION to the RESULTS MATRIX

        results <- rbind(results, this.sim)
      
      
      # h) clean up
      #   remove generated variables to avoid accidentally putting 
      #   incorrect data in the results matrix in future itterations
      
        remove(S.sample , out.mod , T.coef , T.se , T.pval , df , typeIIerror , N )
      }
   }
}



# 4) Summarize results for each combination of parameters
# Average the number of type II errors made over the 
# input values for proportion treated, n size, and effect size
  
  summary.results <- aggregate(list(typeIIerror = results$typeIIerror),
                               by = list( prop.treat = results$prop.treat,
                                          sample.size = results$sample.size,
                                          effect.size = results$effect.size ), 
                               mean)
# 5) Calculate the power for each combination of parameters
  
  summary.results$power <- 1-summary.results$typeIIerror

# view summary of results

  summary.results


# 6) Create Power Plot

  library(ggplot2)
  
  plot.test <- ggplot(summary.results, aes(x = effect.size, 
                                           y = power, group = sample.size))
  plot.test + geom_line(aes(color = sample.size), size = .8)
```





