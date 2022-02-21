# MechaCar Statistical Analysis
A few weeks after starting his new role, Jeremy is approached by upper management about a special project. AutosRUs’ newest prototype, the MechaCar, is suffering from production troubles that are blocking the manufacturing team’s progress. AutosRUs’ upper management has called on Jeremy and the data analytics team to review the production data for insights that may help the manufacturing team.
In this challenge, you’ll help Jeremy and the data analytics team do the following:

 - Perform multiple linear regression analysis to identify which variables in the dataset predict the mpg of MechaCar prototypes
 - Collect summary statistics on the pounds per square inch (PSI) of the suspension coils from the manufacturing lots
 - Run t-tests to determine if the manufacturing lots are statistically different from the mean population
 - Design a statistical study to compare vehicle performance of the MechaCar vehicles against vehicles from other manufacturers. For each statistical analysis, you’ll write a summary interpretation of the findings.

## What is Being Created
 - Deliverable 1: Linear Regression to Predict MPG
 - Deliverable 2: Summary Statistics on Suspension Coils
 - Deliverable 3: T-Test on Suspension Coils
 - Deliverable 4: Design a Study Comparing the MechaCar to the Competition


## Resources
 - Software: R, RStudio
 - Packages: tidyverse, dplyr, ggplot2 and MechaCarChallenge.RScript

## Deliverable 1: Linear Regression to Predict MPG (30 points)
### Deliverable 1 Instructions
The MechaCar_mpg.csv dataset contains mpg test results for 50 prototype MechaCars. The MechaCar prototypes were produced using multiple design specifications to identify ideal vehicle performance. Multiple metrics, such as vehicle length, vehicle weight, spoiler angle, drivetrain, and ground clearance, were collected for each vehicle. Using your knowledge of R, you’ll design a linear model that predicts the mpg of MechaCar prototypes using several variables from the MechaCar_mpg.csv file. Then, you’ll write a short interpretation of the multiple linear regression results in the README.md.
### Results
Linear Model: mpg = 6.267(vehicle_length) + 0.0012(vehicle_weight) + 0.0688(spoiler_angle) + 3.546(ground_clearance) + -3.411(AWD) + (-104.0)

![d1](https://github.com/kajev/MechaCar_Statistical_Analysis/blob/main/images/d1_linear_regr.png)

From the above output we can see that:

The vehicle length, and vehicle ground clearance are statistically likely to provide non-random amounts of variance to the model. That is to say, the vehicle length and vehicle ground clearance have a significant impact on miles per gallon on the MechaCar prototype. Conversely, the vehicle weight, spoiler angle, and All Wheel Drive (AWD) have p-Values that indicate a random amount of variance with the dataset.

The p-Value for this model, p-Value: 5.35e-11, is much smaller than the assumed significance level of 0.05%. This implies we should reject our null hypothesis, which further indcates that the slope of this linear model is not zero.

This linear model has an r-squared value of 0.7149, which means that approximately 71% of all mpg predictions will be determined by this model. Relatively speaking, the multiple regression model does predict mpg of MechaCar prototypes effectively.

If we remove the less impactful independent variables (vehicle weight, spoiler angle, and All Wheel Drive), the predictability does decrease, but not drastically: the r-squared value falls from 0.7149 to 0.674.

![d1](https://github.com/kajev/MechaCar_Statistical_Analysis/blob/main/images/d1_linear_regr_bonus.png)




## Deliverable 2: Create Visualizations for the Trip Analysis (30 points)
### Deliverable 2 Instructions
The MechaCar Suspension_Coil.csv dataset contains the results from multiple production lots. In this dataset, the weight capacities of multiple suspension coils were tested to determine if the manufacturing process is consistent across production lots. Using your knowledge of R, you’ll create a summary statistics table to show:

 - The suspension coil’s PSI continuous variable across all manufacturing lots
 - The following PSI metrics for each lot: mean, median, variance, and standard deviation.

### The total_summary dataframe looks like this

![d1](https://github.com/kajev/MechaCar_Statistical_Analysis/blob/main/images/d2_total_summary.png)

### The lot_summary dataframe looks like this

![d1](https://github.com/kajev/MechaCar_Statistical_Analysis/blob/main/images/d2_lot_summary.png)

### Qesution: The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch. Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually? Why or why not?

A: The first image showed in deliverable 2 show that the variance of the PSI is about 62 which is much below the 100 PSI. However, while lot 1 and lot 2 are even lower than the average variance PSI of all the lots, lot 3 has a variance in the PSI of about 170 which is well over the limit.

You can see the difference in variance by this box plot

![d1](https://github.com/kajev/MechaCar_Statistical_Analysis/blob/main/images/d2_boxplot2.png)



## Deliverable 3: T-Tests on Suspension Coils (20 points)
### Deliverable 3 Instructions
Using your knowledge of R, perform t-tests to determine if all manufacturing lots and each lot individually are statistically different from the population mean of 1,500 pounds per square inch.

#### Technical Analysis
1. In your `MechaCarChallenge.RScript`, write an RScript using the `t.test()` function to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch.
2. Next, write three more RScripts in your `MechaCarChallenge.RScript` using the `t.test()` function and its `subset()` argument to determine if the PSI for each manufacturing lot is statistically different from the population mean of 1,500 pounds per square inch.

- An RScript is written for t-test that compares all manufacturing lots against mean PSI of the population
- An RScript is written for three t-tests that compare each manufacturing lot against mean PSI of the population
- There is a summary of the t-test results across all manufacturing lots and for each lot

The next step is to conduct a t-test on the suspension coil data to determine whether there is a statistical difference between the mean of this provided sample dataset and a hypothesized, potential population dataset. Using the presumed population mean of 1500, we find the following:

There is a summary of the t-test results across all manufacturing **lots**
![d3](https://github.com/kajev/MechaCar_Statistical_Analysis/blob/main/images/d3_t-test.png)

From here we can see the true mean of the sample is 1498.78, which we also saw in the summary statistics above.  **With a p-Value of 0.06, which is higher than the common significance level of 0.05**, there is NOT enough evidence to support rejecting the null hypothesis.  That is to say, the **mean of all three of these manufacturing lots is statistically similar** to the presumed population mean of 1500. 

Next looking at each individual lots:

1. Lot 1 sample actually has the true sample mean of 1500, again as we saw in the summary statistics above. With a p-Value of 1, clearly we cannot reject (i.e. accept) the null hypothesis that there is no statistical difference between the observed sample mean and the presumed population mean (1500).
2. Lot 2 has essentially the same outcome with a sample mean of 1500.02, a p-Value of 0.61; the null hypothesis cannot be rejected, and the sample mean and the population mean of 1500 are statistically similar.
3. However, Lot 3, not surprisingly is a different scenario. Here the sample mean is 1496.14 and the p-Value is 0.04, which is lower than the common significance level of 0.05.  All indicating to **reject the null hypothesis** that **this sample mean and the presumed population mean are not statistically different**.

 ![d3](https://github.com/kajev/MechaCar_Statistical_Analysis/blob/main/images/d3_t-test_lots.png)

How does this information help?  Clearly, something went awry in Lot 3's production cycle. The process needs to be checked for system fails and the suspension coils from this lot need to be inspected to remove those not meeting quality criteria.


## Deliverable 4 Instructions
### Using your knowledge of R, design a statistical study to compare performance of the MechaCar vehicles against performance of vehicles from other manufacturers.
This study would involve collecting data on MechaCar and its comparable models across several different manufacturers over the last 3 years.

* What are the competitions' comparable models
* Which factors will look at the study to determine the relevant to selling price?
* Which cars will MechaCar be competing with head-to-head? which cars will be included in the study?


#### Metrics
Collecting data for comparable models across all major manufacturers for past 3 years for the following metrics:

*  Safety Feature Rating: **Independent Variable**
*  Current Price (Selling): **Dependent Variable**
*  Drive Package : **Independent Variable**
*  Engine (Electric, Hybrid, Gasoline / Conventional): **Independent Variable**
*  Resale Value: **Independent Variable**
*  Average Annual Cost of ownership (Maintenance): **Independent Variable**
*  MPG (Gasoline Efficiency): **Independent Variable**


#### Hypothesis: Null and Alternative
After determining which factors are key for the MechaCar's genre:

 * Null Hypothesis (Ho): MechaCar is priced correctly based on its performance of key factors for its genre.
 * Alternative Hypothesis (Ha): MechaCar is NOT priced correctly based on performance of key factors for its genre.
 
#### Statistical Tests
A **multiple linear regression** would be used to determine the factors that have the highest correlation/predictability with the list selling price (dependent variable); which combination has the greatest impact on price 


