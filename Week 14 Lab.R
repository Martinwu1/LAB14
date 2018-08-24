#######################################################
#######################################################
##
##  Week 14 Lab
##
#######################################################
#######################################################

## Outline:
## Part 1: Formation of Model Formulae
## Part 2: ANOVA for Comparing Two Linear Models

###########################################################
##
## Part 0: preparation
##
###########################################################

## - Change the working directory to the location of the file
## - Download nyc.csv from the webpage:
##   http://www.stat.tamu.edu/~sheather/book/data_sets.php
## - Download simu.csv from Canvas.
## - Place the files in the same location as this R script.

##############################################################
##
## Part 1: Formation of Model Formulae
##
##############################################################

# - Load a simulated data
simu = read.csv("simu.csv", header = T)
summary(simu)

# - "Formula" is a special type of objects that stores a model formula.
# - The general representation of a formula object is
#     response ~ predictors
y~1+x+z
class(y~1+x+z)

# - We will learn how to use different operators to form a model formula.
# - In the following items, A, B, C, etc, indicates variables
#   or an expression created by using the operators introduced in this section.

# - 1 : Represent the intercept, which is included in the model by default
summary(lm(GPA~1, simu))$coef
summary(lm(GPA~Height, simu))$coef

# - . : All other variables
summary(lm(GPA~., simu))$coef
# -     Be cautious that sometimes you added more variables to the data.frame later.
#       Then . will also include those variables.

# - A : If A is a variable, it is either the original variable if it is numerical,
#      or it is represented by dummy varibles if it is a factor.
summary(lm(GPA~SitArea, simu))$coef
summary(lm(GPA~Year, simu))$coef  # 1 less dummy variables if the intercept is included

# - A+B : Include all terms in A and in B
summary(lm(GPA~Sleep + SitArea, simu))$coef
summary(lm(GPA~Year + SitArea, simu))$coef

# - A:B : Include all interaction terms between each term in A and each term in B
# -       Note that interaction between a variable and itself is the original variable
summary(lm(GPA~Height : Sleep, simu))$coef
summary(lm(GPA~Sleep : SitArea, simu))$coef
summary(lm(GPA~SitArea : Year, simu))$coef
summary(lm(GPA~Height : Height, simu))$coef  # The original variable
summary(lm(GPA~(Height+Year) : (Sleep + SitArea), simu))$coef

# - A*B : It is equivalent to A+B+A:B
summary(lm(GPA~Height * Sleep, simu))$coef
summary(lm(GPA~Sleep * SitArea, simu))$coef
summary(lm(GPA~SitArea * Year, simu))$coef
summary(lm(GPA~(Height+Year) * (Sleep + SitArea), simu))$coef

# - A^2 : It is equivalent to A*A, i.e. A+A
summary(lm(GPA~Height^2, simu))$coef
summary(lm(GPA~(Height+SitArea)^2, simu))$coef
summary(lm(GPA~.^2, simu))$coef

# - -A : Remove all terms in A
#        Note that removing the intercept term may affect the behavior of categorical variables.
summary(lm(GPA~-1+Height, simu))$coef
summary(lm(GPA~-1+SitArea, simu))$coef
summary(lm(GPA~.-Height, simu))$coef  # Remove the term Height from all terms
summary(lm(GPA~Height*Sleep-Height, simu))$coef
summary(lm(GPA~.^2 - Height:., simu))$coef  # Remove the term Height and interation between Height and other predictors
summary(lm(GPA~(.-Height)^2, simu))$coef  # Should give the same result as the last one

# - I(expression) : The expression will be calculated mathematically (normal meaning of + and *),
#                   then the results will be used as one term.
summary(lm(GPA~I(Height^2), simu))$coef
summary(lm(GPA~I(Height+Sleep), simu))$coef  # although it does not make much sense to calculate Height+Sleep
summary(lm(GPA~I(Height*Sleep), simu))$coef  # the same as Height:Sleep

# Read the help page in ?formula for more details about formula


# - The fomula() function can be used to obtain the formula from an lm object.
model1 = lm(GPA~.^2, simu)
formula1 = formula(model1)
formula1

# - The update(original-formula, update-format) function can be used to update a formula
# - In the second argument, the period to the left of ~ represents the response in the original formula.
#   The period to the right of ~ represents the terms in the original formula.
update(GPA~Height*Sleep, .~.-Height)
formula2 = update(formula1, .~. - Height:Sleep)
formula2
model2 = lm(formula2, simu)

##############################################################
##
## Part 2: ANOVA for Comparing Two Linear Models
##
##############################################################

# - The function anova() have several usage.
# - We will use this function to compare two linear models.
# - Syntax: anova(reduced-model, full-model)

# Example 1:
model3 = lm(GPA~Year, simu)  # Reduced model
model4 = lm(GPA~Sleep*Year, simu)  # Full model
anova(model3, model4)
anova(model4, model3)  # Reversing the two arguments still gives the correct F-statistic and p-value,
                       # but the difference in df and SS becomes negative.
# To verify the correctness of the F-statistic and pvalue,
df.reduced = model3$df.residual
df.full = model4$df.residual
RSS.reduced = sum(model3$residuals^2)
RSS.full = sum(model4$residuals^2)
F.stat = ( (RSS.reduced - RSS.full)/(df.reduced-df.full) ) / (RSS.full/df.full)
p.value = 1-pf(F.stat, df.reduced-df.full, df.full)
F.stat
p.value

# Example 2:
model5 = lm(GPA~., simu)  # Reduced model
model6 = lm(GPA~.^2, simu)  # Full model
anova(model5, model6)

# Example 3: A example of wrong usage, in which one model does not contain the other.
model7 = lm(GPA~Height +Year, simu)
model8 = lm(GPA~Sleep*Year, simu)
anova(model7, model8)
# anova() does not check whether one model includes the other.
# It simply calculates F-statistic and p-value based on df's and SS's.

# Read the help page in ?anova.lm for more details.
