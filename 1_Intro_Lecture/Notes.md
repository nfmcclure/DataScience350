## Course Purpose
* This course isn't designed to make you an expert.
* This course is designed to point you in the right direction.
* Course Objectives:
  * Statistical tools for data exploration
  * The use of R to apply these tools to real data
  * Using Inferential Statistics to interrogate data
  * Testing and expermental design
  * Bayesian and classical statistics
* See [syllabus](http://nfmcclure.github.io/DataScience350/) for more information.

## Course Outline:
* Week 1: Introduction; Data Exploration; R overview
* Week 2: Probability Distributions; Conditional Probability; Missing Data; Getting/Storing Data
* Week 3: Outliers and Missing Data; Introduction to Hypothesis Testing
* Week 4: Hypothesis Testing; The Central Limit Theorem
* Week 5: Hypothesis Testing; Confidence Intervals; Graph Algorithms
* Week 6: Regression
* Week 7: Regression; Feature Selection; Intro to Bayes
* Week 8: Bayesian Statistics and Computational Statistics
* Week 9: NLP and Guest Lecture
* Week 10: Wrapup and Extra topics

## Course Requirements and Grading
There is a total of 24 possible points. (16 pts for homework + 8 points for individual project)

* Must get 18 points to pass.
* 4 Homework assignments must me made in a production level script (every other one = #1, 3, 5, 7)
* 4 Homework assignments can be made in a regular script format (# 2, 4, 6, 8)
* The individual project must be production level code.

## R Review
* R Resources
  * [R page](http://www.r-project.org/other-docs.html)
  * [Stackoverflow](http://www.stackoverflow.com)
  * ["Little" R Intro](http://cran.r-project.org/doc/contrib/Rossiter-RIntro-ITC.pdf)
  * [Quick R](http://statmethods.net)
  * [One More R Tutorial](http://cyclismo.org/tutorial/R/)
  * [Notes from a 2-day course at UW](http://faculty.washington.edu/tlumley/Rcourse/)
  * [Google's Style Guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html)

## Statistics Review
* Familiar Concepts:
  * Discrete vs. Continuous Distributions
  * Probability
  * y=mx + b
* Statistics is the emphasis of this course.

## SQL Review
* Only basic SQL is needed.
  * Create tables
  * Drop tables
  * Joins (inner, outer, left, right)
  * Temp tables
  * Coalesce, Cast, Case

* SQLite tables are great for smaller, contained SQL databases.
* Accessing SQLite:
  * [SQLite Browser](http://sqlitebrowser.org/)

## Counting Review
* Factorials
  * Count # of ways to order N things = N!
  * 0! = 1
  * 1! = 1
  * 4! = 4*3*2*1 = 24
  * 10! = 10*9* ... *2*1 = 3,628,800
  * N! = (N) * (N-1) * (N-2) * ... * 2 * 1
* Permutations
  * Count # of ways to *order* R things from N things = N! / (N-R)!
  * Ordering Matters. E.g. the group of 'A-B-C' is different than the group 'C-B-A'.
  * Also written as P(N,R)
* Combinations
  * Count # of ways to *group* R things from N things = N! / ( R! (N-R)! )
  * Ordering doesn't matter, so we divide the permutation formula by the # of ways to order R things, R!.
  * Also written as C(N,R) or 
