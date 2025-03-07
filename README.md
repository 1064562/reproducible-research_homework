# Reproducible research: version control and R

Answers for question 1 to 3 in the README.md file in the repositry: 
https://github.com/1064562/logistic_growth

Answers for question 4:
- PART 1: In the plots generated, each line shows a trajectory of the random walk in a two dimensional space. The colour gradient shows the progression of time, as the darker the line becomes, showing earlier times. The x and y axis are coordinates that spatially show where the walking is being simulated. The left plot is showing a walk that starts at (0,0) and then generally moves to the left, towards negative X. The right plot starts at (0,0) too but moves towards the right, towards a positive X. There is quite alot of spread along the y-axis in both, showing quite a high variance. 

- PART 2: A random seed is a number used to initialise a random number generator. A random seed gives the starting point when a computer generates a sequence of random numbers (1). This makes sure that the results are reproducible, so that anyone that reruns the same code with the same seed will obtain the same sequence of output (2).

references: 

(1) Stephanie (2017). Random Seed: Definition. [online] Statistics How To. Available at: https://www.statisticshowto.com/random-seed-definition/.

‌(2) Bansal, J. (2020). How to Use Random Seeds Effectively. [online] Medium. Available at: https://towardsdatascience.com/how-to-use-random-seeds-effectively-54a4cd855a79?gi=c3cc03832340#:~:text=A%20random%20seed%20is%20used [Accessed 7 Dec. 2023].

- PART 3: The updated code has been put in this repositry, in the question-4-code folder as "random_walk.R"

- PART 4: A screenshot of the edits made in the code via the commit history has been put in the same question-4-code folder, as well as comments made in the history section and below:

![Screenshot of changes](question-4-code/code-change-q4.png)

Answers for question 5:

- PART 1: There are 13 columns, and 33 rows in this dataset. 

- PART 2: We can do the log transformation to fit a linear model to the data, this has been done in PART 1 of the Question5.R code.

- PART 3: Using the code in PART 1 of "Question5.R", the exponent (alpha) I obtained was 1.515228 and the scaling factor (beta) was 1181.807. The p-values of both the intercept (scaling factor, which was p = 2.279645e-10) and the slope (alpha, which was p = 6.438498e-10) in the linear model summary met the p<0.05 requirement for both the results to be significantly different. Therefore, we can conclude that these results have a very low chance of being due to random chance. In the paper by Cui, Schlub and Holmes in the Journal of Virology (doi: https://doi.org/10.1128/jvi.00362-14) in Table 2, the dsDNA data for exponent is 1.52 (1.16–1.87) which is almost identical to my results. The dsDNA data for the scaling factor is also nearly identical to mine with the Table showing a scaling factor of 1,182 (246–5,675).

- PART 4: The figure has been reproduced in the Question5.R, PART 2 code.

- PART 5: The code for this is found in PART 3 of the Question5.R file. The estimated volume of a 300kb dsDNA virus is 256,455,557nm^3, using estimated alpha and beta values in the formula (V = (B*L)^a).

- BONUS: Reproductibility means to get consistent or reproducing the same results when using the same data and method. Replicability is getting the same result again, with using the same method but using new data than before. Git and GitHub can be used to enhance reproducibility and replicability. They allow for version control of scripts that allows to track and maintain a snapshot of the development of a project. This is important because this allows for collaboration and splitting of tasks between collegues online in a way that reduces any error a project. But the limitations of git and Github can include potential security breaches / risks. Because Github is cloud based, data is stored on a server and the user does not have alot of control over how secure the data is held on the server. If Github's servers are hacked, then projects could be at risk with no fault of the user. In addition, since Github is ran on a server, it can be very slow especially when handling big file projects and there is no way for the user to improve this speed compared to handling projects on their own computer or server.

## Instructions

The homework for this Computer skills practical is divided into 5 questions for a total of 100 points (plus an optional bonus question worth 10 extra points). First, fork this repo and make sure your fork is made **Public** for marking. Answers should be added to the # INSERT ANSWERS HERE # section above in the **README.md** file of your forked repository.

Questions 1, 2 and 3 should be answered in the **README.md** file of the `logistic_growth` repo that you forked during the practical. To answer those questions here, simply include a link to your logistic_growth repo.

**Submission**: Please submit a single **PDF** file with your candidate number (and no other identifying information), and a link to your fork of the `reproducible-research_homework` repo with the completed answers. All answers should be on the `main` branch.

## Assignment questions 

1) (**10 points**) Annotate the **README.md** file in your `logistic_growth` repo with more detailed information about the analysis. Add a section on the results and include the estimates for $N_0$, $r$ and $K$ (mention which *.csv file you used).
   
2) (**10 points**) Use your estimates of $N_0$ and $r$ to calculate the population size at $t$ = 4980 min, assuming that the population grows exponentially. How does it compare to the population size predicted under logistic growth? 

3) (**20 points**) Add an R script to your repository that makes a graph comparing the exponential and logistic growth curves (using the same parameter estimates you found). Upload this graph to your repo and include it in the **README.md** file so it can be viewed in the repo homepage.
   
4) (**30 points**) Sometimes we are interested in modelling a process that involves randomness. A good example is Brownian motion. We will explore how to simulate a random process in a way that it is reproducible:

   - A script for simulating a random_walk is provided in the `question-4-code` folder of this repo. Execute the code to produce the paths of two random walks. What do you observe? (10 points)
   - Investigate the term **random seeds**. What is a random seed and how does it work? (5 points)
   - Edit the script to make a reproducible simulation of Brownian motion. Commit the file and push it to your forked `reproducible-research_homework` repo. (10 points)
   - Go to your commit history and click on the latest commit. Show the edit you made to the code in the comparison view (add this image to the **README.md** of the fork). (5 points)

5) (**30 points**) In 2014, Cui, Schlub and Holmes published an article in the *Journal of Virology* (doi: https://doi.org/10.1128/jvi.00362-14) showing that the size of viral particles, more specifically their volume, could be predicted from their genome size (length). They found that this relationship can be modelled using an allometric equation of the form **$`V = \beta L^{\alpha}`$**, where $`V`$ is the virion volume in nm<sup>3</sup> and $`L`$ is the genome length in nucleotides.

   - Import the data for double-stranded DNA (dsDNA) viruses taken from the Supplementary Materials of the original paper into Posit Cloud (the csv file is in the `question-5-data` folder). How many rows and columns does the table have? (3 points)
   - What transformation can you use to fit a linear model to the data? Apply the transformation. (3 points)
   - Find the exponent ($\alpha$) and scaling factor ($\beta$) of the allometric law for dsDNA viruses and write the p-values from the model you obtained, are they statistically significant? Compare the values you found to those shown in **Table 2** of the paper, did you find the same values? (10 points)
   - Write the code to reproduce the figure shown below. (10 points)

  <p align="center">
     <img src="https://github.com/josegabrielnb/reproducible-research_homework/blob/main/question-5-data/allometric_scaling.png" width="600" height="500">
  </p>

  - What is the estimated volume of a 300 kb dsDNA virus? (4 points)

**Bonus** (**10 points**) Explain the difference between reproducibility and replicability in scientific research. How can git and GitHub be used to enhance the reproducibility and replicability of your work? what limitations do they have? (e.g. check the platform [protocols.io](https://www.protocols.io/)).
