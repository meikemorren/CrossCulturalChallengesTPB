# Data and Code of Meta Analysis 
### by Meike Morren & Amir Grinstein

This repository contains all the data and code that is used to produce the analyses in *The Cross-Cultural Challenges of Integrating Personal Norms into the Theory of Planned Behavior: A Meta-Analytic Structural Equation Modeling (MASEM) Approach*.

We conducted a meta-analysis on Theory of Planned Behavior (TPB) in the field of environmentally friendly behavior. Additionally, we have added personal norms to the framework to explore how they should be integrated with TPB. During the data collection, we tried to include studies from as many countries as possible so that we could make valid cross-cultural comparisons. This led to a dataset of XX studies from XX countries. For more information on the data collection, please contact meike.morren@vu.nl.

## Files

To inspect the datafile meta analysis_study_2020_R.txt, run the file preparation.R first. Next, to add the moderator variables (i.e. Individualism-Collectivism dimensions), run moderators.R. To conduct the bivariate analyses, go to fixed-random-mixed.R. This file also contains the tests for publication biases (e.g. funnel plots and egger's test). Before you start the MASEM analyses, the data needs to be reshaped into a list, see list.R. You can conduct the MASEM analysis via the file MASEM-onestage.R. Additionally we conducted the moderator analysis in MASEM-onestage-mods.R.

- **preparation.R** : reads in file, creates a wide format (each study one line), and adds labels to the correlations
- **moderators.R** : reads in files of moderator values, imputes missing values, and creates a wide format dataframe
- **fixed-random-mixed.R** : performs bivariate analyses and inspects publication bias 
- **MASEM-onestage.R** : estimates the onestage MASEM and creates table 4
- **MASEM-onestage-mods.R** : adds moderator analyses and creates table 5


## How to use?

You might benefit from our efforts in collecting 2** matrices in which two or more TPB variables (including personal norms) are related to one another. If you download the entire repository, you should be able to run all files, and obtain the tables and results as described in our paper. Please mind that the reading and writing of files make use of relative paths. So first set your working directory in the main folder!