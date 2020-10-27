# Data and Code of Meta Analysis 
### by Meike Morren & Amir Grinstein

This repository contains all the data and code that is used to produce the analyses in *The Cross-Cultural Challenges of Integrating Personal Norms into the Theory of Planned Behavior: A Meta-Analytic Structural Equation Modeling (MASEM) Approach*.

We conducted a meta-analysis on Theory of Planned Behavior (TPB) in the field of environmentally friendly behavior. Additionally, we have added personal norms to the framework to explore how they should be integrated with TPB. During the data collection, we tried to include studies from as many countries as possible so that we could make valid cross-cultural comparisons. This led to a dataset of 255 samples (described in 231 articles) from 50 countries. For more information on the data collection, please contact meike.morren@vu.nl.


## Folders

### Input

The input folder contains three files:
- Hofstede values
- GLOBE values
- The matrices (note that we have collected more variables than used in the paper)

### Code

In the code folder you can find all the code used to obtain the figures and tables in the paper. 
To inspect the datafile meta analysis_study_2020_R.txt, run the file preparation.R first. Next, run moderators.R to add the moderator variables (i.e. Individualism-Collectivism dimensions). To conduct the bivariate analyses, go to fixed-random-mixed.R. This file also contains the tests for publication biases (e.g. funnel plots and egger's test). Before you start the MASEM analyses, the data needs to be reshaped into a list, see list.R. You can conduct the MASEM analysis via the file onestage-masem.R. Additionally we conduct the moderator analysis in onestage-masem-mods.R. 

- **preparation.R** : reads in file, creates a wide format (each study one line), and adds labels to the correlations
- **moderators.R** : reads in files of moderator values, imputes missing values, and creates a wide format dataframe
- **fixed-random-mixed.R** : performs bivariate analyses and inspects publication bias 
- **onestage-masem.R** : estimates the onestage MASEM and creates table 3
- **onestage-masem-mods.R** : adds moderator analyses and creates tables 4 and 5

Our additional analyses of the Campbell Paradigm are reported in one-factor-model.R. We also added the code in which we created the descriptive tables 1 and 2 and figure 4.

### Output

This folder contains two subfolders: Tables and Figures. If you run the code, this will automatically output the tables and figures in these folders.

## How to use?

You might benefit from our efforts in collecting 255 matrices in which two or more TPB variables (including personal norms) are related to one another. If you download the entire repository, you should be able to run all files, and obtain the tables and results as described in our paper. Please mind that the reading and writing of files make use of relative paths. So first set your working directory in the main folder! If you use our data, please cite the paper:

"put citation here"