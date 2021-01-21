# Data and Code of Meta Analysis 

We conducted a meta-analysis on Theory of Planned Behavior (TPB) in the field of environmentally friendly behavior. Additionally, we have added personal norms to the framework to explore how they should be integrated with TPB. During the data collection, we tried to include studies from as many countries as possible so that we could make valid cross-cultural comparisons. This led to a dataset of 255 samples (reported in 231 articles) from 50 countries.

## Folders

### Input

The input folder contains:
- Hofstede values
- GLOBE values
- The matrices (MASEM_study_2020_R.txt)
- The additional information on the studies (MASEM_study_2020_R_meta.txt)

Note that data.txt can be analyzed using the newly-developed, user-friendly shiny app: Jak, S., Li, H., Kolbe, L., & Cheung, M. W. (2020, June 24). webMASEM: a shiny-app for one-stage MASEM. Retrieved from osf.io/wh6d3

### Code

In the code folder you can find all the code used to obtain the figures and tables in the paper. 
To inspect the datafile MASEM_study_2020_R.txt, run the file preparation.R first. Next, run moderators.R to add the moderator variables (i.e. Individualism-Collectivism dimensions). To conduct the bivariate analyses, go to fixed-random-mixed.R. This file also contains the tests for publication biases (e.g. funnel plots and Egger's test). Before you start the MASEM analyses, the data needs to be reshaped into a list, see list.R. You can conduct the MASEM analysis via the file onestage-masem.R. Additionally we conduct the moderator analysis in onestage-masem-mods.R. 

- **preparation.R** : reads in file, creates a wide format (each study one line), and adds labels to the correlations
- **moderators.R** : reads in files of moderator values, imputes missing values, and creates a wide format dataframe
- **fixed-random-mixed.R** : performs bivariate analyses and inspects publication bias 
- **list.R** : creates a list that can be used as input for MASEM analyses
- **onestage-masem.R** : estimates the onestage MASEM and creates table 3
- **onestage-masem-mods.R** : adds moderator analyses and creates tables 4 and 5

We also added the code in which we created the descriptive tables 1 and 2 and figure 4.

### Output

The output folder contains two subfolders: Tables and Figures. If you run the code, this will automatically output the tables and figures in these folders.

## How to use?

You might benefit from our efforts in collecting 255 matrices in which two or more TPB variables (including personal norms) are related to one another. If you download the entire repository, you should be able to run all files, and obtain the tables and results as described in our paper. Please mind that the reading and writing of files make use of relative paths. So first set your working directory to the main folder in preparation.R! 
