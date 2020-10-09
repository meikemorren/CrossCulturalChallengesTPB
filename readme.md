This repository contains all the data and code that is used to produce the analyses in our paper:
  
  The Cross-Cultural Challenges of Integrating Personal Norms into the Theory of Planned Behavior: 
  A Meta-Analytic Structural Equation Modeling (MASEM) Approach

To read in the datafile meta analysis_study_2020_R.txt (change name?), 
run the file preparation.R. 
Next, to add the moderator variables (i.e. Individualism-Collectivism dimensions), run moderators.R. 
To conduct the bivariate analyses, go to fixed-random-mixed.R. This file also contains the tests for publication biases.
Before you start the MASEM analyses, the data needs to be reshaped into a list, see list.R.
You can conduct the MASEM analysis via the file MASEM-onestage.R. Additionally we conducted the moderator analysis in MASEM-onestage-mods.R.

Make sure the directories have the same relative paths when you download the files.