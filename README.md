# Project: Labradoodle or Fried Chicken? In Blakc and White. 
![image](figs/poodleKFC.jpg)

### [Full Project Description](doc/project3_desc.html)

Term: Spring 2017

+ Team # 13
+ Team members
	+ team member 1 Zhishan Wang
	+ team member 2 Yitong Hu
	+ team member 3 Terry Li
	+ team member 4 Jingwen Yin

+ Project summary: 

In this project, we created a classification engine for grayscale images of poodles versus images of fried chickens.First of all, we did the features exraction and selection from the given images.Then, we feed our selected features to several classifieries to computing speed and prediction rate.

+ Feature Extraction and Selection:
     
For the feature extraction part, we tried SIFT features to extract feature again. For the feature selction,we tried bag of words,LASSO,PCA to reduce dimension features.

 + Model Selection:
     
As for the models, we briefly tested several models with different feature extraction and selection combination,like gbm, SVM, Randomforest, Xgboost, logistic, NaiveBayes. From all of these primary models, we found sift features combined with SVM model generally is among the models that perform the well,at the same time,it won't take too much time and it's portable as well. 
     
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
