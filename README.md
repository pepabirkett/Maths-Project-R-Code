# Maths-Project
Authors: Pepa Birkett, Anisah Khanom, Runlin Chen


This repository details the R code used in our Y4 Mathematics Project, The University of Edinburgh. 

## Abstract
Stylometry is the statistical analysis of literary style. Stylometric methods are typically applied to prose to quantitatively answer literary questions on authorship attribution. We present a collection of studies that apply stylometry to different literary forms. To illustrate the conventional uses of stylometry, we study the purported collaboration between the celebrated Alexander Dumas and the lesser known Auguste Maquet. We find evidence that Maquet may have contributed significantly to the writing in one of Dumas works. We also push the boundaries of stylometry by applying it to poetry from the Romantic period and whether stylometry predicates literary style transcending time. We find that the works of poets resist the rationalisation and reduction of stylometry. We also look at novel and screen adaptations. This results in more questions on literary style depending on other features rather than being consistent to one writer and one specific form. The implications of stylometry and literary form are discussed. 

## Data Files
You can access our paper and references here: 
[Github Repository - Maths-Project-R-Code](https://github.com/pepabirkett/Maths-Project-R-Code/Paper-and-References)
All raw data was originally sourced through Project Gutenberg, before we cleaned and preprocessed the data for usage in our analysis: 
https://www.gutenberg.org/

## Data Files
You can access the raw data files here: 
[Github Repository - Maths-Project-R-Code](https://github.com/pepabirkett/Maths-Project-R-Code)
All raw data was originally sourced through Project Gutenberg, before we cleaned and preprocessed the data for usage in our analysis: 
https://www.gutenberg.org/

## Citation
If you use this project in your research, please cite it as:
[Pepa Birkett, Anisah Khanom, Runlin Chen, Maths-project-R-Code, Github repository, 2025](https://github.com/pepabirkett/Maths-Project-R-Code)

## How to Run the Code
1. Install necessary packages, all detailed in R script files
2. Create folders for use with different R-scripts:
3. To use the function word extraction functions, need to have your novel/text from which our features are being extracted in plain text format, without chapter titles, footnotes, etc. You also need a txt file with a vector list of your function words, one  word on each line.
4. Once you have function word vectors representing each of your texts, for both MDS and supervised learning, you need to organise a folder as follows: Create a folder within which exists folders representing each of the authors being used in your analysis. Within each authors folder should be separate txt files, each a representation of the function word proportions of the text, as extracted previously using the function word extraction functions.
