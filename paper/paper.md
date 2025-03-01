---
title: 'PCC: An R Shiny Application for Computing Partial Correlation Coefficients'
tags:
- R
- R shiny
- partial correlation
authors:
- name: "Matthew K. Dinwiddie"
  orcid: 0000-0000-0000-0000
  equal-contrib: yes
  affiliation: 1
- name: "Ann Marie K. Weideman"
  orcid: 0000-0000-0000-0000
  equal-contrib: yes
  affiliation: 2
affiliations:
- name: The Centers for AIDS Research, Biostatistics Core, at the University of North Carolina at Chapel Hill, USA
  index: 1
date: "4 September 2023"
bibliography: paper.bib
---

# Summary

The Partial Correlation Calculator (PCC) is an R Shiny application that facilitates the calculation of partial correlation coefficients. The application uses the `ppcor` R software package with additional functionality to compute partial correlation coefficients, confidence intervals, and p-values [@kim2015]. The graphical user interface (GUI) allows the upload of a data set, variable selection, specification of significance level, and of the correlation type (Pearson, Spearman, or Kendall). The resulting output is publication-friendly; the inference and associated statistical methods can be downloaded as text files and directly inserted into a manuscript.

# Statement of Need

Laboratories and clinical researchers are often interested in quantifying the magnitude of association between variables. This is typically done by computing a correlation coefficient and assessing the direction of this correlation, or by building inferential statistical models. Non-statistical researchers often use point-and-click software like GraphPad Prism [@cite2001] to perform these statistical analyses.

However, in the case of correlation coefficients, there may be a correlation between two variables that is partially explained by one or more other variables, which obfuscates the variables' true correlation. Furthermore, small sample sizes are common in fields such as immunology. Thus, assumptions of statistical models may not be satisfied, and therefore models cannot reliably quantify the effect of variables.

Partial correlation is particularly useful in these situations. Partial correlation is an analytic technique that facilitates measurement of the magnitude of association between two variables, while controlling for one or more extraneous variables. Parametric (Pearson) and non-parametric (Spearman and Kendall) partial correlation coefficients are available, lending the method a high degree of flexibility in its application.

Biostatisticians are frequently approached with requests to compute partial correlations, as laboratory software typically does not offer this functionality. The goal of the PCC is to facilitate computation of partial correlations by laboratories and clinical researchers via a GUI-based web application; the PCC has already been used to positive effect by academic stakeholders

# Usasge

The source code for the PCC is stored in a GitHub repository. The application itself is hosted on the web, but is also accessible via GitHub. The PCC accepts CSV and XLSX files for upload and users can specify the two primary variables they seek to correlate, as well as the variables for adjustment. Analysis parameters, such as the significance level and correlation type, can also be specified. Once all fields have been populated, users press the Analyze button to execute the calculation. The Download button exports results for later reference. The Reset button sets all fields to their default values and clears the uploaded data (Fig. 1).

[Fig 1. PCC graphical user interface]\label{fig:pcc-gui}](gui.png)

Data uploaded to the application is not stored between sessions. Thus, we encourage users to export the results of their analysis by using the download capability. Users are encouraged to visit the GitHub repository for more details on calculator operation, vignette, and sample HIV/AIDS data.

Researchers of any discipline can use the PCC to quantify the strength of an association between two variables while controlling for the presence of extraneous variables.

Consequently, this application should be explored by researchers from any discipline where there is a need to quantify the strength of association between two variables while controlling for the presence of extraneous variables.

# Acknowledgements

This research was supported in part by the University of North Carolina at Chapel Hill Center For AIDS Research (CFAR), an NIH funded program P30AI050410.

# References

@Manual{, title = {R: A Language and Environment for Statistical Computing}, author = {{R Core Team}}, organization = {R Foundation for Statistical Computing}, address = {Vienna, Austria}, year = {2022}, url = {<https://www.R-project.org/>}, }

@Manual{, title = {RStudio: Integrated Development Environment for R}, author = {{RStudio Team}}, organization = {RStudio, PBC.}, address = {Boston, MA}, year = {2020}, url = {<http://www.rstudio.com/>}, }

@Manual{, title = {shiny: Web Application Framework for R}, author = {Winston Chang and Joe Cheng and JJ Allaire and Carson Sievert and Barret Schloerke and Yihui Xie and Jeff Allen and Jonathan McPherson and Alan Dipert and Barbara Borges}, year = {2023}, note = {<https://shiny.posit.co/>, <https://github.com/rstudio/shiny>}, }

@book{hastie01statisticallearning, added-at = {2008-05-16T16:17:42.000+0200}, address = {New York, NY, USA}, author = {Hastie, Trevor and Tibshirani, Robert and Friedman, Jerome}, biburl = {<https://www.bibsonomy.org/bibtex/2f58afc5c9793fcc8ad8389824e57984c/sb3000>}, interhash = {d585aea274f2b9b228fc1629bc273644}, intrahash = {f58afc5c9793fcc8ad8389824e57984c}, keywords = {ml statistics}, publisher = {Springer New York Inc.}, series = {Springer Series in Statistics}, timestamp = {2008-05-16T16:17:43.000+0200}, title = {The Elements of Statistical Learning}, year = 2001 }

@article{kim_ppcor_2015, title = {ppcor: {An} {R} {Package} for a {Fast} {Calculation} to {Semi}-partial {Correlation} {Coefficients}}, volume = {22}, shorttitle = {ppcor}, url = {<https://doi.org/10.5351/CSAM.2015.22.6.665>}, doi = {10.5351/CSAM.2015.22.6.665}, number = {6}, urldate = {2021-07-08}, journal = {Communications for Statistical Applications and Methods}, author = {Kim, Seongho}, month = nov, year = {2015}, keywords = {R-project, correlation, partial correlation, ppcor}, pages = {665--674}, }
