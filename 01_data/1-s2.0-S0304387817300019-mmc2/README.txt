IReadme.txt 

December 2016 and

The following provides basic information on the creation of the final datasets and describes the do files that produce Tables 1-7 and tables A.1, A4.1-A44 and A5.1-A5.4 in the online appendix of the paper: 

Martinez, C. and Perticara, M. “Childcare Effects on Maternal Employment: Evidence from Chile”. XXX.
 

The data are in Stata 11 format; but do files can run either under Stata 11.2 or higher. Questions can be referred to mperticara@uahurtado.cl or mcp45@georgetown.edu
 
********************************* 
SYSTEM REQUIREMENTS 
********************************* 
The do files have been tested on a Windows XP machine running Stata SE 14.1. You need to create the following directories. 
/dta contains all final stata DTA files used as input in DO files 
/do contains do files
/tab_paper stores output for tables in the paper
/appendix stores output for tables in the online appendix
I

********************************* 
DATASETS AND DO FILES 
********************************* 

1) data_analysis.dta: anonymized mother-level dataset. Contains information from both baseline and endline.
2) muestra_validez_ext.dta: school level data-set. Urban primary schools in the municipalities participation in this study. Used to create Table A1 (external validation) in online appendix.
3) FV_tablas_paper.do performs all operations to reproduce paper results.
4) FV_appendix.do performs all operations to reproduce tables in online appendix.
I

I