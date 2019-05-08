PROC IMPORT OUT= WORK.dat 
            DATAFILE= "C:\Users\Owner\Dropbox\LLCP2017XPT\old_diabetes_b
rfss2017.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
