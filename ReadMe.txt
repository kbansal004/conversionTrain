** Multi Touch Attribution**

The current online Double Click Manager (DCM) Tool overattributes partners/channels for last touch. It gives entire credit to the last touch for a conversion. Client was complaining that underperforming channels may not be receiving credit for conversions however they maybe assisting other channels or partners to convert. I ran scripts in R on DCM data to calculate conversions by each channel/partner and computed efficiency based on linear approach. I concluded that there are other partners/channels assisting in conversions rather than only the last touch. Some partners are efficient in form conversions but not cost wise. Apart from this I also calculated the time each conversion is taking so that it can be optimized and which are common paths which customers are using while travelling across various media channels.


Documents attached in this folder:

1. Input data file
2. R code to run the model
3. Output data file
4. Power point slide on how we presented the model results to business


How to run the code?

Step 1. Please download the DCM Data.xlsx file in your local system.
Step 2. Note the file path where you have saved the file.
Step 3. Mention the path in the beginning of code where setwd is written. It's the first line of code setwd("F:\\Multi_Touch_Attribution_Model")
Step 4. Please change the "/" to "\\" otherwise the code will throw error.
Step 5. Yes, now you are ready to run the code.
Step 6. After the code is run successfuly there be 3 output files generated in your mentioned location:

DCM_Partners_ConversionRate.csv - The percents of how many times a path was used
DCM_Partners_OutputHours - Hours between each touch and the total time it took to finally convert
DCM_Partners_OutputPath - The travel paths of each Customer ID  and a metric on how many times each channel was visited

Please Note: The data used for analysis is generated randomly so results might be little skewed or numbers might look unbelievably large or small.