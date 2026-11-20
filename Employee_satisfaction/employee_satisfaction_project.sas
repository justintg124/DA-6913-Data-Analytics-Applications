/*************************************************************************/
/* Statistics Project: Employee Satisfaction Study                      */
/* Goal: Determine which factors impact employee satisfaction           */
/* Author: Justin Gould                                                 */
/*************************************************************************/

/*---------------------------------------------------------------*/
/* Step 1: Import Data                                           */
/*---------------------------------------------------------------*/
PROC IMPORT 
    datafile='/home/u64436407/DA 6813/project_proposal/Employee Attrition.csv'
    dbms=csv
    out=employee_data
    replace;
    getnames=yes;
RUN;

/*---------------------------------------------------------------*/
/* Step 2: Evaulate the Data                                     */
/*---------------------------------------------------------------*/
PROC CONTENTS data=employee_data;
RUN;

PROC PRINT data=employee_data(obs=1000);
RUN;

/*---------------------------------------------------------------*/
/* Step 3: Exploratory Data Analysis                             */
/*---------------------------------------------------------------*/
PROC FREQ data=employee_data;
    tables salary / out=salary_freq;
    tables Work_accident / out=accident_freq;
    tables promotion_last_5years / out=promotion_freq;
    tables dept / out=dept_freq;
RUN;

PROC MEANS data=employee_data 
	n nmiss min max mean;
RUN;

/*---------------------------------------------------------------*/
/* Step 4: Bivariate and Unvivariete Analysis                    */
/*---------------------------------------------------------------*/
PROC CORR data=employee_data;
    var satisfaction_level
        last_evaluation
        number_project
        average_montly_hours
        time_spend_company
        Work_accident
        promotion_last_5years;
        
     TITLE "Pearson Correlation Analysis";
RUN;

/* Graph 1: Dependant Variable Distribution */
PROC SGPLOT DATA=employee_data;
    HISTOGRAM satisfaction_level;
    DENSITY satisfaction_level;

    XAXIS LABEL="Employee Satisfaction Level";
    YAXIS LABEL="Frequency";

    TITLE "Distribution of Employee Satisfaction";
RUN;

/*---------------------------------------------------------------*/
/* Step 5: Data Cleaning - Remove 788 Null Row                   */
/*---------------------------------------------------------------*/
DATA clean_data;
	set employee_data;
	IF NOT MISSING(satisfaction_level);
RUN;

/* Ensure NULL Rows are gone */
PROC MEANS data=clean_data 
	n nmiss min max mean;
RUN;

/*---------------------------------------------------------------*/
/* Step 5: Bivariate Analysis (exclude Emp 1 and Categorical Var)*/
/*---------------------------------------------------------------*/
PROC CORR data=clean_data;
    var satisfaction_level
        last_evaluation
        number_project
        average_montly_hours
        time_spend_company
        Work_accident
        promotion_last_5years;
RUN;

/*---------------------------------------------------------------*/
/* Step 6: Model Fitting -- Original Model                       */
/*---------------------------------------------------------------*/
PROC GLM DATA=clean_data;
    CLASS dept salary;
    MODEL satisfaction_level =
        last_evaluation number_project average_montly_hours
        time_spend_company Work_accident promotion_last_5years
        dept salary / SOLUTION;
RUN;
QUIT;

/* Adjusted R-Square */
PROC IML;
    n=14999; k=17; SST=927.1343753; SSE=867.9827678;
    R_Square=1-SSE/SST;
    Adjusted_R_Square=1-((n-1)/(n-k-1))*(SSE/SST);
    PRINT R_Square Adjusted_R_Square;
RUN;

/*---------------------------------------------------------------*/
/* Step 7: Log Variables                                         */
/*---------------------------------------------------------------*/
DATA hr_transformed;
    SET clean_data;

    log_hours = LOG(average_montly_hours);
    log_evaluation = LOG(last_evaluation);
    log_tenure = LOG(time_spend_company);
    log_projects = LOG(number_project);
RUN;



/*---------------------------------------------------------------*/
/* Step 8: Best Model                                         */
/*---------------------------------------------------------------*/
PROC GLM DATA=hr_transformed;
    CLASS dept salary;

    MODEL satisfaction_level =
        log_evaluation
        log_projects
        log_hours
        log_tenure
/*         Work_accident */
/*         promotion_last_5years */
/*         salary */
/*         dept */
        number_project*average_montly_hours
        log_evaluation*log_projects
/*         last_evaluation*average_montly_hours */
        log_tenure*log_projects
        salary*time_spend_company
        salary*number_project
        dept*log_tenure

        / SOLUTION;
RUN;
QUIT;

PROC IML;
    n=14999; k=22;
    SST=927.1343753; SSE=612.5553039;
    R_Square=1-SSE/SST;
    Adjusted_R_Square=1-((n-1)/(n-k-1))*(SSE/SST);
    PRINT R_Square Adjusted_R_Square;
RUN;
