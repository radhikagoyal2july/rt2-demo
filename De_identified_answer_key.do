
*De-identification Session Answer key
*Preparing the dataset for the lab session 

*Open dataset 
use "/Users/michaelorevba/Desktop/Datasetfake_final.dta", clear  

/**************************************
	Clean up variables 
**************************************/
 
 
*Label all variables that are not labeled
label var	respondent_id2	"Household ID 2"
label var 	area			"Name of area/community"
label var 	County			"Name of County"	
label var 	District		"Name of Disrict"	
label var 	area_m			"Area code"	
label var 	county			"County code"	
label var 	district		"District code"	
label var 	district		"District code"	
label var 	submissiondate	"Sumbmission date of survey"	
label var 	starttime		"Startime of survey"	
label var 	endtime			"Endtime of survey"	
label var 	interview_date	"Date of interview"	

 
/**************************************
	Drop PII Variables in dataset 
**************************************/

*Drop identifiable household characteristics 
drop	respondent_name snn_id date_id contact_id2	///
		computer_id m_id credit_card_num lat lon
		
*Save de-identified public dataset		
saveold "/Users/michaelorevba/Desktop/Datasetfake_public.dta", version(14) replace   
		
*************************** End of do-file ****************************************

