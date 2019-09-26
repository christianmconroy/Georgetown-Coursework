cd \Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester4MPP\DataScienceIntro\dataset

ssc install libjson
ssc install insheetjson

gen business_id =""
gen stars =""
gen date =""
gen reviewstr =""

insheetjson business_id stars date reviewstr using "review.json", table(results) ///
col("business_id" "stars" "date" "text")
