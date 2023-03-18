# Party Composition in US State Legislations (1993-today)

## Code to scrape from [ballotpedia.org](https://ballotpedia.org)

This repository contains an R script to scrape and save data on party composition in US states' upper and lower chambers (State Senate, State House of Representatives) from [Ballotpedia.org](https://ballotpedia.org). 
It outputs two datasets, one each for the upper and lower chamber, containing the following variables:

	- State (excluding overseas territories and the District of Columbia)
	- Year 
	- Number of Democrats, Republicans, Independents, and Other (as categorized by Ballotpedia)
	- Party with majority by number of seats
	- Indicators for entries that contain a note or asterisk providing additional information (useful for adding them to the dataset manually)

I wrote this code in January 2023 for a research project because I could not find the data anywhere. 
I hope that by making it publicly available, I can help out someone who finds themselves in a similar position.
Even though I wrote this code with much care, it comes with absolutely no guarantees whatsoever. 
For example, parts of the code might break if Ballotpedia updates its website in the future.
Therefore, if you encounter an error when running the code, feel free to let me know.		
