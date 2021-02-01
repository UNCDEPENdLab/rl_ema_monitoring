This is the section of the repo designated to the monitoring dashboard
Integration with DataLad, SnakeMake, and Rclone for decentralized data sharing must still take place...

	This would achieve:
	1. An automated backup of all data at all sites
	2. An easy way of sharing data in relatively real-time
	3. Version control of what is likely to be a large dataset

	Why this approach?:
	1. Hosting data on GitHub publically is undesired before publication
	2. GitHub has limits to the amount of data being stored and how it is stored
	3. Takes advantage of unlimited Google Drive storage for Google Business

	Build Design:
	1. R-Markdown skeleteon due to it being somewhat agnostic to programming languages
	2. R-Markdown will act as the front-end, running scripts to access data on the backend
	3. Data to be stored in a shared Google Drive using an Rclone special remote for DataLad
	4. Dashboard to run a monkey-patch of SnakeMake and DataLad integration to automatically pull data needed for dashboard at setup
	5. SnakeMake may also trigger a BIDS conversion of the data to feed into BIDS apps like fMRIPrep (though this may be built adjacently)

If attempting to use demo, please view the official documentation for setting up usage of the PyDrive python package.