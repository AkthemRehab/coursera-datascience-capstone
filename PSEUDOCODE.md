### PSEUDO CODE

1. Sampling Data
	* Use 1% sample size
2. Corpus Creation
	* Transformation
		* Remove none ASCII
		* Remove explicit punctuation with Regexp "[[:punct:]]"
		* Convert all text to lower case
		* Remove punctuation again with "tm" provided transformer
		* Remove numbers
		* Strip white spaces
		* Convert as plain text documents
3. N-Grams Creation
	* Use "RWeka" package to create 1-gram, 2-grams and 3-grams