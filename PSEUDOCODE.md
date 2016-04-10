# PSEUDO CODE

Illustration of the routine took place and the R Objects associated. For user to be able to quickly re-produce the end results.

##  1. Sample Data
* Use 1% sample size
* Directory: `./data/Coursera-SwiftKey/output/en_US`
* File(s): `en_US.blogs.txt`, `en_US.news.txt`, `en_US.twitter.txt`  

##  2. Corpus
R Object: `corpus.RData`
#### Creation
* Use VectorSource instead of DirSource
* Capable of cleaning text documents before construct them as Corpus
#### Transformation
* Remove none ASCII
* Remove explicit punctuation(gsub regxp "[[:punct:]]")
* Convert all text to lower case
* Remove punctuation (from package "tm")
* Remove numbers
* Strip white spaces
* Convert as plain text documents

## 3. N-Grams
### Creation
* R Object(s): `oneGram.RData`, `biGram.RData`, `triGram.RData`
* Use "RWeka" package to create 1-gram, 2-grams and 3-grams
* Together with "parallel" package to improve performance
* These are Vector Objects
### Cleaning
* Although transformation took place at the corpus level but the N-Grams still appeared noisy with some numbers and symbols
* Use grep to further cleaning the data
	* `oneGram.RData`: "^[a-z]+$"
	* `biGram.RData`: "^[a-z]+ [a-z]+$"
	* `triGram.RData`: "^[a-z]+ [a-z]+ [a-z]+$"
* R Object(s): `oneGramCleaned.RData`, `biGramCleaned.RData`, `triGramCleaned.RData`
### Processing
* After cleaning; the intention here is to even further reduce the size of all N-Grams
* To replace words that has frequency of 1 with "<UNK>"
* Use `oneGram.RData` to retrieve terms with frequency of 1
* Replace words that has frequency of 1 with "<UNK>" in all N-Grams
* R Object(s): `oneGramParsed.RData`, `biGramParsed.RData`, `triGramParsed.RData`

## 4. Term(s) Frequency Table
* A Term Frequency Table is a data frame of 2 columns, the first column list the unique terms found in N-Grams and the second column list the number of occurrence of the unique terms.
