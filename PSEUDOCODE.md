# PSEUDO CODE

Illustration of the routines took place and the R Objects associated. For user to be able to quickly re-produce the end results.

##  1. Sample Data
* Use 1% sample size
* Directory: `./data/Coursera-SwiftKey/output/en_US`
* File(s): `en_US.blogs.txt`, `en_US.news.txt`, `en_US.twitter.txt`  
* Look for `week3-sampleData.R`

##  2. Corpus
R Object: `corpus.RData`, look at `week3-constructCorpus.R`
#### Creation
* Use VectorSource instead of DirSource
* Being able to perform primitive text processing before construct as Corpus
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
* Use "RWeka" package to create 1-gram, 2-grams and 3-grams (look for `week3-nGramMaker.R`)
* Together with "parallel" package to improve performance (look for `week3-parallelProcessing.R`)
* Parallel processing returns a list of vector objects; best to unlist them
### Cleaning
* Although transformation took place at the corpus level but the N-Grams still appeared noisy with some numbers and symbols
* Use grep to further cleaning the data
	* `oneGram.RData`: "^[a-z]+$"
	* `biGram.RData`: "^[a-z]+ [a-z]+$"
	* `triGram.RData`: "^[a-z]+ [a-z]+ [a-z]+$"
* R Object(s): `oneGramCleaned.RData`, `biGramCleaned.RData`, `triGramCleaned.RData`
* Look for `week3-nGramParser.R`
### Processing
* After cleaning; the intention here is to even further reduce the size of all N-Grams
* To replace words that has frequency of 1 with "<UNK>"
* Use `oneGram.RData` to retrieve terms with frequency of 1
* Replace words that has frequency of 1 with "<UNK>" in all N-Grams
* R Object(s): `oneGramParsed.RData`, `biGramParsed.RData`, `triGramParsed.RData`
* Look for `week3-nGramParser.R`
## 4. Term(s) Frequency Table
* A Term Frequency Table is a data frame of 2 columns, the first column list the unique terms found in N-Grams and the second column list the number of occurrence of the unique terms.
* R Object(s): `oneGramDataFrame.RData`, `biGramDataFrame.RData`, `triGramDataFrame.RData`
* As example,
	<table>
	<tr><td><b>Term</b></td><td><b>Count</b></td></th>
	<tr><td>hello world</td><td>6</td></tr>
	<tr><td>i am</td><td>5</td></tr>
	<tr><td>he is</td><td>4</td></tr>
	</table>
### Enrich Term(s) Frequency Table
* Break `Term` into `columnKeys` and `rowKeys` to be used to build Transition Matrix
* `columnKeys` refers to the column key of the matrix (vertical)
* `rowKeys` refers to the row key of the matrix (horizontal)
* R Object(s): `biGramDataFrameEnriched.RData`, `triGramDataFrameEnriched.RData`
* Look at `week3-nGramParser-part2.R`

### Reduce Term(s) Frequency Table
* Although cleaning has been perform since the beginning of the routine. However the memory footprint required is still huge to be consumed as an application
* One strategy is to inspect the Frequency Tables with the R `summary` command and subset the Tables if `Count` is equal or larger than the mean 
* Look at `week3-nGramParser-part2.R`

## 5. Transition Matrix 
* A Transition Matrix can be depicted as a State Transition Table; each row and column represents a state; to interpret the matrix, a state indicated by row can transit to a state indicated by column complying to a given probability indicated by the intersection.
* As example, 
	<table>
	<tr><td></td><td>a</td><td>b</td></tr>
	<tr><td>a</td><td>0.5</td><td>0.5</td></tr>
	<tr><td>b</td><td>0.5</td><td>0.5</td></tr>
	</table>
	* state "a" has 50% chances of transit to state "a" or "b"
* Look at `week3-nGramParser-part2.R`
### Smoothing
* **Add 1 Smoothing** as candidate

## 6. Markov Chain Model
* Look at `week3-nGramParser-part3.R`
* Use the "markovchain" library to construct the R Markov Chain object to perform the look up / transition routines
* Look at `week4-MarkovChain.R`

## 7. Shiny Application
* For User Interface look into `ui.R`
* For Server Side code look into `server.R`

## 8. Shiny Application Build Script
* Run `buildShinyApp.R` to build the Shiny Application into Shiny io
* Change the Shiny io credentials accordingly