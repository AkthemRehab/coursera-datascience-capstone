### 2016.03.15

1. Create a generic function that performs data reduction.
	* Read file via `readLines`
	* Within for-loop; If `decide` is true then accumulate else skip
	* Very bad performance
2. How to read large text file efficiently ?
	* CLT
	* Take note of special character "SUB" - One resolution is to read the file as binary
	* Determine the number of lines needed ?
	* Load the data into Corpus, using `VCorpus` function from "tm" package (look up more into this package)
	* Make assumption that only the body texts are important
	* [https://www.youtube.com/watch?v=s3kKlUBa3b0](https://www.youtube.com/watch?v=s3kKlUBa3b0) from [https://www.coursera.org/learn/data-science-project/module/OkCmA/discussions/q_INCeZ_EeWohBKTpGZ3Aw](https://www.coursera.org/learn/data-science-project/module/OkCmA/discussions/q_INCeZ_EeWohBKTpGZ3Aw)
	* [R example for sampling](https://www.coursera.org/learn/data-science-project/module/mb9DN/discussions/CgJEA-nTEeWfwAohgaM63Q)
	* [Suggested "tm" package reference](https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf)
3. What is n-gram?
	* [https://www.coursera.org/learn/data-science-project/module/mb9DN/discussions/aFSPjunbEeWcTQpIg9bO1w](https://www.coursera.org/learn/data-science-project/module/mb9DN/discussions/aFSPjunbEeWcTQpIg9bO1w)
	* E.g. A single word is unigram, a pair is 2-gram
4. Markov Chain
	* [A very good reference](http://setosa.io/ev/markov-chains/)

### 2016.03.11

1. [faster for() loops in R](http://www.r-bloggers.com/faster-for-loops-in-r/)
	* It is **MUCH** faster to create the results an empty vector of the correct size.
	* R seems to be slow in allocating memory for objects.
2. [Speed up the loop operation in R](http://stackoverflow.com/questions/2908822/speed-up-the-loop-operation-in-r)
	* The power of vectorization.
	* Use vectorized commands, e.g. `ifelse` or `diff` is faster than the `apply` family. 

### 2016.03.10

1. [Text Mining Infrastructure in R](https://www.jstatsoft.org/article/view/v025i05)
  	* Consist of R Source Package and example.
2. [https://cran.r-project.org/web/views/NaturalLanguageProcessing.html](https://cran.r-project.org/web/views/NaturalLanguageProcessing.html)
  	* Worth exploring but not immediately useful. 