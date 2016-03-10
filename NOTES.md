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