# Speaker-Recognition-System
I implement a speaker recognition system in haskell. 
The methodology is like, given 2 speakers' speech, we can build a markov model for both of them, and then we are given an unknown speaker's speech, then the system could tell us which speaker the unknown speaker is mostly to be.

To be detailed, the markov model I build is based on n-gram model. Given a input speech and a k, I divide the string into k and (k+1) length strings for each position(if out of index, then wrap arround for simplicity), and count the occurence of each pattern.  The markov model records those pattern in a hashtable, which is implemented by linear-probing.

To calculate the probability of the unknown speaker is the given speaker,  I also divide it into k and (k+1) length string, and then retrieve the occurence of each pattern according to the pre-built model. I use Laplace smoothing to build a theoretically-justifiable solution, with probaility being calculated as log((M+1) / (N+S)) for each position, where M is the occurence of (k+1)-length string at this position, N is the occurence of k-length string at this position, S is the unique characters of the given speech for the model. Finally, I sum up the probabilities at each position to get a likelihood.

At last, to compare the likelihoods of two models, I normalize the results based on the length of the unknown speaker's speech and compare them. 


