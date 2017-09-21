## A Simple _R_ Script for matching records based on Propensity Scores

```
psm(a,b,var,max,min,id, nn)
```
Returns a data frame of matches between data frame _a_ and _b_ given the match probabilities variable _var_, the maximum and minimum precision of probabilities, a unique identifier _id_, and the number of matches per target record _nn_
