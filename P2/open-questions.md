# Open questions

## Exercise 4
According to Happy's documentation there's two main differences between left and right recursion. While left recursion is more memory efficient because it results in a constant-stack parser, it also reverses the list. Right recursion on the other hand is less memory efficient, but it does not reverse the list. 

Normally, the parser combinators we use cannot handle left recursion, because this would cause an infinite loop. 

In the assignment we purposefully chose for right recursion. The inputs are human written and not very likely to be extremely long. 

## Exercise 10
