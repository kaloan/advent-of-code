# Advent of Code

## Haskell solutions to some problems

You can visit the official site [**here**](https://adventofcode.com/events) to look at the problems.

All days are solved in the style of the template directory:

1. One or more files with "test" data - the ones given in examples. Useful when implementing the code and checking for bugs.
1. A file with the proper input.
1. A day\<number-of-day\>.hs file containing the code. You can switch between using the test data and the proper input by changing the filename in the `main` function.

Note that in certain places some code is commented in the `mainWork` function. This is usually code that is used in only the first problem of the day, but may sometimes be prints for more clarity.

The trick for hacking around mutable values in many cases is using maps/sets. Usually quick enough, but sometimes doesn't cut it.
