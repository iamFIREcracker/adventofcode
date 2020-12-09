# Advent of Code solutions in Common Lisp

## TODO

- This works with SBCL only
- Get 2018 solutions to use DEFINE-SOLUTION -- and review the solutions too,
  given that I was just getting stated with CL back then
- 2016/11: it's taking 50+ seconds to generate solutions, and by looking at
  [Reddit's
  megathread](https://www.reddit.com/r/adventofcode/comments/5hoia9/2016_day_11_solutions/)
  it seems like there might be a few optimizations worth implementing
- 2016/14: it takes around 40 seconds to generate the solution for part 2, and
  I am not sure there is a more efficient way to solve this (e.g. faster md5
  implementation, avoid repeated string-to-bytes conversions)
- 2016/18: fiddle with integers and bits instead of creating new strings
  (currently it takes 1.5 seconds to run part2)
- 2016/21: inverse scramble operations instead of bruteforcing with all the
  `8!` permutations of the input
- 2016/23: 160 seconds to naively solve part2 -- try to _optimize_ the input
  by converting add-loops into multiplications?!
- 2019/10: needs some serious refactoring around phases, and vector math
- 2019/17: implement a solution for this (I solved this with pen and paper)
- 2019/18: the implementation is a bit of a mess, and it takes around 10
  seconds to complete
- 2019/19: figure out how to calculate the _slope_ of the upper border of the
  beam
- 2019/24: better logic for recursive neighbors + how to efficiently add
  levels to the state? Currently we end up with way more states than we
  actually use
- 2019/25: implement a solution for this (I solved this by _playing_ it)
- Add PERMUTATIONS function that works with strings too (currently, for
  2016/21 I COERCE'd like there is no tomorrow)
- 2020/08: does a better solution that bruteforce exist for part 2?
- 2020/09: solve part2 using a sliding window instead of BFS + prefix-sum
