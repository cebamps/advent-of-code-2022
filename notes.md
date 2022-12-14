# Day 11: Monkey in the Middle

There are a few things I am not satisfied with in my solution, and I have a few
ideas to experiment with in the future if I re-explore this problem. Some of
the ideas are over the top for such a small problem, but the problem is a good
use case for them anyway.

## Performance characteristics

To be honest, I did not think much about performance here. I would be curious
to see where my bottlenecks are, and if laziness is to blame.

The code isn't that slow, but it still takes a few seconds to run.

## Threading configuration through functions

Parts 1 and 2 use a different computation method for the worry score of items.
This is implemented in a function that is passed down multiple layers of
function calls.

Instead, I could either use a Reader monad or implicit parameters.

## Plenty of partial functions

The worry arithmetic for part 1 allows for division. In part 2, division is not
used and modular arithmetic is required.

To reuse the same code between both parts, I used a single shared data type
that covers both cases. However, this means that the division used in part 1
has to be a partial function, which only succeeds on the data used in part 1.

Instead, I could use polymorphism, with a typeclass that implements the worry
score, and have separate data types for each worry score.

Even fancier, I could use the reflection package for the modular arithmetic
part, to share the modulus through a runtime-created typeclass. (See the paper
Functional Pearls: Implicit Configurations from Kiselyov and Shan, which uses
that exact example of modular arithmetic to demonstrate the technique, if I
recall correctly.)

## Awkward data structure manipulation

Pulling data in and out of an IntMap repeatedly gets tiresome. I feel like
indexed lenses could help a lot with making that clearer.

Combining that with the support in the lens package for the State monad could
also be worth a try.

## Use of the writer monad

I know the writer monad is seen by many as an anti-pattern, and I would be
curious to know what better options exist here. Is it that the discourse around
Writer lacks some nuance?

## Operations are not printable

This one is a rather minor inconvenience.

I parse the operation that computes the new worry score directly into a
function, which means I had to make a dummy Show class to be able to show the
Monkey type.

I could instead follow the usual approach of making GADTs with an applicative
instance, along with an evaluator function. Something like this:

```haskell
data Expr a where
  EOld :: Expr a
  ETerm :: a -> Expr a
  EPlus :: Expr a -> Expr a -> Expr a
  ETimes :: Expr a -> Expr a -> Expr a

instance Show a => Show (Expr a) where
  show EOld = "old"
  show (ETerm x) = show x
  show (EPlus e1 e2) = show e1 <> " + " <> show e2
  show (ETimes e1 e2) = show e1 <> " * " <> show e2

eval :: Int -> Expr Int -> Int
eval old EOld = old
eval _ (ETerm x) = x
eval old (EPlus x y) = eval old x + eval old y
eval old (ETimes x y) = eval old x * eval old y
```

# Day 12: Hill Climbing Algorithm

I did not bother reimplementing Dijkstra, since I already did that last year.

I may have enjoyed implementing a monadic version to support hooking into the
search, if the problem had required that, but that was not the case here.

# Day 14: Regolith Reservoir

The rules that give the next coordinates for a unit of sand to fall into makes
the problem look very much like it wants to be expressed as a graph walk.

The optimization I gave for part 2 combines a graph state with a path into the
graph. Would a zipper provide a nice interface to clean up the solution? The
primitive operations are essentially "walk to the first child", "peek
coordinate", "update current coordinate", "go back to parent". Those are all
pretty abstract.

Another way to view things is that the problem is somewhat decomposable: up to
a certain point, we can get to the same state by shifting the source to any one
of the next coordinates of the path taken by the sand. Once sand settles
immediately, we have to backtrack to an earlier source. Same idea, but
different angle and closer to the intuition of the problem.

# Day 16: Proboscidea Volcanium

Could not succeed with part 2... WIP.

# Day 17: Pyroclastic Flow

For part 2 on this one, it was obvious that there should be a trick that would
allow to skip the simulation. I had a look on forums to confirm that hunch, and
cycle detection was indeed the answer.

I had already implemented an optimization after part 1 that discards the bottom
of the playing field whenever a row was filled, which guaranteed that this
could be considered as a new floor.

Assuming I run that cleanup every time that piece is placed, I can base the
cycle detection on piece and jet timings and that simplified field.

## First attempt at cycle detection

**This approach did not work on the test case.** I could confirm with some
trace prints that this is because the test case never fully fills a row, and
therefore its field does not cycle.

The cycle detection thus needs to rely on more clever data.

After looking a bit online, another solution is obvious, though more complex
too: find any shape that seals the field. This is not foolproof either. In the
few rounds of the sample case shown on the website, the rightmost column
remains completely empty.

Another solution is to try a fixed height, but that's not very satisfying.

## Second attempt

My second attempt keeps the cleanup (even if it may be wasteful). However, I
don't look at the full field anymore.

Instead, when placing each piece, I keep track of how far down it fell, in a
running maximum.

I can then add to the derived value used in cycle detection (`hash`) the top
section of the field according to that height, and the height itself.

That way, if a future state is equal to the current one with respect to that
equality relation, it means that no future piece will fall beyond the top
section of the current state. Indeed, they will start at least as high as the
current state's did, and will not by more than the running maximum, which we
assumed has not increased.

This is not a water-tight approach, but it is a more general heuristic than the
first one: if there is a full row in the cycle, or more generally any seal that
physically limits falls, it will imply an upper bound on the fall height.

This will not work if the maximum fall distance is unbounded.

I think in all of those cases, we identify two peaks to our mountain: one
growing up at speed `x * t` and a smaller one growing up at speed `y * t` with
`y < x`, the maximum fall distance will be unbounded.

Fortunately, both my input and the test input have a bounded maximum fall
distance, as this method finds the cycle.

# Day 20: Grove Positioning System

## Implementation

I tunnel-visioned on mutable vectors and the ST monad because I hadn't had a
chance to work with them so far. But thinking a little harder about the
primitive operations here would have made it clear that a data structure that
supports efficient insertions and deletions would have been more suitable.

I don't quite know what's available in Haskell, though.

To replace the inverse permutation, a doubly linked list would work nicely (no
need to search), and a skip list would work even nicer (logarithmic lookup and
updates), but those are not readily available.

## Performance

(Numbers given here are based on an -O2 build.)

Performance was a bit disappointing at first, so I looked to optimize my
initial part 2 solution to bring total runtime down from its ~63 seconds.

Profiling showed that the main bottleneck is the reading writing, and
allocation in `composeFF`, used for all compositions of permutation
arrays/functions.

Three steps of optimization helped to bring down runtime to 7 seconds!

- carry a buffer around to store the result of the computation
- use unsafe reads (no bounds checking)
- use unboxed vectors (this was the winner one and also the easiest)

I removed the first two optimizations later because the complexity cost was not
so much worth it: without them, runtime is at around 8.5 seconds.

More could be done:

- Now that there is a buffer, it could swap places with the input to avoid
  copying from the buffer into the input after every computation.
- The composition with a rotation permutation on the left can be done almost
  in-place, by shifting elements to the left/right, eliminating the need for a
  buffer vector.
  - However, updating the inverse is another story: this is also a rotation,
    but it is composed on the right. I wonder if there are efficient
    implementations of that.

And maybe I am overlooking something obvious?

# Day 21: Monkey Math

For this one I really wanted to experiment with recursion schemes! It worked
alright, I think.

A few take-aways:

- There is a ton of theory to learn there. What are Mu and Nu? What are all
  those [fun functions](https://hackage.haskell.org/package/recursion-schemes-5.2.2.2/docs/Data-Functor-Foldable.html)
  in `recursion-schemes` `Data.Functor.Foldable`?

- Instead of using errors in my polynomial reduction, I could have used the
  monadic `foldFixM` and friends.

- My use case combines unfold and fold, which is exactly what `refold` is for.
  That's called a [hylomorphism](https://en.wikipedia.org/wiki/Hylomorphism_(computer_science)).

- I don't understand the signature of `refold` in `recursion-schemes`, and the
  [implementation](https://hackage.haskell.org/package/recursion-schemes-5.2.2.2/docs/src/Data.Functor.Foldable.html#hylo)
  is amazing.

- I am peeved that recursive data structures built on `Fix` do not easily get
  Show and Functor typeclass instances. And also that deriving Bifunctor is not
  possible. Low ergonomics make me wonder how this is debugged, and whether
  there should exist a non-Fix isomorphic data structure to convert from/to...
