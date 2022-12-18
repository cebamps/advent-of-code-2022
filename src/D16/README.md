# Part 2 does not work on input

2563 is too low.

I will attempt to debug this later by generating some paths and checking if
they are legal according to my adjacency function.

I can generate paths by picking two sequences of valves (disjoint) out of the
list of valves, and make that the target path of each actor. From such a pair
of sequences, I can create a sequence of nodes and check that node N+1 is
adjacent to node N. Any solution to the problem can be represented by such a
pair of disjoint valve sequences.

I can also compute the objective from the sequence pair and compare that
against the objective computed from my node path.

Hopefully, by testing enough paths, this should uncover the anomaly in my
solution.

Current outputs:

```
% time stack exec d16 < inputs/d16.txt
2114
SearchState {sT = 3, sPos = [Position {pId = "AY", pDistance = 0}], sOpen = fromList ["AY"]}
SearchState {sT = 7, sPos = [Position {pId = "GC", pDistance = 0}], sOpen = fromList ["AY","GC"]}
SearchState {sT = 10, sPos = [Position {pId = "WT", pDistance = 0}], sOpen = fromList ["AY","GC","WT"]}
SearchState {sT = 14, sPos = [Position {pId = "SY", pDistance = 0}], sOpen = fromList ["AY","GC","SY","WT"]}
SearchState {sT = 21, sPos = [Position {pId = "VR", pDistance = 0}], sOpen = fromList ["AY","GC","SY","VR","WT"]}
SearchState {sT = 25, sPos = [Position {pId = "CB", pDistance = 0}], sOpen = fromList ["AY","CB","GC","SY","VR","WT"]}
SearchState {sT = 28, sPos = [Position {pId = "PW", pDistance = 0}], sOpen = fromList ["AY","CB","GC","PW","SY","VR","WT"]}
SearchState {sT = 30, sPos = [Position {pId = "PW", pDistance = 0}], sOpen = fromList ["AY","CB","GC","PW","SY","VR","WT"]}
2563
SearchState {sT = 3, sPos = [Position {pId = "AA", pDistance = 3},Position {pId = "AY", pDistance = 0}], sOpen = fromList ["AY"]}
SearchState {sT = 5, sPos = [Position {pId = "AY", pDistance = 2},Position {pId = "CB", pDistance = 0}], sOpen = fromList ["AY","CB"]}
SearchState {sT = 7, sPos = [Position {pId = "CB", pDistance = 2},Position {pId = "GC", pDistance = 0}], sOpen = fromList ["AY","CB","GC"]}
SearchState {sT = 9, sPos = [Position {pId = "GC", pDistance = 2},Position {pId = "VR", pDistance = 0}], sOpen = fromList ["AY","CB","GC","VR"]}
SearchState {sT = 11, sPos = [Position {pId = "QT", pDistance = 0},Position {pId = "VR", pDistance = 2}], sOpen = fromList ["AY","CB","GC","QT","VR"]}
SearchState {sT = 13, sPos = [Position {pId = "QT", pDistance = 2},Position {pId = "WT", pDistance = 0}], sOpen = fromList ["AY","CB","GC","QT","VR","WT"]}
SearchState {sT = 14, sPos = [Position {pId = "VG", pDistance = 0},Position {pId = "WT", pDistance = 1}], sOpen = fromList ["AY","CB","GC","QT","VG","VR","WT"]}
SearchState {sT = 17, sPos = [Position {pId = "QO", pDistance = 0},Position {pId = "WT", pDistance = 4}], sOpen = fromList ["AY","CB","GC","QO","QT","VG","VR","WT"]}
SearchState {sT = 17, sPos = [Position {pId = "QO", pDistance = 0},Position {pId = "SY", pDistance = 0}], sOpen = fromList ["AY","CB","GC","QO","QT","SY","VG","VR","WT"]}
SearchState {sT = 20, sPos = [Position {pId = "RT", pDistance = 0},Position {pId = "SY", pDistance = 3}], sOpen = fromList ["AY","CB","GC","QO","QT","RT","SY","VG","VR","WT"]}
SearchState {sT = 25, sPos = [Position {pId = "JS", pDistance = 0},Position {pId = "RT", pDistance = 5}], sOpen = fromList ["AY","CB","GC","JS","QO","QT","RT","SY","VG","VR","WT"]}
SearchState {sT = 26, sPos = [Position {pId = "JS", pDistance = 1},Position {pId = "XI", pDistance = 0}], sOpen = fromList ["AY","CB","GC","JS","QO","QT","RT","SY","VG","VR","WT","XI"]}
stack exec d16 < inputs/d16.txt  10.01s user 0.27s system 99% cpu 10.334 total

% time stack exec d16 < inputs/d16-test.txt
1651
SearchState {sT = 2, sPos = [Position {pId = "DD", pDistance = 0}], sOpen = fromList ["DD"]}
SearchState {sT = 5, sPos = [Position {pId = "BB", pDistance = 0}], sOpen = fromList ["BB","DD"]}
SearchState {sT = 9, sPos = [Position {pId = "JJ", pDistance = 0}], sOpen = fromList ["BB","DD","JJ"]}
SearchState {sT = 17, sPos = [Position {pId = "HH", pDistance = 0}], sOpen = fromList ["BB","DD","HH","JJ"]}
SearchState {sT = 21, sPos = [Position {pId = "EE", pDistance = 0}], sOpen = fromList ["BB","DD","EE","HH","JJ"]}
SearchState {sT = 24, sPos = [Position {pId = "CC", pDistance = 0}], sOpen = fromList ["BB","CC","DD","EE","HH","JJ"]}
SearchState {sT = 30, sPos = [Position {pId = "CC", pDistance = 0}], sOpen = fromList ["BB","CC","DD","EE","HH","JJ"]}
1707
SearchState {sT = 2, sPos = [Position {pId = "AA", pDistance = 2},Position {pId = "DD", pDistance = 0}], sOpen = fromList ["DD"]}
SearchState {sT = 3, sPos = [Position {pId = "DD", pDistance = 1},Position {pId = "JJ", pDistance = 0}], sOpen = fromList ["DD","JJ"]}
SearchState {sT = 7, sPos = [Position {pId = "HH", pDistance = 0},Position {pId = "JJ", pDistance = 4}], sOpen = fromList ["DD","HH","JJ"]}
SearchState {sT = 7, sPos = [Position {pId = "BB", pDistance = 0},Position {pId = "HH", pDistance = 0}], sOpen = fromList ["BB","DD","HH","JJ"]}
SearchState {sT = 9, sPos = [Position {pId = "CC", pDistance = 0},Position {pId = "HH", pDistance = 2}], sOpen = fromList ["BB","CC","DD","HH","JJ"]}
SearchState {sT = 11, sPos = [Position {pId = "CC", pDistance = 2},Position {pId = "EE", pDistance = 0}], sOpen = fromList ["BB","CC","DD","EE","HH","JJ"]}
SearchState {sT = 26, sPos = [Position {pId = "CC", pDistance = 2},Position {pId = "EE", pDistance = 0}], sOpen = fromList ["BB","CC","DD","EE","HH","JJ"]}
stack exec d16 < inputs/d16-test.txt  0.24s user 0.01s system 92% cpu 0.270 total
```
