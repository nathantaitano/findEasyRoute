# findEasyRoute

Finds the easiest hiking route between two points by minimizing calories burned, using only elevation data.

Functions in findEasyRoute implement pathfinding algorithms to find least-kcal paths between defined start and endpoints, relying on elevation GIS data and grade-based calorie expenditure formulae from the ACSM + other sources (cited where used).

As of 2017-12-14, it uses Dijkstra's algorithm, takes XYZ elevation data as inputs, and has been tested on 30m resolution data.
