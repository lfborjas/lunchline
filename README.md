# LunchLine


Lil' command line utility -- Persistent/Esqueleto learning project.

## Usage

### Set/show settings

``` sh
cabal run lunchline-exe -- configuration
+--------+---------------+
| Budget | Start Sunday? |
+--------+---------------+
| 100.0  | False         |
+--------+---------------+
```

``` sh
> cabal run lunchline-exe -- configure --budget 10
Up to date
+--------+---------------+
| Budget | Start Sunday? |
+--------+---------------+
| 10.0   | False         |
+--------+---------------+
```

### List/add meal

Will aggregate, and show, entries for the current week.

``` sh
> cabal run lunchline-exe -- list
Up to date
Remaining Budget 3.0
+------------+--------+------------+
| Name       | Amount | Added      |
+------------+--------+------------+
| Chicharron | 7.0    | 2022-04-04 |
+------------+--------+------------+
```

``` sh
> cabal run lunchline-exe -- add --name Chicharron --amount 7.0 --added 2022-04-04
Up to date
Remaining Budget 81.0
+------------+--------+------------+
| Name       | Amount | Added      |
+------------+--------+------------+
| Pernil     | 6.0    | 2022-04-07 |
| Pernil     | 6.0    | 2022-04-06 |
| Chicharron | 7.0    | 2022-04-04 |
+------------+--------+------------+
```
