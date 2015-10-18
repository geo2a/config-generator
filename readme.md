# Generic config generator

This piece of software is designed for generating json configurations files for 
R scripts, which will be executed on h2o-cluster.

## Building

1. Clone this repository.
```
git clone https://github.com/geo2a/config-generator.git
```

2. Enter the project directory 
```
cd config-generator
```

3. Initialize new cabal sandbox 
```
cabal sandbox init
```

4. Install dependencies
```
cabal install --only-dependencies
``` 

5. Build 
```
cabal build
```

Resulting executable will be in dist/build/hs-config-gen directory

## Usage

If you have project sources and configured cabal sandbox, then type
```
cabal run ranges.json
```

If all you have is just an executable: 
```
cfg-gen ranges.json
``` 

### Input

User must specify ranges for params in json config file. For example, 
ranges of params for h2o.randomForest procedure: 

```
{
  "max_after_balance_size_range": [0.1],
  "min_rows_range": [2],
  "nbins_cats_range": [2],
  "xs_range": [[1]],
  "max_depth_range": [3],
  "learn_rate_range": [0.1],
  "balance_classes_range": [true],
  "nfolds_range": [0],
  "nbins_range": [2],
  "score_each_iteration_range": [false],
  "ntrees_range": [5],
  "y_range": ["y"]
}
``` 

### Output

Subdirectory output will contain config files with all possible combinations of 
params from ranges and some additional information, which is for now hardcoded.

Example of resulting file: 
```
{
    "inputParams": {
        "dataFilename": "data/trEmpt.csv"
    },
    "outputParams": {
        "paramsFileName": "params.json",
        "confMatrixFileName": "confMatr.csv",
        "msePlotFileName": "plot.png"
    },
    "methodParams": {
        "max_after_balance_size": 0.1,
        "min_rows": 2,
        "nbins_cats": 2,
        "xs": [
            1
        ],
        "max_depth": 3,
        "learn_rate": 0.1,
        "balance_classes": true,
        "nfolds": 0,
        "nbins": 2,
        "score_each_iteration": false,
        "ntrees": 5,
        "y": "y"
    }
}
```