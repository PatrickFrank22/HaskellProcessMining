# Haskell & Process Mining

This uploaded project was created as part of the bachelor thesis "Haskell & Process Mining".
In particular, it consists of an implementation of the Inductive Miner and Inductive Miner - Infrequent algorithms, as well as two XES and one CSV parser.
The XES parsers are based on libraries xml-conduit and hxt.
The CSV parser is based on library cassava.

All implemented functions related to the Inductive Miner and Inductive Miner - Infrequent as well as the XES and CSV parsers are explained in *Patrick Frank. Haskell & Process Mining, 2024*.
The thesis also shows or argues about the correctness of the Inductive Miner implementations.


# Project Setup

Once the folder has been downloaded from GitHub, and GHC and cabal have been installed, please follow these steps:

1. Select the "app" directory within the downloaded folder in the console.

To install the necessary libraries, run the following commands: 

2. cabal update
3. chmod +x install_libraries.sh 
3. ./install_libraries.sh


# Compile and run the application: 

The application can be compiled and run using the following commands:

1. ghc -O -rtsopts -o haskellApp Main.hs -package base -package time -package bytestring -package filepath -package yesod-form 
2. ./haskellApp +RTS -sstderr


All further information and more detailed explanations can be found in the thesis.


# Additional information:

This project is current as of 19 February 2024 (date of thesis submission).
Changes or improvements made since then can be found in Project "HaskellProcessMiningTool".
