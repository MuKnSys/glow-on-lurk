##  This is a list of hard-coded paths with descriptions that exist in the project at this time.
 All those directories have to be changed depending of their location on current machine before compilation of project. 

### ~/glow-on-lurk/glow/lib/Glow/Main.hs:  
` 51:  "home/user/glow/dapps/coin_flip.glow" ` : directory for example dapp included in glow

### ~/glow-on-lurk/glow/lib/Glow/Mock/Lurk/Consensus.hs:

` 203:  "lurkExecutable = "/home/user/lurk-rs/target/release/fcomm"` : directory for fcomm executable

`  218:  "replExecutable" = "/home/user/Desktop/lurk-rs/target/release/lurkrs"` : directory for lurk Repl executable

#### Temporary files used for evaluation (These files are created when the interaction is triggered)
Directory for tmp files should only be changed to another directory with read/write permissions allowed for this project. 


` 206:  "tempLurkSourceFile = "/tmp/tempLurkSourceFile.json" `: 
    temporary file with library code
    
` 209:  "tempClaimFile" = "/tmp/state-claim.json"  `: 
    temporary file with state-claim code

`    212:  "tempEvalFile" = "/tmp/state-eval.txt" ` : 
    temporary file with evaluated code

`    215:  "emitsFile" = "/tmp/emits.txt" ` :
    temporary file with evaluated code with additional data

  
### ~/glow-on-lurk/glow/lib/Glow/Mock/Lurk/Server.hs:

` 76:   "consensusStatePath" = "/tmp/LurkMockConsensusState.json" ` : 
    temporary file with current consensus state



