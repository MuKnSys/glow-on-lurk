# Installing Glow
In order to compile and run the Glow files we need to install the Glow language, to do this we simply need to go to the Glow repository on GitHub and go through all the steps in the installation instructions. Once properly installed, Glow should be available globally for use. After installation to check if Glow has been installed correctly we can simply type `glow help` in our terminal and check the list of options available for use.

# Installing lurk-rs 
1. Install Rust. If you don't have it already, install Rust programming language by following instructions on the official website (https://www.rust-lang.org/tools/install).

2. Clone the lurk-rs repository using the following command in your terminal:
`$ git clone git@github.com:lurk-lang/lurk-rs.git`

3. Change to the lurk directory and build the project using the following command:

`$ cargo build` 

now go to:  
`lurk-rs/fcomm/bin/`

and tnem `fcomm.rs` here we need to simply find and edit limit of iteractions 
```    
    /// Iteration limit
    #[allow(deprecated)]
    #[clap(short, long, default_value = "1000", value_parser)]
    limit: usize,
```
and set the `default_value` to `"10000"`

after changing this value we can go to `lurk-rs/bin` and execute `lurkrs` file. 

4.Clone the glowOnLurk repository using the following command in your terminal:

`git@github.com:MuKnIO/glowOnLurk.git `


after that you can go into project directory and simply run:  `cabal build –ghc-option=-w all `

#### Starting Server

To start the server after building it we have to go in to the directory where it has been created, by default it will be in: 
`../glowOnLurk/dist-newstyle/build/{system_architecture_version}/{ghc_version}/glow-0.1.0.0/x`
from this place you can choose between three different files. To start a mock server you have to go in `lurk-consensus-mock/build/lurk-consensus-mock` directory and simply execute the `lurk-consensus-mock` file. If the server is running correctly it will show a short notification in the terminal which also informs on which port the server is currently listening.


#### Updating state before deploy

To correctly deploy the contract we need to load demo data from the repository using the `/demo/loadJM` endpoint. This endpoint will overwrite the state included in our LurkMockConsensusState.json, after that we can use the `/state` endpoint to check if our state has been changed correctly.


To start interaction the first thing you have to make is to create and deploy contract to your simulated environment, the way to do this is by going to
`/glow2-haskell/dist-newstyle/build/{system_architecture_version}/{ghc_version}/glow-0.1.0.0/x/run-glow/build/run-glow `
and execute a `run-glow` file. There are two separate ways which we can execute the `run-glow` file. 
First way is by using `deploy` which allows users to create, configure and deploy a new contract to their simulated consensus. To correctly deploy a contract we need to execute a run-glow file with two parameters. The first one is  `deploy` because we want to deploy our contract so we have to define our intentions. As the second parameter we have to indicate the path to one of the example contrats. For example, deploying a coinFlip contract looks like this: 
`./run-glow deploy ~/Desktop/glow/dapps/coin_flip.glow`
Those example contracts are included in the glow language repository. If everything is correct the program enters into interactive mode, which allows the user to provide relevant parameters. 

Coinflip example requires from user do provide:
* The value of wager amount parameter,
* The value of escrow amount,
* Public key for role A,
* Public key for role B.

After giving the required parameters the contract will be deployed on a simulated server. In response, the server will return the generated contract id. 

#### Starting Interaction

If our contract has been deployed correctly we will get a contract id in return. To enter the simulation of interaction mode between two users we have to execute the run-glow file using different parameters. In deploy mode we’ve been declaring “deploy” as our intention and the path to the example contract file. To enter the interaction mode we have to declare our intention as “interact”. Also we have to remember to provide a path to the same example contract as in the deploy process. Executing the run-glow file to enter interactive mode for coinFlip example contract will look like this: 
`./run-glow  interact  ~/Desktop/glow/dapps/coin_flip.glow`

To perform interaction we have to run two different terminals. They will allow us to simulate interaction between two users on the consensus, in both terminals we use the execute command. After correctly entering the interactive mode, in both terminals program will ask for the contract address, this is a moment where we have to put the result of deploying process of our contract, for example: 

`enter contract address:`
`ffd767dd-6714-499a-b887-379a7d4e960c`

After that the program will also ask for required parameters. We have to provide the values of the `wager amount` and `escrow amount `parameter, they have to be the same as those we put in the deploying process. For example:

#### Example interaction terminal A:

```
enter contract address:
ffd767dd-6714-499a-b887-379a7d4e960c

enter privKey:

Jan

enter your role:

A

Enter value of param wagerAmount (GLNatT):

10

Enter value of param escrowAmount (GLNatT):

2
```
#### Example interaction terminal B:
```
enter contract address:
ffd767dd-6714-499a-b887-379a7d4e960c

enter privKey:

Marcin         
                                        
enter your role:

B

Enter value of param wagerAmount (GLNatT):

10

Enter value of param escrowAmount (GLNatT):

2
```


After each step of our interaction we can check if the state is changing correctly by using the /state endpoint. If the interaction has been completed, the program will close and leave us in the terminal. 





