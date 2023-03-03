
# Glow on Lurk interface
### Demo 
[![Demo](https://i.imgur.com/zCzzUNT.jpg)](https://vimeo.com/799928074) 
### The core of implementation consists of
- Lurk runtime library - It can be common for all Glow contracts deployed to the Lurk backend or optimized on a contract basis by including only functions relevant to the contract. This runtime library contains only pure functions which cannot modify the environment in any way.All functions in the runtime library are intended to be provably terminating. The library will consist of:

  - constructors for monadic ADT representing consensus code
  - definitions for built-in glow function
  - interpreter for validating state transitions
- Glow compiler backend targeting Lurk - For each Glow contract,  it will produce a representation of consensus code in the form of monadic ADT, where expressions can use only a set of functions declared in the Lurk runtime library.

### General Architecture
* There is a trusted consensus that can store shared state and execute transactions according to the results of running zk-snark verifiers.
* Each participant can run his local code and produce calls to consensus; each call must be sent together with proof produced by feeding appropriate input to zk-snark.
* This model does not rely on Lurk to obfuscate data from the rest of the participants. It reduces computation and storage workload at the consensus layer by allowing consensus to just verify proof of correctness of the state transition instead of executing consensus code to produce a new state. 

### Compiler requirements
* Accepts file with glow source code as a parameter.
* Produces container with
    * metadata about contract
      * list of participants
      * list of parameters together with types
      * list of used tokens
  * local code for participants
  * lurk consensus code

### Compiler implementation
  * project pass will extract consensus code from contract code in ANF
  * consensus code will be translated into monadic representation specific for lurk backend

### Representation of Glow consensus code 

For execution as Lurk, Glow consensus code is translated into monadic form as an algebraic datatype. On the compiler side, bindings are represented using the first-order abstract syntax (FOAS), and later - during the final translation to Lurk values, we introduce lambdas for every bind, so the Lurk interpreter is operating on higher-order abstract syntax (HOAS). Since FOAS allows symbolic reasoning about code, we chose it on the compiler side to fine-tune scoping rules and prepare the Lurk backend for later optimizations, this choice will also impact the feasibility of future proof of compiler correctness.

On the other hand, interpreting HOAS in the Lurk interpreter lets us rely more on the Lurk compiler in terms of memory management, allowing us to use a very simple interpreter. In terms of memory management, we rely sorely on the Lurk compiler/execution model. We perform only symbolic execution without implementing any sort of VM.

Monadic representation of consensus code on the compiler side uses the following constructors (excerpt of code from our exploratory Agda implementation developed for modeling purposes):

```
data Action (Γ : Context) : Type₀ where
     withdrawA : Expr Γ Nat → Action Γ
     depositA : Expr Γ Nat → Action Γ


data LMonad (Γ : Context) (Τ : GType) : Type where
     action : I → PID → Action Γ → Τ ≡ → LMonad Γ Unitᵍ 
     require : Expr Γ Bool → LMonad Γ Unitᵍ
     expectPub : I → PID → LMonad Γ Τ
     bind : ∀ {x Τ'} → LMonad Γ Τ' → LMonad (Γ , x ⦂ Τ') Τ → LMonad Γ Τ
     next : ∀ {Τ'} → LMonad Γ Τ' → LMonad Γ Τ → LMonad Γ Τ
     pure : Expr Γ Τ → LMonad Γ Τ
     branch : Expr Γ Bool → LMonad Γ Τ → LMonad Γ Τ → LMonad Γ Τ
```
Expr type can only contain pure expressions:
* variables declared in a particular context
* literals
* applications of pure functions
* conditionals assuming that condition and both branches are pure

It can be noted that since on the compiler side we are using FOAS, the LMonad type is not monad in terms of compiler implementation language.

Translation of values of this type into Lurk expression is straightforward since glow-lurk runtime library implements constructors:
```
      (action (lambda (i pid a) (cons 'action (cons i (cons pid (cons a ()))))))

     (withdraw (lambda (n) (cons 'withdraw (cons n ()))))
     (deposit (lambda (n) (cons 'deposit (cons n ()))))
     (publish (lambda (i pid) (cons 'publish (cons i (cons pid ())))))
     (require (lambda (v) (cons 'require (cons v ()))))

     (mk-pure (lambda (a) (cons 'pure (cons a ()))))
     (mk-bind (lambda (a b) (cons 'bind (cons a (cons b ())))))
     (mk-next (lambda (a b) (cons 'next (cons a (cons b ())))))

```
The recursion principle `atomic-action-rec` is also included in the library.

Bindings are implemented via lambdas, which makes this HOAS representation. Since expressions were pure, we can expect them to evaluate to atomic values of Glow types when enclosing lambda is evaluated. This explains the absence of the branch constructor from lurk representation, the branch constructor of compiler representation is translated to lurk conditional, and the condition will always be evaluated to t or nil before the value will be fed to the recursor. Well-scopeness of the resulting code and the appropriate type of expressions are ensured by the LMonad type.

For the next iteration, we consider the representation of consensus code via Kleisli arrows instead of the current Monadic representation, to better express invariants specific to this backend.

### Participant runtime requirements
* Accepts container produced by the compiler.
* Let the user assign identities to roles in the contract.
* Deploys contract to the consensus layer.
* Interprets the local code of the participant
  * interactively asks the user for input when needed
  * produces necessary calls for consensus, together with appropriate proofs
  * sends calls to the consensus layer
  * query consensus layer to get updates on consensus state

### Participant runtime implementation
* Participant runtime will be implemented in the same executable as the compiler. To use it, the user will need to call the compiler with an additional flag while supplying the filename of the container with the compiled contract.
* It will call an external process to generate necessary proofs (this process, for prototyping and testing, will be producing only mock proofs, and later, when needed, this external executable can be swapped to produce actual proofs)
* Consensus will be called via HTTPS requests.
* Participant's runtime will be relatively free of mockups. 
* In order to integrate Glow on Lurk backend into the Filecoin network, it should be sufficient to:
  * switch to different external tools for proof generation
  * provide hostname for actual API
  * make necessary adjustments to the final version of API (but in principle, functionalities should be analogical
  * implement authorization specific to filecoin


### Consensus requirements
* Implements account model.
* Can host contract, meaning that for each contract, there is a record of
  * identities of participants of the contract
  * values of parameters
  * zk-snark verifier
  * state of the contract 
    * list of values published by the users
    * label of the current state (int)
* Allows users to deploy contracts by posting necessary information.
* Accepts calls for interacting with contract
  * internally combines: the state of the contract and value of the call to produce input for the verifier
  * checks proof attached to call against provided arguments. 
  * only if verification succeeded, depending on the data in the call, always performs one of:
    * adds value to the list of published
    * executes a deposit of funds to contract
    * executes a withdraw of funds from the contract
* Function for updating state and producing input for verifier are not contract specific; they are not relying on any metadata during the execution of the contract. Allowed state transition and funds transfers are determined solely by the result of the verification process.
* The simplest model (which we will implement initially) assumes that no state is stored inside zk-snark between calls. Each call is processed independently from the perspective of the verifier, both storing of contract state and authorization can be in the future handled by zk-snark by placing the interpreter inside a simple wrapper. This issue will need to be addressed during future deployment into the Filecoin network. But will not impact compiler architecture or local runtime. (but may require additional off-chain communication between participants)

### Consensus implementation - simulated
For the purpose of simulating the Lurk backend, we will implement consensus as a web service.

It will provide API endpoints for:
* contract deployment
* querying the state of the deployed contract
* sending calls to contract
* querying the balance of accounts
The state of the mock account model (identities, tokens, balances) will be initialized by providing a path to a file with necessary data at the startup of the process. There will be also an endpoint to override the state of the account model. 
To validate proofs, it will call an external process. 

For development purposes, It will be possible to initialize simulated consensus in three modes, with the ascending levels of simulation faithfulness:
* in which it will ignore proofs completely and will accept mock data in place of zk-snark verifier, in this mode it will accept any state change uncritically, allowing users to perform arbitrary actions, not necessary permitted by contract 
* in which it will require lurk code in place of zk-snark verifier during deployment, and will just execute lurk code with appropriate input using rust Lurk implementation. If the interpreter function returned the correct value it will change the state of the contract or perform fund transfer according to call. In this mode, consensus will accept only valid state transitions and funds transfers.
* In the third mode deployment of the contract will require attaching zk-snark verifier, and consensus will call appropriate external process. state transitions will be verified by verifying proof against the input.
Implementing Mock consensus into a separate web server has the benefit of
* Clearly separating requirements set on different parts of the architecture 
* After deployment of Lurk infrastructure on the Filecoin network it will be sufficient to provide a similar API.
* Ability to deploy online and perform distributed tests or more elaborate demonstrations for marketing or training purposes.
### Future consensus implementation - Filecoin network
The proposed simulated environment will validate and provide a demonstration of the entire process consisting of the development, compilation, and execution of the Glow contract on Lurk.

For the Filecoin network, to provide runtime for contracts developed using Glow on Lurk compiler, it will be sufficient to implement functionalities listed in the “consensus requirements” section. And assuming that required functionality will be provided, after necessary adjustment to the API developed compiler infrastructure should be in principle ready for integration. While solving technical problems in that matter is outside of the scope of the current project, we want to note example architectures to implement this.

We expect the Filecoin team/decision to choose the correct approach. It is definitely not necessary to select one of those approaches during the current project since currently, we are targeting only the simulated backend. But it might be helpful to discuss the feasibilities of those example ideas:

#### 1. Implementation via vanilla Filecoin Virtual Machine, without native zk-snark support
Assuming that the zk-snark verifier can be implemented directly on FVM, we can develop a “host” contract that would store necessary state data, and lock participants found during the execution of the contract. This approach assumes no changes to the Filecoin network but relies on the availability of FVM.

#### 2. Implementation via Filecoin Virtual Machine, with zk-snark support
We can imagine that FVM may be extended with primitives to store zk-snark verifiers, and native instructions to verify proofs. The host FVM contract would still contrain glue code for storing contract state, preparing inputs for verifier, holding funds, and providing an endpoint for calling state transitions.

#### 3. Native support for GlowOnLurk in the Filecoin network
Described functionalities could be implemented independently from FVM, by implementing endpoints for deployment, calling, and querying Glow contracts as functionalities of Fielcoin nodes.
