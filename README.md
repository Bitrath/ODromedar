# ODromedar 
_MSc. Cybersecurity (UniPi) Course: Language-Based Technology for Security_

ODromedar is an interpreter for a simple functional language, designed with specialized primitive abstractions to protect program execution from untrusted code. Written primarily in OCaml, this project focuses on implementing robust security mechanisms such as dynamic taint analysis, trusted enclaves, and safe plugin execution.

## Repository File Structure
The repository is structured as follows:

* _**HW1/**_ - The main directory containing the project source code and tests.
  * _**lib/**_ - Contains the core implementation files of the interpreter.
    * _ast.ml_: Defines the Abstract Syntax Tree (AST), formalizing the language's syntax. It handles security models including trust levels (Trusted, Untrusted, BlockLvl) and confidentiality tags (Public, Private) to handle secret abstractions.
    * _environment.ml_: Defines the evaluation environment type (evT) and hosts utility functions such as lookup, clean_lookup, and taint_lookup to check variables and their taint statuses.
    * _interpreter.ml_: The core of the project, containing the central eval() function that evaluates expressions dynamically while taking the environment, taint value, and trust level into account.
  * _**test/**_ - Contains the testing suite to validate the interpreter.
    * _test-HW1.ml_: Utilizes the OUnit2 testing framework to run an extensive suite of 39 custom tests, including scenarios for "Tainted Plugin Execution" and a "Password Checking" simulation.

## Key Features & Security Abstractions
* **Trusted Blocks (TrustedBlock)**: Allows the creation of local, secure enclaves. Variables defined inside a Trusted Block are marked as Private by design and evaluated at a secure BlockLvl.
* **Handle Construct (Handle & HandleCall)**: The only entry-point mechanism to interact with a TrustedBlock from the external environment. The HandleCall constructor allows external code to execute the exposed block function safely without exposing the internal logic.
* **External Plugins (Include & Execute)**: Provides the ability to import external plugins using the Include constructor. In compliance with strict security specifications, the plugin code is treated as tainted and kept uninspected until it is actively called and evaluated via the Execute constructor.
* **Dynamic Taint Analysis**: Built-in taint assertions block evaluations if tainted data unexpectedly flows into secure areas, such as the body of a TrustedBlock or a Handle function.

## Technologies Used
* **Languages**: OCaml (99.2%), Makefile (0.8%)
* **Testing Framework**: OUnit2

## Authors
* Nicolò Zarulli @[Bitrath](https://github.com/Bitrath)
* Luca Cremonese @[baylonp](https://github.com/baylonp)
* Nicola Cavaletti @[nicolacava01](https://github.com/nicolacava01)
* Davide Di Rocco @[Davide-Di-Rocco-88](https://github.com/Davide-Di-Rocco-88)


