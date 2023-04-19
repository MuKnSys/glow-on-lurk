# glow-on-lurk-tests 
### How to run test
1.  First time run:
To install all dependecies execute `./install.sh`.

2. If you are using different version of dependencies you might have to change the directories in the `glow-on-lurk/config.js` . 

3. Run single test:
To run single test case just type into terminal `mocha {test_name}`

4. Running all test cases:
To run all test cases just type into terminal `mocha *test.js`

5. Depending on your hardware, you might have to change timeout value while running test cases. `mocha *test.js --timeout 5000`