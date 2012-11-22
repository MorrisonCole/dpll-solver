#!/bin/bash

if [ $# != 1 ]
then
    echo "Usage:"
    echo "./dpll_test_script <maximum number of clauses you want to read per test (higher number means slower execution)>"
    exit 0
fi

echo -e "## Running DPLL Tester ##\n"
echo -e "The following tests should return 'satisfiable':\n"

expectedResult="satisfiable"
failedTests=0

for i in {1..10}
do
    echo "Test #$i" 
    
    result=`head -n $1 tests/satisfiable/sat_$i.txt | ./dpll`
    echo "Returned: $result"
    echo "Expected: $expectedResult"
    
    if [ "$result" = "$expectedResult" ]
    then
        echo -e "> Correct\n"
    else
        echo -e "> Incorrect\n"
        failedTests=`expr $failedTests + 1`
    fi
done

echo -e "The following tests should return 'unsatisfiable':\n"

expectedResult="unsatisfiable"

for i in {1..10}
do
    echo "Test #$i" 
    
    result=`head -n $1 tests/unsatisfiable/nsat_$i.txt | ./dpll`
    echo "Returned: $result"
    echo "Expected: $expectedResult"
    
    if [ "$result" = "$expectedResult" ]
    then
        echo -e "> Correct\n"
    else
        echo -e "> Incorrect\n"
        failedTests=`expr $failedTests + 1`
    fi
done

echo -e "Failed tests: $failedTests"
