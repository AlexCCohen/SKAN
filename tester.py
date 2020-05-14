import os
import re

print("\n--------------- TESTING ---------------\n")
print("**** Error tests ****\n")

result = []
good = 0
bad = 0

for root, dirs, files in os.walk('./skan_tests/error/'):
    for file in files:
        if '.sk' in file and '.un' not in file:
            result.append(file)

for prog in result:
    base = prog.split('.')[0]
    inp, out, err = os.popen3("./run.sh ./skan_tests/error/" + prog)
    if err.read() != '':
        print('{:<34} {:<12}'.format(prog + " -->", "GOOD"))
        good += 1
    else:
        print('{:<34} {:<12}'.format(prog + " -->", "FAILED"))
        bad +=1 

result_good = []
print("\n**** Good tests *****\n")

for root, dirs, files in os.walk('./skan_tests/'):
    for file in files:
        if '.sk' in file and '.un' not in file and file not in result:
            result_good.append(file)

for prog in result_good:
    base = prog.split('.')[0]
    inp, out, err = os.popen3("./run.sh ./skan_tests/" + prog +
                      "&& ./skan_tests/" + base + ".out")
    if err.read() == '':
        print('{:<34} {:<12}'.format(prog + " -->", "GOOD"))
        good +=1
    else:
        print('{:<34} {:<12}'.format(prog + " -->", "FAILED"))
        bad += 1

print("\n---------------------------------------\n")
print("Passed: " + str(float(good)/float(good+bad)*100) + r"%")
print("Failed: " + str(float(bad)/float(good+bad)*100) + r"%")
print("\n------------ TESTING FINISHED ---------\n")

