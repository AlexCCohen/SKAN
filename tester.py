import os
import re

print("**** Error tests ****")

result = []

for root, dirs, files in os.walk('./skan_tests/error/'):
    for file in files:
        if '.sk' in file and '.un' not in file:
            result.append(file)

for prog in result:
    base = prog.split('.')[0]
    inp, out, err = os.popen3("./run.sh ./skan_tests/error/" + prog)
    if err.read() != '':
        print('{:<34} {:<12}'.format(prog + " -->", "GOOD"))
    else:
        print('{:<34} {:<12}'.format(prog + " -->", "FAILED"))

result_good = []
print('')
print("**** Good tests *****")

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
    else:
        print('{:<34} {:<12}'.format(prog + " -->", "FAILED"))

