import os
import re

result = []
count = 0

for root, dirs, files in os.walk('./skan_tests/'):
    for file in files:
        if '.sk' in file and '.un' not in file:
            result.append(file)

for prog in result:
    base = prog.split('.')[0]
    stream = os.popen("./run.sh ./skan_tests/" + prog +
                      "&& ./skan_tests/" + base + ".out")
    output = stream.read()
    if output == '':
        print(prog + ": GOOD")
    #else:
        #print(prog + ": " + output)