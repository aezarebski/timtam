"""

This script evaluates the likelihood for a range of values timing it in the
process to investigate the time complexity.

$ source venv/bin/activate
$ python timing.py demo-data.json demo-output.json demo-config.jso

"""

import algo1

# Second subplot
# Comparison to the density values when r = 1
# lista = [7, 6, 4, 3]            # births
# listb = []
# listc = []
# listd = [2]                     # sampling
# liste = []
# listf = [5, 1]                  # occurrence
# obs = [lista, listb, listc, listd, liste, listf]

import sys
import json
import timeit
import os

input_json = sys.argv[1]
output_json = sys.argv[2]
config_json = sys.argv[3]

if os.path.isfile(config_json):
    with open(config_json) as config_data:
        foo = json.load(config_data)
        lamb, mu, psi, rhoPairs, omega, uPairs = foo['acSimParams']['mpParameters']
        assert rhoPairs.count(rhoPairs[0]) == len(rhoPairs)
        rho = rhoPairs[0][1]
        r = 1                   # this is the removal probability always fixed to one.
        params = lamb, mu, rho, psi, r, omega
else:
    raise FileNotFoundError


print(input_json)
with open(input_json) as data_json:
    input_data = json.load(data_json)
    obs = [input_data["OBirth"], [], [], input_data["ObsUnscheduledSequenced"], [], input_data["OOccurrence"]]

# params
distinguishRemoval = True


prev_llhd = -1e6
curr_llhd = -1e4
has_converged = False
iter_count = 0
truncation_param = 10
truncation_delta = 5

while iter_count < 15 and (not has_converged):
    truncation_param = truncation_param + truncation_delta
    iter_count = iter_count + 1
    prev_llhd, curr_llhd = curr_llhd, algo1.logDensity( obs, params, distinguishRemoval, truncation_param)
    has_converged = abs(prev_llhd - curr_llhd) < abs(1e-3 * prev_llhd)

print("did it converge?")
print(has_converged)
print(prev_llhd,curr_llhd)
print(truncation_param)

if has_converged:
    num_replicates = 10
    eval_time = timeit.timeit('algo1.logDensity( obs, params, distinguishRemoval, truncation_param)',
                              globals=globals(),
                              number = num_replicates)
    result = {
        "inputJson": input_json,
        "hasConverged": has_converged,
        "truncationParameter": truncation_param,
        "convergedLlhd": curr_llhd,
        "numReplicates": num_replicates,
        "evaluationTime": eval_time
    }
    with open(output_json, 'w') as output_file:
        json.dump(result, output_file)

