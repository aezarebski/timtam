# run.sh
#
# This must be run with bash!!!

NUM_SEEDS=50

Rscript src/generate-config-files.R 1 50


# stack exec -- simulation-study-aggregated-observations out/seed-1/config-1.json
# stack exec -- simulation-study-aggregated-observations out/seed-2/config-2.json
# stack exec -- simulation-study-aggregated-observations out/seed-3/config-3.json
# stack exec -- simulation-study-aggregated-observations out/seed-4/config-4.json
# stack exec -- simulation-study-aggregated-observations out/seed-5/config-5.json
# stack exec -- simulation-study-aggregated-observations out/seed-6/config-6.json
# stack exec -- simulation-study-aggregated-observations out/seed-7/config-7.json
# stack exec -- simulation-study-aggregated-observations out/seed-8/config-8.json
# stack exec -- simulation-study-aggregated-observations out/seed-9/config-9.json
# stack exec -- simulation-study-aggregated-observations out/seed-10/config-10.json

# stack exec -- simulation-study-aggregated-observations out/seed-11/config-11.json
# stack exec -- simulation-study-aggregated-observations out/seed-12/config-12.json
# stack exec -- simulation-study-aggregated-observations out/seed-13/config-13.json
# stack exec -- simulation-study-aggregated-observations out/seed-14/config-14.json
# stack exec -- simulation-study-aggregated-observations out/seed-15/config-15.json
# stack exec -- simulation-study-aggregated-observations out/seed-16/config-16.json
# stack exec -- simulation-study-aggregated-observations out/seed-17/config-17.json
# stack exec -- simulation-study-aggregated-observations out/seed-18/config-18.json
# stack exec -- simulation-study-aggregated-observations out/seed-19/config-19.json
# stack exec -- simulation-study-aggregated-observations out/seed-20/config-20.json

# stack exec -- simulation-study-aggregated-observations out/seed-21/config-21.json
# stack exec -- simulation-study-aggregated-observations out/seed-22/config-22.json
# stack exec -- simulation-study-aggregated-observations out/seed-23/config-23.json
# stack exec -- simulation-study-aggregated-observations out/seed-24/config-24.json
# stack exec -- simulation-study-aggregated-observations out/seed-25/config-25.json
# stack exec -- simulation-study-aggregated-observations out/seed-26/config-26.json
# stack exec -- simulation-study-aggregated-observations out/seed-27/config-27.json
# stack exec -- simulation-study-aggregated-observations out/seed-28/config-28.json
# stack exec -- simulation-study-aggregated-observations out/seed-29/config-29.json
# stack exec -- simulation-study-aggregated-observations out/seed-30/config-30.json

# stack exec -- simulation-study-aggregated-observations out/seed-31/config-31.json
# stack exec -- simulation-study-aggregated-observations out/seed-32/config-32.json
# stack exec -- simulation-study-aggregated-observations out/seed-33/config-33.json
# stack exec -- simulation-study-aggregated-observations out/seed-34/config-34.json
# stack exec -- simulation-study-aggregated-observations out/seed-35/config-35.json
# stack exec -- simulation-study-aggregated-observations out/seed-36/config-36.json
# stack exec -- simulation-study-aggregated-observations out/seed-37/config-37.json
# stack exec -- simulation-study-aggregated-observations out/seed-38/config-38.json
# stack exec -- simulation-study-aggregated-observations out/seed-39/config-39.json
# stack exec -- simulation-study-aggregated-observations out/seed-40/config-40.json

# stack exec -- simulation-study-aggregated-observations out/seed-41/config-41.json
# stack exec -- simulation-study-aggregated-observations out/seed-42/config-42.json
# stack exec -- simulation-study-aggregated-observations out/seed-43/config-43.json
stack exec -- simulation-study-aggregated-observations out/seed-44/config-44.json
# stack exec -- simulation-study-aggregated-observations out/seed-45/config-45.json
# stack exec -- simulation-study-aggregated-observations out/seed-46/config-46.json
# stack exec -- simulation-study-aggregated-observations out/seed-47/config-47.json
# stack exec -- simulation-study-aggregated-observations out/seed-48/config-48.json
# stack exec -- simulation-study-aggregated-observations out/seed-49/config-49.json
# stack exec -- simulation-study-aggregated-observations out/seed-50/config-50.json

Rscript src/plotter.R $NUM_SEEDS
