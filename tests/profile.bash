#!/bin/bash

function print_usage {
    echo "Usage: $0 <path_to_tests>"
    echo "Example: $0 lice_program.lice"
}


if [ $# -eq 0 ]; then
    echo "[Profiler] No arguments provided. Please provide the path to the tests."
    print_usage
    exit 1
fi

echo "[Profiler] Running dune exec command..."

# Start the application in the background
dune exec lice $1 &

# Capture the PID of the last background process started
APP_PID=$!

# Wait for the application to fully start
sleep 2

echo "[Profiler] Profiling using gdbprofiler..."

# Run gdbprofiler with the PID
gdbprofiler -p $APP_PID --cpuprofile example.cpuprofile --callgrind callgrind.out