#!/bin/bash

echo "[Profiler] Running dune exec command..."

# Start the application in the background
dune exec lice ~/Code/Lice/tests/ &

# Capture the PID of the last background process started
APP_PID=$!

# Wait for the application to fully start
sleep 2

echo "[Profiler] Profiling using gdbprofiler..."

# Run gdbprofiler with the PID
gdbprofiler -p $APP_PID --cpuprofile prof.cpuprofile --callgrind callgrind.out
