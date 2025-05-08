#!/usr/bin/env bash
#
# test-runner.sh
# Runs all CAPybara tests in ./tests
#  - Tests named fail-* must print an error starting with "Fatal error:" and exit non-zero
#  - All other tests must compile successfully; we run them and then delete the generated binary

TEST_DIR="tests"

for test_path in "$TEST_DIR"/*; do
  test_name=$(basename "$test_path")
  base="${test_name%.cap}"    # strip the .cap suffix

  if [[ "$test_name" == fail-* ]]; then
    echo "=== $test_name (expected failure) ==="
    output=$(./capybara.native -c "$test_path" 2>&1)
    code=$?
    if [[ $code -eq 0 ]]; then
      echo "FAIL: $test_name: expected failure but compilation succeeded"
    elif [[ $output == Fatal\ error:* ]]; then
      echo "PASS: $test_name failed as expected"
    else
      echo "FAIL: $test_name did not print 'Fatal error:'"
      echo "Output was:"
      echo "$output"
    fi

  else
    echo "=== $test_name (expected success) ==="
    compile_out=$(./capybara.native -c "$test_path" 2>&1)
    compile_code=$?
    if [[ $compile_code -ne 0 ]]; then
      echo "FAIL: $test_name: compilation failed:"
      echo "$compile_out"
      continue
    fi

    # run the freshly-generated program (named $base)
    echo "Running $base"
    lli "$base" > /dev/null 2>&1
    echo "PASS: $test_name"

    # clean up
    rm -f "$TEST_DIR"/"$base"
  fi
done
