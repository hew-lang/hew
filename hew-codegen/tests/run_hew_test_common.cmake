function(_hew_execute_capture)
  cmake_parse_arguments(PARSE_ARGV 0 ARG "" "RESULT_VAR;OUTPUT_VAR;ERROR_VAR;TIMEOUT" "COMMAND")
  if(ARG_TIMEOUT)
    execute_process(
      COMMAND ${ARG_COMMAND}
      RESULT_VARIABLE result
      OUTPUT_VARIABLE out
      ERROR_VARIABLE err
      TIMEOUT ${ARG_TIMEOUT}
    )
  else()
    execute_process(
      COMMAND ${ARG_COMMAND}
      RESULT_VARIABLE result
      OUTPUT_VARIABLE out
      ERROR_VARIABLE err
    )
  endif()

  set(${ARG_RESULT_VAR} "${result}" PARENT_SCOPE)
  set(${ARG_OUTPUT_VAR} "${out}" PARENT_SCOPE)
  set(${ARG_ERROR_VAR} "${err}" PARENT_SCOPE)
endfunction()

function(_hew_load_expected EXPECTED_TEXT_VAR)
  if(DEFINED EXPECTED_FILE)
    file(READ ${EXPECTED_FILE} expected_text)
  elseif(DEFINED EXPECTED)
    string(REPLACE "\\n" "\n" expected_text "${EXPECTED}")
  else()
    message(FATAL_ERROR "Expected either EXPECTED or EXPECTED_FILE to be defined")
  endif()

  set(${EXPECTED_TEXT_VAR} "${expected_text}" PARENT_SCOPE)
endfunction()

function(_hew_assert_output_matches ACTUAL_TEXT EXPECTED_TEXT)
  if(NOT "${ACTUAL_TEXT}" STREQUAL "${EXPECTED_TEXT}")
    message(FATAL_ERROR "Output mismatch:\nExpected:\n${EXPECTED_TEXT}\nGot:\n${ACTUAL_TEXT}")
  endif()
endfunction()

function(_hew_assert_compile_rejected SUCCESS_MESSAGE)
  if(compile_result EQUAL 0)
    message(FATAL_ERROR
      "${SUCCESS_MESSAGE}\n"
      "Program: ${HEW_FILE}\n"
      "Expected error containing: ${EXPECTED_ERROR}")
  endif()

  string(FIND "${compile_err}" "${EXPECTED_ERROR}" error_pos)
  if(error_pos EQUAL -1)
    string(FIND "${compile_out}" "${EXPECTED_ERROR}" error_pos2)
    if(error_pos2 EQUAL -1)
      message(FATAL_ERROR
        "Compilation failed but error message does not contain expected text.\n"
        "Expected: ${EXPECTED_ERROR}\n"
        "Got stderr:\n${compile_err}\n"
        "Got stdout:\n${compile_out}")
    endif()
  endif()
endfunction()

function(hew_run_e2e_test)
  cmake_parse_arguments(PARSE_ARGV 0 ARG
    ""
    "COMPILE_FAILURE_MESSAGE;RUN_FAILURE_MESSAGE;RUN_TIMEOUT"
    "COMPILE_COMMAND;RUN_COMMAND")

  _hew_execute_capture(
    COMMAND ${ARG_COMPILE_COMMAND}
    RESULT_VAR compile_result
    OUTPUT_VAR compile_out
    ERROR_VAR compile_err
  )
  if(NOT compile_result EQUAL 0)
    message(FATAL_ERROR "${ARG_COMPILE_FAILURE_MESSAGE}:\n${compile_out}\n${compile_err}")
  endif()

  _hew_execute_capture(
    COMMAND ${ARG_RUN_COMMAND}
    RESULT_VAR run_result
    OUTPUT_VAR run_out
    ERROR_VAR run_err
    TIMEOUT ${ARG_RUN_TIMEOUT}
  )
  if(NOT run_result EQUAL 0)
    message(FATAL_ERROR "${ARG_RUN_FAILURE_MESSAGE} (exit ${run_result}):\n${run_out}\n${run_err}")
  endif()

  _hew_load_expected(expected_text)
  _hew_assert_output_matches("${run_out}" "${expected_text}")
endfunction()

function(hew_run_reject_test)
  cmake_parse_arguments(PARSE_ARGV 0 ARG "" "SUCCESS_MESSAGE" "COMPILE_COMMAND")

  _hew_execute_capture(
    COMMAND ${ARG_COMPILE_COMMAND}
    RESULT_VAR compile_result
    OUTPUT_VAR compile_out
    ERROR_VAR compile_err
  )
  _hew_assert_compile_rejected("${ARG_SUCCESS_MESSAGE}")
endfunction()
