import os
import re
import subprocess
from typing import Sequence
import pytest

from .discover_tests import OK_RESULT, ErrorResult, TestResult, SingleTest, show_result
from .constants import DEFAULT_EXECUTABLE_PATH, ERROR_PARSE_ERROR, ERROR_REGEX, REQUIRED_EXTENTIONS


EXECUTABLE_PATH = os.environ.get("TYPECHECKER_BINARY")
if EXECUTABLE_PATH is None:
    EXECUTABLE_PATH = DEFAULT_EXECUTABLE_PATH


def run_binary(args: Sequence[str]) -> TestResult:
    result = subprocess.run(
        args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True
    )
    if result.returncode == 0:
        return OK_RESULT
    if "Parse error" in result.stdout:
        return ErrorResult(ERROR_PARSE_ERROR)
    errors = re.findall(ERROR_REGEX, result.stdout)
    assert (
        errors
    ), "Typechecker returned non-zero exit code but error type was not found"
    return ErrorResult(errors[0])


def test_has_optional_extentions(test: SingleTest) -> bool:
    return len(set(test.extentions) - REQUIRED_EXTENTIONS) > 0

def pytest_interpret_result(test: SingleTest, actual_test_result: TestResult) -> None:
    if test_has_optional_extentions(test):
        pytest.skip()
    if test.expected_result != actual_test_result:
        pytest.fail(f"Expected {show_result(test.expected_result)} but got {show_result(actual_test_result)}")


def run_test(test: SingleTest):
    result = run_binary([str(EXECUTABLE_PATH), str(test.path)])
    pytest_interpret_result(test, result)
