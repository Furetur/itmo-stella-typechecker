import os
import re
import subprocess
from typing import Sequence, Set
import pytest

from .discover_tests import OK_RESULT, ErrorResult, OkResult, TestResult, SingleTest
from .constants import (
    DEFAULT_EXECUTABLE_PATH,
    ERROR_PARSE_ERROR,
    ERROR_REGEX,
    REQUIRED_EXTENTIONS,
)


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
        return ErrorResult({ERROR_PARSE_ERROR})
    errors = re.findall(ERROR_REGEX, result.stdout)
    assert (
        errors
    ), "Typechecker returned non-zero exit code but error type was not found"
    return ErrorResult(set(errors))


def test_get_optional_extentions(test: SingleTest) -> Set[str]:
    return set(test.extentions) - REQUIRED_EXTENTIONS


def pytest_interpret_result(test: SingleTest, actual_test_result: TestResult) -> None:
    optional_exts = test_get_optional_extentions(test)
    if optional_exts:
        pytest.skip(reason=f"Optional extentions: {optional_exts}")
    match (test.expected_result, actual_test_result):
        case (OkResult(), OkResult()):
            pass
        case (ErrorResult(expected), ErrorResult(actual)) if actual.issubset(expected):
            pass
        case (ErrorResult(expected), ErrorResult(actual)):
            pytest.fail(f"Expected one of {expected} but got {actual}")
        case _:
            pytest.fail(f"Expected {test.expected_result} but got {actual_test_result}")


def run_test(test: SingleTest):
    result = run_binary([str(EXECUTABLE_PATH), str(test.path)])
    pytest_interpret_result(test, result)
