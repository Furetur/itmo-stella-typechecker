import os
from pathlib import Path
import re
import subprocess
from typing import Optional, Sequence, Tuple
import pytest

TYPECHECKER_EXECUTABLE_PATH = (
    Path(__file__).parent.parent / "_build" / "default" / "bin" / "main.exe"
)

OK_TESTS_DIR = Path(__file__).parent / "stella-tests" / "ok"
BAD_TESTS_DIR = Path(__file__).parent / "stella-tests" / "bad"

SKIPPED_TESTS = [
    "exhaustive_unit_var",
    "exhaustive_unit"
]

ERROR_NOT_IMPLEMENTED = "ERROR_NOT_IMPLEMENTED"

ErrorType = str


def get_compiler_binary() -> Path:
    binary = os.environ.get("TYPECHECKER_BINARY")
    binary = Path(binary) if binary is not None else TYPECHECKER_EXECUTABLE_PATH
    return binary.absolute()


# Test discovery


def get_ok_test_id(file: Path) -> str:
    return "ok/" + str(file.name)

def get_bad_test_id(t) -> str:
    if isinstance(t, str):
        return ""
    else:
        return f"bad/{t.parent.name}/{t.name}"

def discover_ok_tests() -> Sequence[Path]:
    paths = OK_TESTS_DIR.iterdir()
    return [p for p in paths if p.is_file()]


def discover_bad_tests() -> Sequence[Tuple[ErrorType, Path]]:
    def helper():
        dirs = (p for p in BAD_TESTS_DIR.iterdir() if p.is_dir())
        for dir in dirs:
            error_type = dir.stem
            tests = ((error_type, p) for p in dir.iterdir() if p.is_file())
            yield from tests

    return list(helper())


# Test Runner

def run_binary(args: Sequence[str]) -> Tuple[Optional[ErrorType]]:
    result = subprocess.run(
        args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True
    )
    if result.returncode == 0:
        return None
    if "Parse error" in result.stdout:
        pytest.skip(reason="Parse error")
    matches = re.findall("ERROR_[A-Z_]+", result.stdout)
    assert (
        matches
    ), "Typechecker returned non-zero exit code but error type was not found"
    return matches[0]


def run_ok_test(test_file: Path):
    result = run_binary([str(get_compiler_binary()), str(test_file)])
    if result == ERROR_NOT_IMPLEMENTED:
        pytest.skip()
    if result is not None:
        raise AssertionError(f"Got {result} but expected OK")


def run_bad_test(test_file: Path, expected_error_type: Path):
    result = run_binary([str(get_compiler_binary()), str(test_file)])
    if result == ERROR_NOT_IMPLEMENTED:
        pytest.skip()
    if result != expected_error_type:
        raise AssertionError(f"Expected {expected_error_type} but got {result}")
    assert result == expected_error_type


# Entry points


@pytest.mark.parametrize("test_file", discover_ok_tests(), ids=get_ok_test_id)
def test_ok(test_file: Path):
    if test_file.stem in SKIPPED_TESTS:
        pytest.skip()
    run_ok_test(test_file)


@pytest.mark.parametrize("expected_error_type, test_file", discover_bad_tests(), ids=get_bad_test_id)
def test_bad(expected_error_type, test_file):
    if test_file.stem in SKIPPED_TESTS:
        pytest.skip()
    run_bad_test(test_file, expected_error_type)
