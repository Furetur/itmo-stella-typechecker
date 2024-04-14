from dataclasses import dataclass
from pathlib import Path
import re
from typing import Sequence, Tuple

from .constants import EXTENTION_REGEX, OK_TESTS_DIR, BAD_TESTS_DIR


@dataclass(frozen=True)
class ErrorResult:
    error_type: str


@dataclass(frozen=True)
class OkResult:
    pass


OK_RESULT = OkResult()


type TestResult = ErrorResult | OkResult


def show_result(result: TestResult) -> str:
    match result:
        case OkResult():
            return "OK"
        case ErrorResult(error_type=t):
            return t


@dataclass(frozen=True)
class SingleTest:
    path: Path
    expected_result: TestResult
    extentions: Sequence[str]


def read_test(path: Path, expected_result: TestResult) -> SingleTest:
    extentions = re.findall(EXTENTION_REGEX, path.read_text())
    return SingleTest(path=path, expected_result=expected_result, extentions=extentions)


def discover_ok_tests() -> Sequence[SingleTest]:
    paths = OK_TESTS_DIR.iterdir()
    return [read_test(p, OK_RESULT) for p in paths if p.is_file()]


def discover_bad_tests() -> Sequence[SingleTest]:
    def get_tests_for_single_error_type(dir_path: Path) -> Sequence[SingleTest]:
        error_type = dir_path.stem
        return [
            read_test(p, ErrorResult(error_type))
            for p in dir_path.iterdir()
            if p.is_file()
        ]

    tests = []
    for dir in BAD_TESTS_DIR.iterdir():
        if not dir.is_dir():
            continue
        tests.extend(get_tests_for_single_error_type(dir))
    return tests


def discover_tests() -> Sequence[SingleTest]:
    return discover_ok_tests() + discover_bad_tests()

def pytest_discover_tests() -> Sequence[Tuple[SingleTest]]:
    return [(t,) for t in discover_tests()]


def get_pytest_test_id(test: Tuple[SingleTest]) -> str:
    return str(test.path)
