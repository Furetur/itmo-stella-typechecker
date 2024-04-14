from dataclasses import dataclass
from pathlib import Path
import re
from typing import Sequence, Set, Tuple

from .constants import ERROR_REGEX, EXTENTION_REGEX, OK_TESTS_DIR, BAD_TESTS_DIR


@dataclass(frozen=True)
class ErrorResult:
    error_types: Set[str]

    def __str__(self) -> str:
        return str(self.error_types)


@dataclass(frozen=True)
class OkResult:
    def __str__(self) -> str:
        return "OK"


OK_RESULT = OkResult()


type TestResult = ErrorResult | OkResult


@dataclass(frozen=True)
class SingleTest:
    path: Path
    expected_result: TestResult
    extentions: Sequence[str]


def read_test(path: Path, expected_result: TestResult) -> SingleTest:
    text = path.read_text()
    extentions = re.findall(EXTENTION_REGEX, text)
    match expected_result:
        case OkResult():
            pass
        case ErrorResult(error_types):
            additional_error_types = set(re.findall(ERROR_REGEX, text))
            expected_result = ErrorResult(
                error_types=error_types | additional_error_types
            )
    return SingleTest(path=path, expected_result=expected_result, extentions=extentions)


def discover_ok_tests() -> Sequence[SingleTest]:
    paths = OK_TESTS_DIR.iterdir()
    return [read_test(p, OK_RESULT) for p in paths if p.is_file()]


def discover_bad_tests() -> Sequence[SingleTest]:
    def get_tests_for_single_error_type(dir_path: Path) -> Sequence[SingleTest]:
        error_type = dir_path.stem
        return [
            read_test(p, ErrorResult({error_type}))
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
