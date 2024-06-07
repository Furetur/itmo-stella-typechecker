from dataclasses import dataclass
from pathlib import Path
import re
from typing import Iterable, Sequence, Set, Tuple

from .constants import (
    ERROR_REGEX,
    EXTENTION_REGEX,
    STELLA_FILE_SUFFIXES,
    TEST_SUITE_ROOT_DIR,
)


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


def find_expected_errors_in_path(test_file: Path) -> Set[str]:
    parent_dir_names = (dir.name for dir in test_file.parents if dir.is_dir())
    matches = (re.fullmatch(ERROR_REGEX, name) for name in parent_dir_names)
    return set(match.string for match in matches if match)


def find_expected_errors_in_text(test_file: Path) -> Set[str]:
    text = test_file.read_text()
    return set(re.findall(ERROR_REGEX, text))


def get_expected_result(test_file: Path) -> TestResult:
    expected_error_types = find_expected_errors_in_text(
        test_file
    ) | find_expected_errors_in_path(test_file)
    if expected_error_types:
        return ErrorResult(expected_error_types)
    else:
        return OkResult()


def read_test(path: Path) -> SingleTest:
    text = path.read_text()
    extentions = re.findall(EXTENTION_REGEX, text)
    expected_result = get_expected_result(path)
    match expected_result:
        case OkResult():
            pass
        case ErrorResult(error_types):
            additional_error_types = set(re.findall(ERROR_REGEX, text))
            expected_result = ErrorResult(
                error_types=error_types | additional_error_types
            )
    return SingleTest(path=path, expected_result=expected_result, extentions=extentions)


def discover_stella_files() -> Iterable[Path]:
    def helper(dir: Path) -> Iterable[Path]:
        for path in dir.iterdir():
            if path.is_file() and path.suffix in STELLA_FILE_SUFFIXES:
                yield path
            if path.is_dir():
                yield from helper(path)

    return helper(TEST_SUITE_ROOT_DIR)


def discover_tests() -> Sequence[SingleTest]:
    return [read_test(path) for path in discover_stella_files()]


def pytest_discover_tests() -> Sequence[Tuple[SingleTest]]:
    return [(t,) for t in discover_tests()]


def get_pytest_test_id(test: Tuple[SingleTest]) -> str:
    return str(test.path)
